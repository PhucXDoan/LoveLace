/// <reference path="./../ts/LoveLace.ts"/>"

/* WireFrame
	- Basic 3D environment with a camera
	- Rotation and translation supported
	- Projection of points and lines
	- Usage of pointer locking
*/

/********************************************************************************************************************************/
// Settings //

const REFRESH_RATE     = 15     // -- Milliseconds until another render is performed
const ASPECT_RATIO     = 16 / 9 // -- Quotient of the canvas' width and height
const RESIZE_SPEED     = 0.25   // -- LERPing proportion when resizing
const RESIZE_THRESHOLD = 1      // -- Maximum scalar difference before a resizing event occurs

/********************************************************************************************************************************/
// Constants //

const CUBE_VERTICES =
	List (
		Vector3 (-0.5,  0.5,  0.5),
		Vector3 (-0.5,  0.5, -0.5),
		Vector3 (-0.5, -0.5,  0.5),
		Vector3 (-0.5, -0.5, -0.5),
		Vector3 ( 0.5,  0.5,  0.5),
		Vector3 ( 0.5,  0.5, -0.5),
		Vector3 ( 0.5, -0.5,  0.5),
		Vector3 ( 0.5, -0.5, -0.5)
	)

const CUBE_EDGES =
	List (
		Pair (0, 1),
		Pair (0, 2),
		Pair (0, 4),
		Pair (1, 3),
		Pair (1, 5),
		Pair (2, 6),
		Pair (2, 3),
		Pair (3, 7),
		Pair (4, 5),
		Pair (4, 6),
		Pair (5, 7),
		Pair (6, 7)
	)
	.fmap (fboth (from (CUBE_VERTICES)))

/********************************************************************************************************************************/
// Definition of Algebraic Data Types //

type Global =
	{
		CONS : 'Global'

		/**` (.time) :: Global -> Number `*/
		time : number

		/**` (.refreshTime) :: Global -> Number `*/
		refreshTime : number

		/**` (.isRefresh) :: Global -> Boolean `*/
		isRefresh : boolean

		/**` (.scalar) :: Global -> Number `*/
		scalar : number

		/**` (.isResizing) :: Global -> Boolean `*/
		isResizing : boolean

		/**` (.isPointerLocked) :: Global -> Boolean `*/
		isPointerLocked : boolean
	}

type Local =
	{
		CONS : 'State1'

		/**` (.camera) :: Local -> Camera `*/
		camera : Camera
	}

type Camera =
	{
		CONS : 'Camera'

		/**` (.position) :: Camera -> Vector3 `*/
		position : Vector3

		/**` (.rotation) :: Camera -> Vectir3 `*/
		rotation : Vector3

		/**` (.fov) :: Camera -> Number `*/
		fov : number

		/**` (.positionSpeed) :: Camera -> Number `*/
		positionSpeed : number

		/**` (.Speed) :: Camera -> Number `*/
		rotationSpeed : number
	}

/********************************************************************************************************************************/
// Implementation of Algebraic Data Type Constructors //

/**` Global :: { ... } -> Global `*/
const Global = (record : Rec <Global>) : Global => ({ CONS : 'Global', ...record })

/**` State1 :: { ... } -> Local `*/
const State1 = (record : Rec <Local>) : Local => ({ CONS : 'State1', ...record })

/**` Camera :: { ... } -> Camera `*/
const Camera = (record : Rec <Camera>) : Camera => ({ CONS : 'Camera', ...record })

/********************************************************************************************************************************/
// Implementation of Micro-Functions //

// -- Updates the global state
/**` iterateGlobal :: Global -> IO Global `*/
const iterateGlobal = (global : Global) : IO <Global> =>
	Do.IO
		/**` $.present :: Number `*/
		.bindto ('present', _ => Input.time)

		/**` $.refreshTime :: Number `*/
		.fmapto ('refreshTime', $ => (global.refreshTime > REFRESH_RATE ? 0 : global.refreshTime) + $.present - global.time)

		/**` $.maxScalar :: Number `*/
		.bindto ('maxScalar', _ => maximumCanvasScalar)

		/**` $.isResized :: Boolean `*/
		.bindto ('isResized', _ => Input.isWindowResized)

		/**` $.isPointerLocked :: Boolean `*/
		.bindto ('isPointerLocked', _ => Input.isPointerLocked)

		/**` $.isResizing :: Boolean `*/
		.fmapto ('isResizing', $ => ($.isResized || global.isResizing) && diff (global.scalar) ($.maxScalar) > RESIZE_THRESHOLD)

		.fmap
		( $ =>
			Global
			({
				time            : $.present,
				refreshTime     : $.refreshTime,
				isRefresh       : $.refreshTime > REFRESH_RATE,
				scalar          : $.isResizing ? lerp (RESIZE_SPEED) (global.scalar) ($.maxScalar) : global.scalar,
				isResizing      : $.isResizing,
				isPointerLocked : $.isPointerLocked
			})
		)

// -- Calculates the maximum scalar for the canvas
/**` maximumCanvasScalar :: IO Number `*/
const maximumCanvasScalar : IO <number> =
	Do.IO
		/**` $.windowDimensions :: Pair Number Number `*/
		.bindto ('windowDimensions', _ => Input.windowP)

		.fmap ($ => min ($.windowDimensions .fst) ($.windowDimensions .snd * ASPECT_RATIO))

// -- Scales the canvas uniformly
/**` scaleCanvas :: Number -> IO () `*/
const scaleCanvas = (scalar : number) : IO <null> =>
	Reput.canvasWH (scalar) (scalar / ASPECT_RATIO)

// -- Projects a 3D vector to a maybe 2D vector; returns 'Nothing' if it gets culled
/**` projectV3 :: Camera -> Vector3 -> Maybe Vector2 `*/
const projectV3 = (camera : Camera) => (point : Vector3) : Maybe <Vector2> =>
	point

		// -- Translates so the camera is in the origin
		.pipe (V3.sub (camera.position))

		// -- Rotate the scene using YXZ rotation
		.pipe (V3.rotateYXZv (camera.rotation))

		// -- Cull points that are behind the camera
		.pipe (ensure (v => v.z > 0))

		// -- Apply the projection
		.fmap (v => V2.demoteV3 (v) .pipe (V2.unscale (-v.z * tan (camera.fov))))

		// -- Adjust for the aspect ratio
		.fmap (V2.scaleY (ASPECT_RATIO))

		// -- Cull points out of view
		.bind (ensure (v => abs (v.x) < 0.5 && abs (v.y) < 0.5))

		// -- Flip image upside down
		.fmap (V2.conjugate)

		// -- Make projected point relative to the canvas' origin
		.fmap (V2.add (V2.half))

/********************************************************************************************************************************/
// Main //

/**` main :: IO () `*/
const main : IO <null> =
	Do.IO
		/**` $.present :: Number `*/
		.bindto ('present', _ => Input.time)

		/**` $.maxScalar :: Number `*/
		.bindto ('maxScalar', _ => maximumCanvasScalar)

		// -- Requests the browser to lock the mouse
		.side (Output.activatePointerLock)

		// -- Maximizes the canvas
		.also ($ => scaleCanvas ($.maxScalar))

		// -- Starts the project with initial states
		.bind ($ =>
			loop
			(
				Global
				({
					time            : $.present,
					refreshTime     : 0,
					isRefresh       : false,
					scalar          : $.maxScalar,
					isResizing      : false,
					isPointerLocked : false
				})
			)(
				State1
				({
					camera : Camera
						({
							position      : Vector3 (0, 0, 5),
							rotation      : V3.origin,
							fov           : 1,
							positionSpeed : 0.1,
							rotationSpeed : 0.001
						})
				})
			)
		)

/**` loop :: Global -> Local -> IO () `*/
const loop = (global : Global) => (local : Local) : IO <null> =>
	Do.IO
		/**` $.nextGlobal :: Global `*/
		.bindto ('nextGlobal', _ => iterateGlobal (global))

		/**` $.nextLocal :: Global `*/
		.bindto ('nextLocal', $ => update ($.nextGlobal) (local))

		// -- Resizes the canvas
		.also ($ =>
			$.nextGlobal.isRefresh && $.nextGlobal.isResizing
				? scaleCanvas ($.nextGlobal.scalar)
				: idle
		)

		// -- Renders to the canvas
		.also ($ =>
			$.nextGlobal.isRefresh
				? render ($.nextGlobal) ($.nextLocal)
				: idle
		)

		// -- Reset event values
		.side (Output.tick)

		// -- Queue for the next refresh
		.bind ($ => Output.queue (loop ($.nextGlobal) ($.nextLocal)))

/**` render :: Global -> Local -> IO () `*/
const render = (global : Global) => (local : Local) : IO <null> =>
	Output.clearCanvas
		.then (Output.beginPath)
		.side (Reput.Norm.lineThickness (0.005))

		// -- Renders the vertices of the cube
		.then (
			CUBE_VERTICES
				.fmap (projectV3 (local.camera))
				.pipe (fromMaybes)
				.fmap (Output.Norm.fillCircleV (0.01))
				.fmap (Output.beginPath .then)
				.pipe (executeIOs)
		)

		// -- Renders the edges of the cube
		.then (
			CUBE_EDGES
				.fmap (fboth (projectV3 (local.camera)))
				.fmap (pairMaybes)
				.pipe (fromMaybes)
				.fmap (uncurry (Output.Norm.lineV))
				.pipe (executeIOs)
				.pipe (Output.beginPath .then)
				.then (Output.stroke)
		)

/**` update :: Global -> Local -> IO Local `*/
const update = (global : Global) => (local : Local) : IO <Local> =>
	global.isRefresh && global.isPointerLocked
		? Do.IO
			.bindto ('keyQState', _ => Input.keyboard ('KeyQ') .fmap (isD) .fmap (BIT))
			.bindto ('keyEState', _ => Input.keyboard ('KeyE') .fmap (isD) .fmap (BIT))
			.fmapto ('rollDelta', $ => $.keyEState - $.keyQState)

			// -- Camera's delta rotation based on mouse
			.bindto ('rotation', $ =>
				Input.mouseDV
					.fmap (v => Vector3 (v.y, -v.x, $.rollDelta * 10))
					.fmap (V3.scale (local.camera.rotationSpeed))
					.fmap (V3.add (local.camera.rotation))
			)

			// -- Camera's movement in space relative to the camera's view
			.bindto ('direction', _ =>
				Input.wasdY
					.fmap (V3.unrotateYXZv (local.camera.rotation))
					.fmap (V3.scale (local.camera.positionSpeed))
			)

			// -- Returns the new local state
			.fmap ($ =>
				State1
				({
					camera : Camera
						({
							position      : V3.add (local.camera.position) ($.direction),
							rotation      : $.rotation,
							fov           : local.camera.fov,
							positionSpeed : local.camera.positionSpeed,
							rotationSpeed : local.camera.rotationSpeed
						})
				})
			)
		: unit.IO (local)
