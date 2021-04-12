/**` ASPECT_RATIO :: Number `*/
const ASPECT_RATIO = 16 / 9

/********************************************************************************************************************************/

const cubeVertices =
	List
	(
		Vector3D (-0.5) (-0.5) (-0.5),
		Vector3D ( 0.5) (-0.5) (-0.5),
		Vector3D (-0.5) ( 0.5) (-0.5),
		Vector3D ( 0.5) ( 0.5) (-0.5),
		Vector3D (-0.5) (-0.5) ( 0.5),
		Vector3D ( 0.5) (-0.5) ( 0.5),
		Vector3D (-0.5) ( 0.5) ( 0.5),
		Vector3D ( 0.5) ( 0.5) ( 0.5)
	)

/********************************************************************************************************************************/

/**` Camera (Pipeable) `*/
type Camera =
	{
		CONS : 'Camera'

		/**` (.pipe) :: Camera -> (Camera -> a) -> a `*/
		pipe : <a>(morphism : (camera : Camera) => a) => a

		/**` (.position) :: Camera -> Vector3D `*/
		position : Vector3D
	}

/**` Camera :: { ... } -> Camera `*/
const Camera =
	(record : {
		position : Vector3D
	}) : Camera =>
	({
		CONS : 'Camera',
		get pipe() { return (f : any) => f (this) },
		...record
	})

/********************************************************************************************************************************/

/**` Program (Pipeable) `*/
type Program =
	{
		CONS : 'ProgramInitial'

		/**` (.pipe) :: Program -> (Program -> a) -> a `*/
		pipe : <a>(morphism : (program : Program) => a) => a

		/**` (.time) :: Program -> Number `*/
		time : number

		/**` (.camera) :: Program -> Camera `*/
		camera : Camera
	}

/**` ProgramInitial :: { ... } -> Program `*/
const ProgramInitial =
	(record : {
		time   : number
		camera : Camera
	}) : Program =>
	({
		CONS : 'ProgramInitial',
		get pipe() { return (f : any) => f (this) },
		...record
	})

/********************************************************************************************************************************/

/**` fetchMaxCanvasScalar :: IO Number `*/
const fetchMaxCanvasScalar =
	Import.windowDimensions
		.fmap ( fsnd (mul (ASPECT_RATIO)) )
		.fmap ( uncurry (min) )

/**` setCanvasScalar :: Number -> IO () `*/
const setCanvasScalar = (scalar : number) => Mutate.canvasDimensions (scalar) (scalar / ASPECT_RATIO)

/**` projectVector3D :: Camera -> Vector3D -> Vector2D `*/
const projectVector3D = (camera : Camera) => (v : Vector3D) : Vector2D =>
	Vector2D
		(0.5 - (v.x - camera.position.x) / (v.z - camera.position.z))
		(0.5 + (v.y - camera.position.y) / (v.z - camera.position.z) * ASPECT_RATIO)

/********************************************************************************************************************************/

/**` main :: IO () `*/
const main : IO<null> =
	Do.IO
		/**` $.present :: Number `*/
		.bindto ('present') ( _ => Import.timeSinceOpen )

		/**` $.maxCanvasScalar :: Number `*/
		.bindto ('maxCanvasScalar') ( _ => fetchMaxCanvasScalar )

		// -- Maximizes the canvas.
		.also ( $ => uncurry (Mutate.canvasDimensions) (Pair ($.maxCanvasScalar, $.maxCanvasScalar / ASPECT_RATIO)) )

		// -- Starts the loop with initial values.
		.bind
		( $ =>
			loop
			(
				Core
				({
					time         : $.present,
					refreshTime  : 0,
					canvasScalar : $.maxCanvasScalar,
					isRefresh    : false,
					isResizing   : false
				})
			)(
				ProgramInitial
				({
					time   : $.present,
					camera :
						Camera
						({
							position : Vector3D (0) (0) (0)
						})
				})
			)
		)

/**` loop :: Core -> Program -> IO () `*/
const loop = (core : Core) => (program : Program) : IO<null> =>
	Do.IO
		/**` $.updatedCore :: Core `*/
		.bindto ('updatedCore') ( _ => updateCore (core) )

		/**` $.updatedProgram :: Program `*/
		.bindto ('updatedProgram') ( $ => ($.updatedCore.isRefresh ? updateProgram ($.updatedCore) : unit.IO) (program) )

		// -- Resizes the canvas if needed.
		.also ( $ => when ($.updatedCore.isResizing && $.updatedCore.isRefresh) (setCanvasScalar ($.updatedCore.canvasScalar)) )

		// -- Draws the current frame.
		.also ( $ => draw ($.updatedCore) ($.updatedProgram) )

		// -- Reset event values.
		.side ( Effect.tick )

		// -- Queue for the next refresh call.
		.bind ( $ => Effect.queue (loop ($.updatedCore) ($.updatedProgram)) )

/**` draw :: Core -> Program -> IO () `*/
const draw = (core : Core) => (program : Program) : IO<null> =>
	Effect.clearCanvas
		.then
		(
			cubeVertices
				.fmap (projectVector3D (program.camera))
				.fmap (Effect.Norm.fillCircleVector (0.01))
				.fmap (Effect.beginPath.then)
				.pipe (executeIOs)
		)

/**` updateProgram :: Core -> Program -> IO Program `*/
const updateProgram = (core : Core) => (program : Program) : IO<Program> =>
	Do.IO
		/**` $.(keyW|keyA|keyS|keyD|space|lctrl) :: Vertical `*/
		.bindto ('keyW')  ( _ => Import.keyboardKey ('KeyW') )
		.bindto ('keyA')  ( _ => Import.keyboardKey ('KeyA') )
		.bindto ('keyS')  ( _ => Import.keyboardKey ('KeyS') )
		.bindto ('keyD')  ( _ => Import.keyboardKey ('KeyD') )
		.bindto ('space') ( _ => Import.keyboardKey ('Space') )
		.bindto ('lctrl') ( _ => Import.keyboardKey ('ControlLeft') )

		/**` $.cameraMovement :: Vector3D `*/
		.fmapto ('cameraMovement')
		( $ =>
			Vector3D (0) (0) (0)
				.pipe ( translate3D (isDown ($.keyA) ? -1 : 0) (isDown ($.lctrl) ? -1 : 0) (isDown ($.keyW) ? -1 : 0) )
				.pipe ( translate3D (isDown ($.keyD) ?  1 : 0) (isDown ($.space) ?  1 : 0) (isDown ($.keyS) ?  1 : 0) )
				.pipe ( rescale3D (0.1) )
		)

		.fmap
		( $ =>
			ProgramInitial
			({
				time   : core.time,
				camera :
					Camera
					({
						position : translateVector3D ($.cameraMovement) (program.camera.position)
					})
			})
		)
