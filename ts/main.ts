/* Boilerplate
	- Canvas has a fixed aspect ratio
	- Canvas smoothly scales to the maximum size
	- Project is controlled by a fixed refresh rate
	- Basic global and local state is defined
*/

/********************************************************************************************************************************/
// Settings //

const REFRESH_RATE     = 15     // -- Milliseconds until another render is performed
const ASPECT_RATIO     = 16 / 9 // -- Quotient of the canvas' width and height
const RESIZE_SPEED     = 0.25   // -- LERPing proportion when resizing
const RESIZE_THRESHOLD = 1      // -- Maximum scalar difference before a resizing event occurs

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
	}

type Local =
	{
		CONS : 'State1'
	}

/********************************************************************************************************************************/
// Implementation of Algebraic Data Type Constructors //

/**` Global :: { ... } -> Global `*/
const Global = (record : Rec <Global>) : Global => ({ CONS : 'Global', ...record })

/**` State1 :: Local `*/
const State1 : Local =
	{
		CONS : 'State1'
	}

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

		/**` $.isResizing :: Boolean `*/
		.fmapto ('isResizing', $ => ($.isResized || global.isResizing) && diff (global.scalar) ($.maxScalar) > RESIZE_THRESHOLD)

		.fmap
		( $ =>
			Global
			({
				time        : $.present,
				refreshTime : $.refreshTime,
				isRefresh   : $.refreshTime > REFRESH_RATE,
				scalar      : $.isResizing ? lerp (RESIZE_SPEED) (global.scalar) ($.maxScalar) : global.scalar,
				isResizing  : $.isResizing
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

/********************************************************************************************************************************/
// Main //

/**` main :: IO () `*/
const main : IO <null> =
	Do.IO
		/**` $.present :: Number `*/
		.bindto ('present', _ => Input.time)

		/**` $.maxScalar :: Number `*/
		.bindto ('maxScalar', _ => maximumCanvasScalar)

		// -- Maximizes the canvas
		.also ($ => scaleCanvas ($.maxScalar))

		// -- Starts the project with initial states
		.bind ($ =>
			loop
			(
				Global
				({
					time        : $.present,
					refreshTime : 0,
					isRefresh   : false,
					scalar      : $.maxScalar,
					isResizing  : false
				})
			)(
				State1
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

		.side (Output.tick)
		.bind ($ => Output.queue (loop ($.nextGlobal) ($.nextLocal)))

/**` render :: Global -> Local -> IO () `*/
const render = (global : Global) => (local : Local) : IO <null> =>
	idle

/**` update :: Global -> Local -> IO Local `*/
const update = (global : Global) => (local : Local) : IO <Local> =>
	unit.IO (local)
