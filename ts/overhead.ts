/**` ASPECT_RATIO :: Pair Number Number `*/
const ASPECT_RATIO = Pair (16, 9)

/**` RESIZING_THRESHOLD :: Number `*/
const RESIZING_THRESHOLD = 1

/**` RESIZING_SPEED :: Number `*/
const RESIZING_SPEED = 0.1

/**` REFRESH_TIME :: Number `*/
const REFRESH_TIME = 15

/********************************************************************************************************************************/

/**` Core (Pipeable) `*/
type Core =
	{
		CONS : 'Core'

		/**` (.pipe) :: Core -> (Core -> a) -> a `*/
		pipe : <a>(morphism : (core : Core) => a) => a

		/**` (.time) :: Core -> Number `*/
		time : number

		/**` (.refreshTime) :: Core -> Number `*/
		refreshTime : number

		/**` (.canvasScalar) :: Core -> Number `*/
		canvasScalar : number

		/**` (.isRefresh) :: Core -> Boolean `*/
		isRefresh : boolean

		/**` (.isResizing) :: Core -> Boolean `*/
		isResizing : boolean
	}

/**` Core :: { ... } -> Core `*/
const Core =
	(
		{ time, refreshTime, canvasScalar, isRefresh, isResizing } :
		{
			time         : number
			refreshTime  : number
			canvasScalar : number
			isRefresh    : boolean
			isResizing   : boolean
		}
	) : Core =>
	({
		CONS : 'Core',
		get pipe() { return (f : any) => f (this) },
		time, refreshTime, canvasScalar, isRefresh, isResizing
	})

/**` updateCore :: Core -> IO Core `*/
const updateCore = (core : Core) =>
	Do.IO
		/**` $.present :: Number */
		.bindto ('present') ( _ => Import.timeSinceOpen )

		/**` $.maxCanvasScalar :: Number `*/
		.bindto ('maxCanvasScalar') ( _ => fetchMaxCanvasScalar )

		/**` $.isResizing :: Boolean `*/
		.bindto ('isResizing')
		( $ =>
			Import.isWindowResized
				.fmap (b => napprox (core.canvasScalar) ($.maxCanvasScalar) (RESIZING_THRESHOLD) && (core.isResizing || b))
		)

		/**` $.refreshTime :: Number `*/
		.fmapto ('refreshTime') ( $ => (REFRESH_TIME < core.refreshTime ? 0 : core.refreshTime) + $.present - core.time )

		.fmap
		( $ =>
			Core
			({
				time         : $.present,
				isResizing   : $.isResizing,
				refreshTime  : $.refreshTime,
				isRefresh    : $.refreshTime > REFRESH_TIME,
				canvasScalar :
					$.isResizing && $.refreshTime > REFRESH_TIME
						? lerp (RESIZING_SPEED) (core.canvasScalar) ($.maxCanvasScalar)
						: core.canvasScalar
			})
		)

/********************************************************************************************************************************/

/**` Program (Pipeable) `*/
type Program =
	{
		CONS : 'ProgramInitial'

		/**` (.pipe) :: Program -> (Program -> a) -> a `*/
		pipe : <a>(morphism : (program : Program) => a) => a

		/**` (.time) :: Program -> Number `*/
		time : number
	}

/**` ProgramInitial :: { ... } -> Program `*/
const ProgramInitial =
	(
		{ time } :
		{
			time : number
		}
	) : Program =>
	({
		CONS : 'ProgramInitial',
		get pipe() { return (f : any) => f (this) },
		time
	})

/********************************************************************************************************************************/

/**` fetchMaxCanvasScalar :: IO Number `*/
const fetchMaxCanvasScalar = Import.windowDimensions .fmap (p => uncurry (min) (fpair (div) (p) (ASPECT_RATIO)))

/**` setCanvasScalar :: Number -> IO () `*/
const setCanvasScalar = (scalar : number) => uncurry (Mutate.canvasDimensions) (both (mul (scalar)) (ASPECT_RATIO))
