/**` UPDATE_RATE :: Number`*/
const UPDATE_RATE : number = 15

/**` RENDER_RATE :: Number`*/
const RENDER_RATE : number = 15

/**` ASPECT_RATIO :: Number `*/
const ASPECT_RATIO : number = 9 / 16

/**` RESIZE_SPEED :: Number `*/
const RESIZE_SPEED : number = 0.25

/********************************************************************************************************************************/

/** Stores important data and state throughout the program. */
type Core =
	{
		readonly CONS : 'Core'

		/**` (Core).time :: Number `*/
		readonly time : number

		/**` (Core).updatecounter :: Number `*/
		readonly updatecounter : number

		/**` (Core).rendercounter :: Number `*/
		readonly rendercounter : number

		/**` (Core).isUpdate :: Boolean `*/
		readonly isUpdate : boolean

		/**` (Core).isRender :: Boolean `*/
		readonly isRender : boolean

		/**` (Core).isCanvasResizing :: Boolean `*/
		readonly isCanvasResizing : boolean
	}

/** Represents the different states of the program. */
type Program =
	{
		readonly CONS : 'ProgramA'
	} | {
		readonly CONS : 'ProgramB'
	}

/********************************************************************************************************************************/

/**
 * ```
 * Core :: {
 * 	time             :: Number
 * 	updatecounter    :: Number
 * 	rendercounter    :: Number
 * 	isCanvasResizing :: Boolean
 * } -> Core
 * ```
 */
const Core =
	(record : {
		time             : number
		updatecounter    : number
		rendercounter    : number
		isCanvasResizing : boolean
	}) : Core =>
	({
		CONS     : 'Core',
		isUpdate : record.updatecounter > UPDATE_RATE,
		isRender : record.rendercounter > RENDER_RATE,
		...record
	})

/**` updateCore :: Core -> IO Core `*/
const updateCore = (core : Core) : IO<Core> =>
	Do.IO
		/**` $.timeSinceOpen :: Number `*/
		.bindto ('timeSinceOpen') ( _ => Import.timeSinceOpen )

		/**` $.windowDimensions :: (Number, Number) `*/
		.bindto ('windowDimensions') ( _ => Import.windowDimensions )

		/**` $.canvasDimensions :: (Number, Number) `*/
		.bindto ('canvasDimensions') ( _ => Import.canvasDimensions )

		/**` $.isWindowResized :: Boolean `*/
		.bindto ('isWindowResized') ( _ => Import.isWindowResized )

		.fmap
		( $ =>
			Core
			({
				time             : $.timeSinceOpen,
				updatecounter    : (core.updatecounter > UPDATE_RATE ? 0 : core.updatecounter) + $.timeSinceOpen - core.time,
				rendercounter    : (core.rendercounter > RENDER_RATE ? 0 : core.rendercounter) + $.timeSinceOpen - core.time,
				isCanvasResizing :
					(core.isCanvasResizing || $.isWindowResized) &&
					(f => napprox (f ($.windowDimensions)) (f ($.canvasDimensions)) (4))
					(pick (fst ($.windowDimensions) < snd ($.windowDimensions) / ASPECT_RATIO))
			})
		)

/********************************************************************************************************************************/

/**` ProgramA :: Program `*/
const ProgramA : Program =
	{
		CONS : 'ProgramA'
	}

/**` ProgramB :: Program `*/
const ProgramB : Program =
	{
		CONS : 'ProgramB'
	}
