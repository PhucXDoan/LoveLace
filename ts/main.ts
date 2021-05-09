/********************************************************************************************************************************/
// Constants and Settings  //

namespace std
{
	/**` std.refreshRate : Number `*/
	export const refreshRate : number = 15

	/**` std.aspectRatio : Number `*/
	export const aspectRatio : number = 16 / 9

	/**` std.resizeThreshold : Number `*/
	export const resizeThreshold : number = 5

	/**` std.resizeSpeed : Number `*/
	export const resizeSpeed : number = 0.15
}

/********************************************************************************************************************************/
// Definition of Algebraic Data Types //

type Global =
	{
		variation   : 'Global'
		time        : number
		deltaTime   : number
		refreshTime : number
		canvasWidth : number
		isRefresh   : boolean
		isResizing  : boolean
	}

type Local =
	{
		variation : 'Local'
	}

/********************************************************************************************************************************/
// Implementation of Algebraic Data Type Constructors //

/**` Global : { ... } -> Global `*/
const Global =
	(record : {
		time        : number
		deltaTime   : number
		refreshTime : number
		canvasWidth : number
		isRefresh   : boolean
		isResizing  : boolean
	}) : Global => ({ variation : 'Global', ...record })

/**` Local : { ... } -> Local `*/
const Local =
	(record : {

	}) : Local => ({ variation : 'Local', ...record })

/********************************************************************************************************************************/
// Implementation of Micro-Functions //

/********************************************************************************************************************************/
// Main and Loop //

/**` main : IO () `*/
const main : IO <null> =
	Do.IO
		/**` $.present : Number `*/
		.bindto ('present') <number>
			(_ => I.time)

		/**` $.idealCanvasWidth : Number `*/
		.bindto ('idealCanvasWidth') <number>
			(_ =>
				I.windowWH
					.fmap (fsnd (mul (std.aspectRatio)))
					.fmap (uncurry (min))
			)

		.also ($ => O.setCanvasWH ($.idealCanvasWidth) ($.idealCanvasWidth / std.aspectRatio))

		.bind ($ =>
			loop
				(
					Global
					({
						time        : $.present,
						deltaTime   : 0,
						refreshTime : 0,
						canvasWidth : $.idealCanvasWidth,
						isRefresh   : false,
						isResizing  : false
					})
				)(
					Local
					({
					})
				)
		)

/**` loop : Global -> Local -> IO () `*/
const loop = (global : Global) => (local : Local) : IO <null> =>
	Do.IO
		/**` $.present : Number `*/
		.bindto ('present') <number>
			(_ => I.time)

		/**` $.isWindowResized : boolean `*/
		.bindto ('isWindowResized') <boolean>
			(_ => I.isWindowResized)

		/**` $.maxCanvasWidth : Number `*/
		.bindto ('maxCanvasWidth') <number>
			(_ =>
				I.windowWH
					.fmap (fsnd (mul (std.aspectRatio)))
					.fmap (uncurry (min))
			)

		/**` $.nextDeltaTime : Number `*/
		.fmapto ('nextDeltaTime') <number>
			($ => $.present - global.time)

		/**` $.nextRefreshTime : Number `*/
		.fmapto ('nextRefreshTime') <number>
			($ => (global.isRefresh ? 0 : global.refreshTime) + $.nextDeltaTime)

		/**` $.nextIsRefresh : Boolean `*/
		.fmapto ('nextIsRefresh') <boolean>
			($ => $.nextRefreshTime > std.refreshRate)

		/**` $.nextIsResizing : Boolean `*/
		.fmapto ('nextIsResizing') <boolean>
			($ => ($.isWindowResized || global.isResizing) && diff ($.maxCanvasWidth) (global.canvasWidth) > std.resizeThreshold)

		/**` $.nextGlobal : Global `*/
		.fmapto ('nextGlobal') <Global>
			($ =>
				Global
				({
					time        : $.present,
					deltaTime   : $.nextDeltaTime,
					refreshTime : $.nextRefreshTime,
					canvasWidth :
						$.nextIsResizing && $.nextIsRefresh
							? lerp (std.resizeSpeed) (global.canvasWidth) ($.maxCanvasWidth)
							: global.canvasWidth,
					isRefresh   : $.nextIsRefresh,
					isResizing  : $.nextIsResizing
				})
			)

		/**` $.nextLocal : Local `*/
		.bindto ('nextLocal') <Local>
			($ => update ($.nextGlobal) (local))

		.also ($ =>
			$.nextIsResizing && $.nextIsRefresh
				? O.setCanvasWH ($.nextGlobal.canvasWidth) ($.nextGlobal.canvasWidth / std.aspectRatio)
				: idle
		)
		.also ($ => render ($.nextGlobal) ($.nextLocal))
		.side (O.resetState)
		.bind ($ => O.queue (loop ($.nextGlobal) ($.nextLocal)))

/********************************************************************************************************************************/
// Update and Render //

/**` update : Global -> Local -> IO Local `*/
const update = (global : Global) => (local : Local) : IO <Local> =>
	send (local)

/**` render : Global -> Local -> IO () `*/
const render = (global : Global) => (local : Local) : IO <null> =>
	idle
