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
					time : $.present
				})
			)
		)

/**` loop :: Core -> Program -> IO () `*/
const loop = (core : Core) => (program : Program) : IO<null> =>
	Do.IO
		/**` $.updatedCore :: Core `*/
		.bindto ('updatedCore') ( _ => updateCore (core) )

		/**` $.updatedProgram :: Program `*/
		.bindto ('updatedProgram') ( _ => updateProgram (program) )

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
	idle

/**` updateProgram :: Program -> IO Program `*/
const updateProgram = (program : Program) : IO<Program> =>
	unit.IO (program)
