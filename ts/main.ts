/**` fetchMaxCanvasScalar :: IO Number `*/
const fetchMaxCanvasScalar : IO<number> =
	Import.windowDimensions.fmap (p => min (fst (p)) (snd (p) / ASPECT_RATIO))

/**` scaleCanvas :: Number -> IO () `*/
const scaleCanvas = (scalar : number) : IO<null> =>
	Mutate.canvasDimensions (scalar) (scalar * ASPECT_RATIO)

/********************************************************************************************************************************/

/**` main :: IO () `*/
const main : IO<null> =
	Do.IO
		/**` $.timeSinceOpen :: Number `*/
		.bindto ('timeSinceOpen') ( _ => Import.timeSinceOpen )

		// -- Resizes the canvas to fit the window.
		.side   ( _ => fetchMaxCanvasScalar.bind (scaleCanvas) )

		// -- Executes the program loop.
		.bind
		( $ =>
			loop
			(
				Core
				({
					time             : $.timeSinceOpen,
					updatecounter    : 0,
					rendercounter    : 0,
					isCanvasResizing : false
				})
			)(
				ProgramA
			)
		)

/**` loop :: Core -> Program -> IO () `*/
const loop = (core : Core) => (program : Program) : IO<null> =>
	Do.IO
		/**` $.aCore :: Core `*/
		.bindto ('aCore') ( _ => updateCore (core) )

		/**` $.aProgram :: Program `*/
		.bindto ('aProgram') ( _ => updateProgram (core) (program) )

		// -- Resizes the canvas if needed.
		.side
		( _ =>
			core.isCanvasResizing && core.isRender
				? Import.canvasDimensionW
					.bind(currentScalar => fetchMaxCanvasScalar.fmap (lerp (RESIZE_SPEED) (currentScalar)))
					.bind(scaleCanvas)
				: nil
		)

		// -- Renders to the canvas.
		.side   ( _ => render (core) (program) )

		// -- Writes debug info onto the canvas.
		.side   ( _ => Effect.fillText (`${core.time} | ${core.updatecounter} | ${core.rendercounter}`) (10) (10) )

		// -- Reset events.
		.side   ( _ => Effect.tick )

		// -- Queue for the next program loop.
		.bind   ( $ => Effect.queue (loop ($.aCore) ($.aProgram)) )

/**` updateProgram :: Core -> Program -> IO Program `*/
const updateProgram = (core : Core) => (program : Program) : IO<Program> =>
	program.CONS === 'ProgramA' ?
		Do.IO
			/**` $.mouseButtonLeft :: Vertical `*/
			.bindto ('mouseButtonLeft') ( _ => Import.mouseButtonLeft )

			.fmap
			( $ =>
				$.mouseButtonLeft === Vertical.Downward
					? ProgramB
					: ProgramA
			)
	:
	program.CONS === 'ProgramB' ?
		Do.IO
			.fmap ( _ => ProgramB )
	:
		Do.IO
			.fmap ( _ => program )

/**` render :: Core -> Program -> IO () `*/
const render = (core : Core) => (program : Program) : IO<null> =>
	program.CONS === 'ProgramA' ?
		Do.IO
			.side  ( _ => Effect.clearCanvas )
			.side  ( _ => Mutate.fillColor (core.isCanvasResizing ? 'green' : 'red') )
			.side  ( _ => Effect.Norm.fillRectangle (0.1) (0.1) (0.1) (0.1) )
			.side  ( _ => Effect.Norm.fillRectangle (0.8) (0.1) (0.1) (0.1) )
			.side  ( _ => Effect.Norm.fillRectangle (0.8) (0.8) (0.1) (0.1) )
			.side  ( _ => Effect.Norm.fillRectangle (0.1) (0.8) (0.1) (0.1) )
			.fmap  ( _ => null )
	:
	program.CONS === 'ProgramB' ?
		Do.IO
			.side  ( _ => Effect.clearCanvas )
			.side  ( _ => Effect.fillRectangle (50) (25) (75) (100) )
			.fmap  ( _ => null )
	:
		nil
