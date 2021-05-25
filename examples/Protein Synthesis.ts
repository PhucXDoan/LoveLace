/********************************************************************************************************************************/
// Constants and Settings  //

namespace STD
{
	export const refreshRate     = 15
	export const aspectRatio     = 16 / 9
	export const resizeThreshold = 5
	export const resizeSpeed     = 0.25

	export const mainmenu_game_tspeed         = 0.0075
	export const mainmenu_instructions_tspeed = 0.03
	export const mainmenu_credits_tspeed      = 0.03
	export const game_t_game_speed            = 0.005
	export const game_t_ending_speed          = 0.005
	export const ending_t_mainmenu_speed      = 0.01

	export const palette = ['#e76f51', '#f4a261', '#e9c46a', '#2a9d8f', '#264653', '#f77767']

	export namespace MM
	{
		export const arc_xy                      = Vector2 (0.2, 0.65)
		export const arc_r                       = 0.15
		export const arc_min_vel                 = 0.01
		export const arc_accl                    = 0.3
		export const arc_elasticity              = 0.2
		export const arc_drag                    = 0.05
		export const selection_button_texts      = List ("START", "INSTRUCTIONS", "CREDITS")
		export const selection_button_xy         = Vector2 (0.35, 0.5)
		export const selection_button_wh         = Vector2 (0.65, 0.3)
		export const selection_button_fadein     = 0.15
		export const selection_button_fadeout    = 0.75
		export const game_index                  = 0
		export const instructions_index          = 1
		export const credits_index               = 2
		export const back_button_xy              = Vector2 (0.0500, 0.750)
		export const back_button_wh              = Vector2 (0.2000, 0.150)
		export const back_button_text_xy         = Vector2 (0.1475, 0.835)
	}

	export namespace G
	{
		export const questions : List <Question> =
			List (
				Pair ( "What is the first stage of protein synthesis?"     , "TRANSCRIPTION"   ),
				Pair ( "What is the second stage of protein synthesis?"    , "TRANSLATION"     ),
				Pair ( "What organelle does transcription occur in?"       , "NUCLEUS"         ),
				Pair ( "What breaks the hydrogen bonds of DNA?"            , "HELICASE"        ),
				Pair ( "Where does the mRNA travel to after being made?"   , "RIBOSOMES"       ),
				Pair ( "Where does the assembly of proteins take place?"   , "RIBOSOMES"       ),
				Pair ( "What carries amino acids to make proteins?"        , "TRNA"            ),
				Pair ( "Translation ends when the (?) codon is reached."   , "STOP"            ),
				Pair ( "Translation begins when the (?) codon is reached." , "START"           ),
				Pair ( "The 'm' in mRNA is short for (?)."                 , "MESSENGER"       ),
				Pair ( "The 't' in tRNA is short for (?)."                 , "TRANSFER"        ),
				Pair ( "What are proteins made of?"                        , "POLYPEPTIDE"     ),
				Pair ( "What nucleotide is unique to RNA?"                 , "URACIL"          ),
				Pair ( "What nucleotide is unique to DNA?"                 , "THYMINE"         ),
				Pair ( "RNA has the shape of what?"                        , "SINGLEHELIX"     ),
				Pair ( "DNA has the shape of what?"                        , "DOUBLEHELIX"     ),
				Pair ( "If a codon is UUC, then what is its anti-codon?"   , "AAG"             ),
				Pair ( "What performs the assembly of amino acids?"        , "RRNA"            ),
				Pair ( "What bond does the helicase break in DNA?"         , "HYDROGEN"        ),
				Pair ( "Where DNA splits into two strands is called (?)."  , "REPLICATIONFORK" ),
				Pair ( "After the mRNA is done, the two DNA strands (?)."  , "REBOND"          ),
				Pair ( "What does tRNA carry around?"                      , "AMINOACIDS"      ),
				Pair ( "RNA can exit/enter the nucleus while (?) cannot."  , "DNA"             )
			)

		export const letter_amount               = 25
		export const lifespeed_lerp_speed        = 0.25
		export const letter_lifespeed_range      = Pair (0.001, 0.005)
		export const letter_lifespeed_max        = 0.025
		export const letter_radius               = 0.035
		export const letter_max_vel              = 0.005
		export const letter_max_vel_ampped       = 5
		export const gravity_strength            = 0.0005
		export const pull_strength               = 0.01
		export const question_amount             = 8
	}

	export namespace END
	{
		export const back_button_y      = 0.79
		export const back_button_wh     = Vector2 (0.5, 0.125)
		export const back_button_xy     = Vector2 (0.5 - END.back_button_wh.x / 2, END.back_button_y)
		export const back_button_text_y = 0.855
	}
}

/********************************************************************************************************************************/
// Definition of Algebraic Data Types //

type Global =
	{
		variation      : 'Global'
		presentTime    : number
		deltaTime      : number
		refreshTime    : number
		canvasWidth    : number
		seed           : number
		isRefresh      : boolean
		isResizing     : boolean
	}

type Local =
	{
		variation       : 'Mainmenu'
		bgVelocity      : number
		bgOffset        : number
		arcVelocity     : number
		offset          : number
		subarcIndex     : Maybe <number>
		buttonOpacities : List <number>
	} | {
		variation    : 'Game'
		questions    : List <Question>
		answerLength : number
		letters      : List <Letter>
	} | {
		variation : 'Instructions'
		bgOffset  : number
	} | {
		variation : 'Credits'
		bgOffset  : number
	} | {
		variation : 'Ending'
		bgOffset  : number
	} | {
		variation : 'Mainmenu_T_Game'
		keytime   : number
		mainmenu  : Local
		game      : Local
	} | {
		variation    : 'Mainmenu_T_Instructions'
		keytime      : number
		mainmenu     : Local
		instructions : Local
	} | {
		variation : 'Mainmenu_T_Credits'
		keytime   : number
		mainmenu  : Local
		credits   : Local
	} | {
		variation    : 'Instructions_T_Mainmenu'
		keytime      : number
		mainmenu     : Local
		instructions : Local
	} | {
		variation : 'Credits_T_Mainmenu'
		keytime   : number
		mainmenu  : Local
		credits   : Local
	} | {
		variation : 'Game_T_Game'
		keytime   : number
		preGame   : Local
		postGame  : Local
	} | {
		variation : 'Game_T_Ending'
		keytime   : number
		game      : Local
		ending    : Local
	} | {
		variation : 'Ending_T_Mainmenu'
		keytime   : number
		ending    : Local
		mainmenu  : Local
	}

type Letter =
	{
		variation   : 'Letter'
		character   : string
		lifetime    : number
		lifeSpeed   : number
		staticSpeed : number
		position    : Vector2
		target      : Vector2
		velocity    : Vector2
		colorIndex  : number
	}

type Question = Pair <string, string>

/********************************************************************************************************************************/
// Implementation of Micro-Functions and Constants Relating to Local //

/**` initialMainmenu : Local `*/
const initialMainmenu : Local =
	{
		variation       : 'Mainmenu',
		bgVelocity      : 0,
		bgOffset        : 0,
		arcVelocity     : STD.MM.arc_min_vel,
		offset          : 0,
		subarcIndex     : Nothing,
		buttonOpacities : replicate (3) (0)
	}

/********************************************************************************************************************************/
// Implementation of Micro-Functions and Constants Relating to Letters //

/**` randomLetter : Local -> Process Number Letter `*/
const randomLetter = (game : Local) : Process <number, Letter> =>
	game.variation === 'Game' ?
		Do.Process_Random
			/**` $.lifeSpeed : Number `*/
			.bindto ('lifeSpeed') <number> (_ => uncurry (randomFloatRange) (STD.G.letter_lifespeed_range))

			/**` $.character : String `*/
			.bindto ('character') <string> (_ =>
				randomChance (0.75)
					.bind (chance =>
						chance
							? randomChar (head (game.questions).snd)
							: randomUppercase
					)
			)

			/**` $.position : Vector2 `*/
			.bindto ('position') <Vector2> (_ => randomPaddedVector)

			/**` $.target : Vector2 `*/
			.bindto ('target') <Vector2> (_ => randomPaddedVector)

			/**` $.velocity : Vector2 `*/
			.bindto ('velocity') <Vector2> (_ => randomDirectionV2 .fmap (V2.scale (STD.G.letter_max_vel)))

			/**` $.colorIndex : Number `*/
			.bindto ('colorIndex') <number> (_ => randomIntRange (1) (5))

			.fmap
			($ =>
				({
					variation   : 'Letter',
					character   : $.character,
					lifetime    : 0,
					lifeSpeed   : $.lifeSpeed,
					staticSpeed : $.lifeSpeed,
					position    : $.position,
					velocity    : $.velocity,
					target      : $.target,
					colorIndex  : $.colorIndex
				})
			)
	:
		never

/**` iterateLetter : Boolean -> Letter -> Maybe Letter `*/
const iterateLetter = (isFast : boolean) => (letter : Letter) : Maybe <Letter> =>
	letter.lifetime > 1
		? Nothing
		: Just
			(
				{
					variation   : 'Letter',
					character   : letter.character,
					target      : letter.target,
					staticSpeed : letter.staticSpeed,
					lifetime    : letter.lifetime + letter.lifeSpeed,
					lifeSpeed   :
						lerp
							(STD.G.lifespeed_lerp_speed)
							(letter.lifeSpeed)
							(letter.staticSpeed + (isFast ? STD.G.letter_lifespeed_max : 0)),
					position    :
						V2.each
							(lerp (STD.G.pull_strength))
							(V2.translate (letter.position) (letter.velocity))
							(letter.target),
					velocity    :
						V2.each
							(lerp (STD.G.gravity_strength))
							(letter.velocity)
							(
								letter.target
									.pipe (V2.untranslate (letter.position))
									.pipe (V2.scale (isFast ? STD.G.letter_max_vel_ampped : 1))
							),
					colorIndex  : letter.colorIndex
				}
			)

/********************************************************************************************************************************/
// Implementation of General Micro-Functions and Constants //

/**` distIndexToOffset : Number -> Number -> Number `*/
const distIndexToOffset = (index : number) => (offset : number) : number =>
	((index - 2.75) % 9 + offset * 3) .pipe (n => abs (n) > 1.5 ? sign (n) - n / 3 : -n / 3)

/**` nearestSubarcIndex : Number -> Number `*/
const nearestSubarcIndex = (offset : number) : number =>
	roundInt (2.75 - offset * 3) % 3

/**` transitionCurve : Number -> Number `*/
const transitionCurve = (x : number) : number =>
	quarticCurve (x) * 1.01 - 0.005

/**` isSelectionTextInRange : Number -> Number -> Boolean `*/
const isSelectionTextInRange = (index : number) => (offset : number) : boolean =>
	abs (distIndexToOffset (index) (offset)) < 0.05

/**` isOnSelectionButton : Vector2 -> Boolean `*/
const isOnSelectionButton = V2.isInRect (STD.MM.selection_button_xy) (STD.MM.selection_button_wh)

/**` isOnBackButton : Vector2 -> Boolean `*/
const isOnBackButton = V2.isInRect (STD.MM.back_button_xy) (STD.MM.back_button_wh)

/**` randomPaddedVector : Process Number Vector2 `*/
const randomPaddedVector : Process <number, Vector2> =
	randomV2
		.fmap (V2.scale (0.7))
		.fmap (V2.translateXY (0.15) (0.15))

/**` isOnLetter : Vector2 -> Letter -> Boolean `*/
const isOnLetter = (position : Vector2) => (letter : Letter) : boolean =>
	V2.isInCircle
		(STD.G.letter_radius)
		(V2.unscaleY (STD.aspectRatio) (letter.position))
		(V2.unscaleY (STD.aspectRatio) (position))

/********************************************************************************************************************************/
// Main and Loop //

/**` main : IO () `*/
const main : IO <null> =
	Do.IO
		/**` $.seed : Number `*/
		.bindto ('seed') <number> (_ => seed)

		/**` $.nextPresentTime : Number `*/
		.bindto ('nextPresentTime') <number> (_ => time)

		/**` $.idealCanvasWidth : Number `*/
		.bindto ('idealCanvasWidth') <number>
		(_ =>
			windowDimensions
                .fmap (vectorToPair)
				.fmap (fsnd (mul (STD.aspectRatio)))
				.fmap (uncurry (min))
		)

		.also ($ => setCanvasDimensions ($.idealCanvasWidth) ($.idealCanvasWidth / STD.aspectRatio))
		.side (loadFont  ('./font/RobotoMono.ttf'))
		.side (loadFont  ('./font/Hyperlegible.ttf'))
		.side (loadFont  ('./font/Hyperlegible-Italic.ttf'))
		.side (loadImage ('./image/crosshair.png'))
		.side (loadAudio ('./audio/gunshot.mp3'))
		.bind
		($ =>
			loop
				(
					{
						variation      : 'Global',
						presentTime    : $.nextPresentTime,
						deltaTime      : 0,
						refreshTime    : 0,
						canvasWidth    : $.idealCanvasWidth,
						seed           : $.seed,
						isRefresh      : false,
						isResizing     : false
					}
				) (initialMainmenu)
		)

/**` loop : Global -> Local -> IO () `*/
const loop = (global : Global) => (local : Local) : IO <null> =>
	Do.IO
		/**` $.nextPresentTime : Number `*/
		.bindto ('nextPresentTime') <number> (_ => time)

		/**` $.maxCanvasWidth : Number `*/
		.bindto ('maxCanvasWidth') <number>
		(_ =>
			windowDimensions
                .fmap (vectorToPair)
				.fmap (fsnd (mul (STD.aspectRatio)))
				.fmap (uncurry (min))
		)

		/**` $.nextIsResizing : Boolean `*/
		.bindto ('nextIsResizing') <boolean>
		($ =>
			isWindowResized
				.fmap (b => (b || global.isResizing) && napprox (STD.resizeThreshold) ($.maxCanvasWidth) (global.canvasWidth))
		)

		/**` $.nextDeltaTime : Number `*/
		.fmapto ('nextDeltaTime') <number> ($ => $.nextPresentTime - global.presentTime)

		/**` $.nextRefreshTime : Number `*/
		.fmapto ('nextRefreshTime') <number> ($ => (global.isRefresh ? 0 : global.refreshTime) + $.nextDeltaTime)

		/**` $.nextIsRefresh : Boolean `*/
		.fmapto ('nextIsRefresh') <boolean> ($ => $.nextRefreshTime > STD.refreshRate)

		/**` $.nextGlobal : Global `*/
		.fmapto ('nextGlobal') <Global>
		($ =>
			({
				variation      : 'Global',
				presentTime    : $.nextPresentTime,
				deltaTime      : $.nextDeltaTime,
				refreshTime    : $.nextRefreshTime,
				canvasWidth    :
					$.nextIsResizing && $.nextIsRefresh
						? lerp (STD.resizeSpeed) (global.canvasWidth) ($.maxCanvasWidth)
						: global.canvasWidth,
				seed           : uncurry (add) (random .computation (global.seed)),
				isRefresh      : $.nextIsRefresh,
				isResizing     : $.nextIsResizing
			})
		)

		/**` $.nextLocal : Local `*/
		.bindto ('nextLocal') <Local>
		($ => update ($.nextGlobal) (local))

		.also
		($ =>
			$.nextIsResizing && $.nextIsRefresh
				? setCanvasDimensions ($.nextGlobal.canvasWidth) ($.nextGlobal.canvasWidth / STD.aspectRatio)
				: idle
		)

		.side (
			mouseLeft
				.fmap (eq (Vertical.Downward))
				.bind (isDown =>
					isDown
						? playSFX ('./audio/gunshot.mp3')
						: idle
				)
		)

		.also ($ => clearCanvas)
		.also ($ => render ($.nextGlobal) ($.nextLocal))
		.bind ($ => queueIO (loop ($.nextGlobal) ($.nextLocal)))

/********************************************************************************************************************************/

/**` update : Global -> Local -> IO Local `*/
const update = (global : Global) => (local : Local) : IO <Local> =>
	local.variation === 'Mainmenu' ?
		n_mouseLeftClickCoordinates
			.bind (coordinatesMaybe =>
				checkMaybe (isOnSelectionButton) (coordinatesMaybe)
					? send (branch_Mainmenu_Selection (global) (local))
					: update_Mainmenu (global) (local)
			)
	:
	local.variation === 'Game' ?
		local.answerLength < head (local.questions).snd.length
			? update_Game (global) (local)
			: send <Local>
				(
					tail (local.questions).variation === 'Nil'
						?
							{
								variation : 'Game_T_Ending',
								keytime   : 0,
								game      : local,
								ending    :
									{
										variation : 'Ending',
										bgOffset  : 0
									}
							}
						:
							{
								variation : 'Game_T_Game',
								keytime   : 0,
								preGame   : local,
								postGame  :
									{
										variation    : 'Game',
										answerLength : 0,
										letters      : Nil,
										questions    : tail (local.questions)
									}
							}
				)
	:
	local.variation === 'Instructions' ?
		n_mouseLeftClickCoordinates
			.fmap <Local> (coordinatesMaybe =>
				checkMaybe (isOnBackButton) (coordinatesMaybe)
					?
						{
							variation    : 'Instructions_T_Mainmenu',
							keytime      : 0,
							mainmenu     : initialMainmenu,
							instructions : local
						}
					:
						{
							variation : 'Instructions',
							bgOffset  :
								global.isRefresh
									? local.bgOffset + 0.001
									: local.bgOffset
						}
			)
	:
	local.variation === 'Credits' ?
		n_mouseLeftClickCoordinates
			.fmap <Local> (coordinatesMaybe =>
				checkMaybe (isOnBackButton) (coordinatesMaybe)
					?
						{
							variation : 'Credits_T_Mainmenu',
							keytime   : 0,
							mainmenu  : initialMainmenu,
							credits   : local
						}
					:
						{
							variation : 'Credits',
							bgOffset  :
								global.isRefresh
									? local.bgOffset + 0.001
									: local.bgOffset
						}
			)
	:
	local.variation === 'Ending' ?
		n_mouseLeftClickCoordinates
			.fmap (coordinatesMaybe =>
				checkMaybe (V2.isInRect (STD.END.back_button_xy) (STD.END.back_button_wh)) (coordinatesMaybe)
					?
						{
							variation : 'Ending_T_Mainmenu',
							keytime   : 0,
							ending    : local,
							mainmenu  : initialMainmenu
						}
					:
						{
							variation : 'Ending',
							bgOffset  : local.bgOffset + 0.01
						}
			)
	:
	local.variation === 'Mainmenu_T_Game' ?
		local.keytime > 1
			? send (local.game)
			: update_Mainmenu (global) (local.mainmenu)
				.fmap (nextMainmenu =>
					({
						variation : 'Mainmenu_T_Game',
						keytime   : global.isRefresh ? local.keytime + STD.mainmenu_game_tspeed : local.keytime,
						mainmenu  : nextMainmenu,
						game      : local.game
					})
				)
	:
	local.variation === 'Mainmenu_T_Instructions' ?
		local.keytime > 1
			? send (local.instructions)
			: update_Mainmenu (global) (local.mainmenu)
				.fmap (nextMainmenu =>
					({
						variation    : 'Mainmenu_T_Instructions',
						keytime      : global.isRefresh ? local.keytime + STD.mainmenu_instructions_tspeed : local.keytime,
						mainmenu     : nextMainmenu,
						instructions :
							local.instructions.variation === 'Instructions' ?
								{
									variation : 'Instructions',
									bgOffset  :
										global.isRefresh
											? local.instructions.bgOffset + 0.001
											: local.instructions.bgOffset
								}
							:
								never
					})
				)
	:
	local.variation === 'Mainmenu_T_Credits' ?
		local.keytime > 1
			? send (local.credits)
			: update_Mainmenu (global) (local.mainmenu)
				.fmap (nextMainmenu =>
					({
						variation : 'Mainmenu_T_Credits',
						keytime   : global.isRefresh ? local.keytime + STD.mainmenu_credits_tspeed : local.keytime,
						mainmenu  : nextMainmenu,
						credits   :
							local.credits.variation === 'Credits' ?
								{
									variation : 'Credits',
									bgOffset :
										global.isRefresh
											? local.credits.bgOffset + 0.001
											: local.credits.bgOffset
								}
							:
								never
					})
				)
	:
	local.variation === 'Instructions_T_Mainmenu' ?
		local.keytime > 1
			? send (local.mainmenu)
			: update_Mainmenu (global) (local.mainmenu)
				.fmap (nextMainmenu =>
					({
						variation    : 'Instructions_T_Mainmenu',
						keytime      : global.isRefresh ? local.keytime + STD.mainmenu_instructions_tspeed : local.keytime,
						mainmenu     : nextMainmenu,
						instructions :
							local.instructions.variation === 'Instructions' ?
								{
									variation : 'Instructions',
									bgOffset  :
										global.isRefresh
											? local.instructions.bgOffset + 0.001
											: local.instructions.bgOffset
								}
							:
								never
					})
				)
	:
	local.variation === 'Credits_T_Mainmenu' ?
		local.keytime > 1
			? send (local.mainmenu)
			: update_Mainmenu (global) (local.mainmenu)
				.fmap (nextMainmenu =>
					({
						variation : 'Credits_T_Mainmenu',
						keytime   : global.isRefresh ? local.keytime + STD.mainmenu_credits_tspeed : local.keytime,
						mainmenu  : nextMainmenu,
						credits   :
							local.credits.variation === 'Credits' ?
								{
									variation : 'Credits',
									bgOffset  :
										global.isRefresh
											? local.credits.bgOffset + 0.001
											: local.credits.bgOffset
								}
							:
								never
					})
				)
	:
	local.variation === 'Game_T_Game' ?
		local.keytime > 1
			? send (local.postGame)
			: Do.IO
				.bindto ('nextPreGame')  <Local> (_ => update_Game (global) (local.preGame))
				.bindto ('nextPostGame') <Local> (_ => update_Game (global) (local.postGame))
				.fmap
				($ =>
					({
						variation : 'Game_T_Game',
						keytime   : global.isRefresh ? local.keytime + STD.game_t_game_speed : local.keytime,
						preGame   : $.nextPreGame,
						postGame  : $.nextPostGame
					})
				)
	:
	local.variation === 'Game_T_Ending' ?
		local.keytime > 1
			? send (local.ending)
			: update_Game (global) (local.game)
				.fmap (nextGame =>
					({
						variation : 'Game_T_Ending',
						keytime   : global.isRefresh ? local.keytime + STD.game_t_ending_speed : local.keytime,
						game      : nextGame,
						ending    :
							{
								variation : 'Ending',
								bgOffset  :
									local.ending.variation === 'Ending' ?
										global.isRefresh
											? local.ending.bgOffset + 0.01
											: local.ending.bgOffset
									:
										never
							}
					})
				)
	:
	local.variation === 'Ending_T_Mainmenu' ?
		local.keytime > 1
			? send (local.mainmenu)
			: update_Mainmenu (global) (local.mainmenu)
				.fmap (nextMainmenu =>
					({
						variation : 'Ending_T_Mainmenu',
						keytime   :
							global.isRefresh
								? local.keytime + STD.ending_t_mainmenu_speed
								: local.keytime,
						ending    :
							local.ending.variation === 'Ending' ?
								{
									variation : 'Ending',
									bgOffset  : local.ending.bgOffset + 0.01
								}
							:
								never,
						mainmenu  : nextMainmenu
					})
				)
	:
		never

/**` render : Global -> Local -> IO () `*/
const render = (global : Global) => (local : Local) : IO <null> =>
	local.variation === 'Mainmenu' ?
		render_Mainmenu (global) (local)
	:
	local.variation === 'Game' ?
		render_Game (global) (local)
	:
	local.variation === 'Instructions' ?
		render_Instructions (global) (local)
	:
	local.variation === 'Credits' ?
		render_Credits (global) (local)
	:
	local.variation === 'Ending' ?
		render_Ending (global) (local)
	:
	local.variation === 'Mainmenu_T_Game' ?
		local.keytime < 0.25 ?
			executing
			(
					render_Mainmenu (global) (local.mainmenu),

				setFillColor ('Black'),
					beginPath,
					n_fillCircle (quadraticCurve (local.keytime * 4)) (0.5) (0.5)
			)
		:
		local.keytime < 0.5 ?
			executing
			(
					render_Game (global) (local.game),
					render_QuestionIndex (local.game),

				setFillColor ('Black'),
					beginPath,
					n_fillCircle (1 - quadraticCurve (local.keytime * 4 - 1)) (0.5) (0.5)
			)
		:
			executing
			(
					render_Game (global) (local.game),

				setAlpha (1 - quarticCurve (local.keytime * 2 - 1)),
					render_QuestionIndex (local.game),
				setAlpha (1)
			)
	:
	local.variation === 'Mainmenu_T_Instructions' ?
		render_T_Mainmenu_Instructions (global) (local.mainmenu) (local.instructions) (transitionCurve (local.keytime))
	:
	local.variation === 'Mainmenu_T_Credits' ?
		render_T_Mainmenu_Credits (global) (local.mainmenu) (local.credits) (transitionCurve (local.keytime))
	:
	local.variation === 'Instructions_T_Mainmenu' ?
		render_T_Mainmenu_Instructions (global) (local.mainmenu) (local.instructions) (1 - transitionCurve (local.keytime))
	:
	local.variation === 'Credits_T_Mainmenu' ?
		render_T_Mainmenu_Credits (global) (local.mainmenu) (local.credits) (transitionCurve (1 - local.keytime))
	:
	local.variation === 'Game_T_Game' ?
		render_Game_T_Game (global) (local.preGame) (local.postGame) (quadraticCurve (local.keytime))
	:
	local.variation === 'Game_T_Ending' ?
		local.keytime < 0.5 ?
			executing
			(
				render_Game (global) (local.game),

				setAlpha (min (1) (local.keytime * 32)),
					render_ThatsRight,
				setAlpha (1),

				setStrokeColor ('Black'),
					beginPath,
					n_strokeVectorLine (0.5) (0) (0) (quarticCurve (local.keytime * 2))
			)
		:
			executing
			(
				saveCanvasState,
					beginPath,
					n_area (0) (0) (0.5 - quarticCurve (local.keytime * 2 - 1) / 2) (1),
					n_area (0.5 + quarticCurve (local.keytime * 2 - 1) / 2) (0) (1) (1),
				clipEvenOdd,
						render_Game (global) (local.game),
					setAlpha (min (1) (local.keytime * 32)),
						render_ThatsRight,
					setAlpha (1),
				restoreCanvasState,

				saveCanvasState,
					beginPath,
					n_area (0.5 + quarticCurve (local.keytime * 2 - 1) / 2) (0) (0.5 - quarticCurve (local.keytime * 2 - 1) / 2) (1),
				clipEvenOdd,
					render_Ending (global) (local.ending),
				restoreCanvasState,

				setStrokeColor ('Black'),
					beginPath,
					n_strokeVectorLine (0.5 - 1.01 * quarticCurve (local.keytime * 2 - 1) / 2) (0) (0) (1),
					n_strokeVectorLine (0.5 + 1.01 * quarticCurve (local.keytime * 2 - 1) / 2) (0) (0) (1)
			)
	:
	local.variation === 'Ending_T_Mainmenu' ?
		local.keytime < 0.5 ?
			executing
			(
				render_Ending (global) (local.ending),

				setFillColor ('Black'),
					beginPath,
					n_fillCircle (quadraticCurve (local.keytime * 2)) (0.5) (0.5)
			)
		:
			executing
			(
				render_Mainmenu (global) (local.mainmenu),

				setFillColor ('Black'),
					beginPath,
					n_fillCircle (1 - quadraticCurve (local.keytime * 2 - 1)) (0.5) (0.5)
			)
	:
		never

/********************************************************************************************************************************/
// Subprocedures //

/**` branch_Mainmenu_Selection : Global -> Local -> Local `*/
const branch_Mainmenu_Selection = (global : Global) => (mainmenu : Local) : Local =>
	mainmenu.variation === 'Mainmenu' ?
		mainmenu.subarcIndex.variation === 'Just' ?
			mainmenu.subarcIndex.value === STD.MM.game_index ?
				{
					variation : 'Mainmenu_T_Game',
					keytime   : 0,
					mainmenu,
					game      :
						{
							variation    : 'Game',
							answerLength : 0,
							letters      : Nil,
							questions    :
								strictRandomShuffle (STD.G.questions)
									.computation (global.seed)
									.snd
									.pipe (take (STD.G.question_amount))
						}
				}
			:
			mainmenu.subarcIndex.value === STD.MM.instructions_index ?
				{
					variation    : 'Mainmenu_T_Instructions',
					keytime      : 0,
					mainmenu,
					instructions :
						{
							variation : 'Instructions',
							bgOffset  : pi
						}
				}
			:
			mainmenu.subarcIndex.value === STD.MM.credits_index ?
				{
					variation : 'Mainmenu_T_Credits',
					keytime   : 0,
					mainmenu,
					credits   :
						{
							variation : 'Credits',
							bgOffset  : tau
						}
				}
			:
				never
		:
			mainmenu
	:
		never

/**` update_Mainmenu : Global -> Local -> IO Local `*/
const update_Mainmenu = (global : Global) => (mainmenu : Local) : IO <Local> =>
	mainmenu.variation === 'Mainmenu' ?
		Do.IO
			/**` $.mouseScroll : Vertical `*/
			.bindto ('mouseScroll') <Vertical>
			(__ => mouseScroll)

			/**` $.isMouseOnSelectionButton : Boolean `*/
			.bindto ('isMouseOnSelectionButton') <boolean>
			($ => n_mouseCanvasPosition .fmap (V2.isInRect (STD.MM.selection_button_xy) (STD.MM.selection_button_wh)))

			/**` $.nextSubarcIndex : Maybe Number `*/
			.fmapto ('nextSubarcIndex') <Maybe <number>>
			($ =>
				$.isMouseOnSelectionButton
					? mainmenu.subarcIndex.variation === 'Nothing'
						? Just (nearestSubarcIndex (mainmenu.offset))
						: isSelectionTextInRange (mainmenu.subarcIndex.value) (mainmenu.offset)
							? mainmenu.subarcIndex
								.fmap (add (signVertical ($.mouseScroll)))
								.fmap (rmodulo (3))
							: mainmenu.subarcIndex
					: Nothing
			)

			/**` $.nextArcVelocity : Number `*/
			.fmapto ('nextArcVelocity') <number>
			($ =>
				global.isRefresh
					? $.nextSubarcIndex.variation === 'Nothing'
						? lerp (STD.MM.arc_drag) (mainmenu.arcVelocity) (STD.MM.arc_min_vel)
						: lerp
							(STD.MM.arc_accl)
							(mainmenu.arcVelocity)
							(distIndexToOffset ($.nextSubarcIndex.value) (mainmenu.offset) * STD.MM.arc_elasticity)
					: mainmenu.arcVelocity
			)

			/**` $.nextBackgroundVelocity : Number `*/
			.fmapto ('nextBackgroundVelocity') <number>
			($ =>
				global.isRefresh
					? $.nextSubarcIndex.variation === 'Just'
						? lerp (0.1) (mainmenu.bgVelocity) (0.0005)
						: lerp (0.25) (mainmenu.bgVelocity) (0.0125)
					: mainmenu.bgVelocity
			)

			.fmap
			($ =>
				({
					variation       : 'Mainmenu',
					arcVelocity     : $.nextArcVelocity,
					subarcIndex     : $.nextSubarcIndex,
					bgVelocity      : $.nextBackgroundVelocity,
					bgOffset        :
						global.isRefresh
							? mainmenu.bgOffset + $.nextBackgroundVelocity
							: mainmenu.bgOffset,
					offset          :
						global.isRefresh
							? modulo (mainmenu.offset + $.nextArcVelocity) (1)
							: mainmenu.offset,
					buttonOpacities :
						global.isRefresh
							? $.nextSubarcIndex.variation === 'Just'
								? mainmenu.buttonOpacities
									.pipe (imap (index => opacity =>
										extractJust ($.nextSubarcIndex) === index
											? lerp (STD.MM.selection_button_fadein) (opacity) (1)
											: opacity * STD.MM.selection_button_fadeout
									))
								: mainmenu.buttonOpacities
									.fmap (mul (STD.MM.selection_button_fadeout))
							: mainmenu.buttonOpacities
				})
			)
	:
		never

/**` update_Game : Global -> Local -> IO Local `*/
const update_Game = (global : Global) => (game : Local) : IO <Local> =>
	game.variation === 'Game' ?
		Do.IO
			/**` $.mouseClickCoordinates : Maybe Vector2 `*/
			.bindto ('mouseClickCoordinates') <Maybe <Vector2>> (_ => n_mouseLeftClickCoordinates)

			/**` $.isShiftDown : Boolean `*/
			.bindto ('isShiftDown') <boolean> (_ => keyboardKey ('ShiftLeft') .fmap (isDown))

			/**` $.iteratedLetters : List Letter `*/
			.fmapto ('iteratedLetters') <List <Letter>>
			($ =>
				global.isRefresh
					? game.letters .pipe (mapMaybe (iterateLetter ($.isShiftDown)))
					: game.letters
			)

			/**` $.pickedLetters : Pair (Maybe Letter) (List Letter) `*/
			.fmapto ('pickedLetters') <Pair <Maybe <Letter>, List <Letter>>>
			($ =>
				$.mouseClickCoordinates
					.fmap (coordinates => pickOut (isOnLetter (coordinates)) ($.iteratedLetters))
					.pipe (fromJust (Pair (Nothing, $.iteratedLetters)))
			)

			.fmap
			($ =>
				({
					variation    : 'Game',
					questions    : game.questions,
					answerLength :
						game.answerLength < head (game.questions).snd.length &&
						checkMaybe
							((letter : Letter) => letter.character === head (game.questions).snd [game.answerLength])
							($.pickedLetters.fst)
							? game.answerLength + 1
							: game.answerLength,
					letters      :
						len ($.pickedLetters.snd) > STD.G.letter_amount
							? $.pickedLetters.snd
							: $.pickedLetters.snd
								.link (processToList (randomLetter (game)) (global.seed))
								.pipe (take (STD.G.letter_amount))
				})
			)
	:
		never

/**` render_Mainmenu : Global -> Local -> IO () `*/
const render_Mainmenu = (global : Global) => (mainmenu : Local) : IO <null> =>
	mainmenu.variation === 'Mainmenu' ?
		executing
		(
			setFillColor (STD.palette [0]),
				n_fillRect (0) (0) (1) (1),

			setStrokeColor     (STD.palette [1]),
			n_setLineThickness (0.02),
			setLineCap         (LineCap.Round),
				render_MainmenuSubarcs (mainmenu.bgOffset) (1.2),

			setStrokeColor (STD.palette [2]),
			n_setLineThickness (0.01),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.75) (1.35),

			setStrokeColor (STD.palette [3]),
			n_setLineThickness (0.01),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.5) (1.45),

			setStrokeColor (STD.palette [4]),
			n_setLineThickness (0.005),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.25) (1.55),

			setStrokeColor (STD.palette [1]),
			n_setLineThickness (0.02),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.2) (1.85),
			setStrokeColor (STD.palette [2]),
			n_setLineThickness (0.01),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.15) (2.2),

			setStrokeColor (STD.palette [3]),
			n_setLineThickness (0.01),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.125) (2.5),

			setStrokeColor (STD.palette [4]),
			n_setLineThickness (0.005),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.12) (2.75),

			setStrokeColor (STD.palette [1]),
			n_setLineThickness (0.02),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.1) (3.5),

			setStrokeColor (STD.palette [2]),
			n_setLineThickness (0.01),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.0875) (4),

			setStrokeColor (STD.palette [3]),
			n_setLineThickness (0.01),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.05) (4.5),

			setStrokeColor (STD.palette [4]),
			n_setLineThickness (0.005),
				render_MainmenuSubarcs (mainmenu.bgOffset * 0.025) (5),

			saveCanvasState,
			setComposition (Composition.DestinationOut),
			beginPath,
				n_fillCircleV2 (STD.MM.arc_r) (STD.MM.arc_xy),
			restoreCanvasState,

			// -- Button Selection
			setFontFamily      ('RobotoMono'),
			setTextAlign	   (TextAlign.Leftside),
			setTextBaseline    (TextBaseline.Middle),
			n_setFontSize      (0.08),
			n_setLineThickness (0.01),
			setFillColor       ('White'),
			setStrokeColor     ('Black'),
				n_translateV2 (STD.MM.arc_xy),
				n_rotate      (mainmenu.offset + 1 / 12),
				zip (STD.MM.selection_button_texts) (mainmenu.buttonOpacities)
					.fmap (zipped =>
						executing
						(
							setAlpha (zipped.snd),
							n_strokeFillText (zipped.fst) (STD.MM.arc_r + 0.025) (0),
							n_rotate (1 / 3)
						)
					)
					.pipe (executeIOs),
				resetTransformation,
			setAlpha (1),

			// -- Super Title
			setFontFamily      ('Hyperlegible'),
			setTextBaseline    (TextBaseline.Hanging),
			n_setLineThickness (0.0125),
			n_setFontSize      (0.1),
			setMiterLimit      (2),
				beginPath,
				n_strokeFillText ("HANGMAN'S GAMBIT") (0.01) (0.05),

			// -- Sub title
			setFontFamily      ('Hyperlegible-Italic'),
			n_setFontSize      (0.05),
			n_setLineThickness (0.007),
				beginPath,
				n_strokeFillText ("Protein Synthesis") (0.575) (0.19),

			// -- Main Circle
			n_setLineThickness (0.01),
			setStrokeColor     ('Black'),
				beginPath,
				n_strokeCircleV2 (STD.MM.arc_r) (STD.MM.arc_xy),

			// -- Sub arcs
			n_setLineThickness (0.025),
			setLineCap         (LineCap.Round),
				render_MainmenuSubarcs (mainmenu.offset) (1),
			n_setLineThickness (0.01),
				render_MainmenuSubarcs (mainmenu.bgOffset * 1.1) (0.75),
				render_MainmenuSubarcs (mainmenu.bgOffset * 1.2) (0.5),
				render_MainmenuSubarcs (mainmenu.bgOffset * 1.3) (0.25),

				render_Crosshair
		)
	:
		never

/**` render_Game : Global -> Local -> IO () `*/
const render_Game = (global : Global) => (game : Local) : IO <null> =>
	game.variation === 'Game' ?
		executing
		(
			setFillColor (STD.palette [0]),
				n_fillRect (0) (0) (1) (1),

			// -- Question
			setToCenterText,
			setMiterLimit      (2.5),
			n_setLineThickness (0.007),
			n_setFontSize      (0.0425),
			setFontFamily      ('Hyperlegible'),
			setStrokeColor     ('Black'),
			setFillColor       ('White'),
				n_strokeFillText (head (game.questions).fst) (0.5) (0.1),

			// -- Current Answer
			n_setFontSize (0.05),
			setFontFamily ('RobotoMono'),
			head (game.questions).snd
				.slice (0, game.answerLength)
				.padEnd (head (game.questions).snd.length, '_')
				.split ('')
				.join (' ')
				.pipe (currentAnswer => n_strokeFillText (currentAnswer) (0.5) (0.9)),

			// -- Letters
			n_setFontSize      (0.05),
			n_setLineThickness (0.0075),
				game.letters
					.pipe (reverse)
					.fmap (render_Letter)
					.pipe (executeIOs),
			setAlpha (1),

			render_Crosshair
		)
	:
		never

/**` render_Instructions : Global -> Local -> IO () `*/
const render_Instructions = (global : Global) => (instructions : Local) : IO <null> =>
	instructions.variation === 'Instructions' ?
		executing
		(
			setFillColor (STD.palette [5]),
				n_fillRect (0) (0) (1) (1),
				render_InstructionsBgArcs (instructions.bgOffset * 1.25) (0.1) (1),
				render_InstructionsBgArcs (instructions.bgOffset * 1.65) (0.23) (2),
				render_InstructionsBgArcs (instructions.bgOffset * 1.75) (0.3) (3),
				render_InstructionsBgArcs (instructions.bgOffset * 4.15) (0.12) (4),
				render_InstructionsBgArcs (instructions.bgOffset * 4.25) (0.5) (1),
				render_InstructionsBgArcs (instructions.bgOffset * 4.15) (0.7) (2),
				render_InstructionsBgArcs (instructions.bgOffset * 2.5) (0.75) (3),
				render_InstructionsBgArcs (instructions.bgOffset * 3.00) (0.55) (4),


			// -- "INSTRUCTIONS"
			setToCenterText,
			setFontFamily      ('Hyperlegible'),
			n_setFontSize      (0.125),
			n_setLineThickness (0.015),
			setMiterLimit      (2.5),
			setFillColor       ('White'),
			setStrokeColor     ('Black'),
				n_strokeFillText ("INSTRUCTIONS") (0.5) (0.15),

			// -- Instructions
			setToDefaultText,
			n_setFontSize      (0.035),
			n_setLineThickness (0.006),
			setFillColor       ('White'),
			setStrokeColor     ('Black'),
				n_strokeFillMultilineText (
					"- A question will appear on top\n" +
					"- Each question is about Protein Synthesis\n" +
					"- Letters will float across the screen\n" +
					"- Shoot down the letters to answer\n" +
					"- Press SHIFT to go through letters faster\n" +
					"- Solve each question successfully to save Ralph the Cat!"
				) (0.065) (0.03) (0.3),

			// -- Backward Button
			n_setLineThickness (0.01),
				beginPath,
				n_strokeFillRectV2 (STD.MM.back_button_xy) (STD.MM.back_button_wh),

			n_setFontSize (0.075),
			setFontFamily ('RobotoMono'),
			setToCenterText,
				beginPath,
				n_strokeFillTextV2 ("BACK") (STD.MM.back_button_text_xy),

			render_Crosshair
		)
	:
		never

/**` render_Credits : Global -> Local -> IO () `*/
const render_Credits = (global : Global) => (credits : Local) : IO <null> =>
	credits.variation === 'Credits' ?
		executing
		(
			setFillColor (STD.palette [5]),
				n_fillRect (0) (0) (1) (1),
				render_CreditsBgArcs (credits.bgOffset * 2.1) (0.05) (1),
				render_CreditsBgArcs (credits.bgOffset * 2.2) (0.10) (2),
				render_CreditsBgArcs (credits.bgOffset * 2.3) (0.15) (3),
				render_CreditsBgArcs (credits.bgOffset * 2.4) (0.20) (4),
				render_CreditsBgArcs (credits.bgOffset * 2.5) (0.25) (1),
				render_CreditsBgArcs (credits.bgOffset * 2.6) (0.30) (2),
				render_CreditsBgArcs (credits.bgOffset * 2.7) (0.35) (3),
				render_CreditsBgArcs (credits.bgOffset * 2.8) (0.40) (4),


			// -- "CREDITS"
			setToCenterText,
			setFontFamily      ('Hyperlegible'),
			n_setFontSize      (0.125),
			n_setLineThickness (0.015),
			setMiterLimit      (2.5),
			setFillColor       ('White'),
			setStrokeColor     ('Black'),
				n_strokeFillText ("CREDITS") (0.5) (0.15),

			// -- Instructions
			setToDefaultText,
			n_setFontSize      (0.035),
			n_setLineThickness (0.006),
			setFillColor       ('White'),
			setStrokeColor     ('Black'),
				n_strokeFillMultilineText (
					"| Developed by Phuc Doan (KittyGorey)\n" +
					"| Made with the LoveLace framework\n" +
					"| Used SWC, Typescript, and ESLint\n" +
					"| Check out the LoveLace repository at:\n"
				) (0.065) (0.03) (0.3),

			setToCenterText,
			setFontFamily ('Hyperlegible-Italic'),
			n_setFontSize (0.0525),
				n_strokeFillText ("https://github.com/kittygorey/LoveLace") (0.5) (0.625),

			// -- Backward Button
			n_setLineThickness (0.01),
				beginPath,
				n_strokeFillRectV2 (STD.MM.back_button_xy) (STD.MM.back_button_wh),

			// -- Backward Button Text
			n_setFontSize (0.075),
			setFontFamily ('RobotoMono'),
			setToCenterText,
				beginPath,
				n_strokeFillTextV2 ("BACK") (STD.MM.back_button_text_xy),

			render_Crosshair
		)
	:
		never

const render_Ending = (global : Global) => (ending : Local) : IO <null> =>
	ending.variation === 'Ending' ?
		executing
		(
			setFillColor (STD.palette [0]),
				n_fillRect (0) (0) (1) (1),

			setStrokeColor     (STD.palette [1]),
			n_setLineThickness (0.02),
			setLineCap         (LineCap.Round),
				render_EndingSubarcs (ending.bgOffset) (1.2),

			setStrokeColor (STD.palette [2]),
			n_setLineThickness (0.01),
				render_EndingSubarcs (ending.bgOffset * 0.75) (1.35),

			setStrokeColor (STD.palette [3]),
			n_setLineThickness (0.01),
				render_EndingSubarcs (ending.bgOffset * 0.5) (1.45),

			setStrokeColor (STD.palette [4]),
			n_setLineThickness (0.005),
				render_EndingSubarcs (ending.bgOffset * 0.25) (1.55),

			setStrokeColor (STD.palette [1]),
			n_setLineThickness (0.02),
				render_EndingSubarcs (ending.bgOffset * 0.2) (1.85),

			setStrokeColor (STD.palette [2]),
			n_setLineThickness (0.01),
				render_EndingSubarcs (ending.bgOffset * 0.15) (2.2),

			setStrokeColor (STD.palette [3]),
			n_setLineThickness (0.01),
				render_EndingSubarcs (ending.bgOffset * 0.125) (2.5),

			setStrokeColor (STD.palette [4]),
			n_setLineThickness (0.005),
				render_EndingSubarcs (ending.bgOffset * 0.12) (2.75),

			setStrokeColor (STD.palette [1]),
			n_setLineThickness (0.02),
				render_EndingSubarcs (ending.bgOffset * 0.1) (3.5),

			setStrokeColor (STD.palette [2]),
			n_setLineThickness (0.01),
				render_EndingSubarcs (ending.bgOffset * 0.0875) (4),

			setStrokeColor (STD.palette [3]),
			n_setLineThickness (0.01),
				render_EndingSubarcs (ending.bgOffset * 0.05) (4.5),

			setStrokeColor (STD.palette [4]),
			n_setLineThickness (0.005),
				render_EndingSubarcs (ending.bgOffset * 0.025) (5),
			setStrokeColor ('Black'),


			setToCenterText,
			n_setLineThickness (0.01),
			n_setFontSize      (0.1),
			setFontFamily      ('Hyperlegible-Italic'),
			setFillColor       ('White'),
				n_strokeFillText ("YOU SAVED RALPH!") (0.5) (0.125),

			n_setLineThickness (0.005),
			n_setFontSize      (0.03),
			setFontFamily      ('Hyperlegible'),
				n_strokeFillText ("(somehow)") (0.5) (0.225),

			n_setLineThickness (0.0065),
			n_setFontSize      (0.0375),
				n_strokeFillMultilineText (
					"Thanks for playing Hangman's Gambit: Protein Synthesis!\n" +
					"This game was made as a demo for LoveLace\n" +
					"and to spread awareness for Ralph the Tuxedo Cat.\n" +
					"In memory of him, here's a quote:\n"
				) (0.075) (0.5) (0.35),

			n_setFontSize (0.045),
			setFontFamily ('Times New Roman'),
				n_strokeFillText (`"maow" - Ralph the Tuxedo Cat`) (0.5) (0.68),

			n_setLineThickness (0.01),
				beginPath,
				n_strokeFillRectV2 (STD.END.back_button_xy) (STD.END.back_button_wh),

			n_setLineThickness (0.0075),
			n_setFontSize      (0.05),
			setFontFamily      ('RobotoMono'),
			setToCenterText,
				beginPath,
				n_strokeFillText ("BACK TO MAINMENU") (0.5) (STD.END.back_button_text_y),

			render_Crosshair
		)
	:
		never

/**` render_T_Mainmenu_Instructions : Global -> Local -> Local -> Number -> IO () `*/
const render_T_Mainmenu_Instructions =
	(global : Global) => (mainmenu : Local) => (instructions : Local) => (t : number) : IO <null> =>
	executing
	(
		// -- Leftside
		saveCanvasState,
			beginPath,
			n_area (0) (0) (t) (1),
		clipEvenOdd,
			render_Instructions (global) (instructions),
		restoreCanvasState,

		// -- Rightside
		saveCanvasState,
			beginPath,
			n_area (t) (0) (1) (1),
		clipEvenOdd,
			render_Mainmenu (global) (mainmenu),
		restoreCanvasState,

		// -- Division Line
		setStrokeColor     ('Black'),
		n_setLineThickness (0.01),
			beginPath,
			n_line (t) (0) (t) (1),
			stroke,

		render_Crosshair
	)

/**` render_T_Mainmenu_Credits : Global -> Local -> Local -> Number -> IO () `*/
const render_T_Mainmenu_Credits = (global : Global) => (mainmenu : Local) => (credits : Local) => (t : number) : IO <null> =>
	executing
	(
		// -- Leftside
		saveCanvasState,
			beginPath,
			n_area (0) (0) (1 - t) (1),
		clipEvenOdd,
			render_Mainmenu (global) (mainmenu),
		restoreCanvasState,

		// -- Rightside
		saveCanvasState,
			beginPath,
			n_area (1 - t) (0) (1) (1),
		clipEvenOdd,
			render_Credits (global) (credits),
		restoreCanvasState,

		// -- Division Line
		setStrokeColor     ('Black'),
		n_setLineThickness (0.01),
			beginPath,
			n_line (1 - t) (0) (1 - t) (1),
			stroke,

		render_Crosshair
	)

/**` render_Game_T_Game : Global -> Local -> Local -> Number -> IO () `*/
const render_Game_T_Game = (global : Global) => (preGame : Local) => (postGame : Local) => (t : number) : IO <null> =>
	preGame.variation === 'Game' && postGame.variation === 'Game' ?
		t < 0.25 ?
			executing
			(
				render_Game (global) (preGame),

				// -- "THAT'S RIGHT!"
				setAlpha (min (1) (128 * t)),
					render_ThatsRight,
				setAlpha (1)
			)
		:
		t < 0.75 ?
			executing
			(
				// -- Leftside
				saveCanvasState,
					beginPath,
					n_area (0) (0) (2 * quadraticCurve (t) - 0.5) (1),
				clipEvenOdd,
					render_Game (global) (postGame),
					render_QuestionIndex (postGame),
				restoreCanvasState,

				// -- Rightside
				saveCanvasState,
					beginPath,
					n_area (2 * quadraticCurve (t) - 0.5) (0) (1) (1),
				clipEvenOdd,
					render_Game (global) (preGame),
					render_ThatsRight,
				restoreCanvasState,

				// -- Division Line
				setStrokeColor     ('Black'),
				n_setLineThickness (0.01),
					beginPath,
					n_vectorLine (2 * quadraticCurve (t) - 0.5) (0) (0) (1),
					stroke
			)
		:
			executing
			(
				render_Game (global) (postGame),

				setAlpha (bound (0) (1) (1 - quadraticCurve (t * 4 - 3))),
					render_QuestionIndex (postGame),
				setAlpha (1)
			)
	:
		never

/**` render_ThatsRight : IO () `*/
const render_ThatsRight : IO <null> =
	executing
	(
		n_setLineThickness (0.015),
		setStrokeColor     ('Black'),
		setFillColor       ('LightGreen'),
			beginPath,
			n_strokeFillRect (0) (0.4) (1) (0.2),

		setMiterLimit      (2.5),
		n_setLineThickness (0.01),
		n_setFontSize      (0.085),
		setFillColor       ('White'),
		setFontFamily      ('Hyperlegible'),
			beginPath,
			n_strokeFillText ("THAT'S RIGHT!") (0.5) (0.515)
	)

/**` render_QuestionIndex : Local -> IO () `*/
const render_QuestionIndex = (game : Local) : IO <null> =>
	game.variation === 'Game' ?
		executing
		(
			n_setLineThickness (0.015),
			setStrokeColor     ('Black'),
			setFillColor       ('Orange'),
				beginPath,
				n_strokeFillRect (0) (0.4) (1) (0.2),

			setMiterLimit      (2.5),
			n_setLineThickness (0.01),
			n_setFontSize      (0.085),
			setFontFamily      ('Hyperlegible'),
			setFillColor       ('White'),
				beginPath,
				n_strokeFillText
					(`QUESTION #${STD.G.question_amount - len (game.questions) + 1}/${STD.G.question_amount}`)
					(0.5)
					(0.515)
		)
	:
		never

/**` render_Crosshair : IO () `*/
const render_Crosshair : IO <null> =
	Do.IO
		/**` $.position : Vector2 `*/
		.bindto ('position') <Vector2> (_ => n_mouseCanvasPosition)

		/**` $.dimensions : Vector2 `*/
		.bindto ('dimensions') <Vector2> (_ => n_imageDimensions ('./image/crosshair.png'))

		.bind ($ =>
			n_squareImageV2
				('./image/crosshair.png')
				(0.05)
				(V2.untranslateXY (0.025) (0.025 * STD.aspectRatio) ($.position))
		)

/**` render_InstructionsBgArcs : Number -> Number -> Number -> IO () `*/
const render_InstructionsBgArcs = (d : number) => (r : number) => (i : number) : IO <null> =>
	executing
	(
		setStrokeColor (STD.palette [i]),
			beginPath,
			n_strokeArcSection (r) (d) (1 / 6) (0) (0),
			beginPath,
			n_strokeArcSection (r) (d) (1 / 6) (1) (1)
	)

/**` render_CreditsBgArcs : Number -> Number -> Number -> IO () `*/
const render_CreditsBgArcs = (d : number) => (r : number) => (i : number) : IO <null> =>
	executing
	(
		setStrokeColor (STD.palette [i]),
			beginPath,
			n_strokeArcSection (r) (d) (1 / 6) (0.5) (0),
			beginPath,
			n_strokeArcSection (r) (d) (1 / 6) (0.5) (1),
			beginPath,
			n_strokeArcSection (r) (d) (1 / 6) (0) (0.5),
			beginPath,
			n_strokeArcSection (r) (d) (1 / 6) (1) (0.5)
	)

/**` render_MainmenuSubarcs : Number -> Number -> IO () `*/
const render_MainmenuSubarcs = (d : number) => (k : number) : IO <null> =>
	executing
	(
		beginPath, n_strokeArcSectionV2 (STD.MM.arc_r * k) (d        ) (1 / 6) (STD.MM.arc_xy),
		beginPath, n_strokeArcSectionV2 (STD.MM.arc_r * k) (d + 1 / 3) (1 / 6) (STD.MM.arc_xy),
		beginPath, n_strokeArcSectionV2 (STD.MM.arc_r * k) (d + 2 / 3) (1 / 6) (STD.MM.arc_xy)
	)

/**` render_EndingSubarcs : Number -> Number -> IO () `*/
const render_EndingSubarcs = (d : number) => (k : number) : IO <null> =>
	executing
	(
		beginPath, n_strokeArcSectionV2 (STD.MM.arc_r * k) (d        ) (1 / 6) (V2.half),
		beginPath, n_strokeArcSectionV2 (STD.MM.arc_r * k) (d + 1 / 3) (1 / 6) (V2.half),
		beginPath, n_strokeArcSectionV2 (STD.MM.arc_r * k) (d + 2 / 3) (1 / 6) (V2.half)
	)

/**` render_Letter : Letter -> IO () `*/
const render_Letter = (letter : Letter) : IO <null> =>
	executing
	(
		setFillColor (STD.palette [letter.colorIndex]),
		setAlpha (bound (0) (1) (4 * (letter.lifetime - letter.lifetime ** 2))),
			beginPath,
			n_strokeFillCircleV2 (STD.G.letter_radius) (letter.position),
		setFillColor ('White'),
			n_strokeFillTextV2 (letter.character) (letter.position),
		beginPath,
			n_strokeArcSectionV2 (STD.G.letter_radius * 1.25) (letter.lifetime) (1 / 6) (letter.position)
	)
