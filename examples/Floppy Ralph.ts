/********************************************************************************************************************************/

const STD =
{
	macro :
	{
		rec : (variation : string) => <a>(record : Object) : a =>
			({
				variation,
				pipe (f : any) { return f (this) },
				...record
			}) as any
	},

	audio_wingflaps :
		List (
			'./audio/wingflap_0.mp3',
			'./audio/wingflap_1.mp3',
			'./audio/wingflap_2.mp3',
			'./audio/wingflap_3.mp3',
			'./audio/wingflap_4.mp3',
			'./audio/wingflap_5.mp3'
		),

	audio_meows     : List (
		'./audio/meow_0.mp3',
		'./audio/meow_1.mp3',
		'./audio/meow_2.mp3',
		'./audio/meow_loud_lq_0.mp3'
	),

	aspect_ratio    : 9 / 16,
	refresh_rate    : 12,
	resizing_speed  : 0.15,
	bg_speed        : 0.5,
	ground_hitbox_y : 0.9,

	pipe :
	{
		width      : 0.25,
		space_vert : 0.2,
		space_hort : 0.8,
		padding    : 0.25,
		speed      : 0.0125
	},

	player :
	{
		gravity_strength : 0.0005,
		jump_strength    : 0.012,
		max_lift         : 0.006,
		max_drop         : 0.015,
		drag             : 0.9,
		hitbox_dx        : 0.05,
		hitbox_dy        : 0.01,
		hitbox_w         : 0.1,
		hitbox_h         : 0.04,
		x                : 0.05,
		speed            : 0.015
	},

	trans :
	{
		StartToGame     : 0.05,
		GameToGameOver  : 0.025,
		GameOverToStart : 0.02,

		ktf_StartToGame     : (t : number) => t ** 2,
		ktf_GameToGameOver  : (t : number) => t ** 3,
		ktf_GameOverToStart : (t : number) => 1.1 * (t ** 2 - 0.5) + 0.5
	}
}

/********************************************************************************************************************************/

type Global =
	{
		variation      : 'Global'
		pipe           : Pipe <Global>
		isResizing     : boolean
		isRefresh      : boolean
		canvasWidth    : number
		time           : number
		counter        : number
		refreshCounter : number
	}

type Local =
	{
		pipe : Pipe <Local>
	}
		&
	({
		variation : 'Start'
		player    : Player
		distance  : number
	} | {
		variation : 'StartToGame'
		counter   : number
		keytime   : number
		player    : Player
		distance  : number
	} | {
		variation : 'Game'
		player    : Player
		score     : number
		distance  : number
		pipeX     : number
		pipeYs    : List <number>
	} | {
		variation : 'GameToGameOver'
		counter   : number
		keytime   : number
		failGame  : Local
	} | {
		variation : 'GameOver'
		failGame  : Local
	} | {
		variation : 'GameOverToStart'
		counter   : number
		keytime   : number
		failGame  : Local
		newStart  : Local
	})

type Player =
	{
		pipe      : Pipe <Player>
		variation : 'Player'
		rotation  : number
		y         : number
		vy        : number
	}

/********************************************************************************************************************************/

const Global          : (record : Recordize <Global>                              ) => Global = STD.macro.rec ('Global')
const Start           : (record : Recordize <Variation <Local, 'Start'          >>) => Local  = STD.macro.rec ('Start')
const StartToGame     : (record : Recordize <Variation <Local, 'StartToGame'    >>) => Local  = STD.macro.rec ('StartToGame')
const Game            : (record : Recordize <Variation <Local, 'Game'           >>) => Local  = STD.macro.rec ('Game')
const GameToGameOver  : (record : Recordize <Variation <Local, 'GameToGameOver' >>) => Local  = STD.macro.rec ('GameToGameOver')
const GameOver        : (record : Recordize <Variation <Local, 'GameOver'       >>) => Local  = STD.macro.rec ('GameOver')
const GameOverToStart : (record : Recordize <Variation <Local, 'GameOverToStart'>>) => Local  = STD.macro.rec ('GameOverToStart')
const Player          : (record : Recordize <Player>                              ) => Player = STD.macro.rec ('Player')

/********************************************************************************************************************************/

const idealCanvasWidth : IO <number> =
	windowDimensions
		.fmap (v2ToPair)
		.fmap (mapSnd (mul (STD.aspect_ratio)))
		.fmap (uncurry (min))
		.fmap (minus (15))

const setCanvasWidthProportionally = (width : number) : IO <null> =>
	setCanvasDimensions (width) (width / STD.aspect_ratio)

const jumpPlayer = (player : Player) : Player =>
	Player
	({
		rotation : player.rotation,
		y        : player.vy - STD.player.jump_strength + player.y,
		vy       : player.vy - STD.player.jump_strength
	})

const pullPlayer = (player : Player) : Player =>
	Player
	({
		rotation : lerp (0.5) (player.rotation) (player.vy * 4 - 0.015),
		y        : player.y + player.vy,
		vy       :
			player.vy > STD.player.max_drop
				? STD.player.max_drop
				: player.vy < -STD.player.max_lift
					? player.vy * STD.player.drag
					: player.vy + STD.player.gravity_strength
	})

const isCollision = (player : Player) => (pipeX : number) => (pipeY : number) : boolean =>
{
	if (player.y + STD.player.hitbox_dy > STD.ground_hitbox_y)
		return true
	else
	{
		const _kx = pipeX - STD.player.x - STD.player.hitbox_dx
		const _ky = pipeY - player.y - STD.player.hitbox_dy
		return _kx < STD.player.hitbox_w && _kx > -STD.pipe.width && (_ky > STD.pipe.space_vert || _ky < STD.player.hitbox_h)
	}
}

const initialStart : Local =
	Start
	({
		distance : 0,
		player   :
			Player
			({
				rotation : 0,
				y        : 0.5,
				vy       : 0
			})
	})

/********************************************************************************************************************************/

const main =
	Do_IO
		.side (STD.audio_wingflaps .link (STD.audio_meows) .fmap (loadAudio) .pipe (execute_IO))
		.side (loadImage ('./image/bg_flappybird.png'))
		.side (loadImage ('./image/grass_sideview.png'))
		.side (loadImage ('./image/ralph_flappybird.png'))
		.side (loadImage ('./image/pipe_flappybird.png'))
		.side (loadFont  ('./font/FlappyBird.ttf'))

		.bindto ('presentTime')        <number> (_ => time)
		.bindto ('initialCanvasWidth') <number> (_ => idealCanvasWidth)

		.call   ($ => setCanvasWidthProportionally ($.initialCanvasWidth))
		.bind
		($ =>
			loop (
				Global
				({
					isResizing     : false,
					isRefresh      : false,
					counter        : 0,
					refreshCounter : 0,
					canvasWidth    : $.initialCanvasWidth,
					time           : $.presentTime
				})
			)(initialStart)
		)

const loop = (global : Global) => (local : Local) : IO <null> =>
	Do_IO
		.bindto ('presentTime')             <number>  (_ => time)
		.bindto ('currentIdealCanvasWidth') <number>  (_ => idealCanvasWidth)

		.bindto ('isCurrentlyResizing') <boolean>
		($ =>
			global.isResizing && diff (global.canvasWidth) ($.currentIdealCanvasWidth) > 10
				? send (true)
				: isWindowResized
		)

		.fmapto ('deltaTime') <number>
		($ => $.presentTime - global.time)

		.fmapto ('nextRefreshCounter') <number>
		($ => global.refreshCounter > STD.refresh_rate ? $.deltaTime : global.refreshCounter + $.deltaTime)

		.fmapto ('isCurrentlyRefreshing') <boolean>
		($ => $.nextRefreshCounter > STD.refresh_rate)

		.fmapto ('nextGlobal') <Global>
		($ =>
			Global
			({
				isResizing     : $.isCurrentlyResizing,
				isRefresh      : $.isCurrentlyRefreshing,
				time           : $.presentTime,
				refreshCounter : $.nextRefreshCounter,
				counter        : global.counter + $.deltaTime,
				canvasWidth    :
					$.isCurrentlyResizing && $.isCurrentlyRefreshing
						? lerp (STD.resizing_speed) (global.canvasWidth) ($.currentIdealCanvasWidth)
						: global.canvasWidth
			})
		)

		.bindto ('nextLocal') <Local>
		($ => update ($.nextGlobal) (local))

		.call ($ =>
			$.isCurrentlyResizing && $.isCurrentlyRefreshing
				? setCanvasWidthProportionally ($.nextGlobal.canvasWidth)
				: idle
		)

		.call ($ => render ($.nextLocal))
		.bind ($ => queueIO (loop ($.nextGlobal) ($.nextLocal)))

const update = (global : Global) => (local : Local) : IO <Local> =>
	local.variation === 'Start' ?
		keyboardKey ('Space')
			.fmap (isButtonDown)
			.call (isSpaceDown => isSpaceDown ? trigger_wingflap : idle)
			.fmap (isSpaceDown =>
				isSpaceDown
					? StartToGame
						({
							counter  : 0,
							keytime  : STD.trans.ktf_StartToGame (0),
							distance : local.distance,
							player   : jumpPlayer (local.player)
						})
					: global.isRefresh
						? iterate_Start (local)
						: local
			)
	:
	local.variation === 'StartToGame' ?
		local .pipe (global.isRefresh ? update_StartToGame : send)
	:
	local.variation === 'Game' ?
		global.isRefresh
			? Do_IO
				.bindto ('nextPlayer') <Player> (_ => update_player (local.player))
				.fmapto ('isCollided') <boolean> ($ => isCollision ($.nextPlayer) (local.pipeX) (head (local.pipeYs)))
				.call ($ => $.isCollided ? trigger_gameover : idle)
				.fmap ($ =>
					$.isCollided
						? GameToGameOver
							({
								counter  : 0,
								keytime  : STD.trans.ktf_GameToGameOver (0),
								failGame : local
							})
						: local.pipeX < -STD.pipe.width
							? Game
								({
									player   : $.nextPlayer,
									distance : local.distance + STD.player.speed,
									pipeX    : local.pipeX - STD.pipe.speed + STD.pipe.space_hort,
									pipeYs   : tail (local.pipeYs),
									score    : local.score + 1
								})
							: Game
								({
									player   : $.nextPlayer,
									distance : local.distance + STD.player.speed,
									pipeX    : local.pipeX - STD.pipe.speed,
									pipeYs   : local.pipeYs,
									score    : local.score
								})
				)
			: send (local)
	:
	local.variation === 'GameToGameOver' ?
		local
			.pipe (global.isRefresh ? iterate_GameToGameOver : id)
			.pipe (send)
	:
	local.variation === 'GameOver' ?
		keyboardKey ('Space')
			.fmap (spaceState =>
				isButtonDown (spaceState)
					? GameOverToStart
						({
							failGame : local.failGame,
							newStart : initialStart,
							counter  : 0,
							keytime  : STD.trans.ktf_GameOverToStart (0)
						})
					: local
			)
	:
	local.variation === 'GameOverToStart' ?
		local
			.pipe (global.isRefresh ? iterate_GameOverToStart : id)
			.pipe (send)
	:
		never

const render = (local : Local) : IO <null> =>
	local.variation === 'Start' ?
		executing
		(
			setActiveLayer (1),
			clearLayer,

			render_bg     (local.distance * STD.bg_speed),
			render_player (local.player),
			render_title  (0.1),
			render_fg     (local.distance)
		)
	:
	local.variation === 'StartToGame' ?
		executing
		(
			setActiveLayer (1),
			clearLayer,

			render_bg     (local.distance * STD.bg_speed),
			render_player (local.player),
			render_title  (lerp (local.keytime) (0.1) (-0.1)),
			render_score  (0) (lerp (local.keytime) (-0.1) (0.1)),
			render_fg     (local.distance)
		)
	:
	local.variation === 'Game' ?
		executing
		(
			setActiveLayer (1),
			clearLayer,

			render_bg            (local.distance * STD.bg_speed),
			render_Game_wo_score (local),
			render_score         (local.score) (0.1),
			render_fg            (local.distance)
		)
	:
	local.variation === 'GameToGameOver' && local.failGame.variation === 'Game' ?
		executing
		(
			setActiveLayer (1),
			clearLayer,

			render_bg            (local.failGame.distance * STD.bg_speed),
			render_Game_wo_score (local.failGame),
			render_gameoverText  (lerp (local.keytime) (-0.5) (0.5)),
			render_score         (local.failGame.score) (lerp (local.keytime) (0.1) (0.6)),
			render_fg            (local.failGame.distance)
		)
	:
	local.variation === 'GameOver' && local.failGame.variation === 'Game' ?
		executing
		(
			setActiveLayer (1),
			clearLayer,

			render_bg            (local.failGame.distance * STD.bg_speed),
			render_Game_wo_score (local.failGame),
			render_gameoverText  (0.5),
			render_score         (local.failGame.score) (0.6),
			render_fg            (local.failGame.distance)
		)
	:
	local.variation === 'GameOverToStart' && local.failGame.variation === 'Game' && local.newStart.variation === 'Start' ?
		executing
		(
			setActiveLayer (1),
			clearLayer,

			saveLayerState,
			beginPath,
			n_relocateTo (0) (local.keytime),
			n_lineTo     (1) (local.keytime),
			n_lineTo     (1) (1),
			n_lineTo     (0) (1),
			closePath,
			clipEvenOdd,
				render_bg            (local.failGame.distance * STD.bg_speed),
				render_Game_wo_score (local.failGame),
				render_gameoverText  (0.5),
				render_score         (local.failGame.score) (0.6),
				render_fg            (local.failGame.distance),
			restoreLayerState,

			saveLayerState,
			beginPath,
			n_relocateTo (0) (0),
			n_lineTo     (1) (0),
			n_lineTo     (1) (local.keytime),
			n_lineTo     (0) (local.keytime),
			closePath,
			clipEvenOdd,
				render_bg (local.newStart.distance * STD.bg_speed),
				render_player (local.newStart.player),
				render_title  (0.1),
				render_fg     (local.newStart.distance),
			restoreLayerState,

			n_setLineThickness (0.05),
			beginPath,
			n_relocateTo (0) (local.keytime),
			n_lineTo     (1) (local.keytime),
			stroke
		)
	:
		never

/********************************************************************************************************************************/

const iterate_Start = (local : Local) : Local =>
	local.variation === 'Start' ?
		Start
			({
				distance : local.distance + STD.player.speed,
				player   : local.player .pipe (local.player.y > 0.6 ? jumpPlayer : pullPlayer)
			})
	:
		never

const iterate_GameToGameOver = (local : Local) : Local =>
	local.variation === 'GameToGameOver' ?
		local.counter < 1 ?
			GameToGameOver
			({
				failGame : local.failGame,
				keytime  : STD.trans.ktf_GameToGameOver (local.counter),
				counter  : local.counter + STD.trans.GameToGameOver
			})
		:
			GameOver
			({
				failGame : local.failGame
			})
	:
		never

const iterate_GameOverToStart = (local : Local) : Local =>
	local.variation === 'GameOverToStart' ?
		local.counter < 1 ?
			GameOverToStart
			({
				failGame : local.failGame,
				newStart : iterate_Start (local.newStart),
				keytime  : STD.trans.ktf_GameOverToStart (local.counter),
				counter  : local.counter + STD.trans.GameOverToStart
			})
		:
			local.newStart
	:
		never

const update_player = (player : Player) : IO <Player> =>
	keyboardKey ('Space')
		.fmap (eq <ButtonState> ('toDown'))
		.call (isSpacePressed => isSpacePressed ? trigger_wingflap : idle)
		.fmap (isSpacePressed =>
			isSpacePressed
				? jumpPlayer (player)
				: pullPlayer (player)
		)

const update_StartToGame = (local : Local) : IO <Local> =>
	local.variation === 'StartToGame' ?
		local.counter < 1 ?
			update_player (local.player)
				.fmap (player =>
					StartToGame
					({
						counter  : local.counter + STD.trans.StartToGame,
						keytime  : STD.trans.ktf_StartToGame (local.counter),
						distance : local.distance + STD.player.speed,
						player
					})
				)
		: randomSeedIO
			.fmap (seed =>
				Game
				({
					player   : local.player,
					distance : local.distance,
					pipeX    : 1,
					pipeYs   : repeatProcess (randomFloatRange (STD.pipe.padding) (1 - STD.pipe.padding)).computation (seed).snd,
					score    : 0
				})
			)
	:
		never

const render_Game_wo_score = (local : Local) : IO <null> =>
	local.variation === 'Game' ?
		executing
		(
			render_player (local.player),
			render_pipe (local.pipeX) (at (0) (local.pipeYs)),
			render_pipe (local.pipeX + STD.pipe.space_hort) (at (1) (local.pipeYs))
		)
	:
		never

const render_player = (player : Player) : IO <null> =>
	executing
	(
		saveLayerState,
		n_translateLayer (STD.player.x) (player.y),
		n_rotateLayer (player.rotation),
		n_fullScaledImage ('./image/ralph_flappybird.png') (0.2) (0) (0),
		restoreLayerState
	)

const render_title = (y : number) : IO <null> =>
	executing
	(
		setToCenteredTextStyle,
		n_setLineThickness (0.03),
		n_setFontSize      (0.15),
		setFontFamily      ('FlappyBird'),
		setStrokeColor     ('Black'),
		setFillColor       ('White'),
		n_strokeFillText ("Floppy Ralph") (0.5) (y)
	)

const render_pipe = (x : number) => (y : number) : IO <null> =>
	executing
	(
		saveLayerState,
		n_fullScaledImage ('./image/pipe_flappybird.png') (STD.pipe.width) (x) (y),
		scaleLayerAxisY (-1),
		n_fullScaledImage ('./image/pipe_flappybird.png') (STD.pipe.width) (x) (STD.pipe.space_vert - y),
		restoreLayerState
	)

const render_score = (score : number) => (y : number) : IO <null> =>
	executing
	(
		n_setFontSize      (0.1),
		n_setLineThickness (0.02),
		n_strokeFillText (show (score)) (0.5) (y)
	)

const render_gameoverText = (y : number) : IO <null> =>
	executing
	(
		n_setFontSize      (0.15),
		n_setLineThickness (0.03),
		setFontFamily      ('FlappyBird'),
		setFillColor       ('White'),
		setStrokeColor     ('Black'),
		setToCenteredTextStyle,
		n_strokeFillText ("Game Over") (0.5) (y)
	)

const render_bg = (x : number) : IO <null> =>
	Do_IO
		.bindto ('bgImgAspectRatio') <number> (_ => n_imageAspectRatio ('./image/bg_flappybird.png'))
		.fmapto ('bgX')              <number> ($ => -x % $.bgImgAspectRatio)
		.call ($ => n_fullScaledImage ('./image/bg_flappybird.png') ($.bgImgAspectRatio) ($.bgX) (0))
		.call ($ => n_fullScaledImage ('./image/bg_flappybird.png') ($.bgImgAspectRatio) ($.bgX + $.bgImgAspectRatio - 0.001) (0))

const render_fg = (x : number) : IO <null> =>
	Do_IO
		.fmapto ('fgX') <number> (_ => -x % 1)
		.bindto ('fgY') <number>
		(_ => imageAspectRatio ('./image/grass_sideview.png') .fmap (ratio => 1 - STD.aspect_ratio / ratio))
		.call ($ => n_fullScaledImage ('./image/grass_sideview.png') (1) ($.fgX) ($.fgY))
		.bind ($ => n_fullScaledImage ('./image/grass_sideview.png') (1) ($.fgX + 1 - 0.001) ($.fgY))

const trigger_wingflap : IO <null> =
	randomSeedIO
		.fmap (evalProcess (randomElem (STD.audio_wingflaps)))
		.bind (playSFX)

const trigger_gameover : IO <null> =
	randomSeedIO
		.fmap (evalProcess (randomElem (STD.audio_meows)))
		.bind (playSFX)
