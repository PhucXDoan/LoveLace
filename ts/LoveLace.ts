/** Throws an error via a function call. */
const THROW = (error : Error) =>
	{ throw error }

/********************************************************************************************************************************/

/** The `IO` monad.
 *```
 * data IO a
 * (.bind)   :: IO a -> (a -> IO b) -> IO b
 * (.fmap)   :: IO a -> (a ->    b) -> IO b
 * (.bindto) :: IO $ -> String -> ($ -> IO a) -> IO $
 * (.fmapto) :: IO $ -> String -> ($ ->    a) -> IO $
 * (.then)   :: IO a -> IO b -> IO b
 *```
 * The `IO` data constructor should only be used to create new, unique `IO` operations
 * not already defined in `blanc.ts`. Compose other `IO` operations to get a desired
 * effect rather than defining a new `IO` instance.
 */
type IO<a> =
	{
		readonly CONS   : 'IO a'
		readonly INFO   : () => a
		readonly bind   : <b>(reaction    : (evaluation : a) => IO<b>) => IO<b>
		readonly fmap   : <b>(computation : (evaluation : a) =>    b ) => IO<b>
		readonly bindto :
			<k extends string>(name : k)
				=> <b>(reaction : ($ : a) => IO<b>)
				=> IO<a & { [x in k] : b }>
		readonly fmapto :
			<k extends string>(name : k)
				=> <b>(computation : ($ : a) => b)
				=> IO<a & { [x in k] : b }>

		readonly then : <b>(successor : IO<b>) => IO<b>
	}

/** The `Maybe` monad.
 * ```
 * data Maybe a = Nothing | Just a
 * (.bind)   :: Maybe a -> (a -> Maybe b) -> Maybe b
 * (.fmap)   :: Maybe a -> (a ->       b) -> Maybe b
 * (.bindto) :: Maybe $ -> String -> ($ -> Maybe a) -> Maybe $
 * (.fmapto) :: Maybe $ -> String -> ($ ->       a) -> Maybe $
 * ```
 */
type Maybe<a> =
	({
		readonly CONS   : 'Nothing'
	} | {
		readonly CONS   : 'Just a'
		readonly INFO   : a
	}) & {
		readonly bind   : <b>(reaction    : (evaluation : a) => Maybe<b>) => Maybe<b>
		readonly fmap   : <b>(computation : (evaluation : a) =>       b ) => Maybe<b>
		readonly bindto :
			<k extends string>(name : k)
				=> <b>(reaction : ($ : a) => Maybe<b>)
				=> Maybe<a & { [x in k] : b }>
		readonly fmapto :
			<k extends string>(name : k)
				=> <b>(computation : ($ : a) => b)
				=> Maybe<a & { [x in k] : b }>
	}

/** The `State` monad.
 * ```
 * data State s a = State (s -> (s, a))
 * (.bind)   :: State s a -> (a -> State s b) -> State s b
 * (.fmap)   :: State s a -> (a ->         b) -> State s b
 * (.bindto) :: State s $ -> String -> ($ -> State s a) -> State s $
 * (.fmapto) :: State s $ -> String -> ($ ->         b) -> State s $
 * (.then)   :: State s a -> State s b -> State s b
 * ```
 */
type State<s, a> =
	{
		readonly CONS   : 'State (s -> (s, a))'
		readonly INFO   : (inputState : s) => [s, a]
		readonly bind   : <b>(reaction    : (statefulComputationOutput : a) => State<s, b>) => State<s, b>
		readonly fmap   : <b>(computation : (statefulComputationOutput : a) =>          b ) => State<s, b>
		readonly bindto :
			<k extends string>(name : k)
				=> <b>(reaction : ($ : a) => State<s, b>)
				=> State<s, a & { [x in k] : b }>
		readonly fmapto :
			<k extends string>(name : k)
				=> <b>(computation : ($ : a) => b)
				=> State<s, a & { [x in k] : b }>

		readonly then : <b>(successor : State<s, b>) => State<s, b>
	}

/** The `List` monad.
 * ```
 * data List a = List [a]
 * (.bind)   :: List a -> (a -> List b) -> List b
 * (.fmap)   :: List a -> (a ->      b) -> List b
 * (.bindto) :: List $ -> String -> ($ -> List b) -> List $
 * (.fmapto) :: List $ -> String -> ($ ->      b) -> List $
 * (.at)     :: List a -> Number -> a
 * ```
 */
type List<a> =
	{
		readonly CONS   : 'List a'
		readonly INFO   : ReadonlyArray<a>
		readonly bind   : <b>(reaction    : (element : a) => List<b>) => List<b>
		readonly fmap   : <b>(computation : (element : a) =>      b ) => List<b>
		readonly bindto :
			<k extends string>(name : k)
				=> <b>(reaction : ($ : a) => List<b>)
				=> List<a & { [x in k] : b }>
		readonly fmapto :
			<k extends string>(name : k)
				=> <b>(computation : ($ : a) => b)
				=> List<a & { [x in k] : b }>

		readonly at : (index : number) => a
	}

/********************************************************************************************************************************/

// -- Use only for creating new IO operations; otherwise, compose existing IO monads together.
const IO = <a>(sideeffect : () => a) : IO<a> =>
	({
		CONS   : 'IO a',
		INFO   : sideeffect,
		bind   : f => IO(() => f(sideeffect()).INFO()),
		fmap   : f => IO(() => f(sideeffect())),
		bindto : x => f =>
			IO(() => {
				const $ = sideeffect()
				return { ...$, [x]: f($).INFO() } as any
			}),
		fmapto : x => f =>
			IO(() => {
				const $ = sideeffect()
				return { ...$, [x]: f($) } as any
			}),

		then : x => IO(() => (sideeffect(), x.INFO()))
	})

/**` Nothing :: Maybe a `*/
const Nothing : Maybe<any> =
	{
		CONS   : 'Nothing',
		bind   : _ => Nothing,
		fmap   : _ => Nothing,
		bindto : _ => _ => Nothing,
		fmapto : _ => _ => Nothing
	}

/**` Just :: a -> Maybe a `*/
const Just = <a>(value : a) : Maybe<a> =>
	({
		CONS   : 'Just a',
		INFO   : value,
		bind   : f =>
			{
				const x = f(value)
				return x.CONS === 'Nothing'
					? Nothing
					: x
			},
		fmap   : f => Just(f(value)),
		bindto : x => f =>
			{
				const y = f(value)
				return y.CONS === 'Nothing'
					? Nothing
					: Just({ ...value, [x]: y.INFO }) as any
			},
		fmapto : x => f =>
			Just({ ...value, [x]: f(value) }) as any
	})

/**` State :: (s -> (s, a)) -> State s a `*/
const State = <s, a>(statefulComputation : (inputState : s) => [s, a]) : State<s, a> =>
	({
		CONS   : 'State (s -> (s, a))',
		INFO   : statefulComputation,
		bind   : f =>
			State(x => {
				const [y, z] = statefulComputation(x)
				return f(z).INFO(y)
			}),
		fmap   : f =>
			State(x => {
				const [y, z] = statefulComputation(x)
				return [y, f(z)]
			}),
		bindto : k => f =>
			State(x => {
				const [y, $] = statefulComputation(x)
				const [z, w] = f($).INFO(y)
				return [z, { ...$, [k]: w }] as any
			}),
		fmapto : k => f =>
			State(x => {
				const [y, $] = statefulComputation(x)
				return [y, { ...$, [k]: f($) }] as any
			}),

		then : s => State(x => s.INFO(statefulComputation(x)[0]))
	})

/**` List :: [a] -> List a `*/
const List = <a>(...elements : ReadonlyArray<a>) : List<a> =>
	({
		CONS   : 'List a',
		INFO   : elements,
		bind   : f    => List(...elements.flatMap(x => f(x).INFO)),
		fmap   : f => List(...elements.map(x => f(x))),
		bindto : k => f =>
			List(...elements.flatMap($ => f($).INFO.map(x => ({ ...$, [k]: x })))) as any,
		fmapto : k => f =>
			List(...elements.map($ => ({ ...$, [k]: f($) }))) as any,
		at : i =>
			elements[i] === undefined
				? THROW(new RangeError(`Out of bounds index (${i}) occured with 'List' monad; indexing returned 'undefined'`))
				: elements[i] as a
	})

/********************************************************************************************************************************/

/**` data Horizontal = Leftward | Left | CenterX | Right | Rightward `*/
enum Horizontal
{
	Leftward  = 'Leftward :: Horizontal',
	Left      = 'Left :: Horizontal',
	CenterX   = 'CenterX :: Horizontal',
	Right     = 'Right :: Horizontal',
	Rightward = 'Rightward :: Horizontal',
}

/**` data Vertical = Downward | Down | CenterY | Up | Upward `*/
enum Vertical
{
	Downward = 'Downward :: Vertical',
	Down     = 'Down :: Vertical',
	CenterY  = 'None :: Vertical',
	Up       = 'Up :: Vertical',
	Upward   = 'Upward :: Vertical'
}

/**` data Lateral = Backward | Back | CenterZ | Fore | Forward `*/
enum Lateral
{
	Backward = 'Backward :: Lateral',
	Back     = 'Back :: Lateral',
	CenterZ  = 'CenterZ :: Lateral',
	Fore     = 'Fore :: Lateral',
	Forward  = 'Forward :: Lateral'
}

/**` data LineCap = Butt | Round | Square `*/
enum LineCap
{
	Butt   = 'Butt :: LineCap',
	Round  = 'Round :: LineCap',
	Square = 'Square :: LineCap'
}

/**` data LineJoin = Round | Bevel | Miter `*/
enum LineJoin
{
	Round = 'Round :: LineJoin',
	Bevel = 'Bevel :: LineJoin',
	Miter = 'Miter :: LineJoin'
}

/**` data TextAlignment = Start | End | Left | Right | Center `*/
enum TextAlignment
{
	Start  = 'Start :: TextAlignment',
	End    = 'End :: TextAlignment',
	Left   = 'Left :: TextAlignment',
	Right  = 'Right :: TextAlignment',
	Center = 'Center :: TextAlignment'
}

/**` data TextBaseline = Top | Hanging | Middle | Alphabetic | Ideographic | Bottom `*/
enum TextBaseline
{
	Top         = 'Top :: TextBaseline',
	Hanging     = 'Hanging :: TextBaseline',
	Middle      = 'Middle :: TextBaseline',
	Alphabetic  = 'Alphabetic :: TextBaseline',
	Ideographic = 'Ideographic :: TextBaseline',
	Bottom      = 'Bottom :: TextBaseline'
}

/********************************************************************************************************************************/

/**` relaxHorizontal :: Horizontal -> Horizontal `*/
const relaxHorizontal = (direction : Horizontal) : Horizontal =>
	direction === Horizontal.Leftward  ? Horizontal.Left  :
	direction === Horizontal.Rightward ? Horizontal.Right :
	direction

/**` relaxVertical :: Vertical -> Vertical `*/
const relaxVertical = (direction : Vertical) : Vertical =>
	direction === Vertical.Downward ? Vertical.Down :
	direction === Vertical.Upward   ? Vertical.Up   :
	direction

/**` relaxLateral :: Lateral -> Lateral `*/
const relaxLateral = (direction : Lateral) : Lateral =>
	direction === Lateral.Backward ? Lateral.Back :
	direction === Lateral.Forward  ? Lateral.Fore :
	direction

/********************************************************************************************************************************/

const Do =
	{
		IO    : IO    (() => ({})),
		Maybe : Just  ({}),
		State : State ((s : any) => [s, {}]),
		List  : List  ({})
	} as const

/********************************************************************************************************************************/

// __KEYBOARD_KEYS_ARRAY__ :: [String]
const __KEYBOARD_KEYS_ARRAY__ =
	[
		'AltLeft'        , 'AltRight'  , 'ArrowDown'   , 'ArrowLeft'     , 'ArrowRight'   , 'ArrowUp'     , 'Backquote'      ,
		'Backslash'      , 'Backspace' , 'BracketLeft' , 'BracketRight'  , 'CapsLock'     , 'Comma'       , 'ControlLeft'    ,
		'ControlRight'   , 'Delete'    , 'NumpadAdd'   , 'NumpadDecimal' , 'NumpadDivide' , 'NumpadEnter' , 'NumpadMultiply' ,
		'NumpadSubtract' , 'PageUp'    , 'Pagedown'    , 'Period'        , 'Quote'        , 'Semicolon'   , 'ShiftLeft'      ,
		'ShiftRight'     , 'Slash'     , 'End'         , 'Enter'         , 'Equal'        , 'Home'        , 'Insert'         ,
		'Minus'          , 'Space'     , 'Tab'         , 'Digit0'        , 'Digit1'       , 'Digit2'      , 'Digit3'         ,
		'Digit4'         , 'Digit5'    , 'Digit6'      , 'Digit7'        , 'Digit8'       , 'Digit9'      , 'Numpad0'        ,
		'Numpad1'        , 'Numpad2'   , 'Numpad3'     , 'Numpad4'       , 'Numpad5'      , 'Numpad6'     , 'Numpad7'        ,
		'Numpad8'        , 'Numpad9'   , 'KeyA'        , 'KeyB'          , 'KeyC'         , 'KeyD'        , 'KeyE'           ,
		'KeyF'           , 'KeyG'      , 'KeyH'        , 'KeyI'          , 'KeyJ'         , 'KeyK'        , 'KeyL'           ,
		'KeyM'           , 'KeyN'      , 'KeyO'        , 'KeyP'          , 'KeyQ'         , 'KeyR'        , 'KeyS'           ,
		'KeyT'           , 'KeyU'      , 'KeyV'        , 'KeyW'          , 'KeyX'         , 'KeyY'        , 'KeyZ'
	] as const

type KeyboardKey = typeof __KEYBOARD_KEYS_ARRAY__[number]

const __EXTERNAL__ =
	{
		context   : undefined as unknown as CanvasRenderingContext2D,
		resizeID  : undefined as unknown as number,
		isResized : false,
		seed      : (Math.random() - 0.5) * Date.now(),
		mouse     :
			{
				screenX : 0, screenY : 0,
				windowX : 0, windowY : 0,
				canvasX : 0, canvasY : 0,
				deltaX  : 0, deltaY  : 0,
				scroll  : Vertical.CenterY,
				buttons : new Array(5).fill(Vertical.Up) as [Vertical, Vertical, Vertical, Vertical, Vertical]
			},
		keyboard  :
			__KEYBOARD_KEYS_ARRAY__.reduce(($, k) => ({ ...$, [k] : Vertical.Up }), {}) as { [key in KeyboardKey] : Vertical }
	}

namespace Get
{
	export namespace Norm
	{
		/**` Get.Norm.mousePositionScreen :: IO (Number, Number) `*/
		export const mousePositionScreen : IO<[number, number]> =
			IO(() => [__EXTERNAL__.mouse.screenX / screen.width, __EXTERNAL__.mouse.screenY / screen.height])

		/**` Get.Norm.mousePositionScreenX :: IO Number `*/
		export const mousePositionScreenX : IO<number> =
			IO(() => __EXTERNAL__.mouse.screenX / screen.width)

		/**` Get.Norm.mousePositionScreenY :: IO Number `*/
		export const mousePositionScreenY : IO<number> =
			IO(() => __EXTERNAL__.mouse.screenY / screen.height)

		/**` Get.Norm.mousePositionWindow :: IO (Number, Number) `*/
		export const mousePositionWindow : IO<[number, number]> =
			IO(() => [__EXTERNAL__.mouse.windowX / innerWidth, __EXTERNAL__.mouse.windowY / innerHeight])

		/**` Get.Norm.mousePositionWindowX :: IO Number `*/
		export const mousePositionWindowX : IO<number> =
			IO(() => __EXTERNAL__.mouse.windowX / innerWidth)

		/**` Get.Norm.mousePositionWindowY :: IO Number `*/
		export const mousePositionWindowY : IO<number> =
			IO(() => __EXTERNAL__.mouse.windowY / innerHeight)

		/**` Get.Norm.mousePositionCanvas :: IO (Number, Number) `*/
		export const mousePositionCanvas : IO<[number, number]> =
			IO(() => [
				__EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width,
				__EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height
			])

		/**` Get.Norm.mousePositionCanvasX :: IO Number `*/
		export const mousePositionCanvasX : IO<number> =
			IO(() => __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width)

		/**` Get.Norm.mousePositionCanvasY :: IO Number `*/
		export const mousePositionCanvasY : IO<number> =
			IO(() => __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height)

		/**` Get.Norm.mousePositionScreenDelta :: IO (Number, Number) `*/
		export const mousePositionScreenDelta : IO<[number, number]> =
			IO(() => [__EXTERNAL__.mouse.deltaX / screen.width,__EXTERNAL__.mouse.deltaY / screen.height])

		/**` Get.Norm.mousePositionScreenDeltaX :: IO Number `*/
		export const mousePositionScreenDeltaX : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaX / screen.width)

		/**` Get.Norm.mousePositionScreenDeltaY :: IO Number `*/
		export const mousePositionScreenDeltaY : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaY / screen.height)

		/**` Get.Norm.mousePositionWindowDelta :: IO (Number, Number) `*/
		export const mousePositionWindowDelta : IO<[number, number]> =
			IO(() => [__EXTERNAL__.mouse.deltaX / innerWidth, __EXTERNAL__.mouse.deltaY / innerHeight])

		/**` Get.Norm.mousePositionWindowDeltaX :: IO Number `*/
		export const mousePositionWindowDeltaX : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaX / innerWidth)

		/**` Get.Norm.mousePositionWindowDeltaY :: IO Number `*/
		export const mousePositionWindowDeltaY : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaY / innerHeight)

		/**` Get.Norm.mousePositionCanvasDelta :: IO (Number, Number) `*/
		export const mousePositionCanvasDelta : IO<[number, number]> =
			IO(() => [
				__EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width,
				__EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height
			])

		/**` Get.Norm.mousePositionCanvasDeltaX :: IO Number `*/
		export const mousePositionCanvasDeltaX : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width)

		/**` Get.Norm.mousePositionCanvasDeltaY :: IO Number `*/
		export const mousePositionCanvasDeltaY : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height)
	}

	/**` Get.timeSinceBeginning :: IO Number `*/
	export const timeSinceBeginning : IO<number> =
		IO(() => performance.now())

	/**` Get.isWindowResized :: IO Bool `*/
	export const isWindowResized : IO<boolean> =
		IO(() => __EXTERNAL__.isResized)

	/**` Get.universalSeed :: IO Number `*/
	export const universalSeed : IO<number> =
		IO(() => __EXTERNAL__.seed)

	/**` Get.windowDimensions :: IO (Number, Number) `*/
	export const windowDimensions : IO<[number, number]> =
		IO(() => [innerWidth, innerHeight])

	/**` Get.windowDimensionW :: IO Number `*/
	export const windowDimensionW : IO<number> =
		IO(() => innerWidth)

	/**` Get.windowDimensionH :: IO Number `*/
	export const windowDimensionH : IO<number> =
		IO(() => innerHeight)

	/**` Get.canvasDimensions :: IO (Number, Number) `*/
	export const canvasDimensions : IO<[number, number]> =
		IO(() => [__EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height])

	/**` Get.canvasDimensionW :: IO Number `*/
	export const canvasDimensionW : IO<number> =
		IO(() => __EXTERNAL__.context.canvas.width)

	/**` Get.canvasDimensionH :: IO Number `*/
	export const canvasDimensionH : IO<number> =
		IO(() => __EXTERNAL__.context.canvas.height)

	/**` Get.mousePositionScreen :: IO (Number, Number) `*/
	export const mousePositionScreen : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.screenX, __EXTERNAL__.mouse.screenY])

	/**` Get.mousePositionScreenX :: IO Number `*/
	export const mousePositionScreenX : IO<number> =
		IO(() => __EXTERNAL__.mouse.screenX)

	/**` Get.mousePositionScreenY :: IO Number `*/
	export const mousePositionScreenY : IO<number> =
		IO(() => __EXTERNAL__.mouse.screenY)

	/**` Get.mousePositionWindow :: IO (Number, Number) `*/
	export const mousePositionWindow : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.windowX, __EXTERNAL__.mouse.windowY])

	/**` Get.mousePositionWindowX :: IO Number `*/
	export const mousePositionWindowX : IO<number> =
		IO(() => __EXTERNAL__.mouse.windowX)

	/**` Get.mousePositionWindowY :: IO Number `*/
	export const mousePositionWindowY : IO<number> =
		IO(() => __EXTERNAL__.mouse.windowY)

	/**` Get.mousePositionCanvas :: IO (Number, Number) `*/
	export const mousePositionCanvas : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.canvasX, __EXTERNAL__.mouse.canvasY])

	/**` Get.mousePositionCanvasX :: IO Number `*/
	export const mousePositionCanvasX : IO<number> =
		IO(() => __EXTERNAL__.mouse.canvasX)

	/**` Get.mousePositionCanvasY :: IO Number `*/
	export const mousePositionCanvasY : IO<number> =
		IO(() => __EXTERNAL__.mouse.canvasY)

	/**` Get.mousePositionDelta :: IO (Number, Number) `*/
	export const mousePositionDelta : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.deltaX, __EXTERNAL__.mouse.deltaY])

	/**` Get.mousePositionDeltaX :: IO Number `*/
	export const mousePositionDeltaX : IO<number> =
		IO(() => __EXTERNAL__.mouse.deltaX)

	/**` Get.mousePositionDeltaY :: IO Number `*/
	export const mousePositionDeltaY : IO<number> =
		IO(() => __EXTERNAL__.mouse.deltaY)

	/**` Get.mouseScrollDirection :: IO Vertical `*/
	export const mouseScrollDirection : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.scroll)

	/**` Get.mouseButtonLeft :: IO Vertical `*/
	export const mouseButtonLeft : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[0])

	/**` Get.mouseButtonMiddle :: IO Vertical `*/
	export const mouseButtonMiddle : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[1])

	/**` Get.mouseButtonRight :: IO Vertical `*/
	export const mouseButtonRight : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[2])

	/**` Get.mouseButtonEsotericA :: IO Vertical `*/
	export const mouseButtonEsotericA : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[3])

	/**` Get.mouseButtonEsotericB :: IO Vertical `*/
	export const mouseButtonEsotericB : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[4])

	/**` Get.keyboardKey :: KeyboardKey -> IO Vertical `*/
	export const keyboardKey = (keyboardKeyName : KeyboardKey) : IO<Vertical> =>
		IO(() => __EXTERNAL__.keyboard[keyboardKeyName])

	/**` Get.textMeasurement :: String -> IO TextMetrics `*/
	export const textMeasurement = (message : string) : IO<TextMetrics> =>
		IO(() => __EXTERNAL__.context.measureText(message))

	/**` Get.lineWidth :: IO Number `*/
	export const lineWidth : IO<number> =
		IO(() => __EXTERNAL__.context.lineWidth)

	/**` Get.lineCap :: IO LineCap `*/
	export const lineCap : IO<LineCap> =
		IO(() =>
			__EXTERNAL__.context.lineCap === 'butt'   ? LineCap.Butt   :
			__EXTERNAL__.context.lineCap === 'round'  ? LineCap.Round  :
			__EXTERNAL__.context.lineCap === 'square' ? LineCap.Square :
			LineCap.Butt
		)

	/**` Get.lineJoin :: IO LineJoin `*/
	export const lineJoin : IO<LineJoin> =
		IO(() =>
			__EXTERNAL__.context.lineJoin === 'bevel' ? LineJoin.Bevel :
			__EXTERNAL__.context.lineJoin === 'miter' ? LineJoin.Miter :
			__EXTERNAL__.context.lineJoin === 'round' ? LineJoin.Round :
			LineJoin.Miter
		)

	/**` Get.miterLimit :: IO Number `*/
	export const miterLimit : IO<number> =
		IO(() => __EXTERNAL__.context.miterLimit)

	/**` Get.lineDashPattern :: IO (List Number) `*/
	export const lineDashPattern : IO<List<number>> =
		IO(() => List(...__EXTERNAL__.context.getLineDash()))

	/**` Get.lineDashOffset :: IO Number `*/
	export const lineDashOffset : IO<number> =
		IO(() => __EXTERNAL__.context.lineDashOffset)

	/**` Get.font :: IO String `*/
	export const font : IO<string> =
		IO(() => __EXTERNAL__.context.font)

	/**` Get.fontSize :: IO Number `*/
	export const fontSize : IO<number> =
		IO(() => +__EXTERNAL__.context.font.slice(0, __EXTERNAL__.context.font.indexOf("px")))

	/**` Get.fontFamily :: IO String `*/
	export const fontFamily : IO<string> =
		IO(() => __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1))

	/**` Get.textAlignment :: IO TextAlignment `*/
	export const textAlignment : IO<TextAlignment> =
		IO(() =>
				__EXTERNAL__.context.textAlign === 'center' ? TextAlignment.Center :
				__EXTERNAL__.context.textAlign === 'end'    ? TextAlignment.End    :
				__EXTERNAL__.context.textAlign === 'left'   ? TextAlignment.Left   :
				__EXTERNAL__.context.textAlign === 'right'  ? TextAlignment.Right  :
				__EXTERNAL__.context.textAlign === 'start'  ? TextAlignment.Start  :
				TextAlignment.Start
		)

	/**` Get.textBaseline :: IO TextBaseline `*/
	export const textBaseline : IO<TextBaseline> =
		IO(() =>
				__EXTERNAL__.context.textBaseline === 'alphabetic'  ? TextBaseline.Alphabetic  :
				__EXTERNAL__.context.textBaseline === 'bottom'      ? TextBaseline.Bottom      :
				__EXTERNAL__.context.textBaseline === 'hanging'     ? TextBaseline.Hanging     :
				__EXTERNAL__.context.textBaseline === 'ideographic' ? TextBaseline.Ideographic :
				__EXTERNAL__.context.textBaseline === 'middle'      ? TextBaseline.Middle      :
				__EXTERNAL__.context.textBaseline === 'top'         ? TextBaseline.Top         :
				null as any
		)
}

namespace Put
{
	/**` Put.lineWidth :: Number -> IO () `*/
	export const lineWidth = (w : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineWidth = w
			return null
		})

	/**` Put.lineCap :: LineCap -> IO () `*/
	export const lineCap = (cap : LineCap) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineCap =
				cap === LineCap.Butt   ? 'butt'  :
				cap === LineCap.Round  ? 'round' :
				cap === LineCap.Square ? 'square' :
				null as any
			return null
		})

	/**` Put.lineJoin :: LineJoin -> IO () `*/
	export const lineJoin = (join : LineJoin) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineCap =
				join === LineJoin.Bevel ? 'bevel' :
				join === LineJoin.Miter ? 'miter' :
				join === LineJoin.Round ? 'round' :
				null as any
			return null
		})

	/**` Put.miterLimit :: Number -> IO () `*/
	export const miterLimit = (limit : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.miterLimit = limit
			return null
		})

	/**` Put.lineDashPattern :: List Number -> IO () `*/
	export const lineDashPattern = (pattern : List<number>) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.setLineDash(pattern.INFO.slice())
			return null
		})

	/**` Put.lineDashOffset :: Number -> IO () `*/
	export const lineDashOffset = (offset : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineDashOffset = offset
			return null
		})

	/**` Put.font :: String -> IO () `*/
	export const font = (newfont : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font = newfont
			return null
		})

	/**` Put.fontSize :: Number -> IO () `*/
	export const fontSize = (size : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font = size + __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf("px "))
			return null
		})

	/**` Put.fontFamily :: String -> IO () `*/
	export const fontFamily = (family : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font = __EXTERNAL__.context.font.slice(0, __EXTERNAL__.context.font.indexOf("px ") + 3) + family
			return null
		})

	/**` Put.textAlignment :: TextAlignment -> IO () `*/
	export const textAlignment = (alignment : TextAlignment) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.textAlign =
				alignment === TextAlignment.Center ? 'center' :
				alignment === TextAlignment.End    ? 'end'    :
				alignment === TextAlignment.Left   ? 'left'   :
				alignment === TextAlignment.Right  ? 'right'  :
				alignment === TextAlignment.Start  ? 'start'  :
				null as any
			return null
		})

	/**` Put.textBaseline :: TextBaseline -> IO () `*/
	export const textBaseline = (baseline : TextBaseline) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.textBaseline =
				baseline === TextBaseline.Alphabetic  ? 'alphabetic'  :
				baseline === TextBaseline.Bottom      ? 'bottom'      :
				baseline === TextBaseline.Hanging     ? 'hanging'     :
				baseline === TextBaseline.Ideographic ? 'ideographic' :
				baseline === TextBaseline.Middle      ? 'middle'      :
				baseline === TextBaseline.Top         ? 'top'         :
				null as any
			return null
		})

	/**` Put.fill :: String -> IO () `*/
	export const fill = (color : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.fillStyle = color
			return null
		})

	/**` Put.stroke :: String -> IO () `*/
	export const stroke = (color : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.strokeStyle = color
			return null
		})
}

namespace Act
{
	/**` Act.tickExternalState :: IO () `*/
	export const tickExternalState : IO<null> =
		IO(() => {
			__EXTERNAL__.mouse.scroll = Vertical.CenterY
			__EXTERNAL__.isResized    = false
			for (const k in __EXTERNAL__.keyboard)
				(__EXTERNAL__.keyboard as any)[k] = relaxVertical((__EXTERNAL__.keyboard as any)[k])
			for (const k in __EXTERNAL__.mouse.buttons)
				__EXTERNAL__.mouse.buttons[k] = relaxVertical(__EXTERNAL__.mouse.buttons[k] as any)
			return null
		})

	/**` Act.clearRect :: Number -> Number -> Number -> Number -> IO () `*/
	export const clearRect = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.clearRect(x, y, w, h)
			return null
		})

	/**` Act.fillRect :: Number -> Number -> Number -> Number -> IO () `*/
	export const fillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.fillRect(x, y, w, h)
			return null
		})

	/**` Act.strokeRect :: Number -> Number -> Number -> Number -> IO () `*/
	export const strokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.strokeRect(x, y, w, h)
			return null
		})

	/**` Act.fillText :: String -> Number -> Number -> IO () `*/
	export const fillText = (message : string) => (x : number) => (y : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.fillText(message, x, y)
			return null
		})

	/**` Act.strokeText :: String -> Number -> Number -> IO () `*/
	export const strokeText = (message : string) => (x : number) => (y : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.strokeText(message, x, y)
			return null
		})
}

/********************************************************************************************************************************/

onload = () =>
{
	__EXTERNAL__.context = (document.querySelector('canvas') as any).getContext('2d')

	onkeydown = event =>
	{
		if (!event.repeat)
			__EXTERNAL__.keyboard[event.code as KeyboardKey] = Vertical.Downward
	}

	onkeyup = event =>
	{
		__EXTERNAL__.keyboard[event.code as KeyboardKey] = Vertical.Upward
	}

	onmousemove = event =>
	{
		__EXTERNAL__.mouse.windowX = event.x
		__EXTERNAL__.mouse.windowY = event.y
		__EXTERNAL__.mouse.canvasX = event.clientX - __EXTERNAL__.context.canvas.offsetLeft
		__EXTERNAL__.mouse.canvasY = event.clientY - __EXTERNAL__.context.canvas.offsetTop
		__EXTERNAL__.mouse.screenX = event.screenX
		__EXTERNAL__.mouse.screenY = event.screenY
		__EXTERNAL__.mouse.deltaX  = event.movementX
		__EXTERNAL__.mouse.deltaY  = event.movementY
	}

	onmousedown = event =>
	{
		__EXTERNAL__.mouse.buttons[event.button] = Vertical.Downward
	}

	onmouseup = event =>
	{
		__EXTERNAL__.mouse.buttons[event.button] = Vertical.Upward
	}

	onwheel = event =>
	{
		if (event.deltaY < 0)
			__EXTERNAL__.mouse.scroll = Vertical.Up
		else (event.deltaY > 0)
			__EXTERNAL__.mouse.scroll = Vertical.Down
	}

	onresize = () =>
	{
		clearTimeout(__EXTERNAL__.resizeID)
		__EXTERNAL__.resizeID =
			setTimeout(() => { __EXTERNAL__.isResized = true }, 250)
	}
}
