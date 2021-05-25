/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable no-return-assign                  */
/* eslint-disable no-plusplus                       */
/* eslint-disable no-param-reassign                 */
/* eslint-disable no-console                        */
/* eslint-disable no-multi-assign                   */
/* eslint-disable no-loop-func                      */

// -- Maximum amount of recursion/looping allowed; prevents most crashes
const MAXI = 1024
const STAP = "(S.T.A.P.) Stopped To Avoid Phailures"

/**` never : a `*/
declare const never : never
Object.defineProperty(this, "never", { get() { throw `Unexhuastive pattern matching | 'never' was reached` } })

/**` keyboardKeysArray : [String] `*/
const keyboardKeysArray =
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

type KeyboardKey = typeof keyboardKeysArray[number]

/********************************************************************************************************************************/
// Typeclasses //

type Pipe <a> = a & { pipe : <b>(morphism : (value : a) => b) => b       }
type Eq   <a> = a & { eq   :    (value    :                a) => boolean }

/**` eq : Eq a => a -> a -> Boolean `*/
const eq = <a>(leftside : Eq <a>) => (rightside : Eq <a>) : boolean => leftside .eq (rightside)

/********************************************************************************************************************************/
// Algebraic Data Types //

type IO <a> =
	{
		variation : 'IO'

		/**` (IO a).effect : () -> a `*/
		effect : () => a

		/**` (IO a).pipe : (IO a -> b) -> b `*/
		pipe : <b>(morphism : (io : IO <a>) => b) => b

		/**` (IO a).bind : (a -> IO b) -> IO b `*/
		bind : <b>(reaction : (result : a) => IO <b>) => IO <b>

		/**` (IO a).fmap : (a -> b) -> IO b `*/
		fmap : <b>(morphism : (result : a) => b) => IO <b>

		/**` (IO $).bindto : String -> ($ -> IO b) -> IO $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => IO <b>) => IO <a & { [x in k] : b }>

		/**` (IO $).fmapto : String -> ($ -> b) -> IO $ `*/
		fmapto : <k extends string>(name : k) => <b>(morphism : ($ : a) => b) => IO <a & { [x in k] : b }>

		/**` (IO a).then : IO b -> IO b `*/
		then : <b>(successor : IO <b>) => IO <b>

		/**` (IO a).also : (a -> IO b) -> IO a `*/
		also : <b>(reaction : (result : a) => IO <b>) => IO <a>

		/**` (IO a).side : IO b -> IO a `*/
		side : <b>(effect : IO <b>) => IO <a>

		/**` (IO a).cast : b -> IO b `*/
		cast : <b>(replacement : b) => IO <b>
	}

type Process <s, a> =
	{
		variation : 'Process'

		/**` (Process s a).computation : s -> Pair s a `*/
		computation : (state : s) => Pair <s, a>

		/**` (Process s a).pipe : (Process s a -> b) -> b `*/
		pipe : <b>(morphism : (process : Process <s, a>) => b) => b

		/**` (Process s a).bind : (a -> Process s b) -> Process s b `*/
		bind : <b>(reaction : (output : a) => Process <s, b>) => Process <s, b>

		/**` (Process s a).fmap : (a -> b) -> Process s b `*/
		fmap : <b>(morphism : (output : a) => b) => Process <s, b>

		/**` (Process s $).bindto : String -> ($ -> Process s b) -> IO $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => Process <s, b>) => Process <s, a & { [x in k] : b }>

		/**` (Process s $).fmapto : String -> ($ -> b) -> IO $ `*/
		fmapto : <k extends string>(name : k) => <b>(morphism : ($ : a) => b) => Process <s, a & { [x in k] : b }>

		/**` (Process s a).then : Process s b -> Process s b `*/
		then : <b>(successor : Process <s, b>) => Process <s, b>

		/**` (Process s a).also : (a -> Process s b) -> Process s a `*/
		also : <b>(reaction : (output : a) => Process <s, b>) => Process <s, a>

		/**` (Process s a).side : Process s b -> Process s a `*/
		side : <b>(effect : Process <s, b>) => Process <s, a>

		/**` (Process s a).cast : b -> Process s b `*/
		cast : <b>(replacement : b) => Process <s, b>
	}

type List <a> =
	({
		variation : 'Nil'
	} | {
		variation : 'Cons'

		/**` (List a).head : a `*/
		head : a

		/**` (List a).tail : List a `*/
		tail : List <a>
	}) & {
		$HEAD    ?: a
		$TAIL    ?: List <a>
		$LAST    ?: a
		$INIT    ?: List <a>
		$REVERSE ?: List <a>
		$LEN     ?: number

		/**` (List a).pipe : (List a -> b) -> b `*/
		pipe : <b>(morphism : (xs : List <a>) => b) => b

		/**` (List a).eq : (Eq a) => List a -> Boolean `*/
		eq : (xs : List <Eq <a>>) => boolean

		/**` (List a).bind : (a -> List b) -> List b `*/
		bind : <b>(reaction : (element : a) => List <b>) => List <b>

		/**` (List a).fmap : (a -> b) -> List b `*/
		fmap : <b>(morphism : (element : a) => b) => List <b>

		/**` (List $).bindto : String -> ($ -> List b) -> List $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => List <b>) => List <a & { [x in k] : b }>

		/**` (List $).fmapto : String -> ($ -> b) -> List $ `*/
		fmapto : <k extends string>(name : k) => <b>(reaction : ($ : a) => b) => List <a & { [x in k] : b }>

		/**` (List a).link : List a -> List a -> List a `*/
		link : (succeeding : List <a>) => List <a>
	}

type Maybe <a> =
	({
		variation : 'Nothing'
	} | {
		variation : 'Just'

		/**` (Maybe a).value : a `*/
		value : a
	}) & {
		/**` (Maybe a).pipe : (Maybe a -> b) -> b `*/
		pipe : <b>(morphism : (maybe : Maybe <a>) => b) => b

		/**` (Maybe a).eq : (Eq a) => Maybe a -> Boolean `*/
		eq : (maybe : Maybe <Eq <a>>) => boolean

		/**` (Maybe a).bind : (a -> Maybe b) -> Maybe b `*/
		bind : <b>(reaction : (value : a) => Maybe <b>) => Maybe <b>

		/**` (Maybe a).fmap : (a -> b) -> Maybe b `*/
		fmap : <b>(morphism : (value : a) => b) => Maybe <b>

		/**` (Maybe $).bindto : String -> ($ -> Maybe b) -> Maybe $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => Maybe <b>) => Maybe <a & { [x in k] : b }>

		/**` (Maybe $).fmapto : String -> ($ -> b) -> Maybe $ `*/
		fmapto : <k extends string>(name : k) => <b>(reaction : ($ : a) => b) => Maybe <a & { [x in k] : b }>
	}

type Pair <a, b> =
	{
		variation : 'Pair'

		/**` (Pair a b).fst : a `*/
		fst : a

		/**` (Pair a b).snd : b `*/
		snd : b

		/**` (Pair a b).pipe : (Pair a b -> c) -> c `*/
		pipe : <c>(morphism : (pair : Pair <a, b>) => c) => c

		/**` (Pair a b).eq : (Eq a, Eq b) => Pair a b -> Boolean `*/
		eq : (pair : Pair <Eq <a>, Eq <b>>) => boolean
	}

type Either <a, b> =
	({
		variation : 'Left'

		/**` (Either a b).value : a `*/
		value : a
	} | {
		variation : 'Right'

		/**` (Either a b).value : b `*/
		value : b
	}) & {
		/**` (Either a b).pipe : (Either a b -> c) -> c `*/
		pipe : <c>(morphism : (either : Either <a, b>) => c) => c

		/**` (Either a b).eq : (Eq a, Eq b) => Either a b -> Boolean `*/
		eq : (either : Either <Eq <a>, Eq <b>>) => boolean
	}

type Vector2 =
	{
		variation : 'Vector2'

		/**` Vector2.x : Number `*/
		x : number

		/**` Vector2.y : Number `*/
		y : number

		/**` Vector2.pipe : (Vector2 -> a) -> a `*/
		pipe : <a>(morphism : (v2 : Vector2) => a) => a

		/**` Vector2.eq : Vector2 -> Boolean `*/
		eq : (v2 : Vector2) => boolean
	}

type Vector3 =
	{
		variation : 'Vector3'

		/**` Vector3.x : Number `*/
		x : number

		/**` Vector3.y : Number `*/
		y : number

		/**` Vector3.y : Number `*/
		z : number

		/**` Vector3.pipe : (Vector3 -> a) -> a `*/
		pipe : <a>(morphism : (v3 : Vector3) => a) => a

		/**` Vector3.eq : Vector3 -> Boolean `*/
		eq : (v3 : Vector3) => boolean
	}

type Vector4 =
	{
		variation : 'Vector4'

		/**` Vector4.x : Number `*/
		x : number

		/**` Vector4.y : Number `*/
		y : number

		/**` Vector4.y : Number `*/
		z : number

		/**` Vector4.w : Number `*/
		w : number

		/**` Vector4.pipe : (Vector4 -> a) -> a `*/
		pipe : <a>(morphism : (v4 : Vector4) => a) => a

		/**` Vector4.eq : Vector4 -> Boolean `*/
		eq : (v4 : Vector4) => boolean
	}

type Matrix2 =
	{
		variation : 'Matrix2'

		/**` Matrix2.ix : Number `*/
		ix : number

		/**` Matrix2.jx : Number `*/
		jx : number

		/**` Matrix2.iy : Number `*/
		iy : number

		/**` Matrix2.jy : Number `*/
		jy : number

		/**` Matrix2.pipe : (Matrix2 -> a) -> a `*/
		pipe : <a>(morphism : (m2 : Matrix2) => a) => a

		/**` Matrix2.eq : Matrix2 -> boolean `*/
		eq : (m : Matrix2) => boolean
	}

type Matrix3 =
	{
		variation : 'Matrix3'

		/**` Matrix3.ix : Number `*/
		ix : number

		/**` Matrix3.jx : Number `*/
		jx : number

		/**` Matrix3.kx : Number `*/
		kx : number

		/**` Matrix3.iy : Number `*/
		iy : number

		/**` Matrix3.jy : Number `*/
		jy : number

		/**` Matrix3.ky : Number `*/
		ky : number

		/**` Matrix3.iz : Number `*/
		iz : number

		/**` Matrix3.jz : Number `*/
		jz : number

		/**` Matrix3.kz : Number `*/
		kz : number

		/**` Matrix3.pipe : (Matrix3 -> a) -> a `*/
		pipe : <a>(morphism : (m3 : Matrix3) => a) => a

		/**` Matrix3.eq : Matrix3 -> boolean `*/
		eq : (m : Matrix3) => boolean
	}

type Matrix4 =
	{
		variation : 'Matrix4'

		/**` Matrix4.ix : Number `*/
		ix : number

		/**` Matrix4.jx : Number `*/
		jx : number

		/**` Matrix4.kx : Number `*/
		kx : number

		/**` Matrix4.lx : Number `*/
		lx : number

		/**` Matrix4.iy : Number `*/
		iy : number

		/**` Matrix4.jy : Number `*/
		jy : number

		/**` Matrix4.ky : Number `*/
		ky : number

		/**` Matrix4.ly : Number `*/
		ly : number

		/**` Matrix4.iz : Number `*/
		iz : number

		/**` Matrix4.jz : Number `*/
		jz : number

		/**` Matrix4.kz : Number `*/
		kz : number

		/**` Matrix4.lz : Number `*/
		lz : number

		/**` Matrix4.iw : Number `*/
		iw : number

		/**` Matrix4.jw : Number `*/
		jw : number

		/**` Matrix4.kw : Number `*/
		kw : number

		/**` Matrix4.lw : Number `*/
		lw : number

		/**` Matrix4.pipe : (Matrix4 -> a) -> a `*/
		pipe : <a>(morphism : (m4 : Matrix4) => a) => a

		/**` Matrix4.eq : Matrix4 -> boolean `*/
		eq : (m : Matrix4) => boolean
	}

/********************************************************************************************************************************/
// Types of Data Constructors //

type Assert_Nil     <a>    = List   <a>    & { variation : 'Nil'     }
type Assert_Cons    <a>    = List   <a>    & { variation : 'Cons'    }
type Assert_Nothing <a>    = Maybe  <a>    & { variation : 'Nothing' }
type Assert_Just    <a>    = Maybe  <a>    & { variation : 'Just'    }
type Assert_Left    <a, b> = Either <a, b> & { variation : 'Left'    }
type Assert_Right   <a, b> = Either <a, b> & { variation : 'Right'   }

/********************************************************************************************************************************/
// Enums //

enum Vertical
{
	Downward = 'Vertical.Downward : Vertical',
	Down     = 'Vertical.Down : Vertical',
	Rest     = 'Vertical.Rest : Vertical',
	Upward   = 'Vertical.Upward : Vertical',
	Up       = 'Vertical.Up : Vertical'
}

enum LineCap
{
	Butt   = 'LineCap.Butt : LineCap',
	Round  = 'LineCap.Round : LineCap',
	Square = 'LineCap.Square : LineCap'
}

enum LineJoin
{
	Round = 'LineJoin.Round : LineJoin',
	Bevel = 'LineJoin.Bevel : LineJoin',
	Miter = 'LineJoin.Miter : LineJoin'
}

enum TextAlign
{
	Start     = 'TextAlign.Start : TextAlign',
	End       = 'TextAlign.End : TextAlign',
	Leftside  = 'TextAlign.Leftside : TextAlign',
	Rightside = 'TextAlign.Rightside : TextAlign',
	Center    = 'TextAlign.Center : TextAlign'
}

enum TextBaseline
{
	Top         = 'TextBaseline.Top : TextBaseline',
	Hanging     = 'TextBaseline.Hanging : TextBaseline',
	Middle      = 'TextBaseline.Middle : TextBaseline',
	Alphabetic  = 'TextBaseline.Alphabetic : TextBaseline',
	Ideographic = 'TextBaseline.Ideographic : TextBaseline',
	Bottom      = 'TextBaseline.Bottom : TextBaseline'
}

enum Composition
{
	SourceOver      = 'Composition.SourceOver : Composition',
	SourceAtop      = 'Composition.SourceAtop : Composition',
	SourceIn        = 'Composition.SourceIn : Composition',
	SourceOut       = 'Composition.SourceOut : Composition',
	DestinationOver = 'Composition.DestinationOver : Composition',
	DestinationAtop = 'Composition.DestinationAtop : Composition',
	DestinationIn   = 'Composition.DestinationIn : Composition',
	DestinationOut  = 'Composition.DestinationOut : Composition',
	Lighter         = 'Composition.Lighter : Composition',
	Xor             = 'Composition.Xor : Composition',
	Copy            = 'Composition.Copy : Composition',
	Multiply        = 'Composition.Multiply : Composition',
	Screen          = 'Composition.Screen : Composition',
	Overlay         = 'Composition.Overlay : Composition',
	Darken          = 'Composition.Darken : Composition',
	Lighten         = 'Composition.Lighten : Composition',
	ColorDodge      = 'Composition.ColorDodge : Composition',
	ColorBurn       = 'Composition.ColorBurn : Composition',
	HardLight       = 'Composition.HardLight : Composition',
	SoftLight       = 'Composition.SoftLight : Composition',
	Difference      = 'Composition.Difference : Composition',
	Exclusion       = 'Composition.Exclusion : Composition',
	Hue             = 'Composition.Hue : Composition',
	Saturation      = 'Composition.Saturation : Composition',
	Color           = 'Composition.Color : Composition',
	Luminosity      = 'Composition.Luminosity : Composition'
}

/********************************************************************************************************************************/
// Constants and Micro-Functions //

/**` e : Number `*/
const e = 2.718281828459045

/**` ln2 : Number `*/
const ln2 = 0.6931471805599453

/**` ln10 : Number `*/
const ln10 = 2.302585092994046

/**` log2e : Number `*/
const log2e = 1.4426950408889634

/**` log10e : Number `*/
const log10e = 0.4342944819032518

/**` pi : Number `*/
const pi = 3.141592653589793

/**` piDiv180 : Number `*/
const piDiv180 = 0.017453292519943295

/**` invPiDiv180 : Number `*/
const invPiDiv180 = 57.29577951308232

/**` tau : Number `*/
const tau = 6.283185307179586

/**` sqrt2 : Number `*/
const sqrt2 = 1.4142135623730951

/**` invSqrt2 : Number `*/
const invSqrt2 = 0.7071067811865476

/**` abs : Number -> Number `*/
const abs = Math.abs

/**` acos : Number -> Number `*/
const acos = Math.acos

/**` acosh : Number -> Number `*/
const acosh = Math.acosh

/**` asin : Number -> Number `*/
const asin = Math.asin

/**` asinh : Number -> Number `*/
const asinh = Math.asinh

/**` atan : Number -> Number `*/
const atan = Math.atan

/**` atan2 : Number -> Number -> Number `*/
const atan2 = (y : number) => (x : number) : number => Math.atan2 (y, x)

/**` ratan2 : Number -> Number -> Number `*/
const ratan2 = (x : number) => (y : number) : number => Math.atan2 (y, x)

/**` atanh : Number -> Number `*/
const atanh = Math.atanh

/**` cbrt : Number -> Number `*/
const cbrt = Math.cbrt

/**` ceil : Number -> Number `*/
const ceil = Math.ceil

/**` clz32 : Number -> Number `*/
const clz32 = Math.clz32

/**` cos : Number -> Number `*/
const cos = Math.cos

/**` cosh : Number -> Number `*/
const cosh = Math.cosh

/**` exp : Number -> Number `*/
const exp = Math.exp

/**` expm1 : Number -> Number `*/
const expm1 = Math.expm1

/**` floor : Number -> Number `*/
const floor = Math.floor

/**` fround : Number -> Number `*/
const fround = Math.fround

/**` hypot : Number -> Number -> Number `*/
const hypot = (x : number) => (y : number) => Math.hypot (x, y)

/**` ln : Number -> Number `*/
const ln = Math.log

/**` log10 : Number -> Number `*/
const log10 = Math.log10

/**` lnp1 : Number -> Number `*/
const lnp1 = Math.log1p

/**` log2 : Number -> Number `*/
const log2 = Math.log2

/**` max : Number -> Number -> Number `*/
const max = (x : number) => (y : number) : number => Math.max (x, y)

/**` min : Number -> Number -> Number `*/
const min = (x : number) => (y : number) : number => Math.min (x, y)

/**` roundInt : Number -> Number `*/
const roundInt = Math.round

/**` roundStr : Number -> Number -> String `*/
const roundStr = (x : number) => (amount : number) : string => x .toFixed (amount)

/**` rroundStr : Number -> Number -> String `*/
const rroundStr = (amount : number) => (x : number) : string => x .toFixed (amount)

/**` sign : Number -> Number `*/
const sign = Math.sign

/**` sin : Number -> Number `*/
const sin = Math.sin

/**` sinh : Number -> Number `*/
const sinh = Math.sinh

/**` sqrt : Number -> Number `*/
const sqrt = Math.sqrt

/**` tan : Number -> Number `*/
const tan = Math.tan

/**` tanh : Number -> Number `*/
const tanh = Math.tanh

/**` trunc : Number -> Number `*/
const trunc = (x : number) : number => ~~x

/**` negate : Number -> Number `*/
const negate = (x : number) : number => -x

/**` reciprocate : Number -> Number `*/
const reciprocate = (x : number) : number => 1 / x

/**` add : Number -> Number -> Number `*/
const add = (x : number) => (y : number) : number => x + y

/**` sub : Number -> Number -> Number `*/
const sub = (x : number) => (y : number) : number => x - y

/**` rsub : Number -> Number -> Number `*/
const rsub = (y : number) => (x : number) : number => x - y

/**` mul : Number -> Number -> Number `*/
const mul = (x : number) => (y : number) : number => x * y

/**` div : Number -> Number -> Number `*/
const div = (x : number) => (y : number) : number => x / y

/**` rdiv : Number -> Number -> Number `*/
const rdiv = (y : number) => (x : number) : number => x / y

/**` idiv : Number -> Number -> Number `*/
const idiv = (x : number) => (y : number) : number => ~~(x / y)

/**` ridiv : Number -> Number -> Number `*/
const ridiv = (y : number) => (x : number) : number => ~~(x / y)

/**` pow : Number -> Number -> Number `*/
const pow = (x : number) => (y : number) : number => x ** y

/**` rpow : Number -> Number -> Number `*/
const rpow = (y : number) => (x : number) : number => x ** y

/**` mod : Number -> Number -> Number `*/
const mod = (x : number) => (y : number) : number => x % y

/**` rmod : Number -> Number -> Number `*/
const rmod = (y : number) => (x : number) : number => x % y

/**` modulo : Number -> Number -> Number `*/
const modulo = (x : number) => (y : number) : number => (x % y + y) % y

/**` rmodulo : Number -> Number -> Number `*/
const rmodulo = (y : number) => (x : number) : number => (x % y + y) % y

/**` bit : Boolean -> Number `*/
const bit = (b : boolean) : number => +b

/**` bNOT : Number -> Number `*/
const bNOT = (x : number) : number => ~x

/**` bLSHIFT : Number -> Number -> Number `*/
const bLSHIFT = (x : number) => (amount : number) : number => x << amount

/**` rbLSHIFT : Number -> Number -> Number `*/
const rbLSHIFT = (amount : number) => (x : number) : number => x << amount

/**` bRSHIFT : Number -> Number -> Number `*/
const bRSHIFT = (x : number) => (amount : number) : number => x >> amount

/**` rbRSHIFT : Number -> Number -> Number `*/
const rbRSHIFT = (amount : number) => (x : number) : number => x >> amount

/**` bURSHIFT : Number -> Number -> Number `*/
const bURSHIFT = (x : number) => (amount : number) : number => x >>> amount

/**` rbURSHIFT : Number -> Number -> Number `*/
const rbURSHIFT = (amount : number) => (x : number) : number => x >>> amount

/**` bAND : Number -> Number -> Number `*/
const bAND = (x : number) => (y : number) : number => x & y

/**` bNAND : Number -> Number -> Number `*/
const bNAND = (x : number) => (y : number) : number => ~(x & y)

/**` bOR : Number -> Number -> Number `*/
const bOR = (x : number) => (y : number) : number => x | y

/**` bNOR : Number -> Number -> Number `*/
const bNOR = (x : number) => (y : number) : number => ~(x | y)

/**` bXOR : Number -> Number -> Number `*/
const bXOR = (x : number) => (y : number) : number => x ^ y

/**` bNXOR : Number -> Number -> Number `*/
const bNXOR = (x : number) => (y : number) : number => ~(x ^ y)

/**` not : Boolean -> Boolean `*/
const not = (b : boolean) : boolean => !b

/**` and : Boolean -> Boolean -> Boolean `*/
const and = (b : boolean) => (d : boolean) : boolean => b && d

/**` nand : Boolean -> Boolean -> Boolean `*/
const nand = (b : boolean) => (d : boolean) : boolean => !(b && d)

/**` or : Boolean -> Boolean -> Boolean `*/
const or = (b : boolean) => (d : boolean) : boolean => b || d

/**` nor : Boolean -> Boolean -> Boolean `*/
const nor = (b : boolean) => (d : boolean) : boolean => !(b || d)

/**` xor : Boolean -> Boolean -> Boolean `*/
const xor = (b : boolean) => (d : boolean) : boolean => b !== d

/**` nxor : Boolean -> Boolean -> Boolean `*/
const nxor = (b : boolean) => (d : boolean) : boolean => b === d

/**` lt : Number -> Number -> Boolean `*/
const lt = (x : number) => (y : number) : boolean => x < y

/**` lte : Number -> Number -> Boolean `*/
const lte = (x : number) => (y : number) : boolean => x <= y

/**` gt : Number -> Number -> Boolean `*/
const gt = (x : number) => (y : number) : boolean => x >= y

/**` gte : Number -> Number -> Boolean `*/
const gte = (x : number) => (y : number) : boolean => x >= y

/**` less : Number -> Number -> Boolean `*/
const less = (x : number) => (y : number) : boolean => y < x

/**` lessEqual : Number -> Number -> Boolean `*/
const lessEqual = (x : number) => (y : number) : boolean => y <= x

/**` greater : Number -> Number -> Boolean `*/
const greater = (x : number) => (y : number) : boolean => y > x

/**` greaterEqual : Number -> Number -> Boolean `*/
const greaterEqual = (x : number) => (y : number) : boolean => y >= x

/**` approx : Number -> Number -> Number -> Boolean `*/
const approx = (amount : number) => (x : number) => (y : number) : boolean => Math.abs (x - y) < amount

/**` napprox : Number -> Number -> Number -> Boolean `*/
const napprox = (amount : number) => (x : number) => (y : number) : boolean => Math.abs (x - y) > amount

/**` diff : Number -> Number -> Number `*/
const diff = (x : number) => (y : number) : number => Math.abs (x - y)

/**` lerp : Number -> Number -> Number -> Number `*/
const lerp = (t : number) => (x : number) => (y : number) : number => x + (y - x) * t

/**` even : Number -> Boolean `*/
const even = (x : number) : boolean => x % 2 === 0

/**` odd : Number -> Boolean `*/
const odd = (x : number) : boolean => Math.abs (x) % 2 === 1

/**` toDegrees : Number -> Number `*/
const toDegrees = (degrees : number) : number => degrees * invPiDiv180

/**` toRadians : Number -> Number `*/
const toRadians = (degrees : number) : number => degrees * piDiv180

/**` toHexColor : Number -> String `*/
const toHexColor = (decimal : number) : string => `#${((~~Math.abs (decimal)) % 0x1000000) .toString (16) .padStart (6, '0')}`

/**` id : a -> a `*/
const id = <a>(x : a) : a => x

/**` apply : (a -> b) -> b -> a `*/
const apply : <a, b>(f : (x : a) => b) => (x : a) => b = id

/**` rapply : b -> (a -> b) -> a `*/
const rapply = <a>(x : a) => <b>(f : (x : a) => b) : b => f (x)

/**` flip : (a -> b -> c) -> b -> a -> c `*/
const flip = <a, b, c>(f : (x : a) => (y : b) => c) => (y : b) => (x : a) : c => f (x) (y)

/**` notf : (a -> Boolean) -> a -> Boolean `*/
const notf = <a>(predicate : (value : a) => boolean) => (value : a) : boolean =>
	!predicate (value)

/**` until : (a -> Boolean) -> (a -> a) -> a -> a `*/
const until = <a>(predicate : (value : a) => boolean) => (endomorphism : (value : a) => a) => (initial : a) : a =>
{
	while (!predicate (initial))
		initial = endomorphism (initial)
	return initial
}

/**` bound : Number -> Number -> Number -> Number `*/
const bound = (lower : number) => (upper : number) => (x : number) : number =>
	Math.min (Math.max (lower, x), upper)

/**` isIn : Number -> Number -> Number -> Boolean `*/
const isIn = (lower : number) => (upper : number) => (n : number) : boolean => lower < n && n < upper

/**` isInRect : ...6 Number -> Boolean `*/
const isInRect = (rx : number) => (ry : number) => (rw : number) => (rh : number) => (x : number) => (y : number) : boolean =>
	rx < x && x < rx + rw &&
	ry < y && y < ry + rh

/**` isInCircle : ...5 Number -> Boolean `*/
const isInCircle = (r : number) => (cx : number) => (cy : number) => (x : number) => (y : number) : boolean =>
	(x - cx) ** 2 + (y - cy) ** 2 < r ** 2

/**` match : [a] -> Number -> a `*/
const match = <a>(...values : Array <a>) => (index : number) : a =>
	values[index] ?? error (`'match' takes non-negative integers in interval [0, ${values.length}); instead received '${index}'`)

/**` quadraticCurve : Number -> Number `*/
const quadraticCurve = (x : number) : number =>
	x < 0.5
		? 2 * x ** 2
		: 1 - 2 * (1 - x) ** 2


/**` quarticCurve : Number -> Number `*/
const quarticCurve = (x : number) : number =>
	x < 0.5
		? 8 * x ** 4
		: 1 - 8 * (1 - x) ** 4

/**` show : a -> String `*/
const show = <a>(value : a) : string => `${value}`

/**` error : String -> a `*/
const error = (message : string) : any => { throw message }

/**` warn : String -> a -> a `*/
const warn = (message : string) => <a>(value : a) : a => (console.warn(message), value)

/********************************************************************************************************************************/
// Implementation of Algebraic Data Type Constructors //

interface Boolean
{
	/**` Boolean.pipe : (Boolean -> a) -> a `*/
	pipe : <a>(morphism : (bool : boolean) => a) => a

	/**` Boolean.eq : Boolean -> Boolean `*/
	eq : (bool : boolean) => boolean
}

interface Number
{
	/**` Number.pipe : (Number -> a) -> a `*/
	pipe : <a>(morphism : (num : number) => a) => a

	/**` Number.eq : Number -> Boolean `*/
	eq : (num : number) => boolean
}

interface String
{
	/**` String.pipe : (String -> a) -> a `*/
	pipe : <a>(morphism : (str : string) => a) => a

	/**` String.eq : String -> Boolean `*/
	eq : (str : string) => boolean
}

Boolean.prototype.pipe = function (f) { return f (!!this)          }
Number .prototype.pipe = function (f) { return f (+this)           }
String .prototype.pipe = function (f) { return f (this.toString()) }

Boolean.prototype.eq = function (x) { return !!this          === x }
Number .prototype.eq = function (x) { return +this           === x }
String .prototype.eq = function (x) { return this.toString() === x }

/**` IO : (() -> a) -> IO a `*/
const IO = <a>(effect : () => a) : IO <a> =>
	({
		variation : 'IO',
		effect,
		pipe (f) { return f (this) },
		bind : f => IO (() => f (effect ()).effect ()),
		fmap : f => IO (() => f (effect ())),
		bindto : k => f =>
			IO (() => {
				const $ = effect ()
				return { ...$, [k] : f ($).effect () } as any
			}),
		fmapto : k => f =>
			IO (() => {
				const $ = effect ()
				return { ...$, [k] : f ($) } as any
			}),
		then : io => IO (() => (effect (), io.effect ())),
		also : f =>
			IO (() => {
				const x = effect ()
				return f (x).effect (), x
			}),
		side : io =>
			IO (() => {
				const x = effect ()
				return io.effect (), x
			}),
		cast : x => IO (() => (effect (), x))
	})

/**` Process : (s -> Pair s a) -> Process s a `*/
const Process = <s, a>(computation : (state : s) => Pair <s, a>) : Process <s, a> =>
	({
		variation : 'Process',
		computation,
		pipe (f) { return f (this) },
		bind : f =>
			Process (s => {
				const { fst : first, snd : second } = computation (s)
				return f (second).computation (first)
			}),
		fmap : f => Process (s => fsnd (f) (computation (s))),
		bindto : k => f =>
			Process (s => {
				const { fst : first0, snd : second0 } = computation (s)
				const { fst : first1, snd : second1 } = f (second0).computation (first0)
				return Pair (first1, { ...second0, [k] : second1 } as any)
			}),
		fmapto : k => f =>
			Process (s => {
				const { fst : first, snd : second } = computation (s)
				return Pair (first, { ...second, [k] : f (second) } as any)
			}),
		then : p => Process (s => p.computation (computation (s).fst)),
		also : f =>
			Process (s => {
				const { fst : first, snd : second } = computation (s)
				return Pair (f (second).computation (first).fst, second)
			}),
		side : p =>
			Process (s => {
				const { fst : first, snd : second } = computation (s)
				return Pair (p.computation (first).fst, second)
			}),
		cast : x => Process (s => Pair (computation (s).fst, x))
	})

/**` Nil : List a `*/
const Nil : List <any> =
	{
		variation    : 'Nil',
		get $REVERSE () { return this },
		$LEN         : 0,
		fmap         : _  => Nil,
		bind         : _  => Nil,
		fmapto       : _  => _ => Nil,
		bindto       : _  => _ => Nil,
		pipe         : f  => f (Nil),
		link         : id,
		eq           : xs => xs === Nil
	}

/**` Cons : (() -> a) -> (() -> List a) -> List a `*/
const Cons = <a>(lfirst : () => a) => (lrest : () => List <a>) : List <a> =>
	({
		variation : 'Cons',
		get head  () { return this.$HEAD ??= lfirst () },
		get tail  () { return this.$TAIL ??= lrest  () },
		pipe      (f) { return f (this) },
		eq        (xs)
		{
			let ys : List <a> = this
			for (let i = 0; xs.variation === 'Cons' && ys.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(Cons).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
				else if (xs.head .eq (ys.head))
					xs = xs.tail,
					ys = ys.tail
				else
					return false
			return xs.variation === ys.variation
		},
		bind (f)
		{
			let xs = f (this.head)
			let ys = this.tail
			for (let i = 0; xs.variation === 'Nil' && ys.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(Cons).bind' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
				else
					xs = f (ys.head),
					ys = ys.tail
			return ys.variation === 'Cons'
				? Cons (() => (xs as any).head) (() => (xs as any).tail .link (ys .bind (f)))
				: xs
		},
		fmap (f)
		{
			return Cons (() => f (this.head)) (() => this.tail .fmap (f))
		},
		bindto (k)
		{
			return f =>
			{
				let ys : List <a> = this
				let xs : any      = f ((ys as any).head)
				for (let i = 0; xs.variation === 'Nil' && (ys as any).tail.variation === 'Cons'; ++i)
					if (i === MAXI)
						error (`'(Cons).bindto' couldn't find any Cons under the max limit (${MAXI}); ${STAP}`)
					else
						ys = (ys as any).tail,
						xs = f ((ys as any).head)

				xs = xs .fmap (($ : any) => ({ ...(ys as any).head, [k] : $ }) as any)

				return (ys as any).tail.variation === 'Cons'
					? Cons (() => xs.head) (() => xs.tail .link ((ys as any).tail .bindto (k) (f)))
					: xs
			}
		},
		fmapto (k)
		{
			return f => Cons (() => ({ ...this.head, [k] : f (this.head) }) as any) (() => this.tail .fmapto (k) (f))
		},
		link (xs)
		{
			return xs.variation === 'Nil'
				? this
				: Cons (() => this.head) (() => this.tail .link (xs))
		}
	})

/**` List : (...a) -> List a `*/
const List = <a>(...elements : Array <a>) : List <a> =>
{
	let xs : List <a> = Nil
	for (let i = elements.length - 1; ~i; --i)
		xs = prepend (elements[i] as a) (xs)
	return xs
}

/**` Nothing : Maybe a `*/
const Nothing : Maybe <any> =
	{
		variation : 'Nothing',
		pipe      : f => f (Nothing),
		eq        : m => m === Nothing,
		bind      : _ => Nothing,
		fmap      : _ => Nothing,
		bindto    : _ => _ => Nothing,
		fmapto    : _ => _ => Nothing
	}

/**` Just : a -> Maybe a `*/
const Just = <a>(value : a) : Maybe <a> =>
	({
		variation : 'Just',
		value,
		pipe      (f) { return f (this) },
		eq        : m => m.variation === 'Just' && m.value .eq (value),
		bind      : rapply (value),
		fmap      : f => Just (f (value)),
		bindto    : k => f => f (value) .fmap (x => ({ ...value, [k] : x }) as any),
		fmapto    : k => f => Just ({ ...value, [k] : f (value) } as any)
	})

/**` Pair : (a, b) -> Pair a b `*/
const Pair = <a, b>(first : a, second : b) : Pair <a, b> =>
	({
		variation : 'Pair',
		fst       : first,
		snd       : second,
		pipe      (f) { return f (this) },
		eq        : p => p.fst .eq (first) && p.snd .eq (second)
	})

/**` Left : a -> Either a b `*/
const Left = <a, b>(value : a) : Either <a, b> =>
	({
		variation : 'Left',
		value,
		pipe (f) { return f (this) },
		eq   : x => x.variation === 'Left' && x.value .eq (value)
	})

/**` Right : b -> Either a b `*/
const Right = <a, b>(value : b) : Either <a, b> =>
	({
		variation : 'Right',
		value,
		pipe (f) { return f (this) },
		eq   : x => x.variation === 'Right' && x.value .eq (value)
	})

/**` Vector2 : (Number, Number) -> Vector2 `*/
const Vector2 = (x : number, y : number) : Vector2 =>
	({
		variation : 'Vector2',
		x, y,
		pipe (f) { return f (this) },
		eq   : v => v.x === x && v.y === y
	})

/**` Vector3 : (Number, Number, Number) -> Vector3 `*/
const Vector3 = (x : number, y : number, z : number) : Vector3 =>
	({
		variation : 'Vector3',
		x, y, z,
		pipe (f) { return f (this) },
		eq   : v => v.x === x && v.y === y && v.z === z
	})

/**` Vector4 : (Number, Number, Number, Number) -> Vector4 `*/
const Vector4 = (x : number, y : number, z : number, w : number) : Vector4 =>
	({
		variation : 'Vector4',
		x, y, z, w,
		pipe (f) { return f (this) },
		eq   : v => v.x === x && v.y === y && v.z === z && v.w === w
	})

/**` Matrix2 : (...4 Number) -> Matrix2 `*/
const Matrix2 = (
		ix : number, jx : number,
		iy : number, jy : number
	) : Matrix2 =>
	({
		variation : 'Matrix2',
		ix, jx, iy, jy,
		pipe (f) { return f (this) },
		eq   : m =>
			m.ix === ix && m.jx === jx &&
			m.iy === iy && m.jy === jy
	})

/**` Matrix3 : (...9 Number) -> Matrix3 `*/
const Matrix3 = (
		ix : number, jx : number, kx : number,
		iy : number, jy : number, ky : number,
		iz : number, jz : number, kz : number
	) : Matrix3 =>
	({
		variation : 'Matrix3',
		ix, jx, kx, iy, jy, ky, iz, jz, kz,
		pipe (f) { return f (this) },
		eq   : m =>
			m.ix === ix && m.jx === jx && m.kx === kx &&
			m.iy === iy && m.jy === jy && m.ky === ky &&
			m.iz === iz && m.jz === jz && m.kz === kz
	})

/**` Matrix4 : (...16 Number) -> Matrix4 `*/
const Matrix4 = (
		ix : number, jx : number, kx : number, lx : number,
		iy : number, jy : number, ky : number, ly : number,
		iz : number, jz : number, kz : number, lz : number,
		iw : number, jw : number, kw : number, lw : number
	) : Matrix4 =>
	({
		variation : 'Matrix4',
		ix, jx, kx, lx, iy, jy, ky, ly, iz, jz, kz, lz, iw, jw, kw, lw,
		pipe (f) { return f (this) },
		eq   : m =>
			m.ix === ix && m.jx === jx && m.kx === kx && m.lx === lx &&
			m.iy === iy && m.jy === jy && m.ky === ky && m.ly === ly &&
			m.iz === iz && m.jz === jz && m.kz === kz && m.lz === lz &&
			m.iw === iw && m.jw === jw && m.kw === kw && m.lw === lw
	})

/********************************************************************************************************************************/
// Constants and Micro-Functions for IO //

/**` send : a -> IO a `*/
const send = <a>(value : a) : IO <a> =>
	({
		variation : 'IO',
		effect    : () => value,
		pipe      (f) { return f (this) },
		bind      : f  => IO (() => f (value).effect ()),
		fmap      : f  => IO (() => f (value)),
		bindto    : k  => f => IO (() => ({ ...value, [k] : f (value).effect () } as any)),
		fmapto    : k  => f => IO (() => ({ ...value, [k] : f (value) } as any)),
		then      : id,
		also      : f  => IO (() => (f (value).effect (), value)),
		side      : io => IO (() => (io.effect (), value)),
		cast      : send
	})

/**` idle : IO () `*/
const idle : IO <null> =
	({
		variation : 'IO',
		effect    : () => null,
		pipe      (f) { return f (this) },
		bind      : f  => IO (() => f (null).effect ()),
		fmap      : f  => IO (() => f (null)),
		bindto    : _  => _ => error (`'(idle).bindto' was caught used incorrectly; use the Do syntax`),
		fmapto    : _  => _ => error (`'(idle).fmapto' was caught used incorrectly; use the Do syntax`),
		then      : id,
		also      : f  => IO (() => (f (null).effect (), null)),
		side      : io => IO (() => (io.effect (), null)),
		cast      : send
	})

/**` executing : (...IO a) -> IO () `*/
const executing = <a>(...ios : Array <IO <a>>) : IO <null> =>
	IO (() => (ios.forEach(io => io.effect ()), null))

/********************************************************************************************************************************/
// Constants and Micro-Functions for Process //

/**` put : s -> Process s a -> Process s a `*/
const put = <s>(replacement : s) => <a>(process : Process <s, a>) : Process <s, a> =>
	Process (s => Pair (replacement, process.computation (s).snd))

/**` get : Process s a -> Process s s `*/
const get = <s, a>(process : Process <s, a>) : Process <s, s> =>
	Process (s => same (process.computation (s).fst))

/**` runProcess : Process s a -> s -> Pair s a `*/
const runProcess = <s, a>(process : Process <s, a>) =>
	process.computation

/**` execProcess : Process s a -> s -> s `*/
const execProcess = <s, a>(process : Process <s, a>) => (state : s) : s =>
	process.computation (state).fst

/**` evalProcess : Process s a -> s -> a `*/
const evalProcess = <s, a>(process : Process <s, a>) => (state : s) : a =>
	process.computation (state).snd

/**` mapProcess : (Pair s a -> Pair s b) -> Process s a -> Process s b `*/
const mapProcess = <s, a, b>(morphism : (result : Pair <s, a>) => Pair <s, b>) => (process : Process <s, a>) : Process <s, b> =>
	Process (s => morphism (process.computation (s)))

/**` endomapState : (s -> s) -> Process s a -> Process s a `*/
const endomapState = <s, a>(endomorphism : (state : s) => s) => (process : Process <s, a>) : Process <s, a> =>
	Process (s => ffst (endomorphism) (process.computation (s)))

/********************************************************************************************************************************/
// Constants and Micro-Functions for List //

/**` isNil : List a -> Boolean `*/
const isNil = <a>(xs : List <a>) : xs is Assert_Nil <a> =>
	xs.variation === 'Nil'

/**` isCons : List a -> Boolean `*/
const isCons = <a>(xs : List <a>) : xs is Assert_Cons <a> =>
	xs.variation === 'Cons'

/**` head : List a -> a `*/
const head = <a>(xs : List <a>) : a =>
	xs.variation === 'Cons'
		? xs.head
		: error (`'head' received a Nil value`)

/**` tail : List a -> List a `*/
const tail = <a>(xs : List <a>) : List <a> =>
	xs.variation === 'Cons'
		? xs.tail
		: error (`'tail' received a Nil value`)

/**` link : List a -> List a -> List a `*/
const link = <a>(xs : List <a>) => (ys : List <a>) : List <a> =>
	xs .link (ys)

/**` listToArray : List a -> [...a] `*/
const listToArray = <a>(xs : List <a>) : Array <a> =>
{
	const ys : Array <a> = []
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			return warn
				(`'toArray' reached the maximum amount of elements allowed (${MAXI}); ${STAP}`)
				(ys)
		else
			ys.push (xs.head),
			xs = xs.tail
	return ys
}

/**` unchars : List String -> String `*/
const unchars = (xs : List <string>) : string =>
{
	let str = ""
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			return warn
				(`'toString' reached the maximum amount of characters allowed (${MAXI}); ${STAP}`)
				(str)
		else
			str += xs.head,
			xs = xs.tail
	return str
}

/**` chars : String -> List String `*/
const chars = (str : string) : List <string> =>
	(List as any).apply(null, str.split(''))

/**` prepend : a -> List a -> List a `*/
const prepend = <a>(first : a) => (rest : List <a>) : List <a> =>
	rest.variation === 'Nil'
		? singleton (first)
		:
			({
				variation : 'Cons',
				head      : first,
				tail      : rest,
				$HEAD     : first,
				$TAIL     : rest,
				$LAST     : rest.$LAST,
				$LEN      : rest.$LEN! + 1 || undefined,
				pipe      (f) { return f (this) },
				eq        : xs =>
				{
					if (xs.variation === 'Cons' && xs.head .eq (first))
						xs = xs.tail
					else
						return false
					let ys : List <a> = rest
					for (let i = 0; xs.variation === 'Cons' && ys.variation === 'Cons'; ++i)
						if (i === MAXI)
							error (`'(prepend).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
						else if (xs.head .eq (ys.head))
							xs = xs.tail,
							ys = ys.tail
						else
							return false
					return xs.variation === ys.variation
				},
				bind : f =>
				{
					let xs            = f (first)
					let ys : List <a> = rest
					for (let i = 0; xs.variation === 'Nil' && ys.variation === 'Cons'; ++i)
						if (i === MAXI)
							error (`'(prepend).bind' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
						else
							xs = f (ys.head),
							ys = ys.tail
					return ys.variation === 'Cons'
						? Cons (() => (xs as any).head) (() => (xs as any).tail .link (ys .bind (f)))
						: xs
				},
				fmap : f => Cons (() => f (first)) (() => rest .fmap (f)),
				bindto (k)
				{
					return f =>
					{
						let ys : List <a> = this
						let xs : any      = f (first)
						for (let i = 0; xs.variation === 'Nil' && (ys as any).tail.variation === 'Cons'; ++i)
							if (i === MAXI)
								error (`'(prepend).bindto' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
							else
								ys = (ys as any).tail,
								xs = f ((ys as any).head)

						xs = xs .fmap (($ : any) => ({ ...(ys as any).head, [k] : $ }) as any)

						return (ys as any).tail.variation === 'Cons'
							? Cons (() => xs.head) (() => xs.tail .link ((ys as any).tail .bindto (k) (f)))
							: xs
					}
				},
				fmapto : k => f => Cons (() => ({ ...first, [k] : f (first) }) as any) (() => rest .fmapto (k) (f)),
				link (xs)
				{
					return xs.variation === 'Nil'
						? this
						: Cons (() => first) (() => rest .link (xs))
				}
			})

/**` llprepend : (() -> a) -> List a -> List a `*/
const llprepend = <a>(lfirst : () => a) => (rest : List <a>) : List <a> =>
	({
		variation : 'Cons',
		get head  () { return this.$HEAD ??= lfirst() },
		tail      : rest,
		$TAIL     : rest,
		$LAST     : rest.$LAST,
		$LEN      : rest.$LEN! + 1 || undefined,
		pipe      (f) { return f (this) },
		eq        (xs)
		{
			if (xs.variation === 'Cons' && xs.head .eq (this.head))
				xs = xs.tail
			else
				return false
			let ys : List <a> = rest
			for (let i = 0; xs.variation === 'Cons' && ys.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(llprepend).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
				else if (xs.head .eq (ys.head))
					xs = xs.tail,
					ys = ys.tail
				else
					return false
			return xs.variation === ys.variation
		},
		bind (f)
		{
			let xs = f (this.head)
			let ys = rest
			for (let i = 0; xs.variation === 'Nil' && ys.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(llprepend).bind' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
				else
					xs = f (ys.head),
					ys = ys.tail
			return ys.variation === 'Cons'
				? Cons (() => (xs as any).head) (() => (xs as any).tail .link (ys .bind (f)))
				: xs
		},
		fmap (f)
		{
			return Cons (() => f (this.head)) (() => rest .fmap (f))
		},
		bindto (k)
		{
			return f =>
			{
				let ys : List <a> = this
				let xs : any      = f (this.head)
				for (let i = 0; xs.variation === 'Nil' && (ys as any).tail.variation === 'Cons'; ++i)
					if (i === MAXI)
						error (`'(llprepend).bindto' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
					else
						ys = (ys as any).tail,
						xs = f ((ys as any).head)
				xs = xs .fmap (($ : any) => ({ ...(ys as any).head, [k] : $ }) as any)
				return (ys as any).tail.variation === 'Cons'
					? Cons (() => xs.head) (() => xs.tail .link ((ys as any).tail .bindto (k) (f)))
					: xs
			}
		},
		fmapto (k)
		{
			return f => Cons (() => ({ ...this.head, [k] : f (this.head) }) as any) (() => rest .fmapto (k) (f))
		},
		link (xs)
		{
			return xs.variation === 'Nil'
				? this
				: Cons (() => this.head) (() => rest .link (xs))
		}
	})

/**` lrprepend : a -> (() -> List a) -> List a `*/
const lrprepend = <a>(first : a) => (lrest : () => List <a>) : List <a> =>
	({
		variation : 'Cons',
		head      : first,
		get tail  () { return this.$TAIL ??= lrest () },
		$HEAD     : first,
		pipe      (f) { return f (this) },
		eq        (xs)
		{
			if (xs.variation === 'Cons' && xs.head .eq (first))
				xs = xs.tail
			else
				return false
			let ys : List <a> = this.tail
			for (let i = 0; xs.variation === 'Cons' && ys.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(lrprepend).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
				else if (xs.head .eq (ys.head))
					xs = xs.tail,
					ys = ys.tail
				else
					return false
			return xs.variation === ys.variation
		},
		bind (f)
		{
			let xs = f (first)
			let ys = this.tail
			for (let i = 0; xs.variation === 'Nil' && ys.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(lrprepend).bind' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
				else
					xs = f (ys.head),
					ys = ys.tail
			return ys.variation === 'Cons'
				? Cons (() => (xs as any).head) (() => (xs as any).tail .link (ys .bind (f)))
				: xs
		},
		fmap (f)
		{
			return Cons (() => f (first)) (() => this.tail .fmap (f))
		},
		bindto (k)
		{
			return f =>
			{
				let ys : List <a> = this
				let xs : any      = f (first)
				for (let i = 0; xs.variation === 'Nil' && (ys as any).tail.variation === 'Cons'; ++i)
					if (i === MAXI)
						error (`'(lrprepend).bindto' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
					else
						ys = (ys as any).tail,
						xs = f ((ys as any).head)

				xs = xs .fmap (($ : any) => ({ ...(ys as any).head, [k] : $ }) as any)

				return (ys as any).tail.variation === 'Cons'
					? Cons (() => xs.head) (() => xs.tail .link ((ys as any).tail .bindto (k) (f)))
					: xs
			}
		},
		fmapto (k)
		{
			return f => Cons (() => ({ ...first, [k] : f (first) }) as any) (() => this.tail .fmapto (k) (f))
		},
		link (xs)
		{
			return xs.variation === 'Nil'
				? this
				: lrprepend (first) (() => this.tail .link (xs))
		}
	})

/**` append : a -> List a -> List a `*/
const append = <a>(value : a) => (rest : List <a>) : List <a> =>
	rest.variation === 'Nil'
		? singleton (value)
		:
			({
				variation : 'Cons',
				get head  () { return this.$HEAD ??= (rest as any).head                  },
				get tail  () { return this.$TAIL ??= append (value) ((rest as any).tail) },
				$HEAD     : rest.$HEAD,
				$LAST     : value,
				$INIT     : rest,
				$LEN      : rest.$LEN! + 1 || undefined,
				pipe      (f) { return f (this) },
				eq        (xs)
				{
					let ys : List <a> = this
					for (let i = 0; xs.variation === 'Cons' && ys.variation === 'Cons'; ++i)
						if (i === MAXI)
							error (`'(append).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
						else if (xs.head .eq (ys.head))
							xs = xs.tail,
							ys = ys.tail
						else
							return false
					return xs.variation === ys.variation
				},
				bind (f)
				{
					let xs = f (this.head)
					let ys = this.tail
					for (let i = 0; xs.variation === 'Nil' && ys.variation === 'Cons'; ++i)
						if (i === MAXI)
							error (`'(append).bind' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
						else
							xs = f (ys.head),
							ys = ys.tail
					return ys.variation === 'Cons'
						? Cons (() => (xs as any).head) (() => (xs as any).tail .link (ys .bind (f)))
						: xs
				},
				fmap (f)
				{
					return Cons (() => f (this.head)) (() => this.tail .fmap (f))
				},
				bindto (k)
				{
					return f =>
					{
						let ys : List <a> = this
						let xs : any      = f ((ys as any).head)
						for (let i = 0; xs.variation === 'Nil' && (ys as any).tail.variation === 'Cons'; ++i)
							if (i === MAXI)
								error (`'(append).bindto' couldn't find any Cons under the max limit (${MAXI}); ${STAP}`)
							else
								ys = (ys as any).tail,
								xs = f ((ys as any).head)

						xs = xs .fmap (($ : any) => ({ ...(ys as any).head, [k] : $ }) as any)

						return (ys as any).tail.variation === 'Cons'
							? Cons (() => xs.head) (() => xs.tail .link ((ys as any).tail .bindto (k) (f)))
							: xs
					}
				},
				fmapto (k)
				{
					return f => Cons (() => ({ ...this.head, [k] : f (this.head) }) as any) (() => this.tail .fmapto (k) (f))
				},
				link (xs)
				{
					return xs.variation === 'Nil'
						? this
						: Cons (() => this.head) (() => this.tail .link (xs))
				}
			})

/**` singleton : a -> List a `*/
const singleton = <a>(value : a) : List <a> =>
	({
		variation    : 'Cons',
		head         : value,
		tail         : Nil,
		$HEAD        : value,
		$TAIL        : Nil,
		$LAST        : value,
		$INIT        : Nil,
		get $REVERSE () { return this },
		$LEN         : 1,
		pipe         (f) { return f (this) },
		eq           : xs => xs.variation === 'Cons' && xs.tail.variation === 'Nil' && xs.head .eq (value),
		bind         : rapply (value),
		fmap         : f => singleton (f (value)),
		bindto       : k => f => f (value) .fmap (x => ({ ...value, [k] : x }) as any),
		fmapto       : k => f => singleton ({ ...value, [k] : f (value) } as any),
		link         : prepend (value)
	})

/**` repeat : a -> List a `*/
const repeat = <a>(value : a) : List <a> =>
	({
		variation : 'Cons',
		head      : value,
		get tail  () { return this },
		$HEAD     : value,
		get $TAIL () { return this },
		get $INIT () { return this },
		pipe      (f) { return f (this) },
		eq        (xs)
		{
			for (let i = 0; xs.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(repeat).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
				else if (xs.head .eq (value))
					xs = xs.tail
				else
					return false
			return false
		},
		bind   : f =>
		{
			const xs = f (value)
			return xs.variation === 'Nil'
				? error (`'(repeat).bind' received an operation that returned Nil`)
				: cycle (xs)
		},
		fmap   : f => repeat (f (value)),
		bindto : k => f =>
		{
			const xs = f (value)
			return xs.variation === 'Nil'
				? error (`'(repeat).bindto' received an operation that returned Nil`)
				: cycle (xs .fmap (x => ({ ...value, [k] : x}) as any))
		},
		fmapto : k => f => repeat ({ ...value, [k] : f (value) } as any),
		link   (_) { return this }
	})

/**` cycle : List a -> List a `*/
const cycle = <a>(pattern : List <a>) : List <a> =>
	pattern.variation === 'Nil'
		? error (`'cycle' received a Nil list`)
		:
			({
				variation : 'Cons',
				get head  () { return this.$HEAD ??= (pattern as any).head              },
				get tail  () { return this.$TAIL ??= (pattern as any).tail .link (this) },
				get $INIT () { return this },
				pipe      (f) { return f (this) },
				eq        (xs)
				{
					let ys : List <a> = this
					for (let i = 0; xs.variation === 'Cons'; ++i)
						if (i === MAXI)
							error (`'(cycle).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
						else if (xs.head .eq ((ys as any).head))
							xs = xs.tail,
							ys = (ys as any).tail
						else
							return false
					return false
				},
				bind : f => cycle (pattern .bind (f)),
				fmap : f => cycle (pattern .fmap (f)),
				bindto : k => f => cycle (pattern .bindto (k) (f)),
				fmapto : k => f => cycle (pattern .fmapto (k) (f)),
				link (_) { return this }
			})

/**` iterate : (a -> a) -> a -> List a `*/
const iterate = <a>(endomorphism : (value : a) => a) => (initial : a) : List <a> =>
	({
		variation : 'Cons',
		head      : initial,
		get tail  () { return this.$TAIL ??= iterate (endomorphism) (endomorphism (initial)) },
		$HEAD     : initial,
		get $INIT () { return this },
		pipe      (f) { return f (this) },
		eq        (xs)
		{
			let ys : List <a> = this
			for (let i = 0; xs.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(iterate).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
				else if (xs.head .eq ((ys as any).head))
					xs = xs.tail,
					ys = (ys as any).tail
				else
					return false
			return false
		},
		bind (f)
		{
			let xs = f (initial)
			let ys = this.tail
			for (let i = 0; xs.variation === 'Nil'; ++i)
				if (i === MAXI)
					error (`'(iterate).bind' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
				else
					xs = f ((ys as any).head),
					ys = (ys as any).tail
			return ys.variation === 'Cons'
				? Cons (() => (xs as any).head) (() => (xs as any).tail .link (ys .bind (f)))
				: xs
		},
		fmap (f)
		{
			return Cons (() => f (initial)) (() => this.tail .fmap (f))
		},
		bindto (k)
		{
			return f =>
			{
				let ys : List <a> = this
				let xs : any      = f ((ys as any).head)
				for (let i = 0; xs.variation === 'Nil' && (ys as any).tail.variation === 'Cons'; ++i)
					if (i === MAXI)
						error (`'(iterate).bindto' couldn't find any Cons under the max limit (${MAXI}); ${STAP}`)
					else
						ys = (ys as any).tail,
						xs = f ((ys as any).head)

				xs = xs .fmap (($ : any) => ({ ...(ys as any).head, [k] : $ }) as any)

				return (ys as any).tail.variation === 'Cons'
					? Cons (() => xs.head) (() => xs.tail .link ((ys as any).tail .bindto (k) (f)))
					: xs
			}
		},
		fmapto (k)
		{
			return f => Cons (() => ({ ...initial, [k] : f (initial) }) as any) (() => this.tail .fmapto (k) (f))
		},
		link (xs)
		{
			return xs.variation === 'Nil'
				? this
				: lrprepend (initial) (() => this.tail .link (xs))
		}
	})

/**` replicate : Number -> a -> List a `*/
const replicate = (amount : number) => <a>(value : a) : List <a> =>
	amount > 0
		?
			({
				variation : 'Cons',
				head      : value,
				get tail  () { return this.$TAIL ??= replicate (amount - 1) (value) },
				$HEAD     : value,
				pipe      (f) { return f (this) },
				eq        (xs)
				{
					for (let i = 0; i < amount && xs.variation === 'Cons'; ++i)
						if (i === MAXI)
							error (`'(replicate).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
						else if (xs.head .eq (value))
							xs = xs.tail
						else
							return false
					return xs.variation === 'Nil'
				},
				bind   : f => concat (replicate (amount) (f (value))),
				fmap   : f => replicate (amount) (f (value)),
				bindto : k => f => concat (replicate (amount) (f (value) .fmap (x => ({ ...value, [k] : x }) as any))),
				fmapto : k => f => replicate (amount) ({ ...value, [k] : f (value) } as any),
				link (xs)
				{
					return xs.variation === 'Nil'
						? this
						: lrprepend (value) (() => this.tail .link (xs))
				}
			})
		: Nil

/**` countBy : Number -> Number -> List Number `*/
const countBy = (step : number) => (start : number) : List <number> =>
	({
		variation : 'Cons',
		head      : start,
		get tail  () { return this.$TAIL ??= countBy (step) (start + step) },
		$HEAD     : start,
		get $INIT () { return this },
		pipe      (f) { return f (this) },
		eq        (xs)
		{
			for (let i = 0; xs.variation === 'Cons'; ++i)
				if (i === MAXI)
					error (`'(countBy).eq' traversed too many elements for equality (${MAXI}); ${STAP}`)
				else if (xs.head .eq (start + step * i))
					xs = xs.tail
				else
					return false
			return false
		},
		bind (f)
		{
			let i = 0
			let xs = f (start)
			while (xs.variation === 'Nil')
				if (i === MAXI)
					error (`'(countBy).bind' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
				else
					++i, xs = f (start + step * i)
			return Cons (() => (xs as any).head) (() => (xs as any).tail .link (countBy (step) (start + step * i) .bind (f)))
		},
		fmap (f)
		{
			return Cons (() => f (start)) (() => this.tail .fmap (f))
		},
		bindto : _ => error (`'(countBy).bindto' was caught used incorrectly; use the Do syntax`),
		fmapto : _ => error (`'(countBy).fmapto' was caught used incorrectly; use the Do syntax`),
		link (_) { return this }
	})

/**` countDown : Number -> List Number `*/
const countDown : (start : number) => List <number> = countBy (-1)

/**` countUp : Number -> List Number `*/
const countUp : (start : number) => List <number> = countBy (1)

/**` naturals : Number -> List Number `*/
const naturals : List <number> = countUp (0)

/**` concat : List (List a) -> List a `*/
const concat = <a>(xss : List <List <a>>) : List <a> =>
{
	if (xss.variation === 'Nil')
		return Nil

	let xs = xss.head
	let ys = xss.tail
	for (let i = 0; xs.variation === 'Nil' && ys.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'concat' couldn't find Cons under the max limit (${MAXI}); ${STAP}`)
		else
			xs = ys.head,
			ys = ys.tail
	return ys.variation === 'Cons'
		? Cons (() => (xs as any).head) (() => (xs as any).tail .link (concat (ys)))
		: xs
}

/**` all : (a -> Boolean) -> List a -> Boolean `*/
const all = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : boolean =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'all' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (xs.head))
			xs = xs.tail
		else
			return false
	return true
}

/**` any : (a -> Boolean) -> List a -> Boolean `*/
const any = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : boolean =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'any' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (xs.head))
			return true
		else
			xs = xs.tail
	return false
}

/**` at : Number -> List a -> a `*/
const at = (index : number) => <a>(xs : List <a>) : a =>
{
	if (!Number.isInteger (index) || index < 0)
		error (`'at' received index '${index}'; must be a non-negative integer`)
	let i = 0
	while (xs.variation === 'Cons')
		if (i === index)
			return xs.head
		else
			xs = xs.tail,
			++i
	return error (`'at' received index '${index}' for list of length ${i}; must be a non-negative integer less than ${i}`)
}

/**` indexing : List a -> Number -> a `*/
const indexing = <a>(xs : List <a>) => (index : number): a =>
{
	if (!Number.isInteger (index) || index < 0)
		error (`'indexing' received index '${index}'; must be a non-negative integer`)
	let i  = 0
	let ys = xs
	while (ys.variation === 'Cons')
		if (i === index)
			return ys.head
		else
			ys = ys.tail,
			++i
	return error (`'indexing' received index '${index}' for list of length ${i}; must be a non-negative integer less than ${i}`)
}

/**` len : List a -> Number `*/
const len = <a>(xs : List <a>) : number =>
{
	if (xs.$LEN !== undefined)
		return xs.$LEN
	let i  = 0
	let ys = xs
	while (ys.variation === 'Cons')
		if (i === MAXI)
			error (`'len' traversed too many elements (${MAXI}); ${STAP}`)
		else
			++i,
			ys = ys.tail
	return xs.$LEN = i
}

/**` last : List a -> a `*/
const last = <a>(xs : List <a>) : a =>
{
	if (xs.$LAST !== undefined)
		return xs.$LAST
	if (xs.variation === 'Nil')
		error (`'last' received a Nil list`)
	for (let i = 0; (xs as any).tail.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'last' traversed too many elements (${MAXI}); ${STAP}`)
		else
			xs = (xs as any).tail
	return xs.$LAST = (xs as any).head
}

/**` init : List a -> List a `*/
const init = <a>(xs : List <a>) : List <a> =>
{
	if (xs.$INIT !== undefined)
		return xs.$INIT
	if (xs.variation === 'Nil')
		error (`'init' received a Nil list`)
	return xs.$INIT =
		(
			(xs as any).tail.variation === 'Nil'
				? Nil
				: Cons (() => (xs as any).head) (() => init ((xs as any).tail))
		)
}

/**` reverse : List a -> List a `*/
const reverse = <a>(xs : List <a>) : List <a> =>
{
	if (xs.$REVERSE !== undefined)
		return xs.$REVERSE
	let ys : List <a> = Nil
	let zs            = xs
	for (let i = 0; zs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'reverse' traversed too many elements (${MAXI}); ${STAP}`)
		else
			ys = prepend (zs.head) (ys),
			zs = zs.tail
	return ys.$REVERSE = xs, xs.$REVERSE = ys
}

/**` map : (a -> b) -> List a -> List b `*/
const map = <a, b>(morphism : (element : a) => b) => (xs : List <a>) : List <b> =>
	xs .fmap (morphism)

/**` imap : (Number -> a -> b) -> List a -> List b `*/
const imap = <a, b>(imorphism : (index : number) => (element : a) => b) => (xs : List <a>) : List <b> =>
	xs.variation === 'Nil'
		? Nil
		: Cons (() => imorphism (0) (xs.head)) (() => imap (i => imorphism (i + 1)) (xs.tail))

/**` intersperse : a -> List a -> List a `*/
const intersperse = <a>(delimiter : a) => (xs : List <a>) : List <a> =>
	xs.variation === 'Nil' || xs.tail.variation === 'Nil'
		? xs
		: Cons (() => xs.head) (() => prepend (delimiter) (xs.tail))

/**` foldl : (b -> a -> b) -> b -> List a -> b `*/
const foldl = <a, b>(operation : (leftside : b) => (rightside : a) => b) => (initial : b) => (xs : List <a>) : b =>
{
	let x = initial
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'foldl' traversed too many elements (${MAXI}); ${STAP}`)
		else
			x = operation (x) (xs.head),
			xs = xs.tail
	return x
}

/**` foldl1 : (a -> a -> a) -> List a -> a `*/
const foldl1 = <a>(operation : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : a =>
{
	if (xs.variation === 'Nil')
		error (`'fold1' received a Nil list`)
	let x = (xs as any).head
	for (let i = 0; (xs = (xs as any).tail).variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'foldl1' traversed too many elements (${MAXI}); ${STAP}`)
		else
			x = operation (x) (xs.head)
	return x
}

/**` foldr : (a -> b -> b) -> b -> List a -> b `*/
const foldr = <a, b>(operation : (leftside : a) => (rightside : b) => b) => (initial : b) => (xs : List <a>) : b =>
{
	let ys : List <a> = Nil
	if (xs.$REVERSE === undefined)
	{
		let zs = xs
		for (let i = 0; zs.variation === 'Cons'; ++i)
			if (i === MAXI)
				error (`'foldr' traversed too many elements (${MAXI}); ${STAP}`)
			else
				ys = prepend (zs.head) (ys),
				zs = zs.tail
		xs.$REVERSE = ys
	}
	else
		ys = xs.$REVERSE
	let x = initial
	while (ys.variation === 'Cons')
		x = operation (ys.head) (x),
		ys = ys.tail
	return x
}

/**` foldr1 : (a -> b -> b) -> List a -> b `*/
const foldr1 = <a, b>(operation : (leftside : a) => (rightside : b) => b) => (xs : List <a>) : b =>
{
	if (xs.variation === 'Nil')
		error (`'foldr1' received a Nil list`)
	let ys : List <a> = Nil
	if (xs.$REVERSE === undefined)
	{
		let zs = xs
		for (let i = 0; zs.variation === 'Cons'; ++i)
			if (i === MAXI)
				error (`'foldr1' traversed too many elements (${MAXI}); ${STAP}`)
			else
				ys = prepend (zs.head) (ys),
				zs = zs.tail
		xs.$REVERSE = ys
	}
	else
		ys = xs.$REVERSE
	let x = (ys as any).head
	while ((ys = (ys as any).tail).variation === 'Cons')
		x = operation (ys.head) (x)
	return x
}

/**` scanl : (b -> a -> b) -> b -> List b `*/
const scanl = <a, b>(operation : (leftside : b) => (rightside : a) => b) => (initial : b) => (xs : List <a>) : List <b> =>
	xs.variation === 'Nil'
		? singleton (initial)
		: lrprepend (initial) (() => scanl (operation) (operation (initial) (xs.head)) (xs.tail))

/**` scanl1 : (a -> a -> a) -> List a `*/
const scanl1 = <a>(operation : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : List <a> =>
	xs.variation === 'Nil' || xs.tail.variation === 'Nil'
		? xs
		: Cons
			(() => xs.head)
			(() => scanl (operation) (operation (xs.head) ((xs.tail as any).head)) ((xs.tail as any).tail))

/**` scanr : (a -> b -> b) -> b -> List a -> List b `*/
const scanr = <a, b>(operation : (leftside : a) => (rightside : b) => b) => (initial : b) => (xs : List <a>) : List <b> =>
{
	let ys : List <a> = Nil
	if (xs.$REVERSE === undefined)
	{
		let zs = xs
		for (let i = 0; zs.variation === 'Cons'; ++i)
			if (i === MAXI)
				error (`'scanr' traversed too many elements (${MAXI}); ${STAP}`)
			else
				ys = prepend (zs.head) (ys),
				zs = zs.tail
		xs.$REVERSE = ys
	}
	else
		ys = xs.$REVERSE
	let zs = singleton (initial)
	while (ys.variation === 'Cons')
		zs = prepend (operation (ys.head) ((zs as any).head)) (zs),
		ys = ys.tail
	return zs
}

/**` scanr1 : (a -> a -> a) -> List a -> List a `*/
const scanr1 = <a>(operation : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : List <a> =>
{
	if (xs.variation === 'Nil' || xs.tail.variation === 'Nil')
		return xs

	let ys : List <a> = Nil
	if (xs.$REVERSE === undefined)
	{
		let zs : List <a> = xs
		for (let i = 0; zs.variation === 'Cons'; ++i)
			if (i === MAXI)
				error (`'scanr1' traversed too many elements (${MAXI}); ${STAP}`)
			else
				ys = prepend (zs.head) (ys),
				zs = zs.tail
		xs.$REVERSE = ys
	}
	else
		ys = xs.$REVERSE
	let zs = singleton ((ys as any).head)
	while ((ys = (ys as any).tail).variation === 'Cons')
		zs = prepend (operation (ys.head) ((zs as any).head)) (zs)
	return zs
}

/**` take : Number -> List a -> List a `*/
const take = (amount : number) => <a>(xs : List <a>) : List <a> =>
	xs.$LEN! <= amount
		? xs
		: xs.variation === 'Nil' || amount < 1
			? Nil
			: Cons (() => xs.head) (() => take (amount - 1) (xs.tail))

/**` drop : Number -> List a -> List a `*/
const drop = (amount : number) => <a>(xs : List <a>) : List <a> =>
{
	if (xs.$LEN! <= amount)
		return Nil
	for (let i = 1; i <= amount && xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'drop' traversed too many elements (${MAXI}); ${STAP}`)
		else
			xs = xs.tail
	return xs
}

/**` splitAt : Number -> List a -> Pair (List a) (List a) `*/
const splitAt = (index : number) => <a>(xs : List <a>) : Pair <List <a>, List <a>> =>
{
	if (index >= MAXI)
		error (`'splitAt' would be traversing too many elements (${MAXI}); ${STAP}`)
	if (xs.$LEN! <= index)
		return Pair (Nil, xs)
	if (index < 1)
		return Pair (xs, Nil)
	let ys : List <a> = Nil
	for (let i = 1; i <= index && xs.variation === 'Cons'; ++i)
		ys = prepend (xs.head) (ys),
		xs = xs.tail
	let zs : List <a> = Nil
	while (ys.variation === 'Cons')
		zs = prepend (ys.head) (zs),
		ys = ys.tail
	return Pair (zs, xs)
}

/**` takeWhile : (a -> Boolean) -> List a -> List a `*/
const takeWhile = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
	xs.variation === 'Cons' && predicate (xs.head)
		? lrprepend (xs.head) (() => takeWhile (predicate) (xs.tail))
		: Nil

/**` dropWhile : (a -> Boolean) -> List a -> List a `*/
const dropWhile = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`''dropWhile' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (xs.head))
			xs = xs.tail
		else
			return xs
	return Nil
}

/**` span : (a -> Boolean) -> List a -> Pair (List a) (List a) `*/
const span = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <List <a>, List <a>> =>
{
	let ys : List <a> = Nil
	for (let i = 0; xs.variation === 'Cons' && predicate (xs.head); ++i)
		if (i === MAXI)
			error (`'span' traversed too many elements ${MAXI}; ${STAP}`)
		else
			ys = prepend (xs.head) (ys),
			xs = xs.tail
	let zs : List <a> = Nil
	while (ys.variation === 'Cons')
		zs = prepend (ys.head) (zs),
		ys = ys.tail
	return Pair (zs, xs)
}

/**` elem : (Eq a) => a -> List a -> Boolean `*/
const elem = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : boolean =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'elem' traversed too many elements (${MAXI}); ${STAP}`)
		else if (xs.head .eq (value))
			return true
		else
			xs = xs.tail
	return false
}

/**` nelem : (Eq a) => a -> List a -> Boolean `*/
const nelem = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : boolean =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'nelem' traversed too many elements (${MAXI}); ${STAP}`)
		else if (xs.head .eq (value))
			return false
		else
			xs = xs.tail
	return true
}

/**` filter : (a -> Boolean) -> List a -> List a `*/
const filter = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'filter' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (xs.head))
			return lrprepend (xs.head) (() => filter (predicate) ((xs as any).tail))
		else
			xs = xs.tail
	return Nil
}

/**` partition : (a -> Boolean) -> List a -> Pair (List a) (List a) `*/
const partition = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <List <a>, List <a>> =>
	Pair (filter (predicate) (xs), filter (notf (predicate)) (xs))

/**` elemIndices : (Eq a) => a -> List a -> List Number `*/
const elemIndices = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : List <number> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'elemIndices' traversed too many elements (${MAXI}); ${STAP}`)
		else if (xs.head .eq (value))
			return lrprepend (i) (() => elemIndices (value) ((xs as any).tail) .fmap (add (i + 1)))
		else
			xs = xs.tail
	return Nil
}

/**` findIndices : (a -> Boolean) -> List a -> List Number `*/
const findIndices = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <number> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'findIndices' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (xs.head))
			return lrprepend (i) (() => findIndices (predicate) ((xs as any).tail) .fmap (add (i + 1)))
		else
			xs = xs.tail
	return Nil
}

/**` zip : List a -> List b -> List (Pair a b) `*/
const zip = <a>(firsts : List <a>) => <b>(seconds : List <b>) : List <Pair <a, b>> =>
	firsts.variation === 'Nil' || seconds.variation === 'Nil'
		? Nil
		: Cons (() => Pair (firsts.head, seconds.head)) (() => zip (firsts.tail) (seconds.tail))

/**` zipWith : (a -> b -> c) -> List a -> List b -> List c `*/
const zipWith = <a, b, c>(zipper : (first : a) => (second : b) => c) => (firsts : List <a>) => (seconds : List <b>) : List <c> =>
	firsts.variation === 'Nil' || seconds.variation === 'Nil'
		? Nil
		: Cons (() => zipper (firsts.head) (seconds.head)) (() => zipWith (zipper) (firsts.tail) (seconds.tail))

/**` unzip : List (Pair a b) -> Pair (List a) (List b) `*/
const unzip = <a, b>(pairs : List <Pair <a, b>>) : Pair <List <a>, List <b>> =>
	Pair (pairs .fmap (fst), pairs .fmap (snd))

/**` unzipWith : (c -> Pair a b) -> List c -> Pair (List a) (List b) `*/
const unzipWith = <a, b, c>(unzipper : (element : c) => Pair <a, b>) => (xs : List <c>) : Pair <List <a>, List <b>> =>
	xs
		.fmap (unzipper)
		.pipe (pairs => Pair (pairs .fmap (fst), pairs .fmap (snd)))

/**` lowercases : List String `*/
const lowercases : List <string> = chars ('abcdefghijklmnopqrstuvwxyz')

/**` uppercases : List String `*/
const uppercases : List <string> = chars ('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

/********************************************************************************************************************************/
// Constants and Micro-Functions for Maybe //

/**` isNothing : Maybe a -> Boolean `*/
const isNothing = <a>(maybe : Maybe <a>) : maybe is Assert_Nothing <a> =>
	maybe.variation === 'Nothing'

/**` isJust : Maybe a -> Boolean `*/
const isJust = <a>(maybe : Maybe <a>) : maybe is Assert_Just <a> =>
	maybe.variation === 'Just'

/**` ffromJust : b -> (a -> b) -> Maybe a -> b `*/
const ffromJust = <b>(fallback : b) => <a>(morphism : (value : a) => b) => (maybe : Maybe <a>) : b =>
	maybe.variation === 'Just'
		? morphism (maybe.value)
		: fallback

/**` fromJust : a -> Maybe a -> a `*/
const fromJust = <a>(fallback : a) => (maybe : Maybe <a>) : a =>
	maybe.variation === 'Just'
		? maybe.value
		: fallback

/**` extractJust : Maybe a -> a `*/
const extractJust = <a>(maybe : Maybe <a>) : a =>
	maybe.variation === 'Just'
		? maybe.value
		: error (`'extractJust' received a Nothing value`)

/**` ensure : (a -> Boolean) -> a -> Maybe a `*/
const ensure = <a>(predicate : (value : a) => boolean) => (value : a) : Maybe <a> =>
	predicate (value)
		? Just (value)
		: Nothing

/**` maybeIO : Maybe (IO a) -> IO () `*/
const maybeIO = <a>(maybe : Maybe <IO <a>>) : IO <null> =>
	maybe.variation === 'Nothing'
		? idle
		: maybe.value .cast (null)

/**` eqMaybe : (Eq a) => Maybe a -> a -> Boolean `*/
const eqMaybe = <a>(maybe : Maybe <Eq <a>>) => (value : Eq <a>) : boolean =>
	maybe.variation === 'Just' && maybe.value .eq (value)

/**` checkMaybe : (a -> Boolean) -> Maybe a -> Boolean `*/
const checkMaybe = <a>(predicate : (value : a) => boolean) => (maybe : Maybe <a>) : boolean =>
	maybe.variation === 'Just' && predicate (maybe.value)

/********************************************************************************************************************************/
// Constants and Micro-Functions for Pair //

/**` fst : Pair a b -> a `*/
const fst = <a, b>(pair : Pair <a, b>) : a =>
	pair.fst

/**` snd : Pair a b -> b `*/
const snd = <a, b>(pair : Pair <a, b>) : b =>
	pair.snd

/**` fboth : (a -> b) -> Pair a a -> Pair b b `*/
const fboth = <a, b>(morphism : (value : a) => b) => (pair : Pair <a, a>) : Pair <b, b> =>
	Pair (morphism (pair.fst), morphism (pair.snd))

/**` ffst : (a -> c) -> Pair a b -> Pair c b `*/
const ffst = <a, c>(morphism : (value : a) => c) => <b>(pair : Pair <a, b>) : Pair <c, b> =>
	Pair (morphism (pair.fst), pair.snd)

/**` fsnd : (b -> c) -> Pair a b -> Pair a c `*/
const fsnd = <b, c>(morphism : (value : b) => c) => <a>(pair : Pair <a, b>) : Pair <a, c> =>
	Pair (pair.fst, morphism (pair.snd))

/**` swap : Pair a b -> Pair b a `*/
const swap = <a, b>(pair : Pair <a, b>) : Pair <b, a> =>
	Pair (pair.snd, pair.fst)

/**` same : a -> Pair a a `*/
const same = <a>(value : a) : Pair <a, a> =>
	Pair (value, value)

/**` pick : Boolean -> Pair a a -> a `*/
const pick = (bool : boolean) : (<a>(pair : Pair <a, a>) => a) =>
	bool ? fst : snd

/**` curry : (Pair a b -> c) -> a -> b -> c `*/
const curry = <a, b, c>(f : (parameters : Pair <a, b>) => c) => (first : a) => (second : b) : c =>
	f (Pair (first, second))

/**` uncurry : (a -> b -> c) -> Pair a b -> c `*/
const uncurry = <a, b, c>(f : (first : a) => (second : b) => c) => (parameters : Pair <a, b>) : c =>
	f (parameters.fst) (parameters.snd)

/**` pairToArray : Pair a b -> [a, b] `*/
const pairToArray = <a, b>(pair : Pair <a, b>) : [a, b] =>
	[pair.fst, pair.snd]

/********************************************************************************************************************************/
// Constants and Micro-Functions for Either //

/**` isLeft : Either a b -> Boolean `*/
const isLeft = <a, b>(either : Either <a, b>) : either is Assert_Left <a, b> =>
	either.variation === 'Left'

/**` isRight : Either a b -> Boolean `*/
const isRight = <a, b>(either : Either <a, b>) : either is Assert_Right <a, b> =>
	either.variation === 'Right'

/**` mapLeft : (a -> c) -> Either a b -> Either c b `*/
const mapLeft = <a, c>(lf : (left : a) => c) => <b>(either : Either <a, b>) : Either <c, b> =>
	either.variation === 'Left'
		? Left (lf (either.value))
		: either as Either <c, b>

/**` mapRight : (b -> c) -> Either a b -> Either a c `*/
const mapRight = <b, c>(rf : (right : b) => c) => <a>(either : Either <a, b>) : Either <a, c> =>
	either.variation === 'Right'
		? Right (rf (either.value))
		: either as Either <a, c>

/**` fromEither : (a -> c) -> (b -> c) -> Either a b -> c `*/
const fromEither = <a, c>(lf : (left : a) => c) => <b>(rf : (right : b) => c) => (either : Either <a, b>) : c =>
	either.variation === 'Left'
		? lf (either.value)
		: rf (either.value)

/**` fromLeft : a -> Either a b -> a `*/
const fromLeft = <a>(fallback : a) => <b>(either : Either <a, b>) : a =>
	either.variation === 'Left'
		? either.value
		: fallback

/**` fromRight : b -> Either a b -> a `*/
const fromRight = <b>(fallback : b) => <a>(either : Either <a, b>) : b =>
	either.variation === 'Right'
		? either.value
		: fallback

/**` extractLeft : Either a b -> a `*/
const extractLeft = <a, b>(either : Either <a, b>) : a =>
	either.variation === 'Left'
		? either.value
		: error (`'fromLeft' received a Right value`)

/**` extractRight : Either a b -> b `*/
const extractRight = <a, b>(either : Either <a, b>) : b =>
	either.variation === 'Right'
		? either.value
		: error (`'fromRight' received a Left value`)

/********************************************************************************************************************************/
// Constants and Micro-Functions for Vectors and Matrices //

namespace V2
{
	/**` V2.zero : Vector2 `*/
	export const zero : Vector2 = Vector2 (0, 0)

	/**` V2.half : Vector2 `*/
	export const half : Vector2 = Vector2 (0.5, 0.5)

	/**` V2.unit : Vector2 `*/
	export const unit : Vector2 = Vector2 (1, 1)

	/**` V2.promoteV3 : Vector2 -> Vector3 `*/
	export const promoteV3 = (v : Vector2) : Vector3 =>
		Vector3 (v.x, v.y, 0)

	/**` V2.promoteV4 : Vector2 -> Vector4 `*/
	export const promoteV4 = (v : Vector2) : Vector4 =>
		Vector4 (v.x, v.y, 0, 0)

	/**` V2.translateX : Number -> Vector2 -> Vector2 `*/
	export const translateX = (dx : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x + dx, v.y)

	/**` V2.translateY : Number -> Vector2 -> Vector2 `*/
	export const translateY = (dy : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x, v.y + dy)

	/**` V2.translateXY : Number -> Number -> Vector2 -> Vector2 `*/
	export const translateXY = (dx : number) => (dy : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x + dx, v.y + dy)

	/**` V2.untranslateX : Number -> Vector2 -> Vector2 `*/
	export const untranslateX = (dx : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x - dx, v.y)

	/**` V2.untranslateY : Number -> Vector2 -> Vector2 `*/
	export const untranslateY = (dy : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x, v.y - dy)

	/**` V2.untranslateXY : Number -> Number -> Vector2 -> Vector2 `*/
	export const untranslateXY = (dx : number) => (dy : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x - dx, v.y - dy)

	/**` V2.scaleX : Number -> Vector2 -> Vector2 `*/
	export const scaleX = (kx : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x * kx, v.y)

	/**` V2.scaleY : Number -> Vector2 -> Vector2 `*/
	export const scaleY = (ky : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x, v.y * ky)

	/**` V2.scaleXY : Number -> Number -> Vector2 -> Vector2 `*/
	export const scaleXY = (kx : number) => (ky : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x * kx, v.y * ky)

	/**` V2.unscaleX : Number -> Vector2 -> Vector2 `*/
	export const unscaleX = (kx : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x / kx, v.y)

	/**` V2.unscaleY : Number -> Vector2 -> Vector2 `*/
	export const unscaleY = (ky : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x, v.y / ky)

	/**` V2.unscaleXY : Number -> Number -> Vector2 -> Vector2 `*/
	export const unscaleXY = (kx : number) => (ky : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x / kx, v.y / ky)

	/**` V2.scale : Number -> Vector2 -> Vector2 `*/
	export const scale = (k : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x * k, v.y * k)

	/**` V2.unscale : Number -> Vector2 -> Vector2 `*/
	export const unscale = (k : number) => (v : Vector2) : Vector2 =>
		Vector2 (v.x / k, v.y / k)

	/**` V2.translate : Vector2 -> Vector2 -> Vector2 `*/
	export const translate = (v : Vector2) => (w : Vector2) : Vector2 =>
		Vector2 (v.x + w.x, v.y + w.y)

	/**` V2.untranslate : Vector2 -> Vector2 -> Vector2 `*/
	export const untranslate = (v : Vector2) => (w : Vector2) : Vector2 =>
		Vector2 (w.x - v.x, w.y - v.y)

	/**` V2.negate : Vector2 -> Vector2 `*/
	export const negate = (v : Vector2) : Vector2 =>
		Vector2 (-v.x, -v.y)

	/**` V2.reciprocate : Vector2 -> Vector2 `*/
	export const reciprocate = (v : Vector2) : Vector2 =>
		Vector2 (1 / v.x, 1 / v.y)

	/**` V2.norm : Vector2 -> Number `*/
	export const norm = (v : Vector2) : number =>
		Math.sqrt (v.x ** 2 + v.y ** 2)

	/**` V2.normalize : Vector2 -> Vector2 `*/
	export const normalize = (v : Vector2) : Vector2 =>
	{
		const x = Math.sqrt (v.x ** 2 + v.y ** 2)
		return x === 0
			? V2.zero
			: Vector2 (v.x / x, v.y / x)
	}

	/**` V2.dot : Vector2 -> Vector2 -> Number `*/
	export const dot = (v : Vector2) => (w : Vector2) : number =>
		v.x * w.x + v.y * w.y

	/**` V2.transform : Matrix2 -> Vector2 -> Vector2 `*/
	export const transform = (m : Matrix2) => (v : Vector2) : Vector2 =>
		Vector2 (
			m.ix * v.x + m.jx * v.y,
			m.iy * v.x + m.jy * v.y
		)

	/**` V2.map : (Number -> Number) -> Vector2 -> Vector2 `*/
	export const map = (f : (coordinate : number) => number) => (v : Vector2) : Vector2 =>
		Vector2 (f (v.x), f (v.y))

	/**` V2.each : (Number -> Number -> Number) -> Vector2 -> Vector2 -> Vector2 `*/
	export const each = (f : (first : number) => (second : number) => number) => (v : Vector2) => (w : Vector2) : Vector2 =>
		Vector2 (f (v.x) (w.x), f (v.y) (w.y))

	/**` V2.isInRect : Vector2 -> Vector2 -> Vector2 -> Boolean `*/
	export const isInRect = (rxy : Vector2) => (rwh : Vector2) => (xy : Vector2) : boolean =>
		rxy.x < xy.x && xy.x < rxy.x + rwh.x &&
		rxy.y < xy.y && xy.y < rxy.y + rwh.y

	/**` V2.isInCircle : Number -> Vector2 -> Vector2 -> Boolean `*/
	export const isInCircle = (r : number) => (cxy : Vector2) => (xy : Vector2) : boolean =>
		(xy.x - cxy.x) ** 2 + (xy.y - cxy.y) ** 2 < r ** 2
}

namespace V3
{
	/**` V3.zero : Vector3 `*/
	export const zero : Vector3 = Vector3 (0, 0, 0)

	/**` V3.half : Vector3 `*/
	export const half : Vector3 = Vector3 (0.5, 0.5, 0.5)

	/**` V3.unit : Vector3 `*/
	export const unit : Vector3 = Vector3 (1, 1, 1)

	/**` V3.demoteV2 : Vector3 -> Vector2 `*/
	export const demoteV2 = (v : Vector3) : Vector2 =>
		Vector2 (v.x, v.y)

	/**` V3.promoteV4 : Vector3 -> Vector4 `*/
	export const promoteV4 = (v : Vector3) : Vector4 =>
		Vector4 (v.x, v.y, v.z, 0)

	/**` V3.translateX : Number -> Vector3 -> Vector3 `*/
	export const translateX = (dx : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x + dx, v.y, v.z)

	/**` V3.translateY : Number -> Vector3 -> Vector3 `*/
	export const translateY = (dy : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y + dy, v.z)

	/**` V3.translateZ : Number -> Vector3 -> Vector3 `*/
	export const translateZ = (dz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y, v.z + dz)

	/**` V3.translateXYZ : Number -> Number -> Number -> Vector3 -> Vector3 `*/
	export const translateXYZ = (dx : number) => (dy : number) => (dz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x + dx, v.y + dy, v.z + dz)

	/**` V3.untranslateX : Number -> Vector3 -> Vector3 `*/
	export const untranslateX = (dx : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x - dx, v.y, v.z)

	/**` V3.untranslateY : Number -> Vector3 -> Vector3 `*/
	export const untranslateY = (dy : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y - dy, v.z)

	/**` V3.untranslateZ : Number -> Vector3 -> Vector3 `*/
	export const untranslateZ = (dz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y, v.z - dz)

	/**` V3.untranslateXYZ : Number -> Number -> Number -> Vector3 -> Vector3 `*/
	export const untranslateXYZ = (dx : number) => (dy : number) => (dz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x - dx, v.y - dy, v.z - dz)

	/**` V3.scaleX : Number -> Vector3 -> Vector3 `*/
	export const scaleX = (kx : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x * kx, v.y, v.z)

	/**` V3.scaleY : Number -> Vector3 -> Vector3 `*/
	export const scaleY = (ky : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y * ky, v.z)

	/**` V3.scaleZ : Number -> Vector3 -> Vector3 `*/
	export const scaleZ = (kz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y, v.z * kz)

	/**` V3.scaleXYZ : Number -> Number -> Number -> Vector3 -> Vector3 `*/
	export const scaleXYZ = (kx : number) => (ky : number) => (kz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x * kx, v.y * ky, v.z * kz)

	/**` V3.unscaleX : Number -> Vector3 -> Vector3 `*/
	export const unscaleX = (kx : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x / kx, v.y, v.z)

	/**` V3.unscaleY : Number -> Vector3 -> Vector3 `*/
	export const unscaleY = (ky : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y / ky, v.z)

	/**` V3.unscaleZ : Number -> Vector3 -> Vector3 `*/
	export const unscaleZ = (kz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x, v.y, v.z / kz)

	/**` V3.unscaleXYZ : Number -> Number -> Number -> Vector3 -> Vector3 `*/
	export const unscaleXYZ = (kx : number) => (ky : number) => (kz : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x / kx, v.y / ky, v.z / kz)

	/**` V3.scale : Number -> Vector3 -> Vector3 `*/
	export const scale = (k : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x * k, v.y * k, v.z * k)

	/**` V3.unscale : Number -> Vector3 -> Vector3 `*/
	export const unscale = (k : number) => (v : Vector3) : Vector3 =>
		Vector3 (v.x / k, v.y / k, v.z / k)

	/**` V3.translate : Vector3 -> Vector3 -> Vector3 `*/
	export const translate = (v : Vector3) => (w : Vector3) : Vector3 =>
		Vector3 (v.x + w.x, v.y + w.y, v.z + w.z)

	/**` V3.untranslate : Vector3 -> Vector3 -> Vector3 `*/
	export const untranslate = (v : Vector3) => (w : Vector3) : Vector3 =>
		Vector3 (w.x - v.x, w.y - v.y, w.z - v.z)

	/**` V3.negate : Vector3 -> Vector3 `*/
	export const negate = (v : Vector3) : Vector3 =>
		Vector3 (-v.x, -v.y, -v.z)

	/**` V3.reciprocate : Vector3 -> Vector3 `*/
	export const reciprocate = (v : Vector3) : Vector3 =>
		Vector3 (1 / v.x, 1 / v.y, 1 / v.z)

	/**` V3.norm : Vector3 -> Number `*/
	export const norm = (v : Vector3) : number =>
		Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2)

	/**` V3.normalize : Vector3 -> Vector3 `*/
	export const normalize = (v : Vector3) : Vector3 =>
	{
		const x = Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2)
		return x === 0
			? V3.zero
			: Vector3 (v.x / x, v.y / x, v.z / x)
	}

	/**` V3.dot : Vector3 -> Vector3 -> Number `*/
	export const dot = (v : Vector3) => (w : Vector3) : number =>
		v.x * w.x + v.y * w.y + v.z * w.z

	/**` V3.transform : Matrix3 -> Vector3 -> Vector3 `*/
	export const transform = (m : Matrix3) => (v : Vector3) : Vector3 =>
		Vector3 (
			m.ix * v.x + m.jx * v.y + m.kx * v.z,
			m.iy * v.x + m.jy * v.y + m.ky * v.z,
			m.iz * v.x + m.jz * v.y + m.kz * v.z
		)

	/**` V3.map : (Number -> Number) -> Vector3 -> Vector3 `*/
	export const map = (endomorphism : (coordinate : number) => number) => (v : Vector3) : Vector3 =>
		Vector3 (endomorphism (v.x), endomorphism (v.y), endomorphism (v.z))

	/**` V3.each : (Number -> Number -> Number) -> Vector3 -> Vector3 -> Vector3 `*/
	export const each = (f : (first : number) => (second : number) => number) => (v : Vector3) => (w : Vector3) : Vector3 =>
		Vector3 (f (v.x) (w.x), f (v.y) (w.y), f (v.z) (w.z))
}

namespace V4
{
	/**` V4.zero : Vector4 `*/
	export const zero : Vector4 = Vector4 (0, 0, 0, 0)

	/**` V4.half : Vector4 `*/
	export const half : Vector4 = Vector4 (0.5, 0.5, 0.5, 0.5)

	/**` V4.unit : Vector4 `*/
	export const unit : Vector4 = Vector4 (1, 1, 1, 1)

	/**` V4.demoteV2 : Vector4 -> Vector2 `*/
	export const demoteV2 = (v : Vector4) : Vector2 =>
		Vector2 (v.x, v.y)

	/**` V4.demoteV4 : Vector4 -> Vector3 `*/
	export const demoteV4 = (v : Vector4) : Vector3 =>
		Vector3 (v.x, v.y, v.z)

	/**` V4.translateX : Number -> Vector4 -> Vector4 `*/
	export const translateX = (dx : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x + dx, v.y, v.z, v.w)

	/**` V4.translateY : Number -> Vector4 -> Vector4 `*/
	export const translateY = (dy : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y + dy, v.z, v.w)

	/**` V4.translateZ : Number -> Vector4 -> Vector4 `*/
	export const translateZ = (dz : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z + dz, v.w)

	/**` V4.translateW : Number -> Vector4 -> Vector4 `*/
	export const translateW = (dw : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z, v.w + dw)

	/**` V4.translateXYZW : Number -> Number -> Number -> Number -> Vector4 -> Vector4 `*/
	export const translateXYZW = (dx : number) => (dy : number) => (dz : number) => (dw : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x + dx, v.y + dy, v.z + dz, v.w + dw)

	/**` V4.untranslateX : Number -> Vector4 -> Vector4 `*/
	export const untranslateX = (dx : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x - dx, v.y, v.z, v.w)

	/**` V4.untranslateY : Number -> Vector4 -> Vector4 `*/
	export const untranslateY = (dy : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y - dy, v.z, v.w)

	/**` V4.untranslateZ : Number -> Vector4 -> Vector4 `*/
	export const untranslateZ = (dz : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z - dz, v.w)

	/**` V4.untranslateW : Number -> Vector4 -> Vector4 `*/
	export const untranslateW = (dz : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z, v.w - dz)

	/**` V4.untranslateXYZW : Number -> Number -> Number -> Number -> Vector4 -> Vector4 `*/
	export const untranslateXYZW = (dx : number) => (dy : number) => (dz : number) => (dw : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x - dx, v.y - dy, v.z - dz, v.w - dw)

	/**` V4.scaleX : Number -> Vector4 -> Vector4 `*/
	export const scaleX = (kx : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x * kx, v.y, v.z, v.w)

	/**` V4.scaleY : Number -> Vector4 -> Vector4 `*/
	export const scaleY = (ky : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y * ky, v.z, v.w)

	/**` V4.scaleZ : Number -> Vector4 -> Vector4 `*/
	export const scaleZ = (kz : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z * kz, v.w)

	/**` V4.scaleW : Number -> Vector4 -> Vector4 `*/
	export const scaleW = (kw : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z, v.w * kw)

	/**` V4.scaleXYZW : Number -> Number -> Number -> Number -> Vector4 -> Vector4 `*/
	export const scaleXYZW = (kx : number) => (ky : number) => (kz : number) => (kw : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x * kx, v.y * ky, v.z * kz, v.w * kw)

	/**` V4.unscaleX : Number -> Vector4 -> Vector4 `*/
	export const unscaleX = (kx : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x / kx, v.y, v.z, v.w)

	/**` V4.unscaleY : Number -> Vector4 -> Vector4 `*/
	export const unscaleY = (ky : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y / ky, v.z, v.w)

	/**` V4.unscaleZ : Number -> Vector4 -> Vector4 `*/
	export const unscaleZ = (kz : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z / kz, v.w)

	/**` V4.unscaleW : Number -> Vector4 -> Vector4 `*/
	export const unscaleW = (kw : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x, v.y, v.z, v.w / kw)

	/**` V4.unscaleXYZW : Number -> Number -> Number -> Number -> Vector4 -> Vector4 `*/
	export const unscaleXYZW = (kx : number) => (ky : number) => (kz : number) => (kw : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x / kx, v.y / ky, v.z / kz, v.w / kw)

	/**` V4.scale : Number -> Vector4 -> Vector4 `*/
	export const scale = (k : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x * k, v.y * k, v.z * k, v.w * k)

	/**` V4.unscale : Number -> Vector4 -> Vector4 `*/
	export const unscale = (k : number) => (v : Vector4) : Vector4 =>
		Vector4 (v.x / k, v.y / k, v.z / k, v.w / k)

	/**` V4.translate : Vector4 -> Vector4 -> Vector4 `*/
	export const translate = (v : Vector4) => (w : Vector4) : Vector4 =>
		Vector4 (v.x + w.x, v.y + w.y, v.z + w.z, v.w + w.w)

	/**` V4.untranslate : Vector4 -> Vector4 -> Vector4 `*/
	export const untranslate = (v : Vector4) => (w : Vector4) : Vector4 =>
		Vector4 (w.x - v.x, w.y - v.y, w.z - v.z, w.w - v.w)

	/**` V4.negate : Vector4 -> Vector4 `*/
	export const negate = (v : Vector4) : Vector4 =>
		Vector4 (-v.x, -v.y, -v.z, v.w)

	/**` V4.reciprocate : Vector4 -> Vector4 `*/
	export const reciprocate = (v : Vector4) : Vector4 =>
		Vector4 (1 / v.x, 1 / v.y, 1 / v.z, 1 / v.w)

	/**` V3.norm : Vector4 -> Number `*/
	export const norm = (v : Vector4) : number =>
		Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2 + v.w ** 2)

	/**` V3.normalize : Vector4 -> Vector4 `*/
	export const normalize = (v : Vector4) : Vector4 =>
	{
		const x = Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2 + v.w ** 2)
		return x === 0
			? V4.zero
			: Vector4 (v.x / x, v.y / x, v.z / x, v.w / x)
	}

	/**` V4.dot : Vector4 -> Vector4 -> Number `*/
	export const dot = (v : Vector4) => (w : Vector4) : number =>
		v.x * w.x + v.y * w.y + v.z * w.z + v.w * w.w

	/**` V4.transform : Matrix4 -> Vector4 -> Vector4 `*/
	export const transform = (m : Matrix4) => (v : Vector4) : Vector4 =>
		Vector4 (
			m.ix * v.x + m.jx * v.y + m.kx * v.z + m.lx * v.w,
			m.iy * v.x + m.jy * v.y + m.ky * v.z + m.ly * v.w,
			m.iz * v.x + m.jz * v.y + m.kz * v.z + m.lz * v.w,
			m.iw * v.x + m.jw * v.y + m.kw * v.z + m.lw * v.w
		)

	/**` V4.map : (Number -> Number) -> Vector4 -> Vector4 `*/
	export const map = (endomorphism : (coordinate : number) => number) => (v : Vector4) : Vector4 =>
		Vector4 (endomorphism (v.x), endomorphism (v.y), endomorphism (v.z), endomorphism (v.w))

	/**` V4.each : (Number -> Number -> Number) -> Vector4 -> Vector4 -> Vector4 `*/
	export const each = (f : (first : number) => (second : number) => number) => (v : Vector4) => (w : Vector4) : Vector4 =>
		Vector4 (f (v.x) (w.x), f (v.y) (w.y), f (v.z) (w.z), f (v.w) (w.w))
}

namespace M2
{
	/**` M2.identity : Matrix2 `*/
	export const identity : Matrix2 = Matrix2 (1, 0, 0, 1)

	/**` M2.basis : (Vector2, Vector2) -> Matrix2 `*/
	export const basis = (i : Vector2, j : Vector2) : Matrix2 =>
		Matrix2 (i.x, j.x, i.y, j.y)

	/**` M2.compose : Matrix2 -> Matrix2 -> Matrix2 `*/
	export const compose = (m : Matrix2) => (n : Matrix2) : Matrix2 =>
		Matrix2 (
			m.ix * n.ix + m.jx * n.iy,
			m.ix * n.jx + m.jx * n.jy,
			m.iy * n.ix + m.jy * n.iy,
			m.iy * n.jx + m.jy * n.jy
		)
}

namespace M3
{
	/**` M3.identity : Matrix3 `*/
	export const identity : Matrix3 = Matrix3 (1, 0, 0, 0, 1, 0, 0, 0, 1)

	/**` M3.basis : (Vector3, Vector3, Vector3) -> Matrix3 `*/
	export const basis = (i : Vector3, j : Vector3, k : Vector3) : Matrix3 =>
		Matrix3 (i.x, j.x, k.x, i.y, j.y, k.y, i.z, j.z, k.z)

	/**` M3.compose : Matrix3 -> Matrix3 -> Matrix3 `*/
	export const compose = (m : Matrix3) => (n : Matrix3) : Matrix3 =>
		Matrix3 (
			m.ix * n.ix + m.jx * n.iy + m.kx * n.iz,
			m.ix * n.jx + m.jx * n.jy + m.kx * n.jz,
			m.ix * n.kx + m.jx * n.ky + m.kx * n.kz,
			m.iy * n.ix + m.jy * n.iy + m.ky * n.iz,
			m.iy * n.jx + m.jy * n.jy + m.ky * n.jz,
			m.iy * n.kx + m.jy * n.ky + m.ky * n.kz,
			m.iz * n.ix + m.jz * n.iy + m.kz * n.iz,
			m.iz * n.jx + m.jz * n.jy + m.kz * n.jz,
			m.iz * n.kx + m.jz * n.ky + m.kz * n.kz
		)
}

namespace M4
{
	/**` M4.identity : Matrix4 `*/
	export const identity = Matrix4 (1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)

	/**` M4.basis : (Vector4, Vector4, Vector4, Vector4) -> Matrix4 `*/
	export const basis = (i : Vector4, j : Vector4, k : Vector4, l : Vector4) : Matrix4 =>
		Matrix4 (i.x, j.x, k.x, l.x, i.y, j.y, k.y, l.y, i.z, j.z, k.z, l.z, i.w, j.w, k.w, l.w)

	/**` M4.compose : Matrix4 -> Matrix4 -> Matrix4 `*/
	export const compose = (m : Matrix4) => (n : Matrix4) : Matrix4 =>
		Matrix4 (
			m.ix * n.ix + m.jx * n.iy + m.kx * n.iz + m.lx * n.iw,
			m.ix * n.jx + m.jx * n.jy + m.kx * n.jz + m.lx * n.jw,
			m.ix * n.kx + m.jx * n.ky + m.kx * n.kz + m.lx * n.kw,
			m.ix * n.lx + m.jx * n.ly + m.kx * n.lz + m.lx * n.lw,
			m.iy * n.ix + m.jy * n.iy + m.ky * n.iz + m.ly * n.iw,
			m.iy * n.jx + m.jy * n.jy + m.ky * n.jz + m.ly * n.jw,
			m.iy * n.kx + m.jy * n.ky + m.ky * n.kz + m.ly * n.kw,
			m.iy * n.lx + m.jy * n.ly + m.ky * n.lz + m.ly * n.lw,
			m.iz * n.ix + m.jz * n.iy + m.kz * n.iz + m.lz * n.iw,
			m.iz * n.jx + m.jz * n.jy + m.kz * n.jz + m.lz * n.jw,
			m.iz * n.kx + m.jz * n.ky + m.kz * n.kz + m.lz * n.kw,
			m.iz * n.lx + m.jz * n.ly + m.kz * n.lz + m.lz * n.lw,
			m.iw * n.ix + m.jw * n.iy + m.kw * n.iz + m.lw * n.iw,
			m.iw * n.jx + m.jw * n.jy + m.kw * n.jz + m.lw * n.jw,
			m.iw * n.kx + m.jw * n.ky + m.kw * n.kz + m.lw * n.kw,
			m.iw * n.lx + m.jw * n.ly + m.kw * n.lz + m.lw * n.lw
		)
}

/********************************************************************************************************************************/
// Constants and Micro-Functions for Enums //

/**` isDown : Vertical -> Boolean `*/
const isDown = (vertical : Vertical) : boolean =>
	vertical === Vertical.Downward || vertical === Vertical.Down

/**` isUp : Vertical -> Boolean `*/
const isUp = (vertical : Vertical) : boolean =>
	vertical === Vertical.Upward || vertical === Vertical.Up

/**` relaxVertical : Vertical -> Vertical `*/
const relaxVertical = (vertical : Vertical) : Vertical =>
	vertical === Vertical.Downward ? Vertical.Down :
	vertical === Vertical.Upward   ? Vertical.Up  :
	vertical

/**` signVertical : Vertical -> Number `*/
const signVertical = (vertical : Vertical) : number =>
	vertical === Vertical.Downward || vertical === Vertical.Down ? -1 :
	vertical === Vertical.Upward   || vertical === Vertical.Up   ?  1 :
	0

/********************************************************************************************************************************/
// Constants and Micro-Functions for Multiple Algebraic Data Types //

/**` sequenceIOs : List (IO a) -> IO (List a) `*/
const sequenceIOs = <a>(ios : List <IO <a>>) : IO <List <a>> =>
	IO (() => ios .fmap (io => io.effect ()))

/**` executeIOs : List (IO a) -> IO () `*/
const executeIOs = <a>(ios : List <IO <a>>) : IO <null> =>
	IO (() => {
		for (let i = 0, xs = ios; xs.variation === 'Cons'; ++i)
			if (i === MAXI)
				error (`'executeIOs' traversed too many elements (${MAXI}); ${STAP}`)
			else
				xs.head.effect (),
				xs = xs.tail
		return null
	})

/**` maybeHead : List a -> Maybe a `*/
const maybeHead = <a>(xs : List <a>) : Maybe <a> =>
	xs.variation === 'Nil'
		? Nothing
		: Just (xs .head)

/**` maybeLast : List a -> Maybe a `*/
const maybeLast = <a>(xs : List <a>) : Maybe <a> =>
	xs.variation === 'Nil'
		? Nothing
		: Just (last (xs))

/**` maybeTail : List a -> Maybe (List a) `*/
const maybeTail = <a>(xs : List <a>) : Maybe <List <a>> =>
	xs.variation === 'Nil'
		? Nothing
		: Just (xs .tail)

/**` maybeInit : List a -> Maybe (List a) `*/
const maybeInit = <a>(xs : List <a>) : Maybe <List <a>> =>
	xs.variation === 'Nil'
		? Nothing
		: Just (init (xs))

/**` maybeSingleton : Maybe a -> List a `*/
const maybeSingleton = <a>(maybe : Maybe <a>) : List <a> =>
	maybe.variation === 'Nothing'
		? Nil
		: singleton (maybe.value)

/**` pairToVector : Pair Number Number -> Vector2 `*/
const pairToVector = (pair : Pair <number, number>) : Vector2 =>
	Vector2 (pair.fst, pair.snd)

/**` vectorToPair : Vector2 -> Pair Number Number `*/
const vectorToPair = (v : Vector2) : Pair <number, number> =>
	Pair (v.x, v.y)

/**` pairOfMaybes : Pair (Maybe a) (Maybe b) -> Maybe (Pair a b) `*/
const pairOfMaybes = <a, b>(pair : Pair <Maybe <a>, Maybe <b>>) : Maybe <Pair <a, b>> =>
	pair.fst.variation === 'Just' && pair.snd.variation === 'Just'
		? Just (Pair (pair.fst.value, pair.snd.value))
		: Nothing

/**` find : (a -> Boolean) -> List a -> Maybe a `*/
const find = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Maybe <a> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'find' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (xs.head))
			return Just (xs.head)
		else
			xs = xs.tail
	return Nothing
}

/**` elemIndex : (Eq a) => a -> List a -> Maybe Number `*/
const elemIndex = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : Maybe <number> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'elemIndex' traversed too many elements (${MAXI}); ${STAP}`)
		else if (xs.head .eq (value))
			return Just (i)
		else
			xs = xs.tail
	return Nothing
}

/**` findIndex : (a -> Boolean) -> List a -> Maybe Number `*/
const findIndex = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Maybe <number> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'findIndex' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (xs.head))
			return Just (i)
		else
			xs = xs.tail
	return Nothing
}

/**` flattenMaybes : List (Maybe a) -> List a `*/
const flattenMaybes = <a>(maybes : List <Maybe <a>>) : List <a> =>
{
	for (let i = 0; maybes.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'flattenMaybes' traversed too many elements (${MAXI}); ${STAP}`)
		else if (maybes.head.variation === 'Just')
			return lrprepend (maybes.head.value) (() => flattenMaybes ((maybes as any).tail))
		else
			maybes = maybes.tail
	return Nil
}

/**` mapMaybe : (a -> Maybe b) -> List a -> List b `*/
const mapMaybe = <a, b>(mapping : (element : a) => Maybe <b>) => (xs : List <a>) : List <b> =>
{
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'mapMaybe' traversed too many elements (${MAXI}); ${STAP}`)
		else
		{
			const x = mapping (xs.head)
			if (x.variation === 'Just')
				return lrprepend (x.value) (() => mapMaybe (mapping) ((xs as any).tail))
			xs = xs.tail
		}
	return Nil
}

/**` lefts : List (Either a b) -> List a `*/
const lefts = <a, b>(eithers : List <Either <a, b>>) : List <a> =>
{
	for (let i = 0; eithers.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'lefts' traversed too many elements (${MAXI}); ${STAP}`)
		else if (eithers.head.variation === 'Left')
			return lrprepend (eithers.head.value) (() => lefts ((eithers as any).tail))
		else
			eithers = eithers.tail
	return Nil
}

/**` rights : List (Either a b) -> List b `*/
const rights = <a, b>(eithers : List <Either <a, b>>) : List <b> =>
{
	for (let i = 0; eithers.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'rights' traversed too many elements (${MAXI}); ${STAP}`)
		else if (eithers.head.variation === 'Right')
			return lrprepend (eithers.head.value) (() => rights ((eithers as any).tail))
		else
			eithers = eithers.tail
	return Nil
}

/**` maybeAt : Number -> List a -> Maybe a `*/
const maybeAt = (index : number) => <a>(xs : List <a>) : Maybe <a> =>
{
	if (index > MAXI)
		error (`'maybeAt' would be traversing too many elements (${MAXI}); ${STAP}`)
	if (index < 0 || !Number.isInteger (index))
		return Nothing
	for (let i = 0; xs.variation === 'Cons'; ++i)
		if (i === index)
			return Just (xs.head)
		else
			xs = xs .tail
	return Nothing
}

/**` processToList : Process s a -> s -> List a `*/
const processToList = <s, a>(process : Process <s, a>) => (state : s) : List <a> =>
{
	const p = process.computation (state)
	return lrprepend (p.snd) (() => processToList (process) (p.fst))
}

/**` pairProcess : Process s a -> Process s (Pair a a) `*/
const pairProcess = <s, a>(process : Process <s, a>) : Process <s, Pair <a, a>> =>
	Process (s => {
		const p = process.computation (s)
		const q = process.computation (p.fst)
		return Pair (q.fst, Pair (p.snd, q.snd))
	})

/**` repeatProcess : Process s a -> Process s (List a) `*/
const repeatProcess = <s, a>(process : Process <s, a>) : Process <s, List <a>> =>
	Process (s => Pair (s, processToList (process) (s)))

/**` pickOut : (a -> Boolean) -> List a -> Pair (Maybe a) (List a) `*/
const pickOut = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <Maybe <a>, List <a>> =>
{
	let ys            = xs
	let zs : List <a> = Nil
	for (let i = 0; ys.variation === 'Cons'; ++i)
		if (i === MAXI)
			error (`'pickOut' traversed too many elements (${MAXI}); ${STAP}`)
		else if (predicate (ys.head))
		{
			let ws : List <a> = Nil
			while (zs.variation === 'Cons')
				ws = prepend (zs.head) (ws),
				zs = zs.tail

			return Pair (Just (ys.head), ws .link (ys.tail))
		}
		else
			zs = prepend (ys.head) (zs),
			ys = ys.tail

	return Pair (Nothing, xs)
}

/********************************************************************************************************************************/
// Psuedo Random Generators //

/**` random : Process Number Number `*/
const random : Process <number, number> =
	Process (s =>
		Pair (
			Math.abs (161 * s ** 3 - 91 * s ** 2 + 177 * s - 901) % 0xffffff,
			Math.abs (s ** 3 % 23 + 248 * s ** 2 % 34 + 112 * s - 528) % 2048 / 2048
		)
	)

/**` randomFloatRange : Number -> Number -> Process Number Number `*/
const randomFloatRange = (lower : number) => (upper : number) : Process <number, number> =>
	Process (s =>
		Pair (
			Math.abs (698 * s ** 3 - 471 * s ** 2 + 295 * s - 77) % 0xffffff,
			Math.abs (s ** 3 % 196 + 989 * s ** 2 % 786 + 534 * s - 571) % 2048 / 2048 * (upper - lower) + lower
		)
	)

/**` randomIntRange : Number -> Number -> Process Number Number `*/
const randomIntRange = (lower : number) => (upper : number) : Process <number, number> =>
	Process (s =>
		Pair (
			Math.abs (852 * s ** 3 - 274 * s ** 2 + 345 * s - 558) % 0xffffff,
			~~ (abs (71 * s ** 3 % 71 + 570 * s ** 2 % 39 + 509 * s - 72) % 2048 / 2048 * (upper - lower)) + lower
		)
	)

/**` randomV2 : Process Number Vector2 `*/
const randomV2 : Process <number, Vector2> =
	Process (s =>
		Pair (
			Math.abs (84 * s ** 3 + 729 * s ** 2 + 215 * s + 1015) % 0xffffff,
			Vector2 (
				Math.abs (304 * s ** 3 % 582 + 204 * s ** 2 % 288 + 254 * s - 617) % 2048 / 2048,
				Math.abs (906 * s ** 3 % 717 + 518 * s ** 2 % 38 + 112 * s - 581) % 2048 / 2048
			)
		)
	)

/**` randomV3 : Process Number Vector3 `*/
const randomV3 : Process <number, Vector3> =
	Process (s =>
		Pair (
			Math.abs (390 * s ** 3 - 329 * s ** 2 + 22 * s + 41) % 0xffffff,
			Vector3 (
				Math.abs(407 * s ** 3 % 594 + 70 * s ** 2 % 61 + s - 5283) % 2048 / 2048,
				Math.abs(109 * s ** 3 % 200 + 23 * s ** 2 % 8 + 69940 * s - 558) % 2048 / 2048,
				Math.abs(273 * s ** 3 % 286 + 23 * s ** 2 % 60 + 36 * s - 184) % 2048 / 2048
			)
		)
	)

/**` randomV4 : Process Number Vector4 `*/
const randomV4 : Process <number, Vector4> =
	Process (s =>
		Pair (
			abs (350 * s ** 3 - 7527 * s ** 2 + 639 * s - 1011) % 0xffffff,
			Vector4 (
				Math.abs (881 * s ** 3 % 461 + 213 * s ** 2 % 16 + s - 519) % 2048 / 2048,
				Math.abs (75 * s ** 3 % 516 + 75459 * s ** 2 % 19 + 67 * s - 693) % 2048 / 2048,
				Math.abs (3498 * s ** 3 % 242 + 590 * s ** 2 % 27 + 50 * s - 3039) % 2048 / 2048,
				Math.abs (8641 * s ** 3 % 256 + 613 * s ** 2 % 28 + 12 * s - 62) % 2048 / 2048
			)
		)
	)

/**` randomDirectionV2 : Process Number Vector2 `*/
const randomDirectionV2 : Process <number, Vector2> =
	Process (s => {
		const angle = Math.abs (2474 * s ** 3 % 2676 + 369 * s ** 2 % 3871 + 267628 * s % 6048 + 744) % tau
		return Pair (
			Math.abs (372 * s ** 3 - 566 * s ** 2 + 21713 * s + 36769) % 0xffffff,
			Vector2 (Math.cos(angle), Math.sin(angle))
		)
	})

/**` randomDirectionV3 : Process Number Vector3 `*/
const randomDirectionV3 : Process <number, Vector3> =
	Process (s => {
		const angle0 = Math.abs (s ** 3 % 198 + 378 * s ** 2 % 86 + s - 16) % tau
		const angle1 = Math.abs (116 * s ** 3 - 3168 * s ** 2 + 258 * s - 901) % tau
		const c      = Math.sin(angle0)
		return Pair (
			Math.abs (414 * s ** 3 - 607 * s ** 2 + 889 * s - 888) % 0xffffff,
			Vector3 (Math.cos(angle0), c * Math.cos(angle1), c * Math.sin(angle1))
		)
	})

/**` randomDirectionV4 : Process Number Vector4 `*/
const randomDirectionV4 : Process <number, Vector4> =
	Process (s => {
		const angle0 = Math.abs (905 * s ** 3 % 2312 + 633 * s ** 2 % 94975 + 208 * s - 250) % tau
		const angle1 = Math.abs (189 * s ** 3 % 2641 - 466 * s ** 2 % 44291 + 224 * s - 917) % tau
		const angle2 = Math.abs (417 * s ** 3 % 2354 - 262 * s ** 2 % 29516 + 41 * s - 529) % tau
		const c0     = Math.sin(angle0)
		const c1     = c0 * Math.sin(angle1)
		return Pair (
			Math.abs (161 * s ** 3 - 91 * s ** 2 + 177 * s - 901) % 0xffffff,
			Vector4 (Math.cos(angle0), c0 * Math.cos(angle1), c1 * Math.cos(angle2), c1 * Math.sin(angle2))
		)
	})


/**` randomLowercase : Process Number String `*/
const randomLowercase : Process <number, string> =
	Process (s =>
		Pair (
			Math.abs (28721 * s ** 3 % 2999 - 712 * s ** 2 + 3778 * s - 558) % 0xffffff,
			String.fromCharCode(Math.abs(3252 * s ** 3 % 3598 + 945 * s ** 2 % 878 + 503 * s % 379 + 826) % 1028 / 1028 * 26 + 97)
		)
	)

/**` randomUppercase : Process Number String `*/
const randomUppercase : Process <number, string> =
	Process (s =>
		Pair (
			Math.abs (363 * s ** 3 % 384 - 31 * s ** 2 + 67 * s - 793) % 0xffffff,
			String.fromCharCode(Math.abs(3252 * s ** 3 % 3598 + 778 * s ** 2 % 878 + 13 * s % 1701 + 871) % 1028 / 1028 * 26 + 65)
		)
	)

/**` randomElem : List a -> Process Number a `*/
const randomElem = <a>(xs : List <a>) : Process <number, a> =>
{
	if (xs.variation === 'Nil')
		error (`'randomElem' cannot pseudo-randomly pick an element out of Nil`)
	let length = xs.$LEN!
	let ys : List <a> = xs
	if (length === undefined)
	{
		while (ys.variation === 'Cons')
			if (length === MAXI)
				error (`'randomElem' traversed too many elements (${MAXI}); ${STAP}`)
			else
				++length, ys = ys.tail
		xs.$LEN = length
	}
	return Process (s => {
		let i = ~~(Math.abs(71 * s ** 3 % 71 + 570 * s ** 2 % 39 + 177 * s - 59) % 1028 / 1028 * length)
		ys = xs
		while (i)
			--i, ys = (ys as any).tail
		return Pair (
			Math.abs (146 * s ** 3 - 2481 * s ** 2 + 804 * s - 4099) % 0xffffff,
			(ys as any).head
		)
	})
}

/**` randomChar : String -> Process Number String `*/
const randomChar = (str : string) : Process <number, string> =>
	str === ''
		? error (`'randomChar' cannot pseudo-randomly pick a character out of an empty string`)
		:
			Process (s =>
				Pair (
					Math.abs (554 * s ** 3 + 88 * s ** 2 + 1048 * s + 48) % 0xffffff,
					str [~~((2927 * s ** 3 % 2291 - 920 * s ** 2 % 36 + 194 * s % 33 + 556) ** 2 % 1024 / 1024 * str.length)]
				)
			)

/**` randomChance : Number -> Process Number Boolean `*/
const randomChance = (probability : number) : Process <number, boolean> =>
	Process (s =>
		Pair (
			Math.abs(462 * s ** 3 + 261 * s ** 2 - 778 * s - 1510) % 0xffffff,
			Math.abs(1310 * s ** 3 % 9228 - 2461 * s ** 2 % 568 + 8562 * s % 234 + 2827) % 2048 / 2048 < probability
		)
	)

/**` strictRandomShuffle : List a -> Process Number (List a) `*/
const strictRandomShuffle = <a>(xs : List <a>) : Process <number, List <a>> =>
	Process (s => {
		const ys : Array <a> = []
		for (let i = 0; xs.variation === 'Cons'; ++i)
			if (i === MAXI)
				return error (`'strictRandomShuffle' traversed too many elements (${MAXI}); ${STAP}`)
			else
				ys.push (xs.head),
				xs = xs.tail
		let zs : List <a> = Nil
		while (ys.length)
		{
			s = Math.abs(103282 * s ** 3 % 340858 - 6319 * s ** 2 % 152 + 8596 * s % 854 + 52732) % 2048 / 2048
			zs = prepend (ys.splice(Math.floor(s *= ys.length), 1)[0]) (zs)
		}
		return Pair (Math.abs(3119 * s ** 3 % 654340 - 699585 * s ** 2 % 60418 + 85319 * s % 8152 + 52732) % 0xffffff, zs)
	})

/********************************************************************************************************************************/
// Do Notation //

const Do =
	{
		IO             :           IO        <         {}> (() => Object.create(null)),
		Process        : <s> () => Process   <s      , {}> (s => Pair (s, Object.create(null))),
		Process_Random :           Process   <number , {}> (s => Pair (s, Object.create(null))),
		List           :           singleton <         {}> (Object.create(null)),
		Maybe          :           Just      <         {}> (Object.create(null))
	}

/********************************************************************************************************************************/
// IO Interfacing //

const Ψ =
	{
		MUTABLE         : {} as any,
		ctxs            : [] as Array <CanvasRenderingContext2D>,
		ctx             : undefined as unknown as CanvasRenderingContext2D,
		resizeID        : undefined as unknown as number,
		isResized       : false,
		isPointerLocked : false,
		seed            : (Math.random() - 0.5) * Date.now() % 2048,
		debugCounter    : 0,
		image           : Object.create(null) as { [x : string] : HTMLImageElement },
		audio           : Object.create(null) as { [x : string] : HTMLAudioElement },
		mouseSX         : 0, mouseSY : 0,
		mouseWX         : 0, mouseWY : 0,
		mouseCX         : 0, mouseCY : 0,
		mouseDX         : 0, mouseDY : 0,
		mouseScroll     : Vertical.Rest,
		mouseButtons    : Array(5).fill(Vertical.Up) as [Vertical, Vertical, Vertical, Vertical, Vertical],
		keyboard        :
			keyboardKeysArray
				.reduce(($, k) => ({ ...$, [k] : Vertical.Up }), Object.create (null)) as { [x in KeyboardKey] : Vertical }
	}

const __MACRO_nonexisting_image_path__ = (org : string, path : string) : any =>
	error (`'${org}' received an unloaded (possibly non-existing) image at path: '${path}'`)

const __MACRO_nonexisting_audio_path__ = (org : string, path : string) : any =>
	error (`'${org}' received an unloaded (possibly non-existing) audio at path: '${path}'`)

const __MACRO_invalid_index_range      = (org : string, i : number, upper : number) : any =>
	error (`'${org}' received index of '${i}'; must be a integer in interval [0, ${upper})`)

const __MACRO_clickx__   = (i : number) : IO <Maybe <number>> =>
	IO (() => Ψ.mouseButtons[i] === Vertical.Downward ? Just (Ψ.mouseCX) : Nothing)

const __MACRO_clicky__   = (i : number) : IO <Maybe <number>> =>
	IO (() => Ψ.mouseButtons[i] === Vertical.Downward ? Just (Ψ.mouseCY) : Nothing)

const __MACRO_clickv__   = (i : number) : IO <Maybe <Vector2>> =>
	IO (() => Ψ.mouseButtons[i] === Vertical.Downward ? Just (Vector2 (Ψ.mouseCX, Ψ.mouseCY)) : Nothing)

const __MACRO_n_clickx__ = (i : number) : IO <Maybe <number>> =>
	IO (() => Ψ.mouseButtons[i] === Vertical.Downward ? Just (Ψ.mouseCX / Ψ.ctx.canvas.width) : Nothing)

const __MACRO_n_clicky__ = (i : number) : IO <Maybe <number>> =>
	IO (() => Ψ.mouseButtons[i] === Vertical.Downward ? Just (Ψ.mouseCY / Ψ.ctx.canvas.height) : Nothing)

const __MACRO_n_clickv__ = (i : number) : IO <Maybe <Vector2>> =>
	IO (() =>
		Ψ.mouseButtons[i] === Vertical.Downward
			? Just (Vector2 (Ψ.mouseCX / Ψ.ctx.canvas.width, Ψ.mouseCY / Ψ.ctx.canvas.height))
			: Nothing
	)

/**` n_mouseScreenPositionX : IO Number `*/
const n_mouseScreenPositionX : IO <number> =
	IO (() => Ψ.mouseSX / screen.width)

/**` n_mouseScreenPositionY : IO Number `*/
const n_mouseScreenPositionY : IO <number> =
	IO (() => Ψ.mouseSY / screen.height)

/**` n_mouseScreenPosition : IO Vector2 `*/
const n_mouseScreenPosition : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseSX / screen.width, Ψ.mouseSY / screen.height))

/**` mouseScreenPositionX : IO Number `*/
const mouseScreenPositionX : IO <number> =
	IO (() => Ψ.mouseSX)

/**` mouseScreenPositionY : IO Number `*/
const mouseScreenPositionY : IO <number> =
	IO (() => Ψ.mouseSY)

/**` mouseScreenPosition : IO Vector2 `*/
const mouseScreenPosition : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseSX, Ψ.mouseSY))

/**` n_mouseWindowPositionX : IO Number `*/
const n_mouseWindowPositionX : IO <number> =
	IO (() => Ψ.mouseWX / innerWidth)

/**` n_mouseWindowPositionY : IO Number `*/
const n_mouseWindowPositionY : IO <number> =
	IO (() => Ψ.mouseWY / innerHeight)

/**` n_mouseWindowPosition : IO Vector2 `*/
const n_mouseWindowPosition : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseWX / innerWidth, Ψ.mouseWY / innerHeight))

/**` mouseWindowPositionX : IO Number `*/
const mouseWindowPositionX : IO <number> =
	IO (() => Ψ.mouseWX)

/**` mouseWindowPositionY : IO Number `*/
const mouseWindowPositionY : IO <number> =
	IO (() => Ψ.mouseWY)

/**` mouseWindowPosition : IO Vector2 `*/
const mouseWindowPosition : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseWX, Ψ.mouseWY))

/**` n_mouseCanvasPositionX : IO Number `*/
const n_mouseCanvasPositionX : IO <number> =
	IO (() => Ψ.mouseCX / Ψ.ctx.canvas.width)

/**` n_mouseCanvasPositionY : IO Number `*/
const n_mouseCanvasPositionY : IO <number> =
	IO (() => Ψ.mouseCY / Ψ.ctx.canvas.height)

/**` n_mouseCanvasPosition : IO Vector2 `*/
const n_mouseCanvasPosition : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseCX / Ψ.ctx.canvas.width, Ψ.mouseCY / Ψ.ctx.canvas.height))

/**` mouseCanvasPositionX : IO Number `*/
const mouseCanvasPositionX : IO <number> =
	IO (() => Ψ.mouseCX)

/**` mouseCanvasPositionY : IO Number `*/
const mouseCanvasPositionY : IO <number> =
	IO (() => Ψ.mouseCY)

/**` mouseCanvasPosition : IO Vector2 `*/
const mouseCanvasPosition : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseCX, Ψ.mouseCY))

/**` n_mouseDeltaX : IO Number `*/
const n_mouseDeltaX : IO <number> =
	IO (() => Ψ.mouseDX / Ψ.ctx.canvas.width)

/**` n_mouseDeltaY : IO Number `*/
const n_mouseDeltaY : IO <number> =
	IO (() => Ψ.mouseDY / Ψ.ctx.canvas.height)

/**` n_mouseDelta : IO Vector2 `*/
const n_mouseDelta : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseDX / Ψ.ctx.canvas.width, Ψ.mouseDY / Ψ.ctx.canvas.height))

/**` mouseDeltaX : IO Number `*/
const mouseDeltaX : IO <number> =
	IO (() => Ψ.mouseDX)

/**` mouseDeltaY : IO Number `*/
const mouseDeltaY : IO <number> =
	IO (() => Ψ.mouseDY)

/**` mouseDelta : IO Vector2 `*/
const mouseDelta : IO <Vector2> =
	IO (() => Vector2 (Ψ.mouseDX, Ψ.mouseDY))

/**` mouseScroll : IO Vertical `*/
const mouseScroll : IO <Vertical> =
	IO (() => Ψ.mouseScroll)

/**` mouseLeft : IO Vertical `*/
const mouseLeft : IO <Vertical> =
	IO (() => Ψ.mouseButtons[0])

/**` mouseMiddle : IO Vertical `*/
const mouseMiddle : IO <Vertical> =
	IO (() => Ψ.mouseButtons[1])

/**` mouseRight : IO Vertical `*/
const mouseRight : IO <Vertical> =
	IO (() => Ψ.mouseButtons[2])

/**` mouse4th : IO Vertical `*/
const mouse4th : IO <Vertical> =
	IO (() => Ψ.mouseButtons[3])

/**` mouse5th : IO Vertical `*/
const mouse5th : IO <Vertical> =
	IO (() => Ψ.mouseButtons[4])

/**` n_mouseLeftClickCoordinateX : IO (Maybe Number) `*/
const n_mouseLeftClickCoordinateX : IO <Maybe <number>> =
	__MACRO_n_clickx__(0)

/**` n_mouseLeftClickCoordinateY : IO (Maybe Number) `*/
const n_mouseLeftClickCoordinateY : IO <Maybe <number>> =
	__MACRO_n_clicky__(0)

/**` n_mouseLeftClickCoordinates : IO (Maybe Vector2) `*/
const n_mouseLeftClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_n_clickv__(0)

/**` mouseLeftClickCoordinateX : IO (Maybe Number) `*/
const mouseLeftClickCoordinateX : IO <Maybe <number>> =
	__MACRO_clickx__(0)

/**` mouseLeftClickCoordinateY : IO (Maybe Number) `*/
const mouseLeftClickCoordinateY : IO <Maybe <number>> =
	__MACRO_clicky__(0)

/**` mouseLeftClickCoordinates : IO (Maybe Vector2) `*/
const mouseLeftClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_clickv__(0)

/**` n_mouseMiddleClickCoordinateX : IO (Maybe Number) `*/
const n_mouseMiddleClickCoordinateX : IO <Maybe <number>> =
	__MACRO_n_clickx__(1)

/**` n_mouseMiddleClickCoordinateY : IO (Maybe Number) `*/
const n_mouseMiddleClickCoordinateY : IO <Maybe <number>> =
	__MACRO_n_clicky__(1)

/**` n_mouseMiddleClickCoordinates : IO (Maybe Vector2) `*/
const n_mouseMiddleClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_n_clickv__(1)

/**` mouseMiddleClickCoordinateX : IO (Maybe Number) `*/
const mouseMiddleClickCoordinateX : IO <Maybe <number>> =
	__MACRO_clickx__(1)

/**` mouseMiddleClickCoordinateY : IO (Maybe Number) `*/
const mouseMiddleClickCoordinateY : IO <Maybe <number>> =
	__MACRO_clicky__(1)

/**` mouseMiddleClickCoordinates : IO (Maybe Vector2) `*/
const mouseMiddleClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_clickv__(1)

		/****************************************************************/

/**` n_mouseRightClickCoordinateX : IO (Maybe Number) `*/
const n_mouseRightClickCoordinateX : IO <Maybe <number>> =
	__MACRO_n_clickx__(2)

/**` n_mouseRightClickCoordinateY : IO (Maybe Number) `*/
const n_mouseRightClickCoordinateY : IO <Maybe <number>> =
	__MACRO_n_clicky__(2)

/**` n_mouseRightClickCoordinates : IO (Maybe Vector2) `*/
const n_mouseRightClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_n_clickv__(2)

/**` mouseRighClickCoordinateX : IO (Maybe Number) `*/
const mouseRighClickCoordinateX : IO <Maybe <number>> =
	__MACRO_clickx__(2)

/**` mouseRighClickCoordinateY : IO (Maybe Number) `*/
const mouseRighClickCoordinateY : IO <Maybe <number>> =
	__MACRO_clicky__(2)

/**` mouseRighClickCoordinates : IO (Maybe Vector2) `*/
const mouseRighClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_clickv__(2)

/**` n_mouse4thClickCoordinateX : IO (Maybe Number) `*/
const n_mouse4thClickCoordinateX : IO <Maybe <number>> =
	__MACRO_n_clickx__(3)

/**` n_mouse4thClickCoordinateY : IO (Maybe Number) `*/
const n_mouse4thClickCoordinateY : IO <Maybe <number>> =
	__MACRO_n_clicky__(3)

/**` n_mouse4thClickCoordinates : IO (Maybe Vector2) `*/
const n_mouse4thClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_n_clickv__(3)

/**` mouse4thClickCoordinateX : IO (Maybe Number) `*/
const mouse4thClickCoordinateX : IO <Maybe <number>> =
	__MACRO_clickx__(3)

/**` mouse4thClickCoordinateY : IO (Maybe Number) `*/
const mouse4thClickCoordinateY : IO <Maybe <number>> =
	__MACRO_clicky__(3)

/**` mouse4thClickCoordinate : IO (Maybe Vector2) `*/
const mouse4thClickCoordinate : IO <Maybe <Vector2>> =
	__MACRO_clickv__(3)

		/****************************************************************/

/**` n_mouse5thClickCoordinateX : IO (Maybe Number) `*/
const n_mouse5thClickCoordinateX : IO <Maybe <number>> =
	__MACRO_n_clickx__(4)

/**` n_mouse5thClickCoordinateY : IO (Maybe Number) `*/
const n_mouse5thClickCoordinateY : IO <Maybe <number>> =
	__MACRO_n_clicky__(4)

/**` n_mouse5thClickCoordinates : IO (Maybe Vector2) `*/
const n_mouse5thClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_n_clickv__(4)

/**` mouse5thClickCoordinateX : IO (Maybe Number) `*/
const mouse5thClickCoordinateX : IO <Maybe <number>> =
	__MACRO_clickx__(4)

/**` mouse5thClickCoordinateY : IO (Maybe Number) `*/
const mouse5thClickCoordinateY : IO <Maybe <number>> =
	__MACRO_clicky__(4)

/**` mouse5thClickCoordinates : IO (Maybe Vector2) `*/
const mouse5thClickCoordinates : IO <Maybe <Vector2>> =
	__MACRO_clickv__(4)

/**` n_wasdKeys : IO Vector2 `*/
const n_wasdKeys : IO <Vector2> =
	IO (() => {
		const x = isDown (Ψ.keyboard.KeyD) as unknown as number - (isDown (Ψ.keyboard.KeyA) as unknown as number)
		const y = isDown (Ψ.keyboard.KeyS) as unknown as number - (isDown (Ψ.keyboard.KeyW) as unknown as number)
		const l = x ** 2 + y ** 2
		return l === 0
			? V2.zero
			: l === 1
				? Vector2 (x, y)
				: Vector2 (x * invSqrt2, y * invSqrt2)
	})

/**` wasdKeys : IO Vector2 `*/
const wasdKeys : IO <Vector2> =
	IO (() =>
		Vector2 (
			isDown (Ψ.keyboard.KeyD) as unknown as number - (isDown (Ψ.keyboard.KeyA) as unknown as number),
			isDown (Ψ.keyboard.KeyS) as unknown as number - (isDown (Ψ.keyboard.KeyW) as unknown as number)
		)
	)

/**` n_arrowKeys : IO Vector2 `*/
const n_arrowKeys : IO <Vector2> =
	IO (() => {
		const x = isDown (Ψ.keyboard.ArrowRight) as unknown as number - (isDown (Ψ.keyboard.ArrowLeft) as unknown as number)
		const y = isDown (Ψ.keyboard.ArrowDown)  as unknown as number - (isDown (Ψ.keyboard.ArrowUp)   as unknown as number)
		const l = x ** 2 + y ** 2
		return l === 0
			? V2.zero
			: l === 1
				? Vector2 (x, y)
				: Vector2 (x * invSqrt2, y * invSqrt2)
	})

/**` arrowKeys : IO Vector2 `*/
const arrowKeys : IO <Vector2> =
	IO (() =>
		Vector2 (
			isDown (Ψ.keyboard.ArrowRight) as unknown as number - (isDown (Ψ.keyboard.ArrowLeft) as unknown as number),
			isDown (Ψ.keyboard.ArrowDown)  as unknown as number - (isDown (Ψ.keyboard.ArrowUp)   as unknown as number)
		)
	)

/**` keyboardKey : KeyboardKey -> IO Vertical `*/
const keyboardKey = (keyname : KeyboardKey) =>
	IO (() => Ψ.keyboard[keyname])

/**` screenWidth : IO Number `*/
const screenWidth : IO <number> =
	IO (() => screen.width)

/**` screenHeight : IO Number `*/
const screenHeight : IO <number> =
	IO (() => screen.height)

/**` screenDimensions : IO Vector2 `*/
const screenDimensions : IO <Vector2> =
	IO (() => Vector2 (screen.width, screen.height))

/**` windowWidth : IO Number `*/
const windowWidth : IO <number> =
	IO (() => innerWidth)

/**` windowHeight : IO Number `*/
const windowHeight : IO <number> =
	IO (() => innerHeight)

/**` windowDimensions : IO Vector2 `*/
const windowDimensions : IO <Vector2> =
	IO (() => Vector2 (innerWidth, innerHeight))

/**` canvasWidth : IO Number `*/
const canvasWidth : IO <number> =
	IO (() => Ψ.ctx.canvas.width)

/**` canvasHeight : IO Number `*/
const canvasHeight : IO <number> =
	IO (() => Ψ.ctx.canvas.height)

/**` canvasDimensions : IO Vector2 `*/
const canvasDimensions : IO <Vector2> =
	IO (() => Vector2 (Ψ.ctx.canvas.width, Ψ.ctx.canvas.height))

/**` n_textWidth : String -> IO Number `*/
const n_textWidth = (text : string) : IO <number> =>
	IO (() => {
		const { actualBoundingBoxLeft, actualBoundingBoxRight } = Ψ.ctx.measureText(text)
		return (Math.abs (actualBoundingBoxLeft) + Math.abs (actualBoundingBoxRight)) / Ψ.ctx.canvas.width
	})

/**` textWidth : String -> IO Number `*/
const textWidth = (text : string) : IO <number> =>
	IO (() => {
		const { actualBoundingBoxLeft, actualBoundingBoxRight } = Ψ.ctx.measureText(text)
		return Math.abs (actualBoundingBoxLeft) + Math.abs (actualBoundingBoxRight)
	})

/**` n_textHeight : String -> IO Number `*/
const n_textHeight = (text : string) : IO <number> =>
	IO (() => {
		const { actualBoundingBoxAscent, actualBoundingBoxDescent } = Ψ.ctx.measureText(text)
		return (Math.abs (actualBoundingBoxAscent) + Math.abs (actualBoundingBoxDescent)) / Ψ.ctx.canvas.height
	})

/**` textHeight : String -> IO Number `*/
const textHeight = (text : string) : IO <number> =>
	IO (() => {
		const { actualBoundingBoxAscent, actualBoundingBoxDescent } = Ψ.ctx.measureText(text)
		return Math.abs (actualBoundingBoxAscent) + Math.abs (actualBoundingBoxDescent)
	})

/**` n_textDimensions : String -> IO Vector2 `*/
const n_textDimensions = (text : string) : IO <Vector2> =>
	IO (() => {
		const { actualBoundingBoxLeft, actualBoundingBoxRight, actualBoundingBoxAscent, actualBoundingBoxDescent }
			= Ψ.ctx.measureText(text)
		return Vector2 (
			(Math.abs (actualBoundingBoxLeft)   + Math.abs (actualBoundingBoxRight))   / Ψ.ctx.canvas.width,
			(Math.abs (actualBoundingBoxAscent) + Math.abs (actualBoundingBoxDescent)) / Ψ.ctx.canvas.height
		)
	})

/**` textDimensions : String -> IO Vector2 `*/
const textDimensions = (text : string) : IO <Vector2> =>
	IO (() => {
		const { actualBoundingBoxLeft, actualBoundingBoxRight, actualBoundingBoxAscent, actualBoundingBoxDescent }
			= Ψ.ctx.measureText(text)
		return Vector2 (
			Math.abs (actualBoundingBoxLeft)   + Math.abs (actualBoundingBoxRight),
			Math.abs (actualBoundingBoxAscent) + Math.abs (actualBoundingBoxDescent)
		)
	})

/**` n_imageWidth : String -> IO Number `*/
const n_imageWidth = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].width / Ψ.ctx.canvas.width
			: __MACRO_nonexisting_image_path__('n_imageWidth', path)
	)

/**` imageWidth : String -> IO Number `*/
const imageWidth = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].width
			: __MACRO_nonexisting_image_path__('imageWidth', path)
	)

/**` n_imageHeight : String -> IO Number `*/
const n_imageHeight = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].height / Ψ.ctx.canvas.height
			: __MACRO_nonexisting_image_path__('n_imageHeight', path)
	)

/**` imageHeight : String -> IO Number `*/
const imageHeight = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].height
			: __MACRO_nonexisting_image_path__('imageHeight', path)
	)

/**` n_imageDimensions : String -> IO Vector2 `*/
const n_imageDimensions = (path : string) : IO <Vector2> =>
	IO (() =>
		Ψ.image[path]
			? Vector2 (Ψ.image[path].width / Ψ.ctx.canvas.width, Ψ.image[path].height / Ψ.ctx.canvas.height)
			: __MACRO_nonexisting_image_path__('n_imageDimensions', path)
	)

/**` imageDimensions : String -> IO Vector2 `*/
const imageDimensions = (path : string) : IO <Vector2> =>
	IO (() =>
		Ψ.image[path]
			? Vector2 (Ψ.image[path].width, Ψ.image[path].height)
			: __MACRO_nonexisting_image_path__('imageDimensions', path)
	)

/**` n_audioTime : String -> IO Number `*/
const n_audioTime = (path : string) : IO <number> =>
	IO (() =>
		Ψ.audio[path]
			? Ψ.audio[path].currentTime / Ψ.audio[path].duration
			: __MACRO_nonexisting_audio_path__('n_audioTime', path)
	)

/**` audioTime : String -> IO Number `*/
const audioTime = (path : string) : IO <number> =>
	IO (() =>
		Ψ.audio[path]
			? Ψ.audio[path].currentTime
			: __MACRO_nonexisting_audio_path__('audioTime', path)
	)

/**` audioDuration : String -> IO Number `*/
const audioDuration = (path : string) : IO <number> =>
	IO (() =>
		Ψ.audio[path]
			? Ψ.audio[path].duration
			: __MACRO_nonexisting_audio_path__('audioDuration', path)
	)

/**` n_lineThickness : IO Number `*/
const n_lineThickness : IO <number> =
	IO (() => Ψ.ctx.lineWidth / Ψ.ctx.canvas.width)

/**` lineThickness : IO Number `*/
const lineThickness : IO <number> =
	IO (() => Ψ.ctx.lineWidth)

/**` n_lineDashPattern : IO (List Number) `*/
const n_lineDashPattern : IO <List <number>> =
	IO (() => List (...Ψ.ctx.getLineDash().map(x => x / Ψ.ctx.canvas.width)))

/**` lineDashPattern : IO (List Number) `*/
const lineDashPattern : IO <List <number>> =
	IO (() => List (...Ψ.ctx.getLineDash()))

/**` n_lineDashOffset : IO Number `*/
const n_lineDashOffset : IO <number> =
	IO (() => Ψ.ctx.lineDashOffset / Ψ.ctx.canvas.width)

/**` lineDashOffset : IO Number `*/
const lineDashOffset : IO <number> =
	IO (() => Ψ.ctx.lineDashOffset)

/**` miterLimit : IO Number `*/
const miterLimit : IO <number> =
	IO (() => Ψ.ctx.miterLimit)

/**` n_fontSize : IO Number `*/
const n_fontSize : IO <number> =
	IO (() => parseFloat (Ψ.ctx.font) / Ψ.ctx.canvas.width)

/**` fontStyle : IO String `*/
const fontStyle : IO <string> =
	IO (() => Ψ.ctx.font)

/**` fontSize : IO Number `*/
const fontSize : IO <number> =
	IO (() => parseFloat (Ψ.ctx.font))

/**` fontFamily : IO String `*/
const fontFamily : IO <string> =
	IO (() => Ψ.ctx.font.slice(Ψ.ctx.font.indexOf(' ') + 1))

/**` n_shadowOffsetX : IO Number `*/
const n_shadowOffsetX : IO <number> =
	IO (() => Ψ.ctx.shadowOffsetX / Ψ.ctx.canvas.height)

/**` shadowOffsetX : IO Number `*/
const shadowOffsetX : IO <number> =
	IO (() => Ψ.ctx.shadowOffsetX)

/**` n_shadowOffsetY : IO Number `*/
const n_shadowOffsetY : IO <number> =
	IO (() => Ψ.ctx.shadowOffsetY / Ψ.ctx.canvas.height)

/**` shadowOffsetY : IO Number `*/
const shadowOffsetY : IO <number> =
	IO (() => Ψ.ctx.shadowOffsetY)

/**` n_shadowOffset : IO Vector2 `*/
const n_shadowOffset : IO <Vector2> =
	IO (() => Vector2 (Ψ.ctx.shadowOffsetX / Ψ.ctx.canvas.width, Ψ.ctx.shadowOffsetY / Ψ.ctx.canvas.height))

/**` shadowOffset : IO Vector2 `*/
const shadowOffset : IO <Vector2> =
	IO (() => Vector2 (Ψ.ctx.shadowOffsetX, Ψ.ctx.shadowOffsetY))

/**` shadowBlurAmount : IO Number `*/
const shadowBlurAmount : IO <number> =
	IO (() => Ψ.ctx.shadowBlur)

/**` shadowColor : IO String `*/
const shadowColor : IO <string> =
	IO (() => Ψ.ctx.shadowColor)

/**` n_isInEvenOddPath : Number -> : Number -> IO Boolean `*/
const n_isInEvenOddPath = (x : number) => (y : number) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height, 'evenodd'))

/**` n_isInEvenOddPathV2 : Vector2 -> IO Boolean `*/
const n_isInEvenOddPathV2 = (xy : Vector2) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(xy.x / Ψ.ctx.canvas.width, xy.y / Ψ.ctx.canvas.height, 'evenodd'))

/**` isInEvenOddPath : Number -> : Number -> IO Boolean `*/
const isInEvenOddPath = (x : number) => (y : number) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(x, y, 'evenodd'))

/**` isInEvenOddPathV2 : Vector2 -> IO Boolean `*/
const isInEvenOddPathV2 = (xy : Vector2) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(xy.x, xy.y, 'evenodd'))

/**` n_isInNonZeroPath : Number -> : Number -> IO Boolean `*/
const n_isInNonZeroPath = (x : number) => (y : number) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height, 'nonzero'))

/**` n_isInNonZeroPathV2 : Vector2 -> IO Boolean `*/
const n_isInNonZeroPathV2 = (v : Vector2) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(v.x / Ψ.ctx.canvas.width, v.y / Ψ.ctx.canvas.height, 'nonzero'))

/**` isInNonZeroPath : Number -> : Number -> IO Boolean `*/
const isInNonZeroPath = (x : number) => (y : number) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(x, y, 'nonzero'))

/**` isInNonZeroPathV2 : Vector2 -> IO Boolean `*/
const isInNonZeroPathV2 = (v : Vector2) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInPath(v.x, v.y, 'nonzero'))

/**` n_isInStroke : Number -> : Number -> IO Boolean `*/
const n_isInStroke = (x : number) => (y : number) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInStroke(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height))

/**` n_isInStrokeV2 : Vector2 -> IO Boolean `*/
const n_isInStrokeV2 = (v : Vector2) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInStroke(v.x / Ψ.ctx.canvas.width, v.y / Ψ.ctx.canvas.height))

/**` isInStroke : Number -> : Number -> IO Boolean `*/
const isInStroke = (x : number) => (y : number) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInStroke(x, y))

/**` isInStrokeV2 : Vector2 -> IO Boolean `*/
const isInStrokeV2 = (v : Vector2) : IO <boolean> =>
	IO (() => Ψ.ctx.isPointInStroke(v.x, v.y))

/**` transformationM2 : IO Matrix2 `*/
const transformationM2 : IO <Matrix2> =
	IO (() => {
		const { a, c, b, d } = Ψ.ctx.getTransform()
		return Matrix2 (a, c, b, d)
	})

/**` n_transformationM3 : IO Matrix3 `*/
const n_transformationM3 : IO <Matrix3> =
	IO (() => {
		const m = Ψ.ctx.getTransform()
		return Matrix3 (m.a, m.c, m.e / Ψ.ctx.canvas.width, m.b, m.d, m.f / Ψ.ctx.canvas.height, 0, 0, 1)
	})

/**` transformationM3 : IO Matrix3 `*/
const transformationM3 : IO <Matrix3> =
	IO (() => {
		const { a, c, e, b, d, f } = Ψ.ctx.getTransform()
		return Matrix3 (a, c, e, b, d, f, 0, 0, 1)
	})

/**` layerIndex : IO Number `*/
const layerIndex : IO <number> =
	IO (() => Ψ.ctxs.findIndex(ctx => ctx === Ψ.ctx))

/**` alpha : IO Number `*/
const alpha : IO <number> =
	IO (() => Ψ.ctx.globalAlpha)

/**` lineCap : IO LineCap `*/
const lineCap : IO <LineCap> =
	IO (() =>
		Ψ.ctx.lineCap === 'butt'   ? LineCap.Butt   :
		Ψ.ctx.lineCap === 'round'  ? LineCap.Round  :
		Ψ.ctx.lineCap === 'square' ? LineCap.Square : never
	)

/**` lineJoin : IO LineJoin `*/
const lineJoin : IO <LineJoin> =
	IO (() =>
		Ψ.ctx.lineJoin === 'round' ? LineJoin.Round :
		Ψ.ctx.lineJoin === 'bevel' ? LineJoin.Bevel :
		Ψ.ctx.lineJoin === 'miter' ? LineJoin.Miter : never
	)

/**` textAlign : IO TextAlign `*/
const textAlign : IO <TextAlign> =
	IO (() =>
		Ψ.ctx.textAlign === 'center' ? TextAlign.Center    :
		Ψ.ctx.textAlign === 'end'    ? TextAlign.End       :
		Ψ.ctx.textAlign === 'left'   ? TextAlign.Leftside  :
		Ψ.ctx.textAlign === 'right'  ? TextAlign.Rightside :
		Ψ.ctx.textAlign === 'start'  ? TextAlign.Start     : never
	)

/**` textBaseline : IO TextBaseline `*/
const textBaseline : IO <TextBaseline> =
	IO (() =>
		Ψ.ctx.textBaseline === 'alphabetic'  ? TextBaseline.Alphabetic  :
		Ψ.ctx.textBaseline === 'bottom'      ? TextBaseline.Bottom      :
		Ψ.ctx.textBaseline === 'hanging'     ? TextBaseline.Hanging     :
		Ψ.ctx.textBaseline === 'ideographic' ? TextBaseline.Ideographic :
		Ψ.ctx.textBaseline === 'middle'      ? TextBaseline.Middle      :
		Ψ.ctx.textBaseline === 'top'         ? TextBaseline.Top         : never
	)

/**` composition : IO Composition `*/
const composition : IO <Composition> =
	IO (() =>
		Ψ.ctx.globalCompositeOperation === 'source-over'      ? Composition.SourceOver      :
		Ψ.ctx.globalCompositeOperation === 'source-in'        ? Composition.SourceIn        :
		Ψ.ctx.globalCompositeOperation === 'source-out'       ? Composition.SourceOut       :
		Ψ.ctx.globalCompositeOperation === 'source-atop'      ? Composition.SourceAtop      :
		Ψ.ctx.globalCompositeOperation === 'destination-over' ? Composition.DestinationOver :
		Ψ.ctx.globalCompositeOperation === 'destination-in'   ? Composition.DestinationIn   :
		Ψ.ctx.globalCompositeOperation === 'destination-out'  ? Composition.DestinationOut  :
		Ψ.ctx.globalCompositeOperation === 'destination-atop' ? Composition.DestinationAtop :
		Ψ.ctx.globalCompositeOperation === 'lighter'          ? Composition.Lighter         :
		Ψ.ctx.globalCompositeOperation === 'copy'             ? Composition.Copy            :
		Ψ.ctx.globalCompositeOperation === 'xor'              ? Composition.Xor             :
		Ψ.ctx.globalCompositeOperation === 'multiply'         ? Composition.Multiply        :
		Ψ.ctx.globalCompositeOperation === 'screen'           ? Composition.Screen          :
		Ψ.ctx.globalCompositeOperation === 'overlay'          ? Composition.Overlay         :
		Ψ.ctx.globalCompositeOperation === 'darken'           ? Composition.Darken          :
		Ψ.ctx.globalCompositeOperation === 'lighten'          ? Composition.Lighten         :
		Ψ.ctx.globalCompositeOperation === 'color-dodge'      ? Composition.ColorDodge      :
		Ψ.ctx.globalCompositeOperation === 'color-burn'       ? Composition.ColorBurn       :
		Ψ.ctx.globalCompositeOperation === 'hard-light'       ? Composition.HardLight       :
		Ψ.ctx.globalCompositeOperation === 'soft-light'       ? Composition.SoftLight       :
		Ψ.ctx.globalCompositeOperation === 'difference'       ? Composition.Difference      :
		Ψ.ctx.globalCompositeOperation === 'exclusion'        ? Composition.Exclusion       :
		Ψ.ctx.globalCompositeOperation === 'hue'              ? Composition.Hue             :
		Ψ.ctx.globalCompositeOperation === 'saturation'       ? Composition.Saturation      :
		Ψ.ctx.globalCompositeOperation === 'color'            ? Composition.Color           :
		Ψ.ctx.globalCompositeOperation === 'luminosity'       ? Composition.Luminosity      : never
	)

/**` time : IO Number `*/
const time : IO <number> =
	IO (Date.now)

/**` seed : IO Number `*/
const seed : IO <number> =
	IO (() => Ψ.seed)

/**` isWindowResized : IO Boolean `*/
const isWindowResized : IO <boolean> =
	IO (() => Ψ.isResized)

/**` isPointerLocked : IO Boolean `*/
const isPointerLocked : IO <boolean> =
	IO (() => Ψ.isPointerLocked)

/**` beginPath : IO () `*/
const beginPath : IO <null> =
	IO (() => (Ψ.ctx.beginPath(), null))

/**` closePath : IO () `*/
const closePath : IO <null> =
	IO (() => (Ψ.ctx.closePath(), null))

/**` n_relocateTo : Number -> Number -> IO () `*/
const n_relocateTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_relocateToV2 : Vector2 -> IO () `*/
const n_relocateToV2 = (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

/**` relocateTo : Number -> Number -> IO () `*/
const relocateTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x, y), null))

/**` relocateToV2 : Vector2 -> IO () `*/
const relocateToV2 = (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(v.x, v.y), null))

/**` n_lineTo : Number -> Number -> IO () `*/
const n_lineTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_lineToV2 : Vector2 -> IO () `*/
const n_lineToV2 = (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

/**` lineTo : Number -> Number -> IO () `*/
const lineTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(x, y), null))

/**` lineToV2 : Vector2 -> IO () `*/
const lineToV2 = (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(v.x, v.y), null))

/**` n_bezierCurveTo : ...6 Number -> IO () `*/
const n_bezierCurveTo =
	(cx0 : number) => (cy0 : number) =>
	(cx1 : number) => (cy1 : number) =>
	(x   : number) => (y   : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.bezierCurveTo(
			cx0 * Ψ.ctx.canvas.width, cy0 * Ψ.ctx.canvas.height,
			cx1 * Ψ.ctx.canvas.width, cy1 * Ψ.ctx.canvas.height,
			x   * Ψ.ctx.canvas.width, y   * Ψ.ctx.canvas.height
		), null
	))

/**` n_bezierCurveToV2 : Vector2 -> Vector2 -> Vector2 -> IO () `*/
const n_bezierCurveToV2 = (cxy0 : Vector2) => (cxy1 : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.bezierCurveTo(
			cxy0.x * Ψ.ctx.canvas.width, cxy0.y * Ψ.ctx.canvas.height,
			cxy1.x * Ψ.ctx.canvas.width, cxy1.y * Ψ.ctx.canvas.height,
			xy.x   * Ψ.ctx.canvas.width, xy.y   * Ψ.ctx.canvas.height
		), null
	))

/**` bezierCurveTo : ...6 Number -> IO () `*/
const bezierCurveTo =
	(cx0 : number) => (cy0 : number) =>
	(cx1 : number) => (cy1 : number) =>
	(x   : number) => (y   : number) : IO <null> =>
	IO (() => (Ψ.ctx.bezierCurveTo(cx0, cy0, cx1, cy1, x, y), null))

/**` bezierCurveToV2 : Vector2 -> Vector2 -> Vector2 -> IO () `*/
const bezierCurveToV2 = (cxy0 : Vector2) => (cxy1 : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.bezierCurveTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, xy.x, xy.y), null))

/**` n_quadraticCurveTo : Number -> Number -> Number -> Number -> IO () `*/
const n_quadraticCurveTo = (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.quadraticCurveTo(
			cx * Ψ.ctx.canvas.width, cy * Ψ.ctx.canvas.height,
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height
		), null
	))

/**` n_quadraticCurveToV2 : Vector2 -> Vector2 -> IO () `*/
const n_quadraticCurveToV2 = (cxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.quadraticCurveTo(
			cxy.x * Ψ.ctx.canvas.width, cxy.y * Ψ.ctx.canvas.height,
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height
		), null
	))

/**` quadraticCurveTo : Number -> Number -> Number -> Number -> IO () `*/
const quadraticCurveTo = (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.quadraticCurveTo(cx, cy, x, y), null))

/**` quadraticCurveToV2 : Vector2 -> Vector2 -> IO () `*/
const quadraticCurveToV2 = (cxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.quadraticCurveTo(cxy.x, cxy.y, xy.x, xy.y), null))

/**` n_arcTo : ...5 Number -> IO () `*/
const n_arcTo = (r : number) => (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arcTo(
			cx0 * Ψ.ctx.canvas.width, cy0 * Ψ.ctx.canvas.height,
			cx1 * Ψ.ctx.canvas.width, cy1 * Ψ.ctx.canvas.height,
			r   * Ψ.ctx.canvas.width
		), null
	))

/**` n_arcToV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const n_arcToV2 = (r : number) => (cxy0 : Vector2) => (cxy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arcTo(
			cxy0.x * Ψ.ctx.canvas.width, cxy0.y * Ψ.ctx.canvas.height,
			cxy1.x * Ψ.ctx.canvas.width, cxy1.y * Ψ.ctx.canvas.height,
			r      * Ψ.ctx.canvas.width
		), null
	))

/**` arcTo : ...5 Number -> IO () `*/
const arcTo = (r : number) => (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.arcTo(cx0, cy0, cx1, cy1, r), null))

/**` arcToV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const arcToV2 = (r : number) => (cxy0 : Vector2) => (cxy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arcTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, r), null))

/**` n_rect : Number -> Number -> Number -> Number -> IO () `*/
const n_rect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_rectV2 : Vector2 -> Vector2 -> IO () `*/
const n_rectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` rect : Number -> Number -> Number -> Number -> IO () `*/
const rect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x, y, w, h), null))

/**` rectV2 : Vector2 -> Vector2 -> IO () `*/
const rectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_fillRect : Number -> Number -> Number -> Number -> IO () `*/
const n_fillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_fillRectV2 : Vector2 -> Vector2 -> IO () `*/
const n_fillRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` fillRect : Number -> Number -> Number -> Number -> IO () `*/
const fillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(x, y, w, h), null))

/**` fillRectV2 : Vector2 -> Vector2 -> IO () `*/
const fillRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_strokeRect : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_strokeRectV2 : Vector2 -> Vector2 -> IO () `*/
const n_strokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` strokeRect : Number -> Number -> Number -> Number -> IO () `*/
const strokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(x, y, w, h), null))

/**` strokeRectV2 : Vector2 -> Vector2 -> IO () `*/
const strokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_fillStrokeRect : Number -> Number -> Number -> Number -> IO () `*/
const n_fillStrokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` n_fillStrokeRectV2 : Vector2 -> Vector2 -> IO () `*/
const n_fillStrokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` fillStrokeRect : Number -> Number -> Number -> Number -> IO () `*/
const fillStrokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x, y, w, h), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeRectV2 : Vector2 -> Vector2 -> IO () `*/
const fillStrokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillRect : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeFillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` n_strokeFillRectV2 : Vector2 -> Vector2 -> IO () `*/
const n_strokeFillRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` strokeFillRect : Number -> Number -> Number -> Number -> IO () `*/
const strokeFillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x, y, w, h), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillRectV2 : Vector2 -> Vector2 -> IO () `*/
const strokeFillRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_area : Number -> Number -> Number -> Number -> IO () `*/
const n_area = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_areaV2 : Vector2 -> Vector2 -> IO () `*/
const n_areaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.width,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.width
		), null
	))

/**` area : Number -> Number -> Number -> Number -> IO () `*/
const area = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), null))

/**` areaV2 : Vector2 -> Vector2 -> IO () `*/
const areaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

/**` n_fillArea : Number -> Number -> Number -> Number -> IO () `*/
const n_fillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_fillAreaV2 : Vector2 -> Vector2 -> IO () `*/
const n_fillAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), null
	))

/**` fillArea : Number -> Number -> Number -> Number -> IO () `*/
const fillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(x0, y0, x1 - x0, y1 - y0), null))

/**` fillAreaV2 : Vector2 -> Vector2 -> IO () `*/
const fillAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

/**` n_strokeArea : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_strokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
const n_strokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), null
	))

/**` strokeArea : Number -> Number -> Number -> Number -> IO () `*/
const strokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(x0, y0, x1 - x0, y1 - y0), null))

/**` strokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
const strokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

/**` n_fillStrokeArea : Number -> Number -> Number -> Number -> IO () `*/
const n_fillStrokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` n_fillStrokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
const n_fillStrokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` fillStrokeArea : Number -> Number -> Number -> Number -> IO () `*/
const fillStrokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
const fillStrokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillArea : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeFillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` n_strokeFillAreaV2 : Vector2 -> Vector2 -> IO () `*/
const n_strokeFillAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` strokeFillArea : Number -> Number -> Number -> Number -> IO () `*/
const strokeFillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillAreaV2 : Vector2 -> Vector2 -> IO () `*/
const strokeFillAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_arc : ...5 Number -> IO () `*/
const n_arc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
		null
	))

/**` n_arcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const n_arcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
		null
	))

/**` arc : ...5 Number -> IO () `*/
const arc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a1), null))

/**` arcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const arcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a1), null))

/**` n_strokeArc : ...5 Number -> IO () `*/
const n_strokeArc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
		Ψ.ctx.stroke(), null
	))

/**` n_strokeArcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const n_strokeArcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
		Ψ.ctx.stroke(), null
	))

/**` strokeArc : ...5 Number -> IO () `*/
const strokeArc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a1), Ψ.ctx.stroke(), null))

/**` strokeArcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const strokeArcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a1), Ψ.ctx.stroke(), null))

/**` n_arcSection : ...5 Number -> IO () `*/
const n_arcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
		null
	))

/**` n_arcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const n_arcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
		null
	))

/**` arcSection : ...5 Number -> IO () `*/
const arcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a0 + a1), null))

/**` arcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const arcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a0 + a1), null))

/**` n_strokeArcSection : ...5 Number -> IO () `*/
const n_strokeArcSection =
	(r  : number) =>
	(a0 : number) => (a1 : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
		Ψ.ctx.stroke(), null
	))

/**` n_strokeArcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const n_strokeArcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
		Ψ.ctx.stroke(), null
	))

/**` strokeArcSection : ...5 Number -> IO () `*/
const strokeArcSection =
	(r  : number) =>
	(a0 : number) => (a1 : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` strokeArcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
const strokeArcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` n_circle : Number -> Number -> Number -> IO () `*/
const n_circle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), null))

/**` n_circleV2 : Number -> Vector2 -> IO () `*/
const n_circleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), null))

/**` circle : Number -> Number -> Number -> IO () `*/
const circle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), null))

/**` circleV2 : Number -> Vector2 -> IO () `*/
const circleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), null))

/**` n_fillCircle : Number -> Number -> Number -> IO () `*/
const n_fillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.fill(), null
	))

/**` n_fillCircleV2 : Number -> Vector2 -> IO () `*/
const n_fillCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.fill(), null
	))

/**` fillCircle : Number -> Number -> Number -> IO () `*/
const fillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.fill(), null))

/**` fillCircleV2 : Number -> Vector2 -> IO () `*/
const fillCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.fill(), null))

/**` n_strokeCircle : Number -> Number -> Number -> IO () `*/
const n_strokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.stroke(), null
	))

/**` n_strokeCircleV2 : Number -> Vector2 -> IO () `*/
const n_strokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.stroke(), null
	))

/**` strokeCircle : Number -> Number -> Number -> IO () `*/
const strokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.stroke(), null))

/**` strokeCircleV2 : Number -> Vector2 -> IO () `*/
const strokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.stroke(), null))

/**` n_fillStrokeCircle : Number -> Number -> Number -> IO () `*/
const n_fillStrokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` n_fillStrokeCircleV2 : Number -> Vector2 -> IO () `*/
const n_fillStrokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` fillStrokeCircle : Number -> Number -> Number -> IO () `*/
const fillStrokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeCircleV2 : Number -> Vector2 -> IO () `*/
const fillStrokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillCircle : Number -> Number -> Number -> IO () `*/
const n_strokeFillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` n_strokeFillCircleV2 : Number -> Vector2 -> IO () `*/
const n_strokeFillCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
		Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` strokeFillCircle : Number -> Number -> Number -> IO () `*/
const strokeFillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillCircleV2 : Number -> Vector2 -> IO () `*/
const strokeFillCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_elliptic : ...7 Number -> IO () `*/
const n_elliptic =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, a0 * tau, a1 * tau
		), null
	))

/**` n_ellipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const n_ellipticV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.width,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau
		), null
	))

/**` elliptic : ...7 Number -> IO () `*/
const elliptic =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a1), null))

/**` ellipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const ellipticV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a1), null))

/**` n_strokeElliptic : ...7 Number -> IO () `*/
const n_strokeElliptic =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.width,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau
		), Ψ.ctx.stroke(), null
	))

/**` n_strokeEllipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const n_strokeEllipticV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.width,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau
		), Ψ.ctx.stroke(), null
	))

/**` strokeElliptic : ...7 Number -> IO () `*/
const strokeElliptic =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a1), Ψ.ctx.stroke(), null))

/**` strokeEllipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const strokeEllipticV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a1), Ψ.ctx.stroke(), null))

/**` n_ellipticSection : ...7 Number -> IO () `*/
const n_ellipticSection =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.width,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau
		), null
	))

/**` n_ellipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const n_ellipticSectionV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.width,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width,
			a * tau, a0 * tau, (a0 + a1) * tau
		), null
	))

/**` ellipticSection : ...7 Number -> IO () `*/
const ellipticSection =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a0 + a1), null))

/**` ellipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const ellipticSectionV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a0 + a1), null))

/**` n_strokeEllipticSection : ...7 Number -> IO () `*/
const n_strokeEllipticSection =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.width,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width,
			a  * tau, a0 * tau, (a0 + a1) * tau
		), Ψ.ctx.stroke(), null
	))

/**` n_strokeEllipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const n_strokeEllipticSectionV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, a0 * tau, (a0 + a1) * tau
		), Ψ.ctx.stroke(), null
	))

/**` strokeEllipticSection : ...7 Number -> IO () `*/
const strokeEllipticSection =
	(a  : number) => (a0 : number) => (a1 : number) =>
	(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` strokeEllipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
const strokeEllipticSectionV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` n_ellipse : ...5 Number -> IO () `*/
const n_ellipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), null
	))

/**` n_ellipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const n_ellipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), null
	))

/**` ellipse : ...5 Number -> IO () `*/
const ellipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), null))

/**` ellipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const ellipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), null))

/**` n_fillEllipse : ...5 Number -> IO () `*/
const n_fillEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.fill(), null
	))

/**` n_fillEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const n_fillEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.fill(), null
	))

/**` fillEllipse : ...5 Number -> IO () `*/
const fillEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.fill(), null))

/**` fillEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const fillEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.fill(), null))

/**` n_strokeEllipse : ...5 Number -> IO () `*/
const n_strokeEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), null
	))

/**` n_strokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const n_strokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), null
	))

/**` strokeEllipse : ...5 Number -> IO () `*/
const strokeEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.stroke(), null))

/**` strokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const strokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.stroke(), null))

/**` n_fillStrokeEllipse : ...5 Number -> IO () `*/
const n_fillStrokeEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` n_fillStrokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const n_fillStrokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` fillStrokeEllipse : ...5 Number -> IO () `*/
const fillStrokeEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const fillStrokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillEllipse : ...5 Number -> IO () `*/
const n_strokeFillEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` n_strokeFillEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const n_strokeFillEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` strokeFillEllipse : ...5 Number -> IO () `*/
const strokeFillEllipse =
	(a  : number) =>
	(kx : number) => (ky : number) =>
	(x  : number) => (y  : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
const strokeFillEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_fillText : String -> Number -> Number -> IO () `*/
const n_fillText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_fillTextV2 : String -> Vector2 -> IO () `*/
const n_fillTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null))

/**` fillText : String -> Number -> Number -> IO () `*/
const fillText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, x, y), null))

/**` fillTextV2 : String -> Vector2 -> IO () `*/
const fillTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, xy.x, xy.y), null))

/**` n_strokeText : String -> Number -> Number -> IO () `*/
const n_strokeText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_strokeTextV2 : String -> Vector2 -> IO () `*/
const n_strokeTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null))

/**` strokeText : String -> Number -> Number -> IO () `*/
const strokeText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, x, y), null))

/**` strokeTextV2 : String -> Vector2 -> IO () `*/
const strokeTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, xy.x, xy.y), null))

/**` n_fillStrokeText : String -> Number -> Number -> IO () `*/
const n_fillStrokeText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		const nextX = x * Ψ.ctx.canvas.width
		const nextY = y * Ψ.ctx.canvas.height
		Ψ.ctx.fillText  (text, nextX, nextY)
		Ψ.ctx.strokeText(text, nextX, nextY)
		return null
	})

/**` n_fillStrokeText : String -> Vector2 -> IO () `*/
const n_fillStrokeTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => {
		const nextX = xy.x * Ψ.ctx.canvas.width
		const nextY = xy.y * Ψ.ctx.canvas.height
		Ψ.ctx.fillText  (text, nextX, nextY)
		Ψ.ctx.strokeText(text, nextX, nextY)
		return null
	})

/**` fillStrokeText : String -> Number -> Number -> IO () `*/
const fillStrokeText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, x, y), Ψ.ctx.strokeText(text, x, y), null))

/**` fillStrokeText : String -> Vector2 -> IO () `*/
const fillStrokeTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, xy.x, xy.y), Ψ.ctx.strokeText(text, xy.x, xy.y), null))

/**` n_strokeFillText : String -> Number -> Number -> IO () `*/
const n_strokeFillText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		const nextX = x * Ψ.ctx.canvas.width
		const nextY = y * Ψ.ctx.canvas.height
		Ψ.ctx.strokeText(text, nextX, nextY)
		Ψ.ctx.fillText  (text, nextX, nextY)
		return null
	})

/**` n_strokeFillTextV2 : String -> Vector2 -> IO () `*/
const n_strokeFillTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => {
		const nextX = xy.x * Ψ.ctx.canvas.width
		const nextY = xy.y * Ψ.ctx.canvas.height
		Ψ.ctx.strokeText(text, nextX, nextY)
		Ψ.ctx.fillText  (text, nextX, nextY)
		return null
	})

/**` strokeFillText : String -> Number -> Number -> IO () `*/
const strokeFillText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, x, y), Ψ.ctx.fillText(text, x, y), null))

/**` strokeFillText : String -> Vector2 -> IO () `*/
const strokeFillTextV2 = (text : string) => (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, xy.x, xy.y), Ψ.ctx.fillText(text, xy.x, xy.y), null))

/**` n_fillMultilineText : String -> Number -> Number -> Number -> IO () `*/
const n_fillMultilineText = (text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * x
		const nextY = Ψ.ctx.canvas.height * y
		const nextS = Ψ.ctx.canvas.height * spacing
		text.split('\n').forEach((line, i) => Ψ.ctx.fillText(line, nextX, nextY + nextS * i))
		return null
	})

/**` n_fillMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const n_fillMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * xy.x
		const nextY = Ψ.ctx.canvas.height * xy.y
		const nextS = Ψ.ctx.canvas.height * spacing
		text.split('\n').forEach((line, i) => Ψ.ctx.fillText(line, nextX, nextY + nextS * i))
		return null
	})

/**` fillMultilineText : String -> Number -> Number -> Number -> IO () `*/
const fillMultilineText = (text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (text.split('\n').forEach((line, i) => Ψ.ctx.fillText(line, x, y + spacing * i)), null))

/**` fillMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const fillMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => (text.split('\n').forEach((line, i) => Ψ.ctx.fillText(line, xy.x, xy.y + spacing * i)), null))

/**` n_strokeMultilineText : String -> Number -> Number -> Number -> IO () `*/
const n_strokeMultilineText = (text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * x
		const nextY = Ψ.ctx.canvas.height * y
		const nextS = Ψ.ctx.canvas.height * spacing
		text.split('\n').forEach((line, i) => Ψ.ctx.strokeText(line, nextX, nextY + nextS * i))
		return null
	})

/**` n_strokeMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const n_strokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * xy.x
		const nextY = Ψ.ctx.canvas.height * xy.y
		const nextS = Ψ.ctx.canvas.height * spacing
		text.split('\n').forEach((line, i) => Ψ.ctx.strokeText(line, nextX, nextY + nextS * i))
		return null
	})

/**` strokeMultilineText : String -> Number -> Number -> Number -> IO () `*/
const strokeMultilineText = (text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (text.split('\n').forEach((line, i) => Ψ.ctx.strokeText(line, x, y + spacing * i)), null))

/**` strokeMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const strokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => (text.split('\n').forEach((line, i) => Ψ.ctx.strokeText(line, xy.x, xy.y + spacing * i)), null))

/**` n_strokeFillMultilineText : String -> Number -> Number -> Number -> IO () `*/
const n_strokeFillMultilineText =
	(text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * x
		const nextY = Ψ.ctx.canvas.height * y
		const nextS = Ψ.ctx.canvas.height * spacing
		let   lineY = nextY
		text.split('\n').forEach(line => {
			Ψ.ctx.strokeText(line, nextX, lineY)
			Ψ.ctx.fillText  (line, nextX, lineY)
			lineY += nextS
		})
		return null
	})

/**` n_strokeFillMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const n_strokeFillMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * xy.x
		const nextY = Ψ.ctx.canvas.height * xy.y
		const nextS = Ψ.ctx.canvas.height * spacing
		let   lineY = nextY
		text.split('\n').forEach(line => {
			Ψ.ctx.strokeText(line, nextX, lineY)
			Ψ.ctx.fillText  (line, nextX, lineY)
			lineY += nextS
		})
		return null
	})

/**` strokeFillMultilineText : String -> Number -> Number -> Number -> IO () `*/
const strokeFillMultilineText =
	(text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		let lineY = y
		text.split('\n').forEach(line => {
			Ψ.ctx.strokeText(line, x, lineY)
			Ψ.ctx.fillText  (line, x, lineY)
			lineY += spacing
		})
		return null
	})

/**` strokeFillMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const strokeFillMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => {
		let lineY = xy.y
		text.split('\n').forEach(line => {
			Ψ.ctx.strokeText(line, xy.x, lineY)
			Ψ.ctx.fillText  (line, xy.x, lineY)
			lineY += spacing
		})
		return null
	})

/**` n_fillStrokeMultilineText : String -> Number -> Number -> Number -> IO () `*/
const n_fillStrokeMultilineText =
	(text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * x
		const nextY = Ψ.ctx.canvas.height * y
		const nextS = Ψ.ctx.canvas.height * spacing
		let   lineY = nextY
		text.split('\n').forEach(line => {
			Ψ.ctx.fillText  (line, nextX, lineY)
			Ψ.ctx.strokeText(line, nextX, lineY)
			lineY += nextS
		})
		return null
	})

/**` n_fillStrokeMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const n_fillStrokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => {
		const nextX = Ψ.ctx.canvas.width  * xy.x
		const nextY = Ψ.ctx.canvas.height * xy.y
		const nextS = Ψ.ctx.canvas.height * spacing
		let   lineY = nextY
		text.split('\n').forEach(line => {
			Ψ.ctx.fillText  (line, nextX, lineY)
			Ψ.ctx.strokeText(line, nextX, lineY)
			lineY += nextS
		})
		return null
	})

/**` fillStrokeMultilineText : String -> Number -> Number -> Number -> IO () `*/
const fillStrokeMultilineText =
	(text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		let lineY = y
		text.split('\n').forEach(line => {
			Ψ.ctx.fillText  (line, x, lineY)
			Ψ.ctx.strokeText(line, x, lineY)
			lineY += spacing
		})
		return null
	})

/**` fillStrokeMultilineTextV2 : String -> Number -> Vector2 -> IO () `*/
const fillStrokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : Vector2) : IO <null> =>
	IO (() => {
		let lineY = xy.y
		text.split('\n').forEach(line => {
			Ψ.ctx.fillText  (line, xy.x, lineY)
			Ψ.ctx.strokeText(line, xy.x, lineY)
			lineY += spacing
		})
		return null
	})

/**` n_line : Number -> Number -> Number -> Number -> IO () `*/
const n_line = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo(x1 * Ψ.ctx.canvas.width, y1 * Ψ.ctx.canvas.height),
		null
	))

/**` n_lineV2 : Vector2 -> Vector2 -> IO () `*/
const n_lineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo(xy1.x * Ψ.ctx.canvas.width, xy1.y * Ψ.ctx.canvas.height),
		null
	))

/**` line : Number -> Number -> Number -> Number -> IO () `*/
const line = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x0, y0), Ψ.ctx.lineTo(x1, y1), null))

/**` lineV2 : Vector2 -> Vector2 -> IO () `*/
const lineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(xy0.x, xy0.y), Ψ.ctx.lineTo(xy1.x, xy1.y), null))

/**` n_strokeLine : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeLine = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo(x1 * Ψ.ctx.canvas.width, y1 * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` n_strokeLineV2 : Vector2 -> Vector2 -> IO () `*/
const n_strokeLineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo(xy1.x * Ψ.ctx.canvas.width, xy1.y * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` strokeLine : Number -> Number -> Number -> Number -> IO () `*/
const strokeLine = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x0, y0), Ψ.ctx.lineTo(x1, y1), Ψ.ctx.stroke(), null))

/**` strokeLineV2 : Vector2 -> Vector2 -> IO () `*/
const strokeLineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(xy0.x, xy0.y), Ψ.ctx.lineTo(xy1.x, xy1.y), Ψ.ctx.stroke(), null))

/**` n_vectorLine : Number -> Number -> Number -> Number -> IO () `*/
const n_vectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(x        * Ψ.ctx.canvas.width, y        * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((x + dx) * Ψ.ctx.canvas.width, (y + dy) * Ψ.ctx.canvas.height),
		null
	))

/**` n_vectorLineV2 : Vector2 -> Vector2 -> IO () `*/
const n_vectorLineV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy.x           * Ψ.ctx.canvas.width, xy.y           * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((xy.x + dxy.x) * Ψ.ctx.canvas.width, (xy.y + dxy.y) * Ψ.ctx.canvas.height),
		null
	))

/**` vectorLine : Number -> Number -> Number -> Number -> IO () `*/
const vectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x, y), Ψ.ctx.lineTo(x + dx, y + dy), null))

/**` vectorLineV2 : Vector2 -> Vector2 -> IO () `*/
const vectorLineV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(xy.x, xy.y), Ψ.ctx.lineTo(xy.x + dxy.x, xy.y + dxy.y), null))

/**` n_strokeVectorLine : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeVectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(x        * Ψ.ctx.canvas.width, y        * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((x + dx) * Ψ.ctx.canvas.width, (y + dy) * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` n_strokeVectorLineV2 : Vector2 -> Vector2 -> IO () `*/
const n_strokeVectorLineV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy.x           * Ψ.ctx.canvas.width, xy.y           * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((xy.x + dxy.x) * Ψ.ctx.canvas.width, (xy.y + dxy.y) * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` strokeVectorLine : Number -> Number -> Number -> Number -> IO () `*/
const strokeVectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x, y), Ψ.ctx.lineTo(x + dx, y + dy), Ψ.ctx.stroke(), null))

/**` strokeVectorLineV2 : Vector2 -> Vector2 -> IO () `*/
const strokeVectorLineV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(xy.x, xy.y), Ψ.ctx.lineTo(xy.x + dxy.x, xy.y + dxy.y), Ψ.ctx.stroke(), null))

/**` n_image : String -> ...8 Number -> IO () `*/
const n_image =
	(path : string) =>
	(cx   : number) => (cy : number) => (cw : number) => (ch : number) =>
	(x    : number) => (y  : number) => (w  : number) => (h  : number) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
				(Ψ.ctx.drawImage(
					Ψ.image[path],
					cx * Ψ.image[path].width, cy * Ψ.image[path].height,
					cw * Ψ.image[path].width, ch * Ψ.image[path].height,
					x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
					w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
				), null)
			: __MACRO_nonexisting_image_path__('n_image', path)
	)

/**` n_imageV2 : String -> ...4 Vector2 -> IO () `*/
const n_imageV2 =
	(path : string ) =>
	(cxy  : Vector2) => (cwh : Vector2) =>
	(xy   : Vector2) => (wh  : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				cxy.x * Ψ.image[path].width, cxy.y * Ψ.image[path].height,
				cwh.x * Ψ.image[path].width, cwh.y * Ψ.image[path].height,
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null)
		: __MACRO_nonexisting_image_path__('n_imageV2', path)
	)

/**` image : String -> ...8 Number -> IO () `*/
const image =
	(path : string) =>
	(cx   : number) => (cy : number) => (cw : number) => (ch : number) =>
	(x    : number) => (y  : number) => (w  : number) => (h  : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], cx, cy, cw, ch, x, y, w, h), null)
			: __MACRO_nonexisting_image_path__('image', path)
	)

/**` imageV2 : String -> ...4 Vector2 -> IO () `*/
const imageV2 =
	(path : string ) =>
	(cxy  : Vector2) => (cwh : Vector2) =>
	(xy   : Vector2) => (wh  : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], cxy.x, cxy.y, cwh.x, cwh.y, xy.x, xy.y, wh.x, wh.y), null)
			: __MACRO_nonexisting_image_path__('imageV2', path)
	)

/**` n_uncroppedImage : String -> ...4 Number -> IO () `*/
const n_uncroppedImage =
	(path : string) =>
	(x    : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
				(Ψ.ctx.drawImage(
					Ψ.image[path],
					x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
					w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
				), null)
			: __MACRO_nonexisting_image_path__('n_uncroppedImage', path)
	)

/**` n_uncroppedImageV2 : String -> Vector2 -> Vector2 -> IO () `*/
const n_uncroppedImageV2 = (path : string) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null)
		: __MACRO_nonexisting_image_path__('n_uncroppedImageV2', path)
	)

/**` uncroppedImage : String -> ...4 Number -> IO () `*/
const uncroppedImage =
	(path : string) =>
	(x    : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y, w, h), null)
			: __MACRO_nonexisting_image_path__('uncroppedImage', path)
	)

/**` uncroppedImageV2 : String -> Vector2 -> Vector2 -> IO () `*/
const uncroppedImageV2 = (path : string) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, wh.x, wh.y), null)
			: __MACRO_nonexisting_image_path__('uncroppedImageV2', path)
	)

/**` n_fullImage : String -> Number -> Number -> IO () `*/
const n_fullImage = (path : string) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null)
			: __MACRO_nonexisting_image_path__('n_fullImage', path)
	)

/**` n_fullImageV2 : String -> Vector2 -> IO () `*/
const n_fullImageV2 = (path : string) => (xy : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? __MACRO_nonexisting_image_path__('n_fullImageV2', path)
			: (Ψ.ctx.drawImage(Ψ.image[path], xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null)
	)

/**` fullImage : String -> Number -> Number -> IO () `*/
const fullImage = (path : string) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y), null)
			: __MACRO_nonexisting_image_path__('fullImage', path)
	)

/**` fullImageV2 : String -> Vector2 -> IO () `*/
const fullImageV2 = (path : string) => (xy : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y), null)
			: __MACRO_nonexisting_image_path__('fullImageV2', path)
	)

/**` n_fullScaledImage : String -> Number -> Number -> Number -> IO () `*/
const n_fullScaledImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				Ψ.image[path].width * k * Ψ.ctx.canvas.width, Ψ.image[path].height * k * Ψ.ctx.canvas.height
			), null)
		: __MACRO_nonexisting_image_path__('n_fullScaledImage', path)
	)

/**` n_fullScaledImageV2 : String -> Number -> Vector2 -> IO () `*/
const n_fullScaledImageV2 = (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				Ψ.image[path].width * k * Ψ.ctx.canvas.width, Ψ.image[path].height * k * Ψ.ctx.canvas.height
			), null)
		: __MACRO_nonexisting_image_path__('n_fullScaledImageV2', path)
	)

/**` fullScaledImage : String -> Number -> Number -> Number -> IO () `*/
const fullScaledImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y, Ψ.image[path].width * k, Ψ.image[path].height * k), null)
			: __MACRO_nonexisting_image_path__('fullScaledImage', path)
	)

/**` fullScaledImageV2 : String -> Number -> Vector2 -> IO () `*/
const fullScaledImageV2 = (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, Ψ.image[path].width * k, Ψ.image[path].height * k), null)
			: __MACRO_nonexisting_image_path__('fullScaledImageV2', path)
	)

/**` n_squareImage : String -> Number -> Number -> Number -> IO () `*/
const n_squareImage = (path : string) => (w : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				w * Ψ.ctx.canvas.width, w * Ψ.ctx.canvas.width
			), null)
		: __MACRO_nonexisting_image_path__('n_squareImage', path)
	)

/**` n_squareImageV2 : String -> Number -> Vector2 -> IO () `*/
const n_squareImageV2 = (path : string) => (w : number) => (xy : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				w    * Ψ.ctx.canvas.width, w    * Ψ.ctx.canvas.width
			), null)
		: __MACRO_nonexisting_image_path__('n_squareImageV2', path)
	)

/**` squareImage : String -> Number -> Number -> Number -> IO () `*/
const squareImage = (path : string) => (w : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y, w, w), null)
			: __MACRO_nonexisting_image_path__('squareImage', path)
	)

/**` squareImageV2 : String -> Number -> Vector2 -> IO () `*/
const squareImageV2 = (path : string) => (w : number) => (xy : Vector2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, w, w), null)
			: __MACRO_nonexisting_image_path__('squareImageV2', path)
	)

/**` n_clearRect : Number -> Number -> Number -> Number -> IO () `*/
const n_clearRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_clearRectV2 : Vector2 -> Vector2 -> IO () `*/
const n_clearRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` clearRect : Number -> Number -> Number -> Number -> IO () `*/
const clearRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(x, y, w, h), null))

/**` clearRectV2 : Vector2 -> Vector2 -> IO () `*/
const clearRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_clearArea : Number -> Number -> Number -> Number -> IO () `*/
const n_clearArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_clearAreaV2 : Vector2 -> Vector2 -> IO () `*/
const n_clearAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), null
	))

/**` clearArea : Number -> Number -> Number -> Number -> IO () `*/
const clearArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(x0, y0, x1 - x0, y1 - y0), null))

/**` clearAreaV2 : Vector2 -> Vector2 -> IO () `*/
const clearAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

/**` clearLayer : Number -> IO () `*/
const clearLayer = (index : number) : IO <null> =>
	IO (() =>
		index >= 0 && index < Ψ.ctxs.length && Number.isInteger (index)
			? (Ψ.ctx.clearRect(0, 0, Ψ.ctx.canvas.width, Ψ.ctx.canvas.height), null)
			: __MACRO_invalid_index_range('clearLayer', index, Ψ.ctxs.length)
	)

/**` clearCurrentLayer : IO () `*/
const clearCurrentLayer : IO <null> =
	IO (() => (Ψ.ctx.clearRect(0, 0, Ψ.ctx.canvas.width, Ψ.ctx.canvas.height), null))

/**` clearCanvas : IO () `*/
const clearCanvas : IO <null> =
	IO (() => (Ψ.ctxs.forEach(ctx => ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)), null))

/**` fill : IO () `*/
const fill : IO <null> =
	IO (() => (Ψ.ctx.fill(), null))

/**` stroke : IO () `*/
const stroke : IO <null> =
	IO (() => (Ψ.ctx.stroke(), null))

/**` clipEvenOdd : IO () `*/
const clipEvenOdd : IO <null> =
	IO (() => (Ψ.ctx.clip('evenodd'), null))

/**` clipNonZero : IO () `*/
const clipNonZero : IO <null> =
	IO (() => (Ψ.ctx.clip('nonzero'), null))

/**` n_rotate : Number -> IO () `*/
const n_rotate = (angle : number) : IO <null> =>
	IO (() => (Ψ.ctx.rotate(angle * tau), null))

/**` rotate : Number -> IO () `*/
const rotate = (angle : number) : IO <null> =>
	IO (() => (Ψ.ctx.rotate(angle), null))

/**` n_translateX : Number -> IO () `*/
const n_translateX = (dx : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx * Ψ.ctx.canvas.width, 0), null))

/**` n_translateY : Number -> IO () `*/
const n_translateY = (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(0, dy * Ψ.ctx.canvas.height), null))

/**` n_translateXY : Number -> Number -> IO () `*/
const n_translateXY = (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx * Ψ.ctx.canvas.width, dy * Ψ.ctx.canvas.height), null))

/**` n_translateV2 : Vector2 -> IO () `*/
const n_translateV2 = (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.translate(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

/**` translateX : Number -> IO () `*/
const translateX = (dx : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx, 0), null))

/**` translateY : Number -> IO () `*/
const translateY = (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(0, dy), null))

/**` translateXY : Number -> Number -> IO () `*/
const translateXY = (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx, dy), null))

/**` translateV2 : Vector2 -> IO () `*/
const translateV2 = (v : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.translate(v.x, v.y), null))

/**` scale : Number -> IO () `*/
const scale = (k : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(k, k), null))

/**` scaleX : Number -> IO () `*/
const scaleX = (kx : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(kx, 1), null))

/**` scaleY : Number -> IO () `*/
const scaleY = (ky : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(1, ky), null))

/**` scaleXY : Number -> Number -> IO () `*/
const scaleXY = (kx : number) => (ky : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(kx, ky), null))

/**` scaleV2 : Vector2 -> IO () `*/
const scaleV2 = (kxy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.scale(kxy.x, kxy.y), null))

/**` n_applyTransformationM2 : Matrix2 -> IO () `*/
const n_applyTransformationM2 = (m2 : Matrix2) : IO <null> =>
	IO (() => (Ψ.ctx.transform(m2.ix, m2.iy, m2.jx, m2.jy, 0, 0), null))

/**` n_applyTransformationM3 : Matrix3 -> IO () `*/
const n_applyTransformationM3 = (m3 : Matrix3) : IO <null> =>
	IO (() => (Ψ.ctx.transform(m3.ix, m3.iy, m3.jx, m3.jy, m3.kx * Ψ.ctx.canvas.width, m3.ky * Ψ.ctx.canvas.height), null))

/**` applyTransformationM2 : Matrix2 -> IO () `*/
const applyTransformationM2 = (m2 : Matrix2) : IO <null> =>
	IO (() => (Ψ.ctx.transform(m2.ix, m2.iy, m2.jx, m2.jy, 0, 0), null))

/**` applyTransformationM3 : Matrix3 -> IO () `*/
const applyTransformationM3 = (m3 : Matrix3) : IO <null> =>
	IO (() => (Ψ.ctx.transform(m3.ix, m3.iy, m3.jx, m3.jy, m3.kx, m3.ky), null))

/**` setTransformationM2 : Matrix2 -> IO () `*/
const setTransformationM2 = (m2 : Matrix2) : IO <null> =>
	IO (() => (Ψ.ctx.setTransform(m2.ix, m2.iy, m2.jx, m2.jy, 0, 0), null))

/**` n_setTransformationM3 : Matrix3 -> IO () `*/
const n_setTransformationM3 = (m3 : Matrix3) : IO <null> =>
	IO (() => (Ψ.ctx.setTransform(m3.ix, m3.iy, m3.jx, m3.jy, m3.kx * Ψ.ctx.canvas.width, m3.ky * Ψ.ctx.canvas.height), null))

/**` setTransformationM3 : Matrix3 -> IO () `*/
const setTransformationM3 = (m3 : Matrix3) : IO <null> =>
	IO (() => (Ψ.ctx.setTransform(m3.ix, m3.iy, m3.jx, m3.jy, m3.kx, m3.ky), null))

/**` resetTransformation : IO () `*/
const resetTransformation : IO <null> =
	IO (() => (Ψ.ctx.resetTransform(), null))

/**` n_setCanvasWidth : Number -> IO () `*/
const n_setCanvasWidth = (w : number) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => ctx.canvas.width = w * innerWidth), null))

/**` setCanvasWidth : Number -> IO () `*/
const setCanvasWidth = (w : number) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => ctx.canvas.width = w), null))

/**` n_setCanvasHeight : Number -> IO () `*/
const n_setCanvasHeight = (h : number) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => ctx.canvas.height = h * innerHeight), null))

/**` setCanvasHeight : Number -> IO () `*/
const setCanvasHeight = (h : number) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => ctx.canvas.height = h), null))

/**` n_setCanvasDimensions : Number -> Number -> IO () `*/
const n_setCanvasDimensions = (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => (ctx.canvas.width = w * innerWidth, ctx.canvas.height = h * innerHeight)), null))

/**` n_setCanvasDimensionsV2 : Vector2 -> IO () `*/
const n_setCanvasDimensionsV2 = (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => (ctx.canvas.width = wh.x * innerWidth, ctx.canvas.height = wh.y * innerHeight)), null))

/**` setCanvasDimensionsV2 : Vector2 -> IO () `*/
const setCanvasDimensionsV2 = (wh : Vector2) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => (ctx.canvas.width = wh.x, ctx.canvas.height = wh.y)), null))

/**` setCanvasDimensions : Number -> Number -> IO () `*/
const setCanvasDimensions = (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => (ctx.canvas.width = w, ctx.canvas.height = h)), null))

/**` n_setLineThickness : Number -> IO () `*/
const n_setLineThickness = (thickness : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineWidth = thickness * Ψ.ctx.canvas.width, null))

/**` setLineThickness : Number -> IO () `*/
const setLineThickness = (thickness : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineWidth = thickness, null))

/**` n_setLineDashPattern : List Number -> IO () `*/
const n_setLineDashPattern = (pattern : List <number>) : IO <null> =>
	IO (() => (Ψ.ctx.setLineDash(listToArray (pattern).map(x => x * Ψ.ctx.canvas.width)), null))

/**` setLineDashPattern : List Number -> IO () `*/
const setLineDashPattern = (pattern : List <number>) : IO <null> =>
	IO (() => (Ψ.ctx.setLineDash(listToArray (pattern)), null))

/**` n_setLineDashOffset : Number -> IO () `*/
const n_setLineDashOffset = (offset : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineDashOffset = offset * Ψ.ctx.canvas.width, null))

/**` setLineDashOffset : Number -> IO () `*/
const setLineDashOffset = (offset : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineDashOffset = offset, null))

/**` setFontStyle : String -> IO () `*/
const setFontStyle = (font : string) : IO <null> =>
	IO (() => (Ψ.ctx.font = font, null))

/**` n_setFontSize : Number -> IO () `*/
const n_setFontSize = (size : number) : IO <null> =>
	IO (() => (Ψ.ctx.font = `${size * Ψ.ctx.canvas.width}px${Ψ.ctx.font.slice(Ψ.ctx.font.indexOf(' '))}`, null))

/**` setFontSize : Number -> IO () `*/
const setFontSize = (size : number) : IO <null> =>
	IO (() => (Ψ.ctx.font = `${size}px${Ψ.ctx.font.slice(Ψ.ctx.font.indexOf(' '))}`, null))

/**` setFontFamily : String -> IO () `*/
const setFontFamily = (family : string) : IO <null> =>
	IO (() => (Ψ.ctx.font = `${parseFloat(Ψ.ctx.font)}px "${family}"`, null))

/**` n_setShadowRGBA : Number -> Number -> Number -> Number -> IO () `*/
const n_setShadowRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

/**` setShadowRGBAV4 : Vector4 -> IO () `*/
const setShadowRGBAV4 = (v : Vector4) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

/**` setShadowRGBA : Number -> Number -> Number -> Number -> IO () `*/
const setShadowRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = `rgba(${r},${g},${b},${a})`, null))

/**` n_setShadowRGBAV4 : Vector4 -> IO () `*/
const n_setShadowRGBAV4 = (v : Vector4) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w * 255})`, null))

/**` n_setShadowX : Number -> IO () `*/
const n_setShadowX = (x : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x * Ψ.ctx.canvas.width, null))

/**` setShadowX : Number -> IO () `*/
const setShadowX = (x : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x, null))

/**` n_setShadowY : Number -> IO () `*/
const n_setShadowY = (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetY = y * Ψ.ctx.canvas.height, null))

/**` setShadowY : Number -> IO () `*/
const setShadowY = (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetY = y, null))

/**` n_setShadowXY : Number -> Number -> IO () `*/
const n_setShadowXY = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x * Ψ.ctx.canvas.width, Ψ.ctx.shadowOffsetY = y * Ψ.ctx.canvas.height, null))

/**` setShadowXY : Number -> Number -> IO () `*/
const setShadowXY = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x, Ψ.ctx.shadowOffsetY = y, null))

/**` n_setShadowXYV2 : IO () `*/
const n_setShadowXYV2 = (xy : Vector2) : IO <null> =>
	IO (() => (
		Ψ.ctx.shadowOffsetX = xy.x * Ψ.ctx.canvas.width,
		Ψ.ctx.shadowOffsetY = xy.y * Ψ.ctx.canvas.height,
		null
	))

/**` setShadowXYV2 : IO () `*/
const setShadowXYV2 = (xy : Vector2) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = xy.x, Ψ.ctx.shadowOffsetY = xy.y, null))

/**` setShadowColor : String -> IO () `*/
const setShadowColor = (color : string) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = color, null))

/**` n_setFillRGBA : Number -> Number -> Number -> Number -> IO () `*/
const n_setFillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

/**` n_setFillRGBAV4 : Vector4 -> IO () `*/
const n_setFillRGBAV4 = (v : Vector4) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

/**` setFillRGBA : Number -> Number -> Number -> Number -> IO () `*/
const setFillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${r},${g},${b},${a})`, null))

/**` setFillRGBAV4 : Vector4 -> IO () `*/
const setFillRGBAV4 = (v : Vector4) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

/**` n_setStrokeRGBA : Number -> Number -> Number -> Number -> IO () `*/
const n_setStrokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

/**` n_setStrokeRGBAV4 : Vector4 -> IO () `*/
const n_setStrokeRGBAV4 = (v : Vector4) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

/**` setStrokeRGBA : Number -> Number -> Number -> Number -> IO () `*/
const setStrokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = `rgba(${r},${g},${b},${a})`, null))

/**` setStrokeRGBAV4 : Vector4 -> IO () `*/
const setStrokeRGBAV4 = (v : Vector4) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

/**` setFillColor : String -> IO () `*/
const setFillColor = (color : string) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = color, null))

/**` setStrokeColor : String -> IO () `*/
const setStrokeColor = (color : string) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = color, null))

/**` setMiterLimit : Number -> IO () `*/
const setMiterLimit = (limit : number) : IO <null> =>
	IO (() => (Ψ.ctx.miterLimit = limit, null))

/**` setShadowBlurAmount : Number -> IO () `*/
const setShadowBlurAmount = (amount : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowBlur = amount, null))

/**` setAlpha : Number -> IO () `*/
const setAlpha = (alpha : number) : IO <null> =>
	IO (() => (Ψ.ctx.globalAlpha = alpha, null))

/**` setLineCap : LineCap -> IO () `*/
const setLineCap = (linecap : LineCap) : IO <null> =>
	IO (() => (
		Ψ.ctx.lineCap =
			linecap === LineCap.Butt   ? 'butt'   :
			linecap === LineCap.Round  ? 'round'  :
			linecap === LineCap.Square ? 'square' : never, null
	))

/**` setLineJoin : LineJoin -> IO () `*/
const setLineJoin = (linejoin : LineJoin) : IO <null> =>
	IO (() => (
		Ψ.ctx.lineJoin =
			linejoin === LineJoin.Round ? 'round' :
			linejoin === LineJoin.Bevel ? 'bevel' :
			linejoin === LineJoin.Miter ? 'miter' : never, null
	))

/**` setTextAlign : TextAlign -> IO () `*/
const setTextAlign = (alignment : TextAlign) : IO <null> =>
	IO (() => (
		Ψ.ctx.textAlign =
			alignment === TextAlign.Center    ? 'center' :
			alignment === TextAlign.End       ? 'end'    :
			alignment === TextAlign.Leftside  ? 'left'   :
			alignment === TextAlign.Rightside ? 'right'  :
			alignment === TextAlign.Start     ? 'start'  : never, null
	))

/**` setTextBaseline : TextBaseline -> IO () `*/
const setTextBaseline = (baseline : TextBaseline) : IO <null> =>
	IO (() => (
		Ψ.ctx.textBaseline =
			baseline === TextBaseline.Alphabetic  ? 'alphabetic'  :
			baseline === TextBaseline.Bottom      ? 'bottom'      :
			baseline === TextBaseline.Hanging     ? 'hanging'     :
			baseline === TextBaseline.Ideographic ? 'ideographic' :
			baseline === TextBaseline.Middle      ? 'middle'      :
			baseline === TextBaseline.Top         ? 'top'         : never, null
	))

/**` setComposition : Composition -> IO () `*/
const setComposition = (composition : Composition) : IO <null> =>
	IO (() => (
		Ψ.ctx.globalCompositeOperation =
			composition === Composition.SourceOver      ? 'source-over'      :
			composition === Composition.SourceIn        ? 'source-in'        :
			composition === Composition.SourceOut       ? 'source-out'       :
			composition === Composition.SourceAtop      ? 'source-atop'      :
			composition === Composition.DestinationOver ? 'destination-over' :
			composition === Composition.DestinationIn   ? 'destination-in'   :
			composition === Composition.DestinationOut  ? 'destination-out'  :
			composition === Composition.DestinationAtop ? 'destination-atop' :
			composition === Composition.Lighter         ? 'lighter'          :
			composition === Composition.Copy            ? 'copy'             :
			composition === Composition.Xor             ? 'xor'              :
			composition === Composition.Multiply        ? 'multiply'         :
			composition === Composition.Screen          ? 'screen'           :
			composition === Composition.Overlay         ? 'overlay'          :
			composition === Composition.Darken          ? 'darken'           :
			composition === Composition.Lighten         ? 'lighten'          :
			composition === Composition.ColorDodge      ? 'color-dodge'      :
			composition === Composition.ColorBurn       ? 'color-burn'       :
			composition === Composition.HardLight       ? 'hard-light'       :
			composition === Composition.SoftLight       ? 'soft-light'       :
			composition === Composition.Difference      ? 'difference'       :
			composition === Composition.Exclusion       ? 'exclusion'        :
			composition === Composition.Hue             ? 'hue'              :
			composition === Composition.Saturation      ? 'saturation'       :
			composition === Composition.Color           ? 'color'            :
			composition === Composition.Luminosity      ? 'luminosity'       : never, null
	))

/**` setToCenterText : IO () `*/
const setToCenterText : IO <null> =
	IO (() => (Ψ.ctx.textAlign = 'center', Ψ.ctx.textBaseline = 'middle', null))

/**` setToDefaultText : IO () `*/
const setToDefaultText : IO <null> =
	IO (() => (Ψ.ctx.textAlign = 'start', Ψ.ctx.textBaseline = 'alphabetic', null))

/**` setLayer : Number -> IO () `*/
const setLayer = (index : number) : IO <null> =>
	IO (() =>
		index >= 0 && index < Ψ.ctxs.length && Number.isInteger (index)
			? (Ψ.ctx = Ψ.ctxs[index]!, null)
			: __MACRO_invalid_index_range('layer', index, Ψ.ctxs.length)
	)

/**` loadFont : String -> IO () `*/
const loadFont = (path : string) : IO <null> =>
	IO (() => (
		new FontFace(path.slice(path.lastIndexOf('/') + 1, path.lastIndexOf('.')), `url(${path})`)
			.load()
			.then((font : any) => (document as any).fonts.add(font))
			.catch(() => error (`'loadFont' could not load font at path: '${path}'`)),
		null
	))

/**` loadImage : String -> IO () `*/
const loadImage = (path : string) : IO <null> =>
	IO (() => {
		Ψ.image[path]          = new Image
		Ψ.image[path]!.src     = path
		Ψ.image[path]!.onerror = () => error (`'loadImage' could not load image at path: '${path}'`)
		return null
	})

/**` loadAudio : String -> IO () `*/
const loadAudio = (path : string) : IO <null> =>
	IO (() => {
		Ψ.audio[path]          = new Audio(path)
		Ψ.audio[path]!.onerror = () => error (`'loadAudio' could not load audio at path: '${path}'`)
		return null
	})

/**` setAudioTime : String -> Number -> IO () `*/
const setAudioTime = (path : string) => (time : number) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? time >= 0 && time <= Ψ.audio[path].duration
				? (Ψ.audio[path].currentTime = time, null)
				: error (
					`'setAudioTime' received '${time}' as an input; must be in ` +
					`interval [0, ${Ψ.audio[path].duration}] for audio file: '${path}'`
				)
			: __MACRO_nonexisting_audio_path__('setAudioTime', path)
	)

/**` resetAudio : String -> IO () `*/
const resetAudio = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? (Ψ.audio[path].pause(), Ψ.audio[path].currentTime = 0, null)
			: __MACRO_nonexisting_audio_path__('resetAudio', path)
	)

/**` playAudio : String -> IO () `*/
const playAudio = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? (Ψ.audio[path].play(), null)
			: __MACRO_nonexisting_audio_path__('playAudio', path)
	)

/**` pauseAudio : String -> IO () `*/
const pauseAudio = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? (Ψ.audio[path].pause(), null)
			: __MACRO_nonexisting_audio_path__ ('pauseAudio', path)
	)

/**` playSFX : String -> IO () `*/
const playSFX = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? ((Ψ.audio[path].cloneNode() as any).play(), null)
			: __MACRO_nonexisting_audio_path__('playSFX', path)
	)

/**` flush : IO () `*/
const flush : IO <null> =
	IO (() => (console.clear(), null))

/**` log : a -> IO () `*/
const log = <a>(message : a) : IO <null> =>
	IO (() => (console.log(message), null))

/**` warning : a -> IO () `*/
const warning = <a>(message : a) : IO <null> =>
	IO (() => (console.warn(message), null))

/**` debug : a -> IO () `*/
const debug = <a>(message : a) : IO <null> =>
	IO (() => (console.warn(message), null))

/**` delayedLog : Number -> a -> IO () `*/
const delayedLog = (count : number) => <a>(message : a) : IO <null> =>
	IO (() =>
		--Ψ.debugCounter < 0
			? (Ψ.debugCounter = count, console.debug(message), null)
			: null
	)

/**` countLog : String -> IO () `*/
const countLog = (identifier : string) : IO <null> =>
	IO (() => (console.count(identifier), null))

/**` resetCountLog : String -> IO () `*/
const resetCountLog = (identifier : string) : IO <null> =>
	IO (() => (console.countReset(identifier), null))

/**` saveCanvasState : IO () `*/
const saveCanvasState : IO <null> =
	IO (() => (Ψ.ctx.save(), null))

/**` restoreCanvasState : IO () `*/
const restoreCanvasState : IO <null> =
	IO (() => (Ψ.ctx.restore(), null))

/**` requestPointerLock : IO () `*/
const requestPointerLock : IO <null> =
	IO (() => (onmouseup = () => Ψ.isPointerLocked || Ψ.ctxs[0].canvas.requestPointerLock(), null))

/**` deactivatePointerLock : IO () `*/
const deactivatePointerLock : IO <null> =
	IO (() => (document.exitPointerLock(), onmouseup = null))

/**` queueIO : IO a -> IO () `*/
const queueIO = <a>(io : IO <a>) : IO <null> =>
	IO (() => {
		for (const k in Ψ.keyboard)     Ψ.keyboard[k as KeyboardKey] = relaxVertical (Ψ.keyboard[k as KeyboardKey])
		for (const i in Ψ.mouseButtons) Ψ.mouseButtons[i]            = relaxVertical (Ψ.mouseButtons[i]!)
		Ψ.mouseDX     = Ψ.mouseDY = 0
		Ψ.mouseScroll = Vertical.Rest
		Ψ.isResized   = false
		requestAnimationFrame(io.effect)
		return null
	})

/********************************************************************************************************************************/

onload = () =>
{
	Ψ.ctxs = Array.from(document.querySelectorAll('canvas')).map(x => x.getContext('2d')!)
	Ψ.ctx  = Ψ.ctxs[0]!

	onresize    = () => (clearTimeout(Ψ.resizeID), Ψ.resizeID = setTimeout(() => Ψ.isResized = true, 250))
	onmousedown = ev => Ψ.mouseButtons[ev.button]          = Vertical.Downward
	onmouseup   = ev => Ψ.mouseButtons[ev.button]          = Vertical.Upward
	onkeyup     = ev => Ψ.keyboard[ev.code as KeyboardKey] = Vertical.Upward
	onkeydown   = ev =>
		Ψ.keyboard[ev.code as KeyboardKey] =
			ev.repeat
				? Ψ.keyboard[ev.code as KeyboardKey]
				: Vertical.Downward
	onwheel     = ev =>
		Ψ.mouseScroll =
			ev.deltaY < 0 ? Vertical.Up   :
			ev.deltaY > 0 ? Vertical.Down : Vertical.Rest
	onmousemove = ev =>
	{
		Ψ.mouseWX = ev.x,
		Ψ.mouseWY = ev.y,
		Ψ.mouseCX = ev.clientX - Ψ.ctxs[0].canvas.offsetLeft,
		Ψ.mouseCY = ev.clientY - Ψ.ctxs[0].canvas.offsetTop,
		Ψ.mouseSX = ev.screenX,
		Ψ.mouseSY = ev.screenY,
		Ψ.mouseDX  = ev.movementX,
		Ψ.mouseDY  = ev.movementY
	}

	document.onpointerlockchange = () => Ψ.isPointerLocked = document.pointerLockElement === Ψ.ctxs[0].canvas
	Ψ.ctxs[0].canvas.setAttribute("style", "background:white")

	if (typeof main !== 'undefined') main.effect ()
}
