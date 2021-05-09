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

/********************************************************************************************************************************/
// Typeclasses //

type Pipe <a> = a & { pipe : <b>(morphism : (value : a) => b) => b       }
type Eq   <a> = a & { eq   :    (value    :                a) => boolean }

/********************************************************************************************************************************/
// Algebraic Data Types //

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

interface Array <T>
{
	/**` [a].pipe : ([a] -> b) -> b `*/
	pipe : <b>(morphism : (array : Array <T>) => b) => b
}

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

type Mapping <a, b> =
	{
		variation : 'Mapping'

		/**` (Mapping a b).codomain : (Eq a, Eq b) => a -> b `*/
		codomain : (domain : Eq <a>) => Eq <b>

		/**` (Mapping a b).domain : (Eq a, Eq b) => b -> a `*/
		domain : (codomain : Eq <b>) => Eq <a>

		/**` (Mapping a b).pipe : (Mapping a b -> c) -> c `*/
		pipe : <c>(morphism : (mapping : Mapping <a, b>) => c) => c
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
const greater = (x : number) => (y : number) : boolean => y >= x

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

/**` isIn : Number -> Number -> Number -> Boolean `*/
const isIn = (lower : number) => (upper : number) => (n : number) : boolean => lower < n && n < upper

/**` isInRect : Number -> Number -> Number -> Number -> Number -> Number -> Boolean `*/
const isInRect = (rx : number) => (ry : number) => (rw : number) => (rh : number) => (x : number) => (y : number) : boolean =>
	rx < x && x < rx + rw &&
	ry < y && y < ry + rh

/**` show : a -> String `*/
const show = <a>(value : a) : string => `${value}`

/**` error : String -> a `*/
const error = (message : string) : any => { throw message }

/**` warn : String -> a -> a `*/
const warn = (message : string) => <a>(value : a) : a => (console.warn(message), value)

/********************************************************************************************************************************/
// Implementation of Algebraic Data Type Constructors //

Boolean.prototype.pipe =
Number .prototype.pipe =
String .prototype.pipe = (Array.prototype.pipe = function (f) { return f (this as any) }) as any
Boolean.prototype.eq   =
Number .prototype.eq   = (String.prototype.eq  = function (x) { return this === x      }) as any

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

/**` Mapping : (Eq a) => (...Pair a b) -> Mapping a b `*/
const Mapping = <a, b>(...pairs : Array <[Eq <a>, Eq <b>]>) : Mapping <a, b> =>
	({
		variation : 'Mapping',
		codomain  : x => (
			pairs .find (p => p[0] .eq (x as Eq <a>))
				?? error (`'(Mapping).codomain' was non-exhaustive; no corresponding codomain for value '${x}'`)
		)[1],
		domain    : x => (
			pairs .find (p => p[1] .eq (x as Eq <b>))
				?? error (`'(Mapping).domain' was non-exhaustive; no corresponding domain for value '${x}'`)
		)[0],
		pipe (f) { return f (this) }
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
	IO (() => (ios .forEach (io => io.effect ()), null))

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
	List (...str)

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
		Vector2 (v.x - w.x, v.y - w.y)

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
		Vector3 (v.x - w.x, v.y - w.y, v.z - w.z)

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
		Vector4 (v.x - w.x, v.y - w.y, v.z - w.z, v.w - w.w)

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
	export const fromBasis = (i : Vector4, j : Vector4, k : Vector4, l : Vector4) : Matrix4 =>
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
	vertical === Vertical.Upward   || vertical === Vertical.Up  ?  1 :
	0

/**` mappingLineCapToHTML5 : Mapping LineCap CanvasLineCap `*/
const mappingLineCapToHTML5 : Mapping <LineCap, CanvasLineCap> =
	Mapping <LineCap, CanvasLineCap> (
		[LineCap.Butt   , 'butt'  ],
		[LineCap.Round  , 'round' ],
		[LineCap.Square , 'square']
	)

/**` mappingLineJoinToHTML5 : Mapping LineJoin CanvasLineJoin `*/
const mappingLineJoinToHTML5 : Mapping <LineJoin, CanvasLineJoin> =
	Mapping <LineJoin, CanvasLineJoin> (
		[LineJoin.Round , 'round'],
		[LineJoin.Bevel , 'bevel'],
		[LineJoin.Miter , 'miter']
	)

/**` mappingTextAlignToHTML5 : Mapping TextAlign CanvasTextAlign `*/
const mappingTextAlignToHTML5 : Mapping <TextAlign, CanvasTextAlign> =
	Mapping <TextAlign, CanvasTextAlign> (
		[TextAlign.Center    , 'center'],
		[TextAlign.End       , 'end'   ],
		[TextAlign.Leftside  , 'left'  ],
		[TextAlign.Rightside , 'right' ],
		[TextAlign.Start     , 'start' ]
	)

/**` mappingTextBaselineToHTML5 : Mapping TextBaseline CanvasTextBaseline `*/
const mappingTextBaselineToHTML5 : Mapping <TextBaseline, CanvasTextBaseline> =
	Mapping <TextBaseline, CanvasTextBaseline> (
		[TextBaseline.Alphabetic  , 'alphabetic' ],
		[TextBaseline.Bottom      , 'bottom'     ],
		[TextBaseline.Hanging     , 'hanging'    ],
		[TextBaseline.Ideographic , 'ideographic'],
		[TextBaseline.Middle      , 'middle'     ],
		[TextBaseline.Top         , 'top'        ]
	)

/**` mappingCompositionToHTML5 : Mapping Composition String `*/
const mappingCompositionToHTML5 : Mapping <Composition, string> =
	Mapping <Composition, string> (
		[Composition.SourceOver      , 'source-over'     ],
		[Composition.SourceIn        , 'source-in'       ],
		[Composition.SourceOut       , 'source-out'      ],
		[Composition.SourceAtop      , 'source-atop'     ],
		[Composition.DestinationOver , 'destination-over'],
		[Composition.DestinationIn   , 'destination-in'  ],
		[Composition.DestinationOut  , 'destination-out' ],
		[Composition.DestinationAtop , 'destination-atop'],
		[Composition.Lighter         , 'lighter'         ],
		[Composition.Copy            , 'copy'            ],
		[Composition.Xor             , 'xor'             ],
		[Composition.Multiply        , 'multiply'        ],
		[Composition.Screen          , 'screen'          ],
		[Composition.Overlay         , 'overlay'         ],
		[Composition.Darken          , 'darken'          ],
		[Composition.Lighten         , 'lighten'         ],
		[Composition.ColorDodge      , 'color-dodge'     ],
		[Composition.ColorBurn       , 'color-burn'      ],
		[Composition.HardLight       , 'hard-light'      ],
		[Composition.SoftLight       , 'soft-light'      ],
		[Composition.Difference      , 'difference'      ],
		[Composition.Exclusion       , 'exclusion'       ],
		[Composition.Hue             , 'hue'             ],
		[Composition.Saturation      , 'saturation'      ],
		[Composition.Color           , 'color'           ],
		[Composition.Luminosity      , 'luminosity'      ]
	)

/********************************************************************************************************************************/
// Constants and Micro-Functions for Multiple Algebraic Data Types //

/**` sequenceIOs : List (IO a) -> IO (List a) `*/
const sequenceIOs = <a>(ios : List <IO <a>>) : IO <List <a>> =>
	IO (() => ios .fmap (io => io.effect ()))

/**` executeIOs : List (IO a) -> IO a `*/
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

/********************************************************************************************************************************/
// Do Notation //

const Do =
	{
		IO      : IO        <     {}> (() => Object.create(null)),
		Process : Process   <any, {}> (s  => Pair (s, Object.create(null))),
		List    : singleton <     {}> (Object.create(null)),
		Maybe   : Just      <     {}> (Object.create(null))
	}

/********************************************************************************************************************************/
// IO Interfacing //

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

const Ψ =
	{
		MUTABLE         : {} as any,
		ctxs            : [] as Array <CanvasRenderingContext2D>,
		ctx             : undefined as unknown as CanvasRenderingContext2D,
		resizeID        : undefined as unknown as number,
		isResized       : false,
		isPointerLocked : false,
		seed            : (Math.random() - 0.5) * Date.now(),
		debugCounter    : 0,
		image           : Object.create(null) as { [x : string] : HTMLImageElement },
		audio           : Object.create(null) as { [x : string] : HTMLAudioElement },
		mouseScreenX    : 0, mouseScreenY : 0,
		mouseWindowX    : 0, mouseWindowY : 0,
		mouseCanvasX    : 0, mouseCanvasY : 0,
		mouseDeltaX     : 0, mouseDeltaY  : 0,
		mouseScroll     : Vertical.Rest,
		mouseButtons    : Array(5).fill(Vertical.Up) as [Vertical, Vertical, Vertical, Vertical, Vertical],
		keyboard        :
			keyboardKeysArray.reduce(
				($, k) => ({ ...$, [k] : Vertical.Up }),
				Object.create (null)
			) as { [x in KeyboardKey] : Vertical }
	}

namespace I
{
	/**` I.n_mouseScreenX : IO Number `*/
	export const n_mouseScreenX : IO <number> = IO (() => Ψ.mouseScreenX / Ψ.ctx.canvas.width)

	/**` I.n_mouseScreenY : IO Number `*/
	export const n_mouseScreenY : IO <number> = IO (() => Ψ.mouseScreenY / Ψ.ctx.canvas.height)

	/**` I.n_mouseScreenXY : IO (Pair Number Number) `*/
	export const n_mouseScreenXY : IO <Pair <number, number>> =
		IO (() => Pair (Ψ.mouseScreenX / Ψ.ctx.canvas.width, Ψ.mouseScreenY / Ψ.ctx.canvas.height))

	/**` I.n_mouseScreenV2 : IO Vector2 `*/
	export const n_mouseScreenV2 : IO <Vector2> =
		IO (() => Vector2 (Ψ.mouseScreenX / Ψ.ctx.canvas.width, Ψ.mouseScreenY / Ψ.ctx.canvas.height))

	/**` I.n_mouseWindowX : IO Number `*/
	export const n_mouseWindowX : IO <number> = IO (() => Ψ.mouseWindowX / Ψ.ctx.canvas.width)

	/**` I.n_mouseWindowY : IO Number `*/
	export const n_mouseWindowY : IO <number> = IO (() => Ψ.mouseWindowY / Ψ.ctx.canvas.height)

	/**` I.n_mouseWindowXY : IO (Pair Number Number) `*/
	export const n_mouseWindowXY : IO <Pair <number, number>> =
		IO (() => Pair (Ψ.mouseWindowX / Ψ.ctx.canvas.width, Ψ.mouseWindowY / Ψ.ctx.canvas.height))

	/**` I.n_mouseWindowV2 : IO Vector2 `*/
	export const n_mouseWindowV2 : IO <Vector2> =
		IO (() => Vector2 (Ψ.mouseWindowX / Ψ.ctx.canvas.width, Ψ.mouseWindowY / Ψ.ctx.canvas.height))

	/**` I.n_mouseCanvasX : IO Number `*/
	export const n_mouseCanvasX : IO <number> = IO (() => Ψ.mouseCanvasX / Ψ.ctx.canvas.width)

	/**` I.n_mouseCanvasY : IO Number `*/
	export const n_mouseCanvasY : IO <number> = IO (() => Ψ.mouseCanvasY / Ψ.ctx.canvas.height)

	/**` I.n_mouseCanvasXY : IO (Pair Number Number) `*/
	export const n_mouseCanvasXY : IO <Pair <number, number>> =
		IO (() => Pair (Ψ.mouseCanvasX / Ψ.ctx.canvas.width, Ψ.mouseCanvasY / Ψ.ctx.canvas.height))

	/**` I.n_mouseCanvasV2 : IO Vector2 `*/
	export const n_mouseCanvasV2 : IO <Vector2> =
		IO (() => Vector2 (Ψ.mouseCanvasX / Ψ.ctx.canvas.width, Ψ.mouseCanvasY / Ψ.ctx.canvas.height))

	/**` I.n_mouseDeltaX : IO Number `*/
	export const n_mouseDeltaX : IO <number> = IO (() => Ψ.mouseDeltaX / Ψ.ctx.canvas.width)

	/**` I.n_mouseDeltaY : IO Number `*/
	export const n_mouseDeltaY : IO <number> = IO (() => Ψ.mouseDeltaY / Ψ.ctx.canvas.height)

	/**` I.n_mouseDeltaXY : IO (Pair Number Number) `*/
	export const n_mouseDeltaXY : IO <Pair <number, number>> =
		IO (() => Pair (Ψ.mouseDeltaX / Ψ.ctx.canvas.width, Ψ.mouseDeltaY / Ψ.ctx.canvas.height))

	/**` I.n_mouseDeltaV2 : IO Vector2 `*/
	export const n_mouseDeltaV2 : IO <Vector2> =
		IO (() => Vector2 (Ψ.mouseDeltaX / Ψ.ctx.canvas.width, Ψ.mouseDeltaY / Ψ.ctx.canvas.height))

	/**` I.n_textW : String -> IO Number `*/
	export const n_textW = (text : string) : IO <number> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Math.abs (m.actualBoundingBoxLeft) + Math.abs (m.actualBoundingBoxRight) / Ψ.ctx.canvas.width
		})

	/**` I.n_textH : String -> IO Number `*/
	export const n_textH = (text : string) : IO <number> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Math.abs (m.actualBoundingBoxAscent) + Math.abs (m.actualBoundingBoxDescent) / Ψ.ctx.canvas.height
		})

	/**` I.n_textWH : String -> IO (Pair Number Number) `*/
	export const n_textWH = (text : string) : IO <Pair <number, number>> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Pair (
				(Math.abs (m.actualBoundingBoxLeft)   + Math.abs (m.actualBoundingBoxRight))   / Ψ.ctx.canvas.width,
				(Math.abs (m.actualBoundingBoxAscent) + Math.abs (m.actualBoundingBoxDescent)) / Ψ.ctx.canvas.height
			)
		})

	/**` I.n_textV2 : String -> IO Vector2 `*/
	export const n_textV2 = (text : string) : IO <Vector2> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Vector2 (
				(Math.abs (m.actualBoundingBoxLeft)   + Math.abs (m.actualBoundingBoxRight))   / Ψ.ctx.canvas.width,
				(Math.abs (m.actualBoundingBoxAscent) + Math.abs (m.actualBoundingBoxDescent)) / Ψ.ctx.canvas.height
			)
		})

	/**` I.n_lineThickness : IO Number `*/
	export const n_lineThickness : IO <number> = IO (() => Ψ.ctx.lineWidth / Ψ.ctx.canvas.width)

	/**` I.n_lineDashPattern : IO (List Number) `*/
	export const n_lineDashPattern : IO <List <number>> =
		IO (() => List (...Ψ.ctx.getLineDash() .map (x => x / Ψ.ctx.canvas.width)))

	/**` I.n_lineDashOffset : IO Number `*/
	export const n_lineDashOffset : IO <number> = IO (() => Ψ.ctx.lineDashOffset / Ψ.ctx.canvas.width)

	/**` I.n_fontSize : IO Number `*/
	export const n_fontSize : IO <number> = IO (() => parseFloat (Ψ.ctx.font) / Ψ.ctx.canvas.width)

	/**` I.n_shadowX : IO Number `*/
	export const n_shadowX : IO <number> = IO (() => Ψ.ctx.shadowOffsetX / Ψ.ctx.canvas.height)

	/**` I.n_shadowY : IO Number `*/
	export const n_shadowY : IO <number> = IO (() => Ψ.ctx.shadowOffsetY / Ψ.ctx.canvas.height)

	/**` I.n_shadowXY : IO (Pair Number Number) `*/
	export const n_shadowXY : IO <Pair <number, number>> =
		IO (() => Pair (Ψ.ctx.shadowOffsetX / Ψ.ctx.canvas.width, Ψ.ctx.shadowOffsetY / Ψ.ctx.canvas.height))

	/**` I.n_shadowV2 : IO Vector2 `*/
	export const n_shadowV2 : IO <Vector2> =
		IO (() => Vector2 (Ψ.ctx.shadowOffsetX / Ψ.ctx.canvas.width, Ψ.ctx.shadowOffsetY / Ψ.ctx.canvas.height))

	/**` I.n_isInEvenOddPathXY : Number -> : Number -> IO Boolean `*/
	export const n_isInEvenOddPathXY = (x : number) => (y : number) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInPath(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height, 'evenodd'))

	/**` I.n_isInEvenOddPathV2 : Vector2 -> IO Boolean `*/
	export const n_isInEvenOddPathV2 = (v : Vector2) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInPath(v.x / Ψ.ctx.canvas.width, v.y / Ψ.ctx.canvas.height, 'evenodd'))

	/**` I.n_isInNonZeroPathXY : Number -> : Number -> IO Boolean `*/
	export const n_isInNonZeroPathXY = (x : number) => (y : number) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInPath(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height, 'nonzero'))

	/**` I.n_isInNonZeroPathV2 : Vector2 -> IO Boolean `*/
	export const n_isInNonZeroPathV2 = (v : Vector2) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInPath(v.x / Ψ.ctx.canvas.width, v.y / Ψ.ctx.canvas.height, 'nonzero'))

	/**` I.n_isInStrokeXY : Number -> : Number -> IO Boolean `*/
	export const n_isInStrokeXY = (x : number) => (y : number) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInStroke(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height))

	/**` I.n_isInStrokeV2 : Vector2 -> IO Boolean `*/
	export const n_isInStrokeV2 = (v : Vector2) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInStroke(v.x / Ψ.ctx.canvas.width, v.y / Ψ.ctx.canvas.height))

	/**` I.n_matrix : IO Matrix3 `*/
	export const n_matrix : IO <Matrix3> =
		IO (() => {
			const m = Ψ.ctx.getTransform()
			return Matrix3 (m.a, m.c, m.e / Ψ.ctx.canvas.width, m.b, m.d, m.f / Ψ.ctx.canvas.height, 0, 0, 1)
		})

	/**` I.n_wasd : IO Vector2 `*/
	export const n_wasd : IO <Vector2> =
		IO (() => {
			const x = bit (isDown (Ψ.keyboard.KeyD)) - bit (isDown (Ψ.keyboard.KeyA))
			const y = bit (isDown (Ψ.keyboard.KeyS)) - bit (isDown (Ψ.keyboard.KeyW))
			const l = x ** 2 + y ** 2
			return l === 0
				? V2.zero
				: l === 1
					? Vector2 (x, y)
					: Vector2 (x * invSqrt2, y * invSqrt2)
		})

	/**` I.n_arrows : IO Vector2 `*/
	export const n_arrows : IO <Vector2> =
		IO (() => {
			const x = bit (isDown (Ψ.keyboard.ArrowRight)) - bit (isDown (Ψ.keyboard.ArrowLeft))
			const y = bit (isDown (Ψ.keyboard.ArrowDown))  - bit (isDown (Ψ.keyboard.ArrowUp))
			const l = x ** 2 + y ** 2
			return l === 0
				? V2.zero
				: l === 1
					? Vector2 (x, y)
					: Vector2 (x * invSqrt2, y * invSqrt2)
		})

	/**` I.n_imageW : String -> IO Number `*/
	export const n_imageW = (path : string) : IO <number> =>
		IO (() =>
			Ψ.image[path]
				? Ψ.image[path].width / Ψ.ctx.canvas.width
				: error (`'I.n_imageW' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.n_imageH : String -> IO Number `*/
	export const n_imageH = (path : string) : IO <number> =>
		IO (() =>
			Ψ.image[path]
				? Ψ.image[path].height / Ψ.ctx.canvas.height
				: error (`'I.n_imageH' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.n_imageWH : String -> IO (Pair Number Number) `*/
	export const n_imageWH = (path : string) : IO <Pair <number, number>> =>
		IO (() =>
			Ψ.image[path]
				? Pair (Ψ.image[path].width / Ψ.ctx.canvas.width, Ψ.image[path].height / Ψ.ctx.canvas.height)
				: error (`'I.n_imageWH' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.n_imageV2 : String -> IO Vector2 `*/
	export const n_imageV2 = (path : string) : IO <Vector2> =>
		IO (() =>
			Ψ.image[path]
				? Vector2 (Ψ.image[path].width / Ψ.ctx.canvas.width, Ψ.image[path].height / Ψ.ctx.canvas.height)
				: error (`'I.n_imageV2' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.time : IO Number `*/
	export const time : IO <number> = IO (Date.now)

	/**` I.isWindowResized : IO Boolean `*/
	export const isWindowResized : IO <boolean> = IO (() => Ψ.isResized)

	/**` I.isPointerLocked : IO Boolean `*/
	export const isPointerLocked : IO <boolean> = IO (() => Ψ.isPointerLocked)

	/**` I.seed : IO Number `*/
	export const seed : IO <number> = IO (() => Ψ.seed)

	/**` I.mouseScreenX : IO Number `*/
	export const mouseScreenX : IO <number> = IO (() => Ψ.mouseScreenX)

	/**` I.mouseScreenY : IO Number `*/
	export const mouseScreenY : IO <number> = IO (() => Ψ.mouseScreenY)

	/**` I.mouseScreenXY : IO (Pair Number Number) `*/
	export const mouseScreenXY : IO <Pair <number, number>> = IO (() => Pair (Ψ.mouseScreenX, Ψ.mouseScreenY))

	/**` I.mouseScreenV2 : IO Vector2 `*/
	export const mouseScreenV2 : IO <Vector2> = IO (() => Vector2 (Ψ.mouseScreenX, Ψ.mouseScreenY))

	/**` I.mouseWindowX : IO Number `*/
	export const mouseWindowX : IO <number> = IO (() => Ψ.mouseWindowX)

	/**` I.mouseWindowY : IO Number `*/
	export const mouseWindowY : IO <number> = IO (() => Ψ.mouseWindowY)

	/**` I.mouseWindowXY : IO (Pair Number Number) `*/
	export const mouseWindowXY : IO <Pair <number, number>> = IO (() => Pair (Ψ.mouseWindowX, Ψ.mouseWindowY))

	/**` I.mouseWindowV2 : IO Vector2 `*/
	export const mouseWindowV2 : IO <Vector2> = IO (() => Vector2 (Ψ.mouseWindowX, Ψ.mouseWindowY))

	/**` I.mouseCanvasX : IO Number `*/
	export const mouseCanvasX : IO <number> = IO (() => Ψ.mouseCanvasX)

	/**` I.mouseCanvasY : IO Number `*/
	export const mouseCanvasY : IO <number> = IO (() => Ψ.mouseCanvasY)

	/**` I.mouseCanvasXY : IO (Pair Number Number) `*/
	export const mouseCanvasXY : IO <Pair <number, number>> = IO (() => Pair (Ψ.mouseCanvasX, Ψ.mouseCanvasY))

	/**` I.mouseCanvasV2 : IO Vector2 `*/
	export const mouseCanvasV2 : IO <Vector2> = IO (() => Vector2 (Ψ.mouseCanvasX, Ψ.mouseCanvasY))

	/**` I.mouseDeltaX : IO Number `*/
	export const mouseDeltaX : IO <number> = IO (() => Ψ.mouseDeltaX)

	/**` I.mouseDeltaY : IO Number `*/
	export const mouseDeltaY : IO <number> = IO (() => Ψ.mouseDeltaY)

	/**` I.mouseDeltaXY : IO (Pair Number Number) `*/
	export const mouseDeltaXY : IO <Pair <number, number>> = IO (() => Pair (Ψ.mouseDeltaX, Ψ.mouseDeltaY))

	/**` I.mouseDeltaV2 : IO Vector2 `*/
	export const mouseDeltaV2 : IO <Vector2> = IO (() => Vector2 (Ψ.mouseDeltaX, Ψ.mouseDeltaY))

	/**` I.mouseScroll : IO Vertical `*/
	export const mouseScroll : IO <Vertical> = IO (() => Ψ.mouseScroll)

	/**` I.mouseButtonLeft : IO Vertical `*/
	export const mouseButtonLeft : IO <Vertical> = IO (() => Ψ.mouseButtons[0])

	/**` I.mouseButtonMid : IO Vertical `*/
	export const mouseButtonMid : IO <Vertical> = IO (() => Ψ.mouseButtons[1])

	/**` I.mouseButtonRight : IO Vertical `*/
	export const mouseButtonRight : IO <Vertical> = IO (() => Ψ.mouseButtons[2])

	/**` I.mouseButtonA : IO Vertical `*/
	export const mouseButtonA : IO <Vertical> = IO (() => Ψ.mouseButtons[3])

	/**` I.mouseButtonB : IO Vertical `*/
	export const mouseButtonB : IO <Vertical> = IO (() => Ψ.mouseButtons[4])

	/**` I.key : KeyboardKey -> IO Vertical `*/
	export const key = (keyname : KeyboardKey) : IO <Vertical> => IO (() => Ψ.keyboard[keyname])

	/**` I.screenW : IO Number `*/
	export const screenW : IO <number> = IO (() => screen.width)

	/**` I.screenH : IO Number `*/
	export const screenH : IO <number> = IO (() => screen.height)

	/**` I.screenWH : IO (Pair Number Number) `*/
	export const screenWH : IO <Pair <number, number>> = IO (() => Pair (screen.width, screen.height))

	/**` I.screenV2 : IO Vector2 `*/
	export const screenV2 : IO <Vector2> = IO (() => Vector2 (screen.width, screen.height))

	/**` I.windowW : IO Number `*/
	export const windowW : IO <number> = IO (() => innerWidth)

	/**` I.windowH : IO Number `*/
	export const windowH : IO <number> = IO (() => innerHeight)

	/**` I.windowWH : IO (Pair Number Number) `*/
	export const windowWH : IO <Pair <number, number>> = IO (() => Pair (innerWidth, innerHeight))

	/**` I.windowV2 : IO Vector2 `*/
	export const windowV2 : IO <Vector2> = IO (() => Vector2 (innerWidth, innerHeight))

	/**` I.canvasW : IO Number `*/
	export const canvasW : IO <number> = IO (() => Ψ.ctx.canvas.width)

	/**` I.canvasH : IO Number `*/
	export const canvasH : IO <number> = IO (() => Ψ.ctx.canvas.height)

	/**` I.canvasWH : IO (Pair Number Number) `*/
	export const canvasWH : IO <Pair <number, number>> = IO (() => Pair (Ψ.ctx.canvas.width, Ψ.ctx.canvas.height))

	/**` I.canvasV2 : IO Vector2 `*/
	export const canvasV2 : IO <Vector2> = IO (() => Vector2 (Ψ.ctx.canvas.width, Ψ.ctx.canvas.height))

	/**` I.layer : IO Number `*/
	export const layer : IO <number> = IO (() => Ψ.ctxs .findIndex (ctx => ctx === Ψ.ctx))

	/**` I.textW : String -> IO Number `*/
	export const textW = (text : string) : IO <number> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Math.abs (m.actualBoundingBoxLeft) + Math.abs (m.actualBoundingBoxRight)
		})

	/**` I.textH : String -> IO Number `*/
	export const textH = (text : string) : IO <number> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Math.abs (m.actualBoundingBoxAscent) + Math.abs (m.actualBoundingBoxDescent)
		})

	/**` I.textWH : String -> IO (Pair Number Number) `*/
	export const textWH = (text : string) : IO <Pair <number, number>> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Pair (
				Math.abs (m.actualBoundingBoxLeft)   + Math.abs (m.actualBoundingBoxRight),
				Math.abs (m.actualBoundingBoxAscent) + Math.abs (m.actualBoundingBoxDescent)
			)
		})

	/**` I.textV2 : String -> IO Vector2 `*/
	export const textV2 = (text : string) : IO <Vector2> =>
		IO (() => {
			const m = Ψ.ctx.measureText(text)
			return Vector2 (
				Math.abs (m.actualBoundingBoxLeft)   + Math.abs (m.actualBoundingBoxRight),
				Math.abs (m.actualBoundingBoxAscent) + Math.abs (m.actualBoundingBoxDescent)
			)
		})

	/**` I.lineThickness : IO Number `*/
	export const lineThickness : IO <number> = IO (() => Ψ.ctx.lineWidth)

	/**` I.lineDashPattern : IO (List Number) `*/
	export const lineDashPattern : IO <List <number>> = IO (() => List (...Ψ.ctx.getLineDash()))

	/**` I.lineDashOffset : IO Number `*/
	export const lineDashOffset : IO <number> = IO (() => Ψ.ctx.lineDashOffset)

	/**` I.miterLimit : IO Number `*/
	export const miterLimit : IO <number> = IO (() => Ψ.ctx.miterLimit)

	/**` I.font : IO String `*/
	export const font : IO <string> = IO (() => Ψ.ctx.font)

	/**` I.fontSize : IO Number `*/
	export const fontSize : IO <number> = IO (() => parseFloat (Ψ.ctx.font))

	/**` I.fontFamily : IO String `*/
	export const fontFamily : IO <string> = IO (() => Ψ.ctx.font .slice (Ψ.ctx.font .indexOf (' ') + 1))

	/**` I.shadowBlurAmount : IO Number `*/
	export const shadowBlurAmount : IO <number> = IO (() => Ψ.ctx.shadowBlur)

	/**` I.shadowColor : IO String `*/
	export const shadowColor : IO <string> = IO (() => Ψ.ctx.shadowColor)

	/**` I.shadowX : IO Number `*/
	export const shadowX : IO <number> = IO (() => Ψ.ctx.shadowOffsetX)

	/**` I.shadowY : IO Number `*/
	export const shadowY : IO <number> = IO (() => Ψ.ctx.shadowOffsetY)

	/**` I.shadowXY : IO (Pair Number Number) `*/
	export const shadowXY : IO <Pair <number, number>> = IO (() => Pair (Ψ.ctx.shadowOffsetX, Ψ.ctx.shadowOffsetY))

	/**` I.shadowV2 : IO Vector2 `*/
	export const shadowV2 : IO <Vector2> = IO (() => Vector2 (Ψ.ctx.shadowOffsetX, Ψ.ctx.shadowOffsetY))

	/**` I.isInEvenOddPathXY : Number -> : Number -> IO Boolean `*/
	export const isInEvenOddPathXY = (x : number) => (y : number) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInPath(x, y, 'evenodd'))

	/**` I.isInEvenOddPathV2 : Vector2 -> IO Boolean `*/
	export const isInEvenOddPathV2 = (v : Vector2) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(v.x, v.y, 'evenodd'))

	/**` I.isInNonZeroPathXY : Number -> : Number -> IO Boolean `*/
	export const isInNonZeroPathXY = (x : number) => (y : number) : IO <boolean> =>
		IO (() => Ψ.ctx.isPointInPath(x, y, 'nonzero'))

	/**` I.isInNonZeroPathV2 : Vector2 -> IO Boolean `*/
	export const isInNonZeroPathV2 = (v : Vector2) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(v.x, v.y, 'nonzero'))

	/**` I.isInStrokeXY : Number -> : Number -> IO Boolean `*/
	export const isInStrokeXY = (x : number) => (y : number) : IO <boolean> => IO (() => Ψ.ctx.isPointInStroke(x, y))

	/**` I.isInStrokeV2 : Vector2 -> IO Boolean `*/
	export const isInStrokeV2 = (v : Vector2) : IO <boolean> => IO (() => Ψ.ctx.isPointInStroke(v.x, v.y))

	/**` I.matrix : IO Matrix3 `*/
	export const matrix : IO <Matrix3> =
		IO (() => {
			const m = Ψ.ctx.getTransform()
			return Matrix3 (m.a, m.c, m.e, m.b, m.d, m.f, 0, 0, 1)
		})

	/**` I.alpha : IO Number `*/
	export const alpha : IO <number> = IO (() => Ψ.ctx.globalAlpha)

	/**` I.lineCap : IO LineCap `*/
	export const lineCap : IO <LineCap> = IO (() => mappingLineCapToHTML5 .domain (Ψ.ctx.lineCap))

	/**` I.lineJoin : IO LineJoin `*/
	export const lineJoin : IO <LineJoin> = IO (() => mappingLineJoinToHTML5 .domain (Ψ.ctx.lineJoin))

	/**` I.textAlign : IO TextAlign `*/
	export const textAlign : IO <TextAlign> = IO (() => mappingTextAlignToHTML5 .domain (Ψ.ctx.textAlign))

	/**` I.textBaseline : IO TextBaseline `*/
	export const textBaseline : IO <TextBaseline> = IO (() => mappingTextBaselineToHTML5 .domain (Ψ.ctx.textBaseline))

	/**` I.composition : IO Composition `*/
	export const composition : IO <Composition> = IO (() => mappingCompositionToHTML5 .domain (Ψ.ctx.globalCompositeOperation))

	/**` I.wasd : IO Vector2 `*/
	export const wasd : IO <Vector2> =
		IO (() =>
			Vector2 (
				bit (isDown (Ψ.keyboard.KeyD)) - bit (isDown (Ψ.keyboard.KeyA)),
				bit (isDown (Ψ.keyboard.KeyS)) - bit (isDown (Ψ.keyboard.KeyW))
			)
		)

	/**` I.arrows : IO Vector2 `*/
	export const arrows : IO <Vector2> =
		IO (() =>
			Vector2 (
				bit (isDown (Ψ.keyboard.ArrowRight)) - bit (isDown (Ψ.keyboard.ArrowLeft)),
				bit (isDown (Ψ.keyboard.ArrowDown))  - bit (isDown (Ψ.keyboard.ArrowUp))
			)
		)

	/**` I.imageW : String -> IO Number `*/
	export const imageW = (path : string) : IO <number> =>
		IO (() =>
			Ψ.image[path]
				? Ψ.image[path].width
				: error (`'I.imageW' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.imageH : String -> IO Number `*/
	export const imageH = (path : string) : IO <number> =>
		IO (() =>
			Ψ.image[path]
				? Ψ.image[path].height
				: error (`'I.imageH' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.imageWH : String -> IO (Pair Number Number) `*/
	export const imageWH = (path : string) : IO <Pair <number, number>> =>
		IO (() =>
			Ψ.image[path]
				? Pair (Ψ.image[path].width, Ψ.image[path].height)
				: error (`'I.imageWH' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.imageV2 : String -> IO Vector2 `*/
	export const imageV2 = (path : string) : IO <Vector2> =>
		IO (() =>
			Ψ.image[path]
				? Vector2 (Ψ.image[path].width, Ψ.image[path].height)
				: error (`'I.imageV2' received an unloaded (possibly non-existing) image at path: ${path}`)
		)

	/**` I.audioDuration : String -> IO Number `*/
	export const audioDuration = (path : string) : IO <number> =>
		IO (() =>
			Ψ.audio[path]
				? Ψ.audio[path].duration
				: error (`'I.audioDuration' received an unloaded (possibly non-existing) audio at path: ${path}`)
		)

	/**` I.audioTime : String -> IO Number `*/
	export const audioTime = (path : string) : IO <number> =>
		IO (() =>
			Ψ.audio[path]
				? Ψ.audio[path].currentTime
				: error (`'I.audioTime' received an unloaded (possibly non-existing) audio at path: ${path}`)
		)
}

namespace O
{
	/**` O.n_setCanvasW : Number -> IO () `*/
	export const n_setCanvasW = (w : number) : IO <null> => IO (() => (Ψ.ctx.canvas.width = w * innerWidth, null))

	/**` O.n_setCanvasH : Number -> IO () `*/
	export const n_setCanvasH = (h : number) : IO <null> => IO (() => (Ψ.ctx.canvas.height = h * innerHeight, null))

	/**` O.n_setCanvasWH : Number -> Number -> IO () `*/
	export const n_setCanvasWH = (w : number) => (h : number) : IO <null> =>
		IO (() => (Ψ.ctx.canvas.width = w * innerWidth, Ψ.ctx.canvas.height = h * innerHeight, null))

	/**` O.n_setCanvasV2 : Vector2 -> IO () `*/
	export const n_setCanvasV2 = (wh : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.canvas.width = wh.x * innerWidth, Ψ.ctx.canvas.height = wh.y * innerHeight, null))

	/**` O.n_setLineThickness : Number -> IO () `*/
	export const n_setLineThickness = (thickness : number) : IO <null> =>
		IO (() => (Ψ.ctx.lineWidth = thickness * Ψ.ctx.canvas.width, null))

	/**` O.n_setLineDashPattern : List Number -> IO () `*/
	export const n_setLineDashPattern = (pattern : List <number>) : IO <null> =>
		IO (() => (Ψ.ctx.setLineDash(listToArray (pattern).map(x => x * Ψ.ctx.canvas.width)), null))

	/**` O.n_setLineDashOffset : Number -> IO () `*/
	export const n_setLineDashOffset = (offset : number) : IO <null> =>
		IO (() => (Ψ.ctx.lineDashOffset = offset * Ψ.ctx.canvas.width, null))

	/**` O.n_setFontSize : Number -> IO () `*/
	export const n_setFontSize = (size : number) : IO <null> =>
		IO (() => (Ψ.ctx.font = `${size * Ψ.ctx.canvas.width}px${Ψ.ctx.font.slice(Ψ.ctx.font.indexOf(' '))}`, null))

	/**` O.n_setShadowRGBA : (Number, Number, Number, Number) -> IO () `*/
	export const n_setShadowRGBA = (r : number, g : number, b : number, a : number) : IO <null> =>
		IO (() => (Ψ.ctx.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

	/**` O.n_setShadowX : Number -> IO () `*/
	export const n_setShadowX = (x : number) : IO <null> => IO (() => (Ψ.ctx.shadowOffsetX = x * Ψ.ctx.canvas.width, null))

	/**` O.n_setShadowY : Number -> IO () `*/
	export const n_setShadowY = (y : number) : IO <null> => IO (() => (Ψ.ctx.shadowOffsetY = y * Ψ.ctx.canvas.height, null))

	/**` O.n_setShadowXY : Number -> Number -> IO `*/
	export const n_setShadowXY = (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.shadowOffsetX = x * Ψ.ctx.canvas.width, Ψ.ctx.shadowOffsetY = y * Ψ.ctx.canvas.height, null))

	/**` O.n_setShadowV2 : IO Vector2 `*/
	export const n_setShadowV2 = (xy : Vector2) : IO <null> =>
		IO (() => (
			Vector2 (
				Ψ.ctx.shadowOffsetX = xy.x * Ψ.ctx.canvas.width,
				Ψ.ctx.shadowOffsetY = xy.y * Ψ.ctx.canvas.height
			), null)
		)

	/**` O.n_setMatrix : Matrix3 -> IO () `*/
	export const n_setMatrix = (m3 : Matrix3) : IO <null> =>
		IO (() => (
			Ψ.ctx.setTransform(
				m3.ix, m3.iy, m3.jx, m3.jy,
				m3.kx * Ψ.ctx.canvas.width, m3.ky * Ψ.ctx.canvas.height
			), null)
		)

	/**` O.n_setFillRGBA : (Number, Number, Number, Number) -> IO () `*/
	export const n_setFillRGBA = (r : number, g : number, b : number, a : number) : IO <null> =>
		IO (() => (Ψ.ctx.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

	/**` O.n_setFillV4 : Vector4 -> IO () `*/
	export const n_setFillV4 = (v : Vector4) : IO <null> =>
		IO (() => (Ψ.ctx.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

	/**` O.n_setStrokeRGBA : (Number, Number, Number, Number) -> IO () `*/
	export const n_setStrokeRGBA = (r : number, g : number, b : number, a : number) : IO <null> =>
		IO (() => (Ψ.ctx.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

	/**` O.n_setStrokeV4 : Vector4 -> IO () `*/
	export const n_setStrokeV4 = (v : Vector4) : IO <null> =>
		IO (() => (Ψ.ctx.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

	/**` O.n_drawImage : String -> ...8 Number -> IO () `*/
	export const n_drawImage =
		(path : string) =>
		(cx   : number) => (cy : number) => (cw : number) => (ch : number) =>
		(x    : number) => (y  : number) => (w  : number) => (h  : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				cx * Ψ.image[path].width, cy * Ψ.image[path].height, cw * Ψ.image[path].width, ch * Ψ.image[path].height,
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_drawImageV2 : String -> ...4 Vector2 -> IO () `*/
	export const n_drawImageV2 =
		(path : string ) =>
		(cxy  : Vector2) => (cwh : Vector2) =>
		(xy   : Vector2) => (wh  : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				cxy.x * Ψ.image[path].width, cxy.y * Ψ.image[path].height,
				cwh.x * Ψ.image[path].width, cwh.y * Ψ.image[path].height,
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_drawUncroppedImage : String -> ...4 Number -> IO () `*/
	export const n_drawUncroppedImage =
		(path : string) =>
		(x    : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawUncroppedImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_drawUncroppedImageV2 : String -> Vector2 -> Vector2 -> IO () `*/
	export const n_drawUncroppedImageV2 = (path : string) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawUncroppedImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height, wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_drawFullImage : String -> Number -> Number -> IO () `*/
	export const n_drawFullImage = (path : string) => (x : number) => (y : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawFullImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height)
			return null
		})

	/**` O.n_drawFullImageV2 : String -> Vector2 -> IO () `*/
	export const n_drawFullImageV2 = (path : string) => (xy : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawFullImage' received an unloaded image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height)
			return null
		})

	/**` O.n_drawFullScaledImage : String -> Number -> Number -> Number -> IO () `*/
	export const n_drawFullScaledImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawFullScaledImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				Ψ.image[path].width * k * Ψ.ctx.canvas.width, Ψ.image[path].height * k * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_drawFullScaledImageV2 : String -> Number -> Vector2 -> IO () `*/
	export const n_drawFullScaledImageV2 = (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawFullScaledImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				Ψ.image[path].width * k * Ψ.ctx.canvas.width, Ψ.image[path].height * k * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_drawSquareImage : String -> Number -> Number -> Number -> IO () `*/
	export const n_drawSquareImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawSquareImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				k * Ψ.ctx.canvas.width, k * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_drawSquareImageV2 : String -> Number -> Vector2 -> IO () `*/
	export const n_drawSquareImageV2 = (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.n_drawSquareImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				k * Ψ.ctx.canvas.width, k * Ψ.ctx.canvas.height
			)
			return null
		})

	/**` O.n_clearRect : Number -> Number -> Number -> Number -> IO () `*/
	export const n_clearRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.clearRect(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_clearRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_clearRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.clearRect(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_clearArea : Number -> Number -> Number -> Number -> IO () `*/
	export const n_clearArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.clearRect(
				x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height,
				(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_clearAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_clearAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.clearRect(
				xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height,
				(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_rotate : Number -> IO () `*/
	export const n_rotate = (angle : number) : IO <null> => IO (() => (Ψ.ctx.rotate(angle * tau), null))

	/**` O.n_translateX : Number -> IO () `*/
	export const n_translateX = (dx : number) : IO <null> =>
		IO (() => (Ψ.ctx.translate(dx * Ψ.ctx.canvas.width, 0), null))

	/**` O.n_translateY : Number -> IO () `*/
	export const n_translateY = (dy : number) : IO <null> =>
		IO (() => (Ψ.ctx.translate(0, dy * Ψ.ctx.canvas.height), null))

	/**` O.n_translateXY : Number -> Number -> IO () `*/
	export const n_translateXY = (dx : number) => (dy : number) : IO <null> =>
		IO (() => (Ψ.ctx.translate(dx * Ψ.ctx.canvas.width, dy * Ψ.ctx.canvas.height), null))

	/**` O.n_translateV2 : Vector2 -> IO () `*/
	export const n_translateV2 = (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.translate(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

	/**` O.n_transform2 : Matrix2 -> IO () `*/
	export const n_transform2 = (m2 : Matrix2) : IO <null> =>
		IO (() => (Ψ.ctx.transform(m2.ix, m2.iy, m2.jx, m2.jy, 0, 0), null))

	/**` O.n_transform3 : Matrix3 -> IO () `*/
	export const n_transform3 = (m3 : Matrix3) : IO <null> =>
		IO (() => (Ψ.ctx.transform(m3.ix, m3.iy, m3.jx, m3.jy, m3.kx * Ψ.ctx.canvas.width, m3.ky * Ψ.ctx.canvas.height), null))

	/**` O.n_moveTo : Number -> Number -> IO () `*/
	export const n_moveTo = (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

	/**` O.n_moveToV2 : Vector2 -> IO () `*/
	export const n_moveToV2 = (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

	/**` O.n_bezierCurveTo : ...6 Number -> IO () `*/
	export const n_bezierCurveTo =
		(cx0 : number) => (cy0 : number) =>
		(cx1 : number) => (cy1 : number) =>
		(x   : number) => (y   : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.bezierCurveTo(
				cx0 * Ψ.ctx.canvas.width, cy0 * Ψ.ctx.canvas.height,
				cx1 * Ψ.ctx.canvas.width, cy1 * Ψ.ctx.canvas.height,
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_bezierCurveToV2 : Vector2 -> Vector2 -> Vector2 -> IO () `*/
	export const n_bezierCurveToV2 = (cxy0 : Vector2) => (cxy1 : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.bezierCurveTo(
				cxy0.x * Ψ.ctx.canvas.width, cxy0.y * Ψ.ctx.canvas.height,
				cxy1.x * Ψ.ctx.canvas.width, cxy1.y * Ψ.ctx.canvas.height,
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_quadraticCurveTo : Number -> Number -> Number -> Number -> IO () `*/
	export const n_quadraticCurveTo = (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.quadraticCurveTo(
				cx * Ψ.ctx.canvas.width, cy * Ψ.ctx.canvas.height, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_quadraticCurveToV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_quadraticCurveToV2 = (cxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.quadraticCurveTo(
				cxy.x * Ψ.ctx.canvas.width, cxy.y * Ψ.ctx.canvas.height, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_arcTo : ...5 Number -> IO () `*/
	export const n_arcTo = (r : number) => (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arcTo(
				cx0 * Ψ.ctx.canvas.width, cy0 * Ψ.ctx.canvas.height,
				cx1 * Ψ.ctx.canvas.width, cy1 * Ψ.ctx.canvas.height,
				r * Ψ.ctx.canvas.width
			), null
		))

	/**` O.n_arcToV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_arcToV2 = (r : number) => (cxy0 : Vector2) => (cxy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arcTo(
				cxy0.x * Ψ.ctx.canvas.width, cxy0.y * Ψ.ctx.canvas.height,
				cxy1.x * Ψ.ctx.canvas.width, cxy1.y * Ψ.ctx.canvas.height,
				r * Ψ.ctx.canvas.width
			), null
		))

	/**` O.n_rect : Number -> Number -> Number -> Number -> IO () `*/
	export const n_rect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height),
			null
		))

	/**` O.n_rectV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_rectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_fillRect : Number -> Number -> Number -> Number -> IO () `*/
	export const n_fillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.fillRect(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height),
			null
		))

	/**` O.n_fillRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_fillRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.fillRect(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_strokeRect : Number -> Number -> Number -> Number -> IO () `*/
	export const n_strokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.strokeRect(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height),
			null
		))

	/**` O.n_strokeRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_strokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.strokeRect(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_fillStrokeRect : Number -> Number -> Number -> Number -> IO () `*/
	export const n_fillStrokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
			), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_fillStrokeRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_fillStrokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_area : Number -> Number -> Number -> Number -> IO () `*/
	export const n_area = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(
				x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height,
				(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_areaV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_areaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(
				xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.width,
				(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.width
			), null
		))

	/**` O.n_fillArea : Number -> Number -> Number -> Number -> IO () `*/
	export const n_fillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.fillRect(
				x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height,
				(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_fillAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_fillAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.fillRect(
				xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height,
				(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_strokeArea : Number -> Number -> Number -> Number -> IO () `*/
	export const n_strokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.strokeRect(
				x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height,
				(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
			), null))

	/**` O.n_strokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_strokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.strokeRect(
				xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height,
				(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
			), null
		))

	/**` O.n_fillStrokeArea : Number -> Number -> Number -> Number -> IO () `*/
	export const n_fillStrokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(
				x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height,
				(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
			), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_fillStrokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_fillStrokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.rect(
				xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height,
				(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
			), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_arc : ...5 Number -> IO () `*/
	export const n_arc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
			null
		))

	/**` O.n_arcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const n_arcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
			null
		))

	/**` O.n_strokeArc : ...5 Number -> IO () `*/
	export const n_strokeArc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_strokeArcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const n_strokeArcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_arcSection : ...5 Number -> IO () `*/
	export const n_arcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
			null
		))

	/**` O.n_arcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const n_arcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
			null
		))

	/**` O.n_strokeArcSection : ...5 Number -> IO () `*/
	export const n_strokeArcSection =
		(r  : number) =>
		(a0 : number) => (a1 : number) =>
		(x  : number) => (y  : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_strokeArcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const n_strokeArcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_circle : Number -> Number -> Number -> IO () `*/
	export const n_circle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), null))

	/**` O.n_circleV2 : Number -> Vector2 -> IO () `*/
	export const n_circleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
			null
		))

	/**` O.n_fillCircle : Number -> Number -> Number -> IO () `*/
	export const n_fillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
			Ψ.ctx.fill(), null
		))

	/**` O.n_fillCircleV2 : Number -> Vector2 -> IO () `*/
	export const n_fillCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
			Ψ.ctx.fill(), null
		))

	/**` O.n_strokeCircle : Number -> Number -> Number -> IO () `*/
	export const n_strokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_strokeCircleV2 : Number -> Vector2 -> IO () `*/
	export const n_strokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_fillStrokeCircle : Number -> Number -> Number -> IO () `*/
	export const n_fillStrokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
			Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_fillStrokeCircleV2 : Number -> Vector2 -> IO () `*/
	export const n_fillStrokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau),
			Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_elliptic : ...7 Number -> IO () `*/
	export const n_elliptic =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
				a * tau, a0 * tau, a1 * tau
			), null
		))

	/**` O.n_ellipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_ellipticV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.width,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau), null))

	/**` O.n_strokeElliptic : ...7 Number -> IO () `*/
	export const n_strokeElliptic =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.width,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau), Ψ.ctx.stroke(), null))

	/**` O.n_strokeEllipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_strokeEllipticV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.width,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau), Ψ.ctx.stroke(), null))

	/**` O.n_ellipticSection : ...7 Number -> IO () `*/
	export const n_ellipticSection =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.width,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau), null))

	/**` O.n_ellipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_ellipticSectionV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.width,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width,
				a * tau, a0 * tau, (a0 + a1) * tau
			), null
		))

	/**` O.n_strokeEllipticSection : ...7 Number -> IO () `*/
	export const n_strokeEllipticSection =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.width,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width,
				a * tau, a0 * tau, (a0 + a1) * tau
			), Ψ.ctx.stroke(), null
		))

	/**` O.n_strokeEllipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_strokeEllipticSectionV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
				a * tau, a0 * tau, (a0 + a1) * tau
			),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_ellipse : ...5 Number -> IO () `*/
	export const n_ellipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			),
			null
		))

	/**` O.n_ellipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_ellipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			),
			null
		))

	/**` O.n_fillEllipse : ...5 Number -> IO () `*/
	export const n_fillEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			),
			Ψ.ctx.fill(), null
		))

	/**` O.n_fillEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_fillEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			),
			Ψ.ctx.fill(), null
		))

	/**` O.n_strokeEllipse : ...5 Number -> IO () `*/
	export const n_strokeEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			), Ψ.ctx.stroke(), null
		))

	/**` O.n_strokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_strokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			), Ψ.ctx.stroke(), null
		))

	/**` O.n_fillStrokeEllipse : ...5 Number -> IO () `*/
	export const n_fillStrokeEllipse =
		(a  : number) =>
		(kx : number) => (ky : number) =>
		(x  : number) => (y  : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_fillStrokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const n_fillStrokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.ellipse(
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
				a * tau, 0, tau
			), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
		))

	/**` O.n_fillText : a -> Number -> Number -> IO () `*/
	export const n_fillText = <a>(text : a) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.fillText(text as any, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

	/**` O.n_fillTextV2 : a -> Vector2 -> IO () `*/
	export const n_fillTextV2 = <a>(text : a) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.fillText(text as any, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null))

	/**` O.n_strokeText : a -> Number -> Number -> IO () `*/
	export const n_strokeText = <a>(text : a) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.strokeText(text as any, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

	/**` O.n_strokeTextV2 : a -> Vector2 -> IO () `*/
	export const n_strokeTextV2 = <a>(text : a) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.strokeText(text as any, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null))

	/**` O.n_fillStrokeText : a -> Number -> Number -> IO () `*/
	export const n_fillStrokeText = <a>(text : a) => (x : number) => (y : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.fillText(text as any, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height),
			Ψ.ctx.strokeText(text as any, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height),
			null
		))

	/**` O.n_fillStrokeText : a -> Vector2 -> IO () `*/
	export const n_fillStrokeTextV2 = <a>(text : a) => (xy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.fillText(text as any, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height),
			Ψ.ctx.strokeText(text as any, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height),
			null
		))

	/**` O.n_line : Number -> Number -> Number -> Number -> IO () `*/
	export const n_line = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height),
			Ψ.ctx.lineTo(x1 * Ψ.ctx.canvas.width, y1 * Ψ.ctx.canvas.height),
			null
		))

	/**` O.n_lineV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_lineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height),
			Ψ.ctx.lineTo(xy1.x * Ψ.ctx.canvas.width, xy1.y * Ψ.ctx.canvas.height),
			null
		))

	/**` O.n_strokeLine : Number -> Number -> Number -> Number -> IO () `*/
	export const n_strokeLine = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height),
			Ψ.ctx.lineTo(x1 * Ψ.ctx.canvas.width, y1 * Ψ.ctx.canvas.height),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_strokeLineV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_strokeLineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height),
			Ψ.ctx.lineTo(xy1.x * Ψ.ctx.canvas.width, xy1.y * Ψ.ctx.canvas.height),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_vector : Number -> Number -> Number -> Number -> IO () `*/
	export const n_vector = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.width),
			Ψ.ctx.lineTo((x + dx) * Ψ.ctx.canvas.width, (y + dy) * Ψ.ctx.canvas.width),
			null
		))

	/**` O.n_vectorV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_vectorV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.width),
			Ψ.ctx.lineTo((xy.x + dxy.x) * Ψ.ctx.canvas.width, (xy.y + dxy.y) * Ψ.ctx.canvas.width),
			null
		))

	/**` O.n_strokeVector : Number -> Number -> Number -> Number -> IO () `*/
	export const n_strokeVector = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.width),
			Ψ.ctx.lineTo((x + dx) * Ψ.ctx.canvas.width, (y + dy) * Ψ.ctx.canvas.width),
			Ψ.ctx.stroke(), null
		))

	/**` O.n_strokeVectorV2 : Vector2 -> Vector2 -> IO () `*/
	export const n_strokeVectorV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
		IO (() => (
			Ψ.ctx.moveTo(xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.width),
			Ψ.ctx.lineTo((xy.x + dxy.x) * Ψ.ctx.canvas.width, (xy.y + dxy.y) * Ψ.ctx.canvas.width),
			Ψ.ctx.stroke(), null
		))

	/**` O.requestPointerLock : IO () `*/
	export const requestPointerLock : IO <null> =
		IO (() => (onmouseup = () => Ψ.isPointerLocked || Ψ.ctx.canvas.requestPointerLock(), null))

	/**` O.deactivatePointerLock : IO () `*/
	export const deactivatePointerLock : IO <null> = IO (() => onmouseup = null)

	/**` O.setCanvasW : Number -> IO () `*/
	export const setCanvasW = (w : number) : IO <null> => IO (() => (Ψ.ctx.canvas.width = w, null))

	/**` O.setCanvasH : Number -> IO () `*/
	export const setCanvasH = (h : number) : IO <null> => IO (() => (Ψ.ctx.canvas.height = h, null))

	/**` O.setCanvasWH : Number -> Number -> IO () `*/
	export const setCanvasWH = (w : number) => (h : number) : IO <null> =>
		IO (() => (Ψ.ctx.canvas.width = w, Ψ.ctx.canvas.height = h, null))

	/**` O.setCanvasV2 : Vector2 -> IO () `*/
	export const setCanvasV2 = (wh : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.canvas.width = wh.x, Ψ.ctx.canvas.height = wh.y, null))

	/**` O.setLayer : Number -> IO () `*/
	export const setLayer = (index : number) : IO <null> =>
		IO (() =>
			index < 0 || index > Ψ.ctxs.length || !Number.isInteger (index)
				? error (`'O.setLayer' received index of '${index}'; must be a integer in interval [0, ${Ψ.ctxs.length})`)
				: (Ψ.ctx = Ψ.ctxs[index]!, null)
		)

	/**` O.setLineThickness : Number -> IO () `*/
	export const setLineThickness = (thickness : number) : IO <null> => IO (() => (Ψ.ctx.lineWidth = thickness, null))

	/**` O.setLineDashPattern : List Number -> IO () `*/
	export const setLineDashPattern = (pattern : List <number>) : IO <null> =>
		IO (() => (Ψ.ctx.setLineDash(listToArray (pattern)), null))

	/**` O.setLineDashOffset : Number -> IO () `*/
	export const setLineDashOffset = (offset : number) : IO <null> => IO (() => (Ψ.ctx.lineDashOffset = offset, null))

	/**` O.setMiterLimit : Number -> IO () `*/
	export const setMiterLimit = (limit : number) : IO <null> => IO (() => (Ψ.ctx.miterLimit = limit, null))

	/**` O.setFont : String -> IO `*/
	export const setFont = (font : string) : IO <null> => IO (() => (Ψ.ctx.font = font, null))

	/**` O.setFontSize : Number -> IO () `*/
	export const setFontSize = (size : number) : IO <null> =>
		IO (() => (Ψ.ctx.font = `${size}px${Ψ.ctx.font.slice(Ψ.ctx.font.indexOf(' '))}`, null))

	/**` O.setFontFamily : String -> IO () `*/
	export const setFontFamily = (family : string) : IO <null> =>
		IO (() => (Ψ.ctx.font = `${parseFloat(Ψ.ctx.font)}px "${family}"`, null))

	/**` O.setShadowBlurAmount : Number -> IO () `*/
	export const setShadowBlurAmount = (amount : number) : IO <null> => IO (() => (Ψ.ctx.shadowBlur = amount, null))

	/**` O.setShadowColor : String -> IO () `*/
	export const setShadowColor = (color : string) : IO <null> => IO (() => (Ψ.ctx.shadowColor = color, null))

	/**` O.setShadowRGBA : (Number, Number, Number, Number) -> IO () `*/
	export const setShadowRGBA = (r : number, g : number, b : number, a : number) : IO <null> =>
		IO (() => (Ψ.ctx.shadowColor = `rgba(${r},${g},${b},${a})`, null))

	/**` O.setShadowV4 : Vector4 -> IO () `*/
	export const setShadowV4 = (v : Vector4) : IO <null> =>
		IO (() => (Ψ.ctx.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

	/**` O.setShadowX : Number -> IO () `*/
	export const setShadowX = (x : number) : IO <null> => IO (() => (Ψ.ctx.shadowOffsetX = x, null))

	/**` O.setShadowY : Number -> IO () `*/
	export const setShadowY = (y : number) : IO <null> => IO (() => (Ψ.ctx.shadowOffsetY = y, null))

	/**` O.setShadowXY : Number -> Number -> IO `*/
	export const setShadowXY = (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.shadowOffsetX = x, Ψ.ctx.shadowOffsetY = y, null))

	/**` O.setShadowV2 : IO Vector2 `*/
	export const setShadowV2 = (xy : Vector2) : IO <null> =>
		IO (() => (Vector2 (Ψ.ctx.shadowOffsetX = xy.x, Ψ.ctx.shadowOffsetY = xy.y), null))

	/**` O.setMatrix : Matrix3 -> IO () `*/
	export const setMatrix = (m3 : Matrix3) : IO <null> =>
		IO (() => (Ψ.ctx.setTransform(m3.ix, m3.iy, m3.jx, m3.jy, m3.kx, m3.ky), null))

	/**` O.resetMatrix : IO () `*/
	export const resetMatrix : IO <null> = IO (() => (Ψ.ctx.resetTransform, null))

	/**` O.setAlpha : Number -> IO () `*/
	export const setAlpha = (alpha : number) : IO <null> => IO (() => (Ψ.ctx.globalAlpha = alpha, null))

	/**` O.setFillColor : String -> IO () `*/
	export const setFillColor = (color : string) : IO <null> => IO (() => (Ψ.ctx.fillStyle = color, null))

	/**` O.setFillRGBA : (Number, Number, Number, Number) -> IO () `*/
	export const setFillRGBA = (r : number, g : number, b : number, a : number) : IO <null> =>
		IO (() => (Ψ.ctx.fillStyle = `rgba(${r},${g},${b},${a})`, null))

	/**` O.setFillV4 : Vector4 -> IO () `*/
	export const setFillV4 = (v : Vector4) : IO <null> =>
		IO (() => (Ψ.ctx.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

	/**` O.setStrokeColor : String -> IO () `*/
	export const setStrokeColor = (color : string) : IO <null> => IO (() => (Ψ.ctx.strokeStyle = color, null))

	/**` O.setStrokeRGBA : (Number, Number, Number, Number) -> IO () `*/
	export const setStrokeRGBA = (r : number, g : number, b : number, a : number) : IO <null> =>
		IO (() => (Ψ.ctx.strokeStyle = `rgba(${r},${g},${b},${a})`, null))

	/**` O.setStrokeV4 : Vector4 -> IO () `*/
	export const setStrokeV4 = (v : Vector4) : IO <null> =>
		IO (() => (Ψ.ctx.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

	/**` O.setLineCap : LineCap -> IO () `*/
	export const setLineCap = (linecap : LineCap) : IO <null> =>
		IO (() => (Ψ.ctx.lineCap = mappingLineCapToHTML5 .codomain (linecap), null))

	/**` O.setLineJoin : LineJoin -> IO () `*/
	export const setLineJoin = (linejoin : LineJoin) : IO <null> =>
		IO (() => (Ψ.ctx.lineJoin = mappingLineJoinToHTML5 .codomain (linejoin), null))

	/**` O.setTextAlign : TextAlign -> IO () `*/
	export const setTextAlign = (alignment : TextAlign) : IO <null> =>
		IO (() => (Ψ.ctx.textAlign = mappingTextAlignToHTML5 .codomain (alignment), null))

	/**` O.setTextBaseline : TextBaseline -> IO () `*/
	export const setTextBaseline = (baseline : TextBaseline) : IO <null> =>
		IO (() => (Ψ.ctx.textBaseline = mappingTextBaselineToHTML5 .codomain (baseline), null))

	/**` O.setComposition : Composition -> IO () `*/
	export const setComposition = (composition : Composition) : IO <null> =>
		IO (() => (Ψ.ctx.globalCompositeOperation = mappingCompositionToHTML5 .codomain (composition), null))

	/**` O.setToCenterText : IO () `*/
	export const setCenterText : IO <null> =
		IO (() => (Ψ.ctx.textAlign = 'center', Ψ.ctx.textBaseline = 'middle', null))

	/**` O.setToDefaultText : IO () `*/
	export const defaultText : IO <null> =
		IO (() => (Ψ.ctx.textAlign = 'start', Ψ.ctx.textBaseline = 'alphabetic', null))

	/**` O.resetState : IO () `*/
	export const resetState : IO <null> =
		IO (() => {
			for (const k in Ψ.keyboard)     Ψ.keyboard[k as KeyboardKey] = relaxVertical (Ψ.keyboard[k as KeyboardKey])
			for (const i in Ψ.mouseButtons) Ψ.mouseButtons[i]            = relaxVertical (Ψ.mouseButtons[i]!)
			Ψ.mouseDeltaX = Ψ.mouseDeltaY = 0
			Ψ.mouseScroll = Vertical.Rest
			Ψ.isResized   = false
			return null
		})

	/**` O.setAudioTime : String -> Number -> IO () `*/
	export const setAudioTime = (path : string) => (time : number) : IO <null> =>
		IO (() => {
			if (!Ψ.audio[path])
				error (`'O.setAudioTime' received an unloaded (possibly non-existing) audio at path: '${path}'`)
			if (time < 0 || time > Ψ.audio[path].duration)
				error (`'O.setAudioTime' received '${time}' as an input; \
must be in interval [0, ${Ψ.audio[path].duration}] for audio file: '${path}'`)
			Ψ.audio[path].currentTime = time
			return null
		})

	/**` O.resetAudio : String -> IO () `*/
	export const resetAudio = (path : string) : IO <null> =>
		IO (() => {
			if (!Ψ.audio[path])
				error (`'O.resetAudio' received an unloaded (possibly non-existing) audio at path: '${path}'`)
			Ψ.audio[path].pause()
			Ψ.audio[path].currentTime = 0
			return null
		})

	/**` O.flush : IO () `*/
	export const flush : IO <null> = IO (() => (console.clear(), null))

	/**` O.log : a -> IO () `*/
	export const log = <a>(message : a) : IO <null> => IO (() => (console.log(message), null))

	/**` O.warning : a -> IO () `*/
	export const warning = <a>(message : a) : IO <null> => IO (() => (console.warn(message), null))

	/**` O.debug : Number -> a -> IO () `*/
	export const debug = (count : number) => <a>(message : a) : IO <null> =>
		IO (() => {
			if (--Ψ.debugCounter < 0)
				Ψ.debugCounter = count,
				console.debug(message)
			return null
		})

	/**` O.count : String -> IO () `*/
	export const count = <a>(identifier : string) : IO <null> => IO (() => (console.count(identifier), null))

	/**` O.resetCount : String -> IO () `*/
	export const resetCount = <a>(identifier : string) : IO <null> => IO (() => (console.countReset(identifier), null))

	/**` O.queue : IO a -> IO () `*/
	export const queue = <a>(effect : IO <a>) : IO <null> => IO (() => (requestAnimationFrame(effect.effect), null))

	/**` O.loadImage : String -> IO () `*/
	export const loadImage = (path : string) : IO <null> =>
		IO (() => {
			Ψ.image[path]          = new Image
			Ψ.image[path]!.src     = path
			Ψ.image[path]!.onerror = () => error (`'O.loadImage' could not load image at path: '${path}'`)
			return null
		})

	/**` O.loadAudio : String -> IO () `*/
	export const loadAudio = (path : string) : IO <null> =>
		IO (() => {
			Ψ.audio[path]          = new Audio(path)
			Ψ.audio[path]!.onerror = () => error (`'O.loadAudio' could not load audio at path: '${path}'`)
			return null
		})

	/**` O.loadFont : String -> IO () `*/
	export const loadFont = (path : string) : IO <null> =>
		IO (() => {
			new FontFace(path.slice(path.lastIndexOf('/') + 1, path.lastIndexOf('.')), `url(${path})`)
				.load()
				.then((font : any) => (document as any).fonts.add(font))
				.catch(() => error (`'loadFont' could not load font at path: '${path}'`))
			return null
		})

	/**` O.drawImage : String -> ...8 Number -> IO () `*/
	export const drawImage =
		(path : string) =>
		(cx   : number) => (cy : number) => (cw : number) => (ch : number) =>
		(x    : number) => (y  : number) => (w  : number) => (h  : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], cx, cy, cw, ch, x, y, w, h)
			return null
		})

	/**` O.drawImageV2 : String -> ...4 Vector2 -> IO () `*/
	export const drawImageV2 =
		(path : string ) =>
		(cxy  : Vector2) => (cwh : Vector2) =>
		(xy   : Vector2) => (wh  : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], cxy.x, cxy.y, cwh.x, cwh.y, xy.x, xy.y, wh.x, wh.y)
			return null
		})

	/**` O.drawUncroppedImage : String -> ...4 Number -> IO () `*/
	export const drawUncroppedImage =
		(path : string) =>
		(x    : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawUncroppedImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], x, y, w, h)
			return null
		})

	/**` O.drawUncroppedImageV2 : String -> Vector2 -> Vector2 -> IO () `*/
	export const drawUncroppedImageV2 = (path : string) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawUncroppedImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, wh.x, wh.y)
			return null
		})

	/**` O.drawFullImage : String -> Number -> Number -> IO () `*/
	export const drawFullImage = (path : string) => (x : number) => (y : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawFullImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], x, y)
			return null
		})

	/**` O.drawFullImageV2 : String -> Vector2 -> IO () `*/
	export const drawFullImageV2 = (path : string) => (xy : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawFullImage' received an unloaded image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y)
			return null
		})

	/**` O.drawFullScaledImage : String -> Number -> Number -> Number -> IO () `*/
	export const drawFullScaledImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawFullScaledImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], x, y, Ψ.image[path].width * k, Ψ.image[path].height * k)
			return null
		})

	/**` O.drawFullScaledImageV2 : String -> Number -> Vector2 -> IO () `*/
	export const drawFullScaledImageV2 = (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawFullScaledImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, Ψ.image[path].width * k, Ψ.image[path].height * k)
			return null
		})

	/**` O.drawSquareImage : String -> Number -> Number -> Number -> IO () `*/
	export const drawSquareImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawSquareImage' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], x, y, k, k)
			return null
		})

	/**` O.drawSquareImageV2 : String -> Number -> Vector2 -> IO () `*/
	export const drawSquareImageV2 = (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
		IO (() => {
			if (!Ψ.image[path])
				error (`'O.drawSquareImageV2' received an unloaded (possibly non-existing) image at path: '${path}'`)
			Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, k, k)
			return null
		})

	/**` O.playAudio : String -> IO () `*/
	export const playAudio = (path : string) : IO <null> =>
		IO (() => {
			if (!Ψ.audio[path])
				error (`'O.playAudio' received an unloaded (possibly non-existing) audio at path: '${path}'`)
			Ψ.audio[path].play()
			return null
		})

	/**` O.pauseAudio : String -> IO () `*/
	export const pauseAudio = (path : string) : IO <null> =>
		IO (() => {
			if (!Ψ.audio[path])
				error (`'O.pauseAudio' received an unloaded (possibly non-existing) audio at path: '${path}'`)
			Ψ.audio[path].pause()
			return null
		})

	/**` O.playSFX : String -> IO () `*/
	export const playSFX = (path : string) : IO <null> =>
		IO (() => {
			if (Ψ.audio[path])
				(Ψ.audio[path].cloneNode() as any).play()
			else
				error (`'O.playSFX' received an unloaded (possibly non-existing) audio at path: '${path}'`)
			return null
		})

	/**` O.clearLayer : IO () `*/
	export const clearLayer : IO <null> =
		IO (() => (Ψ.ctx.clearRect(0, 0, Ψ.ctx.canvas.width, Ψ.ctx.canvas.height), null))

	/**` O.clearCanvas : IO () `*/
	export const clearCanvas : IO <null> =
		IO (() => (Ψ.ctxs.forEach(ctx => ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)), null))

	/**` O.clearRect : Number -> Number -> Number -> Number -> IO () `*/
	export const clearRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (Ψ.ctx.clearRect(x, y, w, h), null))

	/**` O.clearRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const clearRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.clearRect(xy.x, xy.y, wh.x, wh.y), null))

	/**` O.clearArea : Number -> Number -> Number -> Number -> IO () `*/
	export const clearArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.clearRect(x0, y0, x1 - x0, y1 - y0), null))

	/**` O.clearAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const clearAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.clearRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

	/**` O.fill : IO () `*/
	export const fill : IO <null> = IO (() => (Ψ.ctx.fill(), null))

	/**` O.stroke : IO () `*/
	export const stroke : IO <null> = IO (() => (Ψ.ctx.stroke(), null))

	/**` O.save : IO () `*/
	export const save : IO <null> = IO (() => (Ψ.ctx.save(), null))

	/**` O.restore : IO () `*/
	export const restore : IO <null> = IO (() => (Ψ.ctx.restore(), null))

	/**` O.clipEvenOdd : IO () `*/
	export const clipEvenOdd : IO <null> = IO (() => (Ψ.ctx.clip('evenodd'), null))

	/**` O.clipNonZero : IO () `*/
	export const clipNonZero : IO <null> = IO (() => (Ψ.ctx.clip('nonzero'), null))

	/**` O.rotate : Number -> IO () `*/
	export const rotate = (angle : number) : IO <null> => IO (() => (Ψ.ctx.rotate(angle), null))

	/**` O.scaleX : Number -> IO () `*/
	export const scaleX = (kx : number) : IO <null> => IO (() => (Ψ.ctx.scale(kx, 1), null))

	/**` O.scaleY : Number -> IO () `*/
	export const scaleY = (ky : number) : IO <null> => IO (() => (Ψ.ctx.scale(1, ky), null))

	/**` O.scaleXY : Number -> Number -> IO () `*/
	export const scaleXY = (kx : number) => (ky : number) : IO <null> => IO (() => (Ψ.ctx.scale(kx, ky), null))

	/**` O.scaleV2 : Vector2 -> IO () `*/
	export const scaleV2 = (v : Vector2) : IO <null> => IO (() => (Ψ.ctx.scale(v.x, v.y), null))

	/**` O.translateX : Number -> IO () `*/
	export const translateX = (dx : number) : IO <null> => IO (() => (Ψ.ctx.translate(dx, 0), null))

	/**` O.translateY : Number -> IO () `*/
	export const translateY = (dy : number) : IO <null> => IO (() => (Ψ.ctx.translate(0, dy), null))

	/**` O.translateXY : Number -> Number -> IO () `*/
	export const translateXY = (dx : number) => (dy : number) : IO <null> => IO (() => (Ψ.ctx.translate(dx, dy), null))

	/**` O.translateV2 : Vector2 -> IO () `*/
	export const translateV2 = (v : Vector2) : IO <null> => IO (() => (Ψ.ctx.translate(v.x, v.y), null))

	/**` O.transform2 : Matrix2 -> IO () `*/
	export const transform2 = (m2 : Matrix2) : IO <null> =>
		IO (() => (Ψ.ctx.transform(m2.ix, m2.iy, m2.jx, m2.jy, 0, 0), null))

	/**` O.transform3 : Matrix3 -> IO () `*/
	export const transform3 = (m3 : Matrix3) : IO <null> =>
		IO (() => (Ψ.ctx.transform(m3.ix, m3.iy, m3.jx, m3.jy, m3.kx, m3.ky), null))

	/**` O.beginPath : IO () `*/
	export const beginPath : IO <null> = IO (() => (Ψ.ctx.beginPath(), null))

	/**` O.closePath : IO () `*/
	export const closePath : IO <null> = IO (() => (Ψ.ctx.closePath(), null))

	/**` O.moveTo : Number -> Number -> IO () `*/
	export const moveTo = (x : number) => (y : number) : IO <null> => IO (() => (Ψ.ctx.moveTo(x, y), null))

	/**` O.moveToV2 : Vector2 -> IO () `*/
	export const moveToV2 = (v : Vector2) : IO <null> => IO (() => (Ψ.ctx.moveTo(v.x, v.y), null))

	/**` O.bezierCurveTo : ...6 Number -> IO () `*/
	export const bezierCurveTo =
		(cx0 : number) => (cy0 : number) =>
		(cx1 : number) => (cy1 : number) =>
		(x   : number) => (y   : number) : IO <null> =>
		IO (() => (Ψ.ctx.bezierCurveTo(cx0, cy0, cx1, cy1, x, y), null))

	/**` O.bezierCurveToV2 : Vector2 -> Vector2 -> Vector2 -> IO () `*/
	export const bezierCurveToV2 = (cxy0 : Vector2) => (cxy1 : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.bezierCurveTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, xy.x, xy.y), null))

	/**` O.quadraticCurveTo : Number -> Number -> Number -> Number -> IO () `*/
	export const quadraticCurveTo = (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.quadraticCurveTo(cx, cy, x, y), null))

	/**` O.quadraticCurveToV2 : Vector2 -> Vector2 -> IO () `*/
	export const quadraticCurveToV2 = (cxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.quadraticCurveTo(cxy.x, cxy.y, xy.x, xy.y), null))

	/**` O.arcTo : ...5 Number -> IO () `*/
	export const arcTo = (r : number) => (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.arcTo(cx0, cy0, cx1, cy1, r), null))

	/**` O.arcToV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const arcToV2 = (r : number) => (cxy0 : Vector2) => (cxy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arcTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, r), null))

	/**` O.rect : Number -> Number -> Number -> Number -> IO () `*/
	export const rect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (Ψ.ctx.rect(x, y, w, h), null))

	/**` O.rectV2 : Vector2 -> Vector2 -> IO () `*/
	export const rectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), null))

	/**` O.fillRect : Number -> Number -> Number -> Number -> IO () `*/
	export const fillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (Ψ.ctx.fillRect(x, y, w, h), null))

	/**` O.fillRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const fillRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.fillRect(xy.x, xy.y, wh.x, wh.y), null))

	/**` O.strokeRect : Number -> Number -> Number -> Number -> IO () `*/
	export const strokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (Ψ.ctx.strokeRect(x, y, w, h), null))

	/**` O.strokeRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const strokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.strokeRect(xy.x, xy.y, wh.x, wh.y), null))

	/**` O.fillStrokeRect : Number -> Number -> Number -> Number -> IO () `*/
	export const fillStrokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
		IO (() => (Ψ.ctx.rect(x, y, w, h), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.fillStrokeRectV2 : Vector2 -> Vector2 -> IO () `*/
	export const fillStrokeRectV2 = (xy : Vector2) => (wh : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.area : Number -> Number -> Number -> Number -> IO () `*/
	export const area = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), null))

	/**` O.areaV2 : Vector2 -> Vector2 -> IO () `*/
	export const areaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

	/**` O.fillArea : Number -> Number -> Number -> Number -> IO () `*/
	export const fillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.fillRect(x0, y0, x1 - x0, y1 - y0), null))

	/**` O.fillAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const fillAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.fillRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

	/**` O.strokeArea : Number -> Number -> Number -> Number -> IO () `*/
	export const strokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.strokeRect(x0, y0, x1 - x0, y1 - y0), null))

	/**` O.strokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const strokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.strokeRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

	/**` O.fillStrokeArea : Number -> Number -> Number -> Number -> IO () `*/
	export const fillStrokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.fillStrokeAreaV2 : Vector2 -> Vector2 -> IO () `*/
	export const fillStrokeAreaV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.arc : ...5 Number -> IO () `*/
	export const arc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, a0, a1), null))

	/**` O.arcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const arcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a1), null))

	/**` O.strokeArc : ...5 Number -> IO () `*/
	export const strokeArc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, a0, a1), Ψ.ctx.stroke(), null))

	/**` O.strokeArcV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const strokeArcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a1), Ψ.ctx.stroke(), null))

	/**` O.arcSection : ...5 Number -> IO () `*/
	export const arcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, a0, a0 + a1), null))

	/**` O.arcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const arcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a0 + a1), null))

	/**` O.strokeArcSection : ...5 Number -> IO () `*/
	export const strokeArcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, a0, a0 + a1), Ψ.ctx.stroke(), null))

	/**` O.strokeArcSectionV2 : Number -> Number -> Number -> Vector2 -> IO () `*/
	export const strokeArcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a0 + a1), Ψ.ctx.stroke(), null))

	/**` O.circle : Number -> Number -> Number -> IO () `*/
	export const circle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), null))

	/**` O.circleV2 : Number -> Vector2 -> IO () `*/
	export const circleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), null))

	/**` O.fillCircle : Number -> Number -> Number -> IO () `*/
	export const fillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.fill(), null))

	/**` O.fillCircleV2 : Number -> Vector2 -> IO () `*/
	export const fillCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.fill(), null))

	/**` O.strokeCircle : Number -> Number -> Number -> IO () `*/
	export const strokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.stroke(), null))

	/**` O.strokeCircleV2 : Number -> Vector2 -> IO () `*/
	export const strokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.stroke(), null))

	/**` O.fillStrokeCircle : Number -> Number -> Number -> IO () `*/
	export const fillStrokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.fillStrokeCircleV2 : Number -> Vector2 -> IO () `*/
	export const fillStrokeCircleV2 = (r : number) => (v : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.elliptic : ...7 Number -> IO () `*/
	export const elliptic =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a1), null))

	/**` O.ellipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const ellipticV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a1), null))

	/**` O.strokeElliptic : ...7 Number -> IO () `*/
	export const strokeElliptic =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a1), Ψ.ctx.stroke(), null))

	/**` O.strokeEllipticV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const strokeEllipticV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a1), Ψ.ctx.stroke(), null))

	/**` O.ellipticSection : ...7 Number -> IO () `*/
	export const ellipticSection =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a0 + a1), null))

	/**` O.ellipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const ellipticSectionV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a0 + a1), null))

	/**` O.strokeEllipticSection : ...7 Number -> IO () `*/
	export const strokeEllipticSection =
		(a  : number) => (a0 : number) => (a1 : number) =>
		(kx : number) => (ky : number) => (x  : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a0 + a1), Ψ.ctx.stroke(), null))

	/**` O.strokeEllipticSectionV2 : Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
	export const strokeEllipticSectionV2 =
		(a   : number ) => (a0 : number ) => (a1 : number) =>
		(kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a0 + a1), Ψ.ctx.stroke(), null))

	/**` O.ellipse : ...5 Number -> IO () `*/
	export const ellipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), null))

	/**` O.ellipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const ellipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), null))

	/**` O.fillEllipse : ...5 Number -> IO () `*/
	export const fillEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.fill(), null))

	/**` O.fillEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const fillEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.fill(), null))

	/**` O.strokeEllipse : ...5 Number -> IO () `*/
	export const strokeEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.stroke(), null))

	/**` O.strokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const strokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.stroke(), null))

	/**` O.fillStrokeEllipse : ...5 Number -> IO () `*/
	export const fillStrokeEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.fillStrokeEllipseV2 : Number -> Vector2 -> Vector2 -> IO () `*/
	export const fillStrokeEllipseV2 = (a : number) => (kxy : Vector2) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

	/**` O.fillText : a -> Number -> Number -> IO () `*/
	export const fillText = <a>(text : a) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.fillText(text as any, x, y), null))

	/**` O.fillTextV2 : a -> Vector2 -> IO () `*/
	export const fillTextV2 = <a>(text : a) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.fillText(text as any, xy.x, xy.y), null))

	/**` O.strokeText : a -> Number -> Number -> IO () `*/
	export const strokeText = <a>(text : a) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.strokeText(text as any, x, y), null))

	/**` O.strokeTextV2 : a -> Vector2 -> IO () `*/
	export const strokeTextV2 = <a>(text : a) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.strokeText(text as any, xy.x, xy.y), null))

	/**` O.fillStrokeText : a -> Number -> Number -> IO () `*/
	export const fillStrokeText = <a>(text : a) => (x : number) => (y : number) : IO <null> =>
		IO (() => (Ψ.ctx.fillText(text as any, x, y), Ψ.ctx.strokeText(text as any, x, y), null))

	/**` O.fillStrokeText : a -> Vector2 -> IO () `*/
	export const fillStrokeTextV2 = <a>(text : a) => (xy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.fillText(text as any, xy.x, xy.y), Ψ.ctx.strokeText(text as any, xy.x, xy.y), null))

	/**` O.line : Number -> Number -> Number -> Number -> IO () `*/
	export const line = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(x0, y0), Ψ.ctx.lineTo(x1, y1), null))

	/**` O.lineV2 : Vector2 -> Vector2 -> IO () `*/
	export const lineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(xy0.x, xy0.y), Ψ.ctx.lineTo(xy1.x, xy1.y), null))

	/**` O.strokeLine : Number -> Number -> Number -> Number -> IO () `*/
	export const strokeLine = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(x0, y0), Ψ.ctx.lineTo(x1, y1), Ψ.ctx.stroke(), null))

	/**` O.strokeLineV2 : Vector2 -> Vector2 -> IO () `*/
	export const strokeLineV2 = (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(xy0.x, xy0.y), Ψ.ctx.lineTo(xy1.x, xy1.y), Ψ.ctx.stroke(), null))

	/**` O.vector : Number -> Number -> Number -> Number -> IO () `*/
	export const vector = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(x, y), Ψ.ctx.lineTo(x + dx, y + dy), null))

	/**` O.vectorV2 : Vector2 -> Vector2 -> IO () `*/
	export const vectorV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(xy.x, xy.y), Ψ.ctx.lineTo(xy.x + dxy.x, xy.y + dxy.y), null))

	/**` O.strokeVector : Number -> Number -> Number -> Number -> IO () `*/
	export const strokeVector = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(x, y), Ψ.ctx.lineTo(x + dx, y + dy), Ψ.ctx.stroke(), null))

	/**` O.strokeVectorV2 : Vector2 -> Vector2 -> IO () `*/
	export const strokeVectorV2 = (xy : Vector2) => (dxy : Vector2) : IO <null> =>
		IO (() => (Ψ.ctx.moveTo(xy.x, xy.y), Ψ.ctx.lineTo(xy.x + dxy.x, xy.y + dxy.y), Ψ.ctx.stroke(), null))
}

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
		Ψ.mouseWindowX = ev.x,
		Ψ.mouseWindowY = ev.y,
		Ψ.mouseCanvasX = ev.clientX - Ψ.ctx.canvas.offsetLeft,
		Ψ.mouseCanvasY = ev.clientY - Ψ.ctx.canvas.offsetTop,
		Ψ.mouseScreenX = ev.screenX,
		Ψ.mouseScreenY = ev.screenY,
		Ψ.mouseDeltaX  = ev.movementX,
		Ψ.mouseDeltaY  = ev.movementY
	}

	document.onpointerlockchange = () => Ψ.isPointerLocked = document.pointerLockElement === Ψ.ctx.canvas

	if (main) main.effect ()
}
