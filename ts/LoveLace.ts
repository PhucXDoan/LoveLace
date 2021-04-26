/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable no-return-assign                  */
/* eslint-disable no-plusplus                       */
/* eslint-disable no-param-reassign                 */
/* eslint-disable no-console                        */
/* eslint-disable no-multi-assign                   */

/********************************************************************************************************************************/
// Settings | Esoterics //

const THROW = (message : string) : never => { throw new Error (message) }
const MAXARRAY     = 1024 // -- Maximum length of array when converting lists to primitive arrays
const MAXSTRING    = 1024 // -- Maximum length of string when converting lists to primitive strings
const MAX_LIST_OPS = 1024 // -- Maximum amount of operations that can be done on lists
const ERROR =
{
	MAX_LIST_OPS : (op : string, org? : string) =>
	THROW(`'${op}' reached the max amount of traversal (${MAX_LIST_OPS}) allowed in a list ${org ? `| origin : '${org}'` : ''}`),
	BINDING_NILS : (org : string, op : string) =>
	THROW(`'(${op})' was used on a infinite list with an operation that always return a nil | origin : '${org}'`),
	ONLY_INTEGER : (org : string, n : number) =>
	THROW(`'${org}' only accepts integers as an amount; instead received '${n}'`),
	ONLY_NATURAL : (org : string, n : number) =>
	THROW(`'${org}' only accepts natural numbers (0 inclusive); instead received '${n}'`),
	ONLY_CONS : (org : string) =>
	THROW(`'${org}' only accepts non-empty lists`)
}

type Rec <a> = Omit <a, 'CONS'>

/********************************************************************************************************************************/
// Typeclasses //

type Pipe <a> =
	{
		/**` (.pipe) :: (Pipe a) => a -> (a -> b) -> b `*/
		pipe : <b>(morphism : (value : Pipe <a>) => b) => b
	} & a

type Eq <a> =
	{
		/**` (.eq) :: (Eq a) => a -> a -> Boolean `*/
		eq : (expression : Eq <a>) => boolean
	} & a

/********************************************************************************************************************************/
// Definitions of Algebraic Data Types //

interface Boolean
{
	/**` (.pipe) :: Boolean -> (Boolean -> a) -> a `*/
	pipe : <a>(morphism : (bool : boolean) => a) => a

	/**` (.eq) :: Boolean -> Boolean -> Boolean `*/
	eq : (bool : boolean) => boolean
}

interface Number
{
	/**` (.pipe) :: Number -> (Number -> a) -> a `*/
	pipe : <a>(morphism : (num : number) => a) => a

	/**` (.eq) :: Number -> Number -> Boolean `*/
	eq : (num : number) => boolean
}

interface String
{
	/**` (.pipe) :: String -> (String -> a) -> a `*/
	pipe : <a>(morphism : (str : string) => a) => a

	/**` (.eq) :: String -> String -> Boolean `*/
	eq : (str : string) => boolean
}

type Pair <a, b> =
	{
		CONS : 'Pair'

		/**` (.pipe) :: Pair a b -> (Pair a b -> c) -> c `*/
		pipe : <c>(morphism : (pair : Pair <a, b>) => c) => c

		/**` (.eq) :: (Eq a, Eq b) => Pair a b -> Pair a b -> Boolean `*/
		eq : (pair : Pair <Eq <a>, Eq <b>>) => boolean

		/**` (.fst) :: Pair a b -> a `*/
		fst : a

		/**` (.snd) :: Pair a b -> b `*/
		snd : b
	}

type IO <a> =
	{
		CONS : 'IO'
		INFO : () => a

		/**` (.pipe) :: IO a -> (IO a -> b) -> b `*/
		pipe : <b>(morphism : (io : IO <a>) => b) => b

		/**` (.bind) :: IO a -> (a -> IO b) -> IO b `*/
		bind : <b>(reaction : (outcome : a) => IO <b>) => IO <b>

		/**` (.fmap) :: IO a -> (a -> b) -> IO b `*/
		fmap : <b>(morphism : (outcome : a) => b) => IO <b>

		/**` (.bindto) :: IO $ -> (String, $ -> IO a) -> IO $ `*/
		bindto : <k extends string, b>(name : k, reaction : ($ : a) => IO <b>) => IO <a & { [x in k] : b }>

		/**` (.fmapto) :: IO $ -> (String, $ -> a) -> IO $ `*/
		fmapto : <k extends string, b>(name : k, morphism : ($ : a) => b) => IO <a & { [x in k] : b }>

		/**` (.then) :: IO a -> IO b -> IO b `*/
		then : <b>(successor : IO <b>) => IO <b>

		/**` (.side) :: IO a -> IO b -> IO a `*/
		side : <b>(effect : IO <b>) => IO <a>

		/**` (.also) :: IO a -> (a -> IO b) -> IO a `*/
		also : <b>(reaction : (outcome : a) => IO <b>) => IO <a>

		/**` (.cast) :: IO a -> b -> IO b `*/
		cast : <b>(replacement : b) => IO <b>
	}

type Maybe <a> =
	({ CONS : 'Nothing' } | { CONS : 'Just', INFO : a }) &
	{
		/**` (.pipe) :: Maybe a -> (Maybe a -> b) -> b `*/
		pipe : <b>(morphism : (maybe : Maybe <a>) => b) => b

		/**` (.eq) :: (Eq a) => Maybe a -> Maybe a -> Boolean `*/
		eq : (maybe : Maybe <Eq <a>>) => boolean

		/**` (.bind) :: Maybe a -> (a -> Maybe b) -> Maybe b `*/
		bind : <b>(reaction : (possibility : a) => Maybe <b>) => Maybe <b>

		/**` (.fmap) :: Maybe a -> (a -> b) -> Maybe b `*/
		fmap : <b>(morphism : (possibility : a) => b) => Maybe <b>

		/**` (.bindto) :: Maybe $ -> (String, $ -> Maybe a) -> Maybe $ `*/
		bindto : <k extends string, b>(name : k, reaction : ($ : a) => Maybe <b>) => Maybe <a & { [x in k] : b }>

		/**` (.fmapto) :: Maybe $ -> (String, $ -> a) -> Maybe $ `*/
		fmapto : <k extends string, b>(name : k, morphism : ($ : a) => b) => Maybe <a & { [x in k] : b }>

		/**` (.cast) :: Maybe a -> b -> Maybe b `*/
		cast : <b>(replacement : b) => Maybe <b>
	}

type Process <s, a> =
	{
		CONS : 'Process'
		INFO : (state : s) => Pair <s, a>

		/**` (.pipe) :: Process s a -> (Process s a -> b) -> b `*/
		pipe : <b>(morphism : (process : Process <s, a>) => b) => b

		/**` (.bind) :: Process s a -> (a -> Process s b) -> Process s b `*/
		bind : <b>(reaction : (output : a) => Process <s, b>) => Process <s, b>

		/**` (.fmap) :: Process s a -> (a -> b) -> Process s b `*/
		fmap : <b>(morphism : (output : a) => b) => Process <s, b>

		/**` (.bindto) :: Process s $ -> (String, a -> Process s b) -> Process s $ `*/
		bindto : <k extends string, b>(name : k, reaction : ($ : a) => Process <s, b>) => Process <s, a & { [x in k] : b }>

		/**` (.fmapto) :: Process s $ -> (String, a -> b) -> Process s $ `*/
		fmapto : <k extends string, b>(name : k, morphism : ($ : a) => b) => Process <s, a & { [x in k] : b }>

		/**` (.then) :: Process s a -> Process s b -> Process s b `*/
		then : <b>(successor : Process <s, b>) => Process <s, b>

		/**` (.side) :: Process s a -> Process s b -> Process s a `*/
		side : <b>(effect : Process <s, b>) => Process <s, a>

		/**` (.also) :: Process s a -> (a -> Process s b) -> Process s a `*/
		also : <b>(reaction : (output : a) => Process <s, b>) => Process <s, a>

		/**` (.cast) :: Process s a -> b -> Process s b `*/
		cast : <b>(replacement : b) => Process <s, b>
	}

type List <a> =
	({ CONS : 'Nil' } | { CONS : 'Cons' }) &
	{
		/**` (.pipe) :: List a -> (List a -> b) -> b `*/
		pipe : <b>(morphism : (xs : List <a>) => b) => b

		/**` (.eq) :: (Eq a) => List a -> List a -> Boolean `*/
		eq : (xs : List <Eq <a>>) => boolean

		/**` (.bind) :: List a -> (a -> List b) -> List b `*/
		bind : <b>(reaction : (element : a) => List <b>) => List <b>

		/**` (.fmap) :: List a -> (a -> b) -> List b `*/
		fmap : <b>(morphism : (element : a) => b) => List <b>

		/**` (.bindto) :: List $ -> (String, $ -> List a) -> List $ `*/
		bindto : <k extends string, b>(name : k, reaction : ($ : a) => List <b>) => List <a & { [x in k] : b }>

		/**` (.fmapto) :: List $ -> (String, $ -> a) -> List $ `*/
		fmapto : <k extends string, b>(name : k, morphism : ($ : a) => b) => List <a & { [x in k] : b }>

		/**` (.cast) :: List a -> b -> List b `*/
		cast : <b>(value : b) => List <b>

		/**` (.link) :: List a -> List a -> List a `*/
		link : (xs : List <a>) => List <a>

		/**` (.head) :: a `*/
		head : a

		/**` (.tail) :: List a `*/
		tail : List <a>

		$head    ?: a
		$tail    ?: List <a>
		$last    ?: a
		$init    ?: List <a>
		$reverse ?: List <a>
		$len     ?: number
	}

type Either <a, b> =
	({ CONS : 'Left', INFO : a } | { CONS : 'Right', INFO : b }) &
	{
		/**` (.pipe) :: Either a b -> (Either a b -> c) -> c `*/
		pipe : <c>(morphism : (either : Either <a, b>) => c) => c

		/**` (.eq) :: (Eq a, Eq b) => Either a b -> Either a b -> Boolean `*/
		eq : (either : Either <Eq <a>, Eq <b>>) => boolean
	}

type Vector2 =
	{
		CONS : 'Vector2'

		/**` (.pipe) :: Vector2 -> (Vector2 -> a) -> a `*/
		pipe : <a>(morphism : (v : Vector2) => a) => a

		/**` (.eq) :: Vector2 -> Vector2 -> Boolean `*/
		eq : (v : Vector2) => boolean

		/**` (.x) :: Vector2 -> Number `*/
		x : number

		/**` (.y) :: Vector2 -> Number `*/
		y : number
	}

type Vector3 =
	{
		CONS : 'Vector3'

		/**` (.pipe) :: Vector3 -> (Vector3 -> a) -> a `*/
		pipe : <a>(morphism : (v : Vector3) => a) => a

		/**` (.eq) :: Vector3 -> Vector3 -> Boolean `*/
		eq : (v : Vector3) => boolean

		/**` (.x) :: Vector3 -> Number `*/
		x : number

		/**` (.y) :: Vector3 -> Number `*/
		y : number

		/**` (.z) :: Vector3 -> Number `*/
		z : number
	}

type Vector4 =
	{
		CONS : 'Vector4'

		/**` (.pipe) :: Vector4 -> (Vector4 -> a) -> a `*/
		pipe : <a>(morphism : (v : Vector4) => a) => a

		/**` (.eq) :: Vector4 -> Vector4 -> Boolean `*/
		eq : (v : Vector4) => boolean

		/**` (.x) :: Vector4 -> Number `*/
		x : number

		/**` (.y) :: Vector4 -> Number `*/
		y : number

		/**` (.z) :: Vector4 -> Number `*/
		z : number

		/**` (.w) :: Vector4 -> Number `*/
		w : number
	}

type Matrix2 =
	{
		CONS : 'Matrix2'

		/**` (.pipe) :: Matrix2 -> (Matrix2 -> a) -> a `*/
		pipe : <a>(morphism : (m : Matrix2) => a) => a

		/**` (.eq) :: Matrix2 -> Matrix2 -> boolean `*/
		eq : (m : Matrix2) => boolean

		/**` (.ix) :: Matrix2 -> Number `*/
		ix : number

		/**` (.jx) :: Matrix2 -> Number `*/
		jx : number

		/**` (.iy) :: Matrix2 -> Number `*/
		iy : number

		/**` (.jy) :: Matrix2 -> Number `*/
		jy : number
	}

type Matrix3 =
	{
		CONS : 'Matrix3'

		/**` (.pipe) :: Matrix3 -> (Matrix3 -> a) -> a `*/
		pipe : <a>(morphism : (m : Matrix3) => a) => a

		/**` (.eq) :: Matrix3 -> Matrix3 -> Boolean `*/
		eq : (m : Matrix3) => boolean

		/**` (.ix) :: Matrix3 -> Number `*/
		ix : number

		/**` (.jx) :: Matrix3 -> Number `*/
		jx : number

		/**` (.kx) :: Matrix3 -> Number `*/
		kx : number

		/**` (.iy) :: Matrix3 -> Number `*/
		iy : number

		/**` (.jy) :: Matrix3 -> Number `*/
		jy : number

		/**` (.ky) :: Matrix3 -> Number `*/
		ky : number

		/**` (.iz) :: Matrix3 -> Number `*/
		iz : number

		/**` (.jz) :: Matrix3 -> Number `*/
		jz : number

		/**` (.kz) :: Matrix3 -> Number `*/
		kz : number
	}

type Matrix4 =
	{
		CONS : 'Matrix4'

		/**` (.eq) :: Matrix4 -> Matrix4 -> boolean `*/
		eq : (m : Matrix4) => boolean

		/**` (.pipe) :: Matrix4 -> (Matrix4 -> a) -> a `*/
		pipe : <a>(morphism : (m : Matrix4) => a) => a

		/**` (.ix) :: Matrix4 -> Number `*/
		ix : number

		/**` (.jx) :: Matrix4 -> Number `*/
		jx : number

		/**` (.kx) :: Matrix4 -> Number `*/
		kx : number

		/**` (.lx) :: Matrix4 -> Number `*/
		lx : number

		/**` (.iy) :: Matrix4 -> Number `*/
		iy : number

		/**` (.jy) :: Matrix4 -> Number `*/
		jy : number

		/**` (.ky) :: Matrix4 -> Number `*/
		ky : number

		/**` (.ly) :: Matrix4 -> Number `*/
		ly : number

		/**` (.iz) :: Matrix4 -> Number `*/
		iz : number

		/**` (.jz) :: Matrix4 -> Number `*/
		jz : number

		/**` (.kz) :: Matrix4 -> Number `*/
		kz : number

		/**` (.lz) :: Matrix4 -> Number `*/
		lz : number

		/**` (.iw) :: Matrix4 -> Number `*/
		iw : number

		/**` (.jw) :: Matrix4 -> Number `*/
		jw : number

		/**` (.kw) :: Matrix4 -> Number `*/
		kw : number

		/**` (.lw) :: Matrix4 -> Number `*/
		lw : number
	}

type TextMeasurement =
	{
		CONS : 'TextMeasurement'

		/**` (.pipe) :: TextMeasurement -> (TextMeasurement -> a) -> a `*/
		pipe : <a>(morphism : (metrics : TextMeasurement) => a) => a

		/**` (.eq) :: TextMeasurement -> TextMeasurement -> Boolean `*/
		eq : (metrics : TextMeasurement) => boolean

		/**` (.text) :: TextMeasurement -> String `*/
		text : string

		/**` (.width) :: TextMeasurement -> Number `*/
		width : number

		/**` (.height) :: TextMeasurement -> Number `*/
		height : number
	}

type Mapping <a, b> =
	{
		CONS : 'Mapping'

		/**` (.codomain) :: (Eq a, Eq b) => Mapping a b -> a -> b `*/
		codomain : (domain : Eq <a>) => Eq <b>

		/**` (.domain) :: (Eq a, Eq b) => Mapping a b -> b -> a `*/
		domain : (codomain : Eq <b>) => Eq <a>
	}

/********************************************************************************************************************************/
// Definitions of Algebraic Data Type Constructors //

type Nothing <a>    = Maybe  <a>    & { CONS : 'Nothing' }
type Just    <a>    = Maybe  <a>    & { CONS : 'Just'    }
type Nil     <a>    = List   <a>    & { CONS : 'Nil'     }
type Cons    <a>    = List   <a>    & { CONS : 'Cons'    }
type Left    <a, b> = Either <a, b> & { CONS : 'Left'    }
type Right   <a, b> = Either <a, b> & { CONS : 'Right'   }

/********************************************************************************************************************************/
// Definitions of Enumerators //

enum X
{
	L    = 'X.L :: X',
	LL   = 'X.LL :: X',
	Rest = 'X.Rest :: X',
	R    = 'X.R :: X',
	RR   = 'X.RR :: X'
}

enum Y
{
	D    = 'Y.D :: Y',
	DD   = 'Y.DD :: Y',
	Rest = 'Y.Rest :: Y',
	U    = 'Y.U :: Y',
	UU   = 'Y.UU :: Y'
}

enum Z
{
	B    = 'Z.B :: Z',
	BB   = 'Z.BB :: Z',
	Rest = 'Z.Rest :: Z',
	F    = 'Z.F :: Z',
	FF   = 'Z.FF :: Z'
}

enum LineCap
{
	Butt   = 'LineCap.Butt :: LineCap',
	Round  = 'LineCap.Round :: LineCap',
	Square = 'LineCap.Square :: LineCap'
}

enum LineJoin
{
	Round = 'LineJoin.Round :: LineJoin',
	Bevel = 'LineJoin.Bevel :: LineJoin',
	Miter = 'LineJoin.Miter :: LineJoin'
}

enum TextAlign
{
	Start     = 'TextAlign.Start :: TextAlign',
	End       = 'TextAlign.End :: TextAlign',
	Leftside  = 'TextAlign.Leftside :: TextAlign',
	Rightside = 'TextAlign.Rightside :: TextAlign',
	Center    = 'TextAlign.Center :: TextAlign'
}

enum TextBaseline
{
	Top         = 'TextBaseline.Top :: TextBaseline',
	Hanging     = 'TextBaseline.Hanging :: TextBaseline',
	Middle      = 'TextBaseline.Middle :: TextBaseline',
	Alphabetic  = 'TextBaseline.Alphabetic :: TextBaseline',
	Ideographic = 'TextBaseline.Ideographic :: TextBaseline',
	Bottom      = 'TextBaseline.Bottom :: TextBaseline'
}

enum Composition
{
	SourceOver      = 'Composition.SourceOver :: Composition',
	SourceAtop      = 'Composition.SourceAtop :: Composition',
	SourceIn        = 'Composition.SourceIn :: Composition',
	SourceOut       = 'Composition.SourceOut :: Composition',
	DestinationOver = 'Composition.DestinationOver :: Composition',
	DestinationAtop = 'Composition.DestinationAtop :: Composition',
	DestinationIn   = 'Composition.DestinationIn :: Composition',
	DestinationOut  = 'Composition.DestinationOut :: Composition',
	Lighter         = 'Composition.Lighter :: Composition',
	Xor             = 'Composition.Xor :: Composition',
	Copy            = 'Composition.Copy :: Composition',
	Multiply        = 'Composition.Multiply :: Composition',
	Screen          = 'Composition.Screen :: Composition',
	Overlay         = 'Composition.Overlay :: Composition',
	Darken          = 'Composition.Darken :: Composition',
	Lighten         = 'Composition.Lighten :: Composition',
	ColorDodge      = 'Composition.ColorDodge :: Composition',
	ColorBurn       = 'Composition.ColorBurn :: Composition',
	HardLight       = 'Composition.HardLight :: Composition',
	SoftLight       = 'Composition.SoftLight :: Composition',
	Difference      = 'Composition.Difference :: Composition',
	Exclusion       = 'Composition.Exclusion :: Composition',
	Hue             = 'Composition.Hue :: Composition',
	Saturation      = 'Composition.Saturation :: Composition',
	Color           = 'Composition.Color :: Composition',
	Luminosity      = 'Composition.Luminosity :: Composition'
}
/********************************************************************************************************************************/
// Globalization of Typeclass and Algebraic Data Types //

/**` pipe :: (Pipe a) => a -> (a -> b) -> b `*/
const pipe = <a>(value : Pipe <a>) => <b>(morphism : (fluid : Pipe <a>) => b) : b => value .pipe (morphism)

/**` eq :: (Eq a) => a -> a -> Boolean `*/
const eq = <a>(x : Eq <a>) => (y : Eq <a>) : boolean => x .eq (y)

/**` neq :: (Eq a) => a -> a -> Boolean `*/
const neq = <a>(x : Eq <a>) => (y : Eq <a>) : boolean => !x .eq (y)

/**` fst :: Pair a b -> a `*/
const fst = <a, b>(pair : Pair <a, b>) : a => pair .fst

/**` snd :: Pair a b -> b `*/
const snd = <a, b>(pair : Pair <a, b>) : b => pair .snd

/**` link :: List a -> List a -> List a `*/
const link = <a>(firsts : List <a>) => (seconds : List <a>) : List <a> => firsts .link (seconds)

/**` head :: List a -> a `*/
const head = <a>(xs : List <a>) : a => xs .head

/**` tail :: List a -> List a `*/
const tail = <a>(xs : List <a>) : List <a> => xs .tail

/********************************************************************************************************************************/
// Implementations of Constants and Micro-Functions //

/**` E :: Number `*/
const E = 2.718281828459045

/**` LN2 :: Number `*/
const LN2 = 0.6931471805599453

/**` LN10 :: Number `*/
const LN10 = 2.302585092994046

/**` LOG2E :: Number `*/
const LOG2E = 1.4426950408889634

/**` LOG10E :: Number `*/
const LOG10E = 0.4342944819032518

/**` PI :: Number `*/
const PI = 3.141592653589793

/**` TAU :: Number `*/
const TAU = 6.283185307179586

/**` INVSQRT2 :: Number `*/
const INVSQRT2 = 0.7071067811865476

/**` SQRT2 :: Number `*/
const SQRT2 = 1.4142135623730951

/**` abs :: Number -> Number `*/
const abs = Math.abs

/**` acos :: Number -> Number `*/
const acos = Math.acos

/**` acosh :: Number -> Number `*/
const acosh = Math.acosh

/**` add :: Number -> Number -> Number `*/
const add = (x : number) => (y : number) : number => x + y

/**` AND :: Number -> Number -> Number `*/
const AND = (x : number) => (y : number) : number => x & y

/**` and :: Boolean -> Boolean -> Boolean `*/
const and = (x : boolean) => (y : boolean) : boolean => x && y

/**` applyWhen :: Boolean -> (a -> a) -> a -> a `*/
const applyWhen = (condition : boolean) => <a>(f : (x : a) => a) : ((x : a) => a) => condition ? f : id

/**` approx :: Number -> Number -> Number -> Boolean `*/
const approx = (x : number) => (y : number) => (error : number) : boolean => Math.abs (x - y) < error

/**` napprox :: Number -> Number -> Number -> Boolean `*/
const napprox = (x : number) => (y : number) => (error : number) : boolean => Math.abs (x - y) > error

/**` asin :: Number -> Number `*/
const asin = Math.asin

/**` asinh :: Number -> Number `*/
const asinh = Math.asinh

/**` atan :: Number -> Number `*/
const atan = Math.atan

/**` atan2 :: Number -> Number -> Number `*/
const atan2 = (y : number) => (x : number) : number => Math.atan2 (y, x)

/**` ratan2 :: Number -> Number -> Number `*/
const ratan2 = (x : number) => (y : number) : number => Math.atan2 (y, x)

/**` atanh :: Number -> Number `*/
const atanh = Math.atanh

/**` BIT :: Boolean -> Number `*/
const BIT = (x : boolean) : (0 | 1) => x ? 1 : 0

/**` cbrt :: Number -> Number `*/
const cbrt = Math.cbrt

/**` ceil :: Number -> Number `*/
const ceil = Math.ceil

/**` clz32 :: Number -> Number `*/
const clz32 = Math.clz32

/**` cos :: Number -> Number `*/
const cos = Math.cos

/**` cosh :: Number -> Number `*/
const cosh = Math.cosh

/**` diff :: Number -> Number -> Number `*/
const diff = (x : number) => (y : number) : number => Math.abs (x - y)

/**` div :: Number -> Number -> Number `*/
const div = (x : number) => (y : number) : number => x / y

/**` rdiv :: Number -> Number -> Number `*/
const rdiv = (y : number) => (x : number) : number => x / y

/**` even :: Number -> Boolean `*/
const even = (x : number) : boolean => x % 2 === 0

/**` exp :: Number -> Number `*/
const exp = Math.exp

/**` expm1 :: Number -> Number `*/
const expm1 = Math.expm1

/**` flip :: (a -> b -> c) -> b -> a -> c `*/
const flip = <a, b, c>(f : (x : a) => (y : b) => c) => (y : b) => (x : a) : c => f (x) (y)

/**` floor :: Number -> Number `*/
const floor = Math.floor

/**` fround :: Number -> Number `*/
const fround = Math.fround

/**` greater :: Number -> Number -> Boolean `*/
const greater = (x : number) => (y : number) : boolean => y > x

/**` greaterEqual :: Number -> Number -> Boolean `*/
const greaterEqual = (x : number) => (y : number) : boolean => y >= x

/**` gt :: Number -> Number -> Boolean `*/
const gt = (x : number) => (y : number) : boolean => x > y

/**` gte :: Number -> Number -> Boolean `*/
const gte = (x : number) => (y : number) : boolean => x >= y

/**` id :: a -> a `*/
const id = <a>(x : a) : a => x

/**` isInsideExclusive :: Number -> Number -> Number -> Boolean `*/
const isInsideExclusive = (n : number) => (lower : number) => (upper : number) : boolean => lower < n && n < upper

/**` isInsideInclusive :: Number -> Number -> Number -> Boolean `*/
const isInsideInclusive = (n : number) => (lower : number) => (upper : number) : boolean => lower <= n && n <= upper

/**` isOutsideExclusive :: Number -> Number -> Number -> Boolean `*/
const isOutsideExclusive = (n : number) => (lower : number) => (upper : number) : boolean => n < lower || upper < n

/**` isOutsideInclusive :: Number -> Number -> Number -> Boolean `*/
const isOutsideInclusive = (n : number) => (lower : number) => (upper : number) : boolean => n <= lower || upper <= n

/**` ln :: Number -> Number `*/
const ln = Math.log

/**` log10 :: Number -> Number `*/
const log10 = Math.log10

/**` lnp1 :: Number -> Number `*/
const lnp1 = Math.log1p

/**` log2 :: Number -> Number `*/
const log2 = Math.log2

/**` LSHIFT :: Number -> Number `*/
const LSHIFT = (x : number) => (y : number) : number => x << y

/**` rLSHIFT :: Number -> Number `*/
const rLSHIFT = (y : number) => (x : number) : number => x << y

/**` lerp :: Number -> Number -> Number -> Number `*/
const lerp = (t : number) => (x : number) => (y : number) : number => x + (y - x) * t

/**` less :: Number -> Number -> Boolean `*/
const less = (x : number) => (y : number) : boolean => y < x

/**` lessEqual :: Number -> Number -> Boolean `*/
const lessEqual = (x : number) => (y : number) : boolean => y <= x

/**` lt :: Number -> Number -> Boolean `*/
const lt = (x : number) => (y : number) : boolean => x < y

/**` lte :: Number -> Number -> Boolean `*/
const lte = (x : number) => (y : number) : boolean => x <= y

/**` max :: Number -> Number -> Number `*/
const max = (x : number) => (y : number) : number => Math.max (x, y)

/**` min :: Number -> Number -> Number `*/
const min = (x : number) => (y : number) : number => Math.min (x, y)

/**` mod :: Number -> Number -> Number `*/
const mod = (x : number) => (y : number) : number => x % y

/**` rmod :: Number -> Number -> Number `*/
const rmod = (y : number) => (x : number) : number => x % y

/**` mul :: Number -> Number -> Number `*/
const mul = (x : number) => (y : number) : number => x * y

/**` NAND :: Number -> Number -> Number `*/
const NAND = (x : number) => (y : number) : number => ~(x & y)

/**` nand :: Boolean -> Boolean -> Boolean `*/
const nand = (x : boolean) => (y : boolean) : boolean => !(x && y)

/**` negate :: Number -> Number `*/
const negate = (x : number) : number => -x

/**` NOR :: Number -> Number -> Number `*/
const NOR = (x : number) => (y : number) : number => ~(x | y)

/**` nor :: Boolean -> Boolean -> Boolean `*/
const nor = (x : boolean) => (y : boolean) : boolean => !(x || y)

/**` NOT :: Number -> Number `*/
const NOT = (x : number) : number => ~x

/**` not :: Boolean -> Boolean `*/
const not = (x : boolean) : boolean => !x

/**` odd :: Number -> Boolean `*/
const odd = (x : number) : boolean => Math.abs (x) % 2 === 1

/**` OR :: Number -> Number -> Number `*/
const OR = (x : number) => (y : number) : number => x | y

/**` or :: Boolean -> Boolean -> Boolean `*/
const or = (x : boolean) => (y : boolean) : boolean => x || y

/**` pow :: Number -> Number -> Number `*/
const pow = (x : number) => (y : number) : number => x ** y

/**` rpow :: Number -> Number -> Number `*/
const rpow = (y : number) => (x : number) : number => x ** y

/**` pythagoras :: Number -> Number -> Number `*/
const pythagoras = (x : number) => (y : number) : number => Math.sqrt (x * x + y * y)

/**` reciprocate :: Number -> Number `*/
const reciprocate = (x : number) : number => 1 / x

/**` round :: Number -> Number `*/
const round = Math.round

/**` RSHIFT :: Number -> Number -> Number `*/
const RSHIFT = (x : number) => (y : number) : number => x >> y

/**` rRSHIFT :: Number -> Number -> Number `*/
const rRSHIFT = (y : number) => (x : number) : number => x >> y

/**` sign :: Number -> Number `*/
const sign = Math.sign

/**` sin :: Number -> Number `*/
const sin = Math.sin

/**` sinh :: Number -> Number `*/
const sinh = Math.sinh

/**` sqrt :: Number -> Number `*/
const sqrt = Math.sqrt

/**` sub :: Number -> Number -> Number `*/
const sub = (x : number) => (y : number) : number => x - y

/**` rsub :: Number -> Number -> Number `*/
const rsub = (y : number) => (x : number) : number => x - y

/**` tan :: Number -> Number `*/
const tan = Math.tan

/**` tanh :: Number -> Number `*/
const tanh = Math.tanh

/**` toHexColor :: Number -> String `*/
const toHexColor = (decimal : number) : string => `#${((~~Math.abs (decimal)) % 16777216) .toString (16) .padStart (6, '0')}`

/**` trunc :: Number -> Number `*/
const trunc = (x : number) : number => ~~x

/**` URSHIFT :: Number -> Number -> Number `*/
const URSHIFT = (x : number) => (y : number) : number => x >>> y

/**` rURSHIFT :: Number -> Number -> Number `*/
const rURSHIFT = (y : number) => (x : number) : number => x >>> y

/**` XOR :: Number -> Number -> Number `*/
const XOR = (x : number) => (y : number) : number => x ^ y

/**` xor :: Boolean -> Boolean -> Boolean `*/
const xor = (x : boolean) => (y : boolean) : boolean => x !== y

/********************************************************************************************************************************/
// Implementation of Algebraic Data Type Constructors //

Boolean.prototype.pipe = Number.prototype.pipe = (String.prototype.pipe = function (f) { return f (this as any) }) as any
Boolean.prototype.eq   = Number.prototype.eq   = (String.prototype.eq   = function (x) { return this === x      }) as any

/**` Pair :: (a, b) -> Pair a b `*/
const Pair = <a, b>(first : a, second : b) : Pair <a, b> =>
	({
		CONS : 'Pair',
		pipe (f) { return f (this) },
		eq   : x => x .fst .eq (first as Eq <a>) && x .snd .eq (second as Eq <b>),
		fst  : first,
		snd  : second
	})

/**` IO :: (() -> a) -> IO a `*/
const IO = <a>(sideeffect : () => a) : IO <a> =>
	({
		CONS   : 'IO',
		INFO   : sideeffect,
		pipe   (f) { return f (this) },
		bind   : f => IO (() => f (sideeffect ()).INFO ()),
		fmap   : f => IO (() => f (sideeffect ())),
		bindto : (k, f) =>
			IO (() => {
				const $ = sideeffect ()
				return { ...$, [k] : f ($).INFO () } as any
			}),
		fmapto : (k, f) =>
			IO (() => {
				const $ = sideeffect ()
				return { ...$, [k] : f ($) } as any
			}),
		then   : x => IO (() => (sideeffect (), x.INFO ())),
		side   : x =>
			IO (() => {
				const y = sideeffect ()
				x.INFO ()
				return y
			}),
		also   : f =>
			IO (() => {
				const y = sideeffect ()
				f(y).INFO ()
				return y
			}),
		cast    : x => IO (() => (sideeffect (), x))
	})

/**` Nothing :: Maybe a `*/
const Nothing : Maybe <any> =
	{
		CONS   : 'Nothing',
		pipe   : f => f (Nothing),
		eq     : x => x === Nothing,
		bind   : _ => Nothing,
		fmap   : _ => Nothing,
		bindto : _ => Nothing,
		fmapto : _ => Nothing,
		cast   : _ => Nothing
	}

/**` Just :: a -> Maybe a `*/
const Just = <a>(value : a) : Maybe <a> =>
	({
		CONS   : 'Just',
		INFO   : value,
		pipe   (f) { return f (this) },
		eq     : x => x.CONS === 'Just' && x.INFO .eq (value as Eq <a>),
		bind   : f => f (value),
		fmap   : f => Just (f (value)),
		bindto : (k, f) => f (value) .fmap (x => ({ ...value, [k] : x }) as any),
		fmapto : (k, f) => Just ({ ...value, [k] : f (value) } as any),
		cast   : Just
	})

/**` Process :: (s -> Pair s a) -> Process s a `*/
const Process = <s, a>(computation : (state : s) => Pair <s, a>) : Process <s, a> =>
	({
		CONS : 'Process',
		INFO : computation,
		pipe (f) { return f (this) },
		bind : f =>
			Process (s => {
				const x = computation (s)
				return f (x .snd).INFO (x .fst)
			}),
		fmap : f =>
			Process (s => {
				const x = computation (s)
				return Pair (x .fst, f (x .snd))
			}),
		bindto : (k, f) =>
			Process (s => {
				const x = computation (s), y = f (x .snd).INFO (x .fst)
				return Pair (y .fst, { ...x .snd, [k] : y .snd } as any)
			}),
		fmapto : (k, f) =>
			Process (s => {
				const x = computation (s)
				return Pair (x .fst, { ...x .snd, [k] : f (x .snd) } as any)
			}),
		then   : x => Process (s => x.INFO (computation (s) .fst)),
		side   : x =>
			Process (s => {
				const y = computation (s)
				return Pair (x.INFO (y .fst) .fst, y.snd)
			}),
		also   : f =>
			Process (s => {
				const y = computation (s)
				return Pair (f (y .snd).INFO (y .fst) .fst, y.snd)
			}),
		cast   : x => Process (s => Pair (computation (s) .fst, x))
	})

/**` Nil :: List a `*/
const Nil : List <any> =
	{
		CONS   : 'Nil',
		pipe   : f  => f (Nil),
		eq     : xs => xs === Nil,
		bind   : _  => Nil,
		fmap   : _  => Nil,
		bindto : _  => Nil,
		fmapto : _  => Nil,
		cast   : _  => Nil,
		link   : xs => xs,
		get head () { return THROW (`'(.head)' cannot be used on an empty list`) },
		get tail () { return THROW (`'(.tail)' cannot be used on an empty list`) }
	}

/**` Cons :: (() -> a) -> (() -> List a) -> List a `*/
const Cons = <a>(lfirst: () => a) => (lrest : () => List <a>) : List <a> =>
	({
		CONS   : 'Cons',
		pipe   (f) { return f (this) },
		eq     (xs)
		{
			let ys : List <a> = this
			for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs .tail, ys = ys .tail)
				if (i === MAX_LIST_OPS)
					ERROR.MAX_LIST_OPS ('(.eq)', 'Cons')
				else if (!xs .head .eq (ys .head as Eq <a>))
					return false
			return xs.CONS === ys.CONS
		},
		bind   (f)
		{
			const xs = f (this .head)
			return xs.CONS === 'Nil'
				? this .tail .bind (f)
				: Cons (() => xs .head) (() => xs .tail .link (this .tail .bind (f)))
		},
		fmap   (f)
		{
			return Cons (() => f (this .head)) (() => this .tail .fmap (f))
		},
		bindto (k, f)
		{
			return this .bind ($ => f ($) .fmap (x => ({ ...$, [k] : x }) as any))
		},
		fmapto (k, f)
		{
			return this .fmap ($ => ({ ...$, [k] : f ($) }) as any)
		},
		cast   (x)
		{
			return this .fmap (_ => x)
		},
		link   (xs)
		{
			return xs.CONS === 'Nil'
				? this
				: Cons (() => this .head) (() => this .tail .link (xs))
		},
		get head () { return this.$head ??= lfirst () },
		get tail () { return this.$tail ??= lrest  () }
	})

/**` List :: (...a) -> List a `*/
const List = <a>(...elements : Array <a>) : List <a> =>
{
	let xs : List <a> = Nil
	for (let i = elements .length - 1; ~i; --i)
		xs = prepend (elements[i] as a) (xs)
	return xs
}

/**` Left :: a -> Either a b `*/
const Left = <a, b>(lefty : a) : Either <a, b> =>
	({
		CONS : 'Left',
		INFO : lefty,
		pipe (f) { return f (this) },
		eq   : x => x.CONS === 'Left' && x.INFO .eq (lefty as Eq <a>)
	})

/**` Right :: b -> Either a b `*/
const Right = <a, b>(righty : b) : Either <a, b> =>
	({
		CONS : 'Right',
		INFO : righty,
		pipe (f) { return f (this) },
		eq   : x => x.CONS === 'Right' && x.INFO .eq (righty as Eq <b>)
	})

/**` Vector2 :: (Number, Number) -> Vector2 `*/
const Vector2 = (x : number, y : number) : Vector2 =>
	({
		CONS : 'Vector2',
		eq   : v => v.x === x && v.y === y,
		pipe (f) { return f (this) },
		x, y
	})

/**` Vector3 :: (Number, Number, Number) -> Vector3 `*/
const Vector3 = (x : number, y : number, z : number) : Vector3 =>
	({
		CONS : 'Vector3',
		eq   : v => v.x === x && v.y === y && v.z === z,
		pipe (f) { return f (this) },
		x, y, z
	})

/**` Vector4 :: (Number, Number, Number, Number) -> Vector4 `*/
const Vector4 = (x : number, y : number, z : number, w : number) : Vector4 =>
	({
		CONS : 'Vector4',
		eq   : v => v.x === x && v.y === y && v.z === z && v.w === w,
		pipe (f) { return f (this) },
		x, y, z, w
	})

/**` Matrix2 :: (...4 Number) -> Matrix2 `*/
const Matrix2 = (
		ix : number, jx : number,
		iy : number, jy : number
	) : Matrix2 =>
	({
		CONS : 'Matrix2',
		pipe (f) { return f (this) },
		eq   : m =>
			m.ix === ix && m.jx === jx &&
			m.iy === iy && m.jy === jy,
		ix, jx, iy, jy
	})

/**` Matrix3 :: (...9 Number) -> Matrix3 `*/
const Matrix3 = (
		ix : number, jx : number, kx : number,
		iy : number, jy : number, ky : number,
		iz : number, jz : number, kz : number
	) : Matrix3 =>
	({
		CONS : 'Matrix3',
		pipe (f) { return f (this) },
		eq : m =>
			m.ix === ix && m.jx === jx && m.kx === kx &&
			m.iy === iy && m.jy === jy && m.ky === ky &&
			m.iz === iz && m.jz === jz && m.kz === kz,
		ix, jx, kx, iy, jy, ky, iz, jz, kz
	})

/**` Matrix4 :: (...16 Number) -> Matrix4 `*/
const Matrix4 = (
		ix : number, jx : number, kx : number, lx : number,
		iy : number, jy : number, ky : number, ly : number,
		iz : number, jz : number, kz : number, lz : number,
		iw : number, jw : number, kw : number, lw : number
	) : Matrix4 =>
	({
		CONS : 'Matrix4',
		pipe (f) { return f (this) },
		eq   : m =>
			m.ix === ix && m.jx === jx && m.kx === kx && m.lx === lx &&
			m.iy === iy && m.jy === jy && m.ky === ky && m.ly === ly &&
			m.iz === iz && m.jz === jz && m.kz === kz && m.lz === lz &&
			m.iw === iw && m.jw === jw && m.kw === kw && m.lw === lw,
		ix, jx, kx, lx, iy, jy, ky, ly, iz, jz, kz, lz, iw, jw, kw, lw
	})

/**` TextMeasurement :: String -> Number -> Number -> TextMeasurement `*/
const TextMeasurement = (text : string) => (width : number) => (height : number) : TextMeasurement =>
	({
		CONS : 'TextMeasurement',
		pipe (f) { return f (this) },
		eq   : m => m.text === text && m.width === width && m.height === height,
		text, width, height
	})

/**` Mapping :: (...Pair a b) -> Mapping a b `*/
const Mapping = <a, b>(...pairs : Array <[Eq <a>, Eq <b>]>) : Mapping <a, b> =>
	({
		CONS     : 'Mapping',
		codomain : x => (
			pairs .find (p => p[0] .eq (x as Eq <a>))
				?? THROW (`'(.codomain)' was non-exhaustive in 'Mapping'; no corresponding codomain for value '${x}'`)
		)[1],
		domain   : x => (
			pairs .find (p => p[1] .eq (x as Eq <b>))
				?? THROW (`'(.domain)' was non-exhaustive in 'Mapping'; no corresponding domain for value '${x}'`)
		)[0]
	})

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for 'Pair' //

/**` curry :: (Pair a b -> c) -> a -> b -> c `*/
const curry = <a, b, c>(f : (parameters : Pair <a, b>) => c) => (first : a) => (second : b) : c =>
	f (Pair (first, second))

/**` uncurry :: (a -> b -> c) -> Pair a b -> c `*/
const uncurry = <a, b, c>(f : (first : a) => (second : b) => c) => (parameters : Pair <a, b>) : c =>
	f (parameters .fst) (parameters .snd)

/**` swap :: Pair a b -> Pair b a `*/
const swap = <a, b>(pair : Pair <a, b>) : Pair <b, a> =>
	Pair (pair .snd, pair .fst)

/**` ffst :: (a -> c) -> Pair a b -> Pair c b `*/
const ffst = <a, c>(morphism : (first : a) => c) => <b>(pair : Pair <a, b>) : Pair <c, b> =>
	Pair (morphism (pair .fst), pair .snd)

/**` fsnd :: (b -> c) -> Pair a b -> Pair a c `*/
const fsnd = <b, c>(morphism : (second : b) => c) => <a>(pair : Pair <a, b>) : Pair <a, c> =>
	Pair (pair .fst, morphism (pair .snd))

/**` fboth :: (a -> b) -> Pair a a -> Pair b b `*/
const fboth = <a, b>(morphism : (value : a) => b) => (pair : Pair <a, a>) : Pair <b, b> =>
	Pair (morphism (pair .fst), morphism (pair .snd))

/**` pick :: Boolean -> Pair a a -> a `*/
const pick = (bool : boolean) : (<a>(pair : Pair <a, a>) => a) =>
	bool ? fst : snd

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for 'IO' //

/**` idle :: IO () `*/
const idle =
	IO (() => null)

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for 'Maybe' //

/**` isNothing :: Maybe a -> Boolean `*/
const isNothing = <a>(maybe : Maybe <a>) : maybe is Nothing <a> => maybe.CONS === 'Nothing'

/**` isJust :: Maybe a -> Boolean `*/
const isJust = <a>(maybe : Maybe <a>) : maybe is Just <a> => maybe.CONS === 'Just'

/**` ffromMaybe :: b -> (a -> b) -> Maybe a -> b `*/
const ffromMaybe = <b>(fallback : b) => <a>(morphism : (possibility : a) => b) => (maybe : Maybe <a>) : b =>
	maybe.CONS === 'Nothing'
		? fallback
		: morphism (maybe.INFO)

/**` fromJust :: Maybe a -> a `*/
const fromJust = <a>(maybe : Maybe <a>) : a =>
	maybe.CONS === 'Nothing'
		? THROW (`'fromJust' cannot be used on 'Nothing'`)
		: maybe.INFO

/**` fromMaybe :: a -> Maybe a -> a `*/
const fromMaybe = <a>(fallback : a) => (maybe : Maybe <a>) : a =>
	maybe.CONS === 'Nothing'
		? fallback
		: maybe.INFO

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for 'Process' //

/**` put :: s -> Process s a -> Process s a `*/
const put = <s>(replacement : s) => <a>(process : Process <s, a>) : Process <s, a> =>
	Process (s => Pair (replacement, process.INFO (s) .snd))

/**` get :: Process s a -> Process s s `*/
const get = <s, a>(process : Process <s, a>) : Process <s, s> =>
	Process (s => {
		const x = process.INFO (s) .fst
		return Pair (x, x)
	})

/**` runProcess :: Process s a -> s -> Pair s a `*/
const runProcess = <s, a>(process : Process <s, a>) => (state : s) : Pair <s, a> =>
	process.INFO (state)

/**` execProcess :: Process s a -> s -> s `*/
const execProcess = <s, a>(process : Process <s, a>) => (state : s) : s =>
	process.INFO (state) .fst

/**` evalProcess :: Process s a -> s -> a `*/
const evalProcess = <s, a>(process : Process <s, a>) => (state : s) : a =>
	process.INFO (state) .snd

/**` mapProcess :: (Pair s a -> Pair s b) -> Process s a -> Process s b `*/
const mapProcess = <s, a, b>(morphism : (result : Pair <s, a>) => Pair <s, b>) => (process : Process <s, a>) : Process <s, b> =>
	Process (s => morphism (process.INFO (s)))

/**` endomapState :: (s -> s) -> Process s a -> Process s a `*/
const endomapState = <s, a>(endomorphism : (state : s) => s) => (process : Process <s, a>) : Process <s, a> =>
	Process (s => {
		const x = process.INFO (s)
		return Pair (endomorphism (x .fst), x .snd)
	})

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for 'List' //

/**` isNil :: List a -> Boolean `*/
const isNil = <a>(xs : List <a>) : xs is Nil <a> => xs.CONS === 'Nil'

/**` isCons :: List a -> Boolean `*/
const isCons = <a>(xs : List <a>) : xs is Cons <a> => xs.CONS === 'Cons'

/**` array :: List a -> [a] `*/
const array = <a>(xs : List <a>) : Array <a> =>
{
	const ys : Array <a> = []
	for (let i = 0; xs.CONS === 'Cons'; ++i, ys.push(xs .head), xs = xs .tail)
		if (i === MAXARRAY)
		{
			console.warn(`'array' has reached the maximum array representation possible (${MAXARRAY}) for the given list`)
			break
		}

	return ys
}

/**` string :: List String -> String `*/
const string = (xs : List <string>) : string =>
{
	let str = ""
	for (let i = 0; xs.CONS === 'Cons'; ++i, str += xs .head)
		if (i === MAXSTRING)
		{
			console.warn(`'string' has reached the maximum string representation possible (${MAXSTRING}) for the given list`)
			break
		}
	return str
}

/**` chars :: String -> List String `*/
const chars = (str : string) : List <string> =>
	str
		?
			({
				CONS   : 'Cons',
				pipe   (f) { return f (this) },
				eq     (xs)
				{
					for (let i = 0; i < str.length && xs.CONS === 'Cons'; ++i, xs = xs .tail)
						if (xs .head !== str[i])
							return false
					return xs.CONS === 'Nil'
				},
				bind   (f)
				{
					const xs = f (str [0]!)
					return xs.CONS === 'Nil'
						? this .tail .bind (f)
						: Cons (() => xs .head) (() => xs .tail .link (this .tail .bind (f)))
				},
				fmap   (f)
				{
					return Cons (() => f (str[0]!)) (() => this .tail .fmap (f))
				},
				bindto : _ => THROW (`'(.bindto)' was used on a list of characters; likely done on accident | origin : 'chars'`),
				fmapto : _ => THROW (`'(.fmapto)' was used on a list of characters; likely done on accident | origin : 'chars'`),
				cast   (x)
				{
					return lprepend (x) (() => this .tail .cast (x))
				},
				link   (xs)
				{
					return lprepend (str[0]!) (() => this .tail .link (xs))
				},
				head  : str[0]!,
				get tail () { return this.$tail ??= chars (str .slice (1)) },
				$head : str[0]!,
				$last : str[str.length - 1],
				$len  : str.length
			})
		: Nil

/**` singleton :: a -> List a `*/
const singleton = <a>(value : a) : List <a> =>
	({
		CONS   : 'Cons',
		pipe   (f) { return f (this) },
		eq     : xs => xs.CONS === 'Cons' && xs .tail.CONS === 'Nil' && xs .head .eq (value as Eq <a>),
		bind   : f  => f (value),
		fmap   : f => singleton (f (value)),
		bindto : (k, f) => f (value) .fmap (x => ({ ...value, [k] : x }) as any),
		fmapto : (k, f) => singleton ({ ...value, [k] : f (value) } as any),
		cast   : singleton,
		link   : prepend (value),
		head   : value,
		tail   : Nil,
		$head  : value,
		$tail  : Nil,
		$last  : value,
		$init  : Nil,
		get $reverse () { return this },
		$len   : 1
	})

/**` prepend :: a -> List a -> List a `*/
const prepend = <a>(first : a) => (rest : List <a>) : List <a> =>
	({
		CONS   : 'Cons',
		pipe   (f) { return f (this) },
		eq     : xs =>
		{
			if (xs.CONS === 'Nil' || !xs .head .eq (first as Eq <a>))
				return false
			let ys = rest
			xs = xs .tail
			for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs .tail, ys = ys .tail)
				if (i === MAX_LIST_OPS)
					ERROR.MAX_LIST_OPS ('(.eq)', 'prepend')
				else if (!xs .head .eq (ys .head as Eq <a>))
					return false
			return xs.CONS === ys.CONS
		},
		bind   : f =>
		{
			const xs = f (first)
			return xs.CONS === 'Nil'
				? rest .bind (f)
				: Cons (() => xs .head) (() => xs .tail .link (rest .bind (f)))
		},
		fmap   : f => Cons (() => f (first)) (() => rest .fmap (f)),
		bindto : (k, f) =>
		{
			const xs = f (first) .fmap (x => ({ ...first, [k] : x }) as any)
			return xs.CONS === 'Nil'
				? rest .bindto (k, f)
				: Cons (() => xs .head) (() => xs .tail .link (rest .bindto (k, f)))
		},
		fmapto : (k, f) => Cons (() => ({ ...first, [k] : f (first) }) as any) (() => rest .fmapto (k, f)),
		cast   : x  => lprepend (x)     (() => rest .cast (x )),
		link   : xs => lprepend (first) (() => rest .link (xs)),
		head  : first,
		tail  : rest,
		$head : first,
		$tail : rest,
		$last : rest.CONS === 'Nil' ? first : rest.$last,
		$len  : rest.$len === undefined ? undefined : 1 + rest.$len
	})

/**` lprepend :: a -> (() -> List a) -> List a `*/
const lprepend = <a>(first : a) => (lrest : () => List <a>) : List <a> =>
	({
		CONS   : 'Cons',
		pipe   (f) { return f (this) },
		eq     (xs)
		{
			if (xs.CONS === 'Nil' || !xs .head .eq (first as Eq <a>))
				return false
			let ys = this .tail
			xs = xs .tail
			for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs .tail, ys = ys .tail)
				if (i === MAX_LIST_OPS)
					ERROR.MAX_LIST_OPS ('(.eq)', 'lprepend')
				else if (!xs .head .eq (ys .head as Eq <a>))
					return false
			return xs.CONS === ys.CONS
		},
		bind   (f)
		{
			const xs = f (first)
			return xs.CONS === 'Nil'
				? this .tail .bind (f)
				: Cons (() => xs .head) (() => xs .tail .link (this .tail .bind (f)))
		},
		fmap   (f)
		{
			return Cons (() => f (first)) (() => this .tail .fmap (f))
		},
		bindto (k, f)
		{
			const xs = f (first) .fmap (x => ({ ...first, [k] : x }) as any)
			return xs.CONS === 'Nil'
				? this .tail .bindto (k, f)
				: Cons (() => xs .head) (() => xs .tail .link (this .tail .bindto (k, f)))
		},
		fmapto (k, f)
		{
			return Cons (() => ({ ...first, [k] : f (first) }) as any) (() => this .tail .fmapto (k, f))
		},
		cast   (x)
		{
			return lprepend (x) (() => this .tail .cast (x))
		},
		link   (xs)
		{
			return lprepend (first) (() => this .tail .link (xs))
		},
		head  : first,
		get tail () { return this.$tail ??= lrest () },
		$head : first
	})

/**` llprepend :: (() -> a) -> List a -> List a `*/
const llprepend = <a>(lfirst : () => a) => (rest : List <a>) : List <a> =>
	({
		CONS   : 'Cons',
		pipe   (f) { return f (this) },
		eq     (xs)
		{
			if (xs.CONS === 'Nil' || !xs .head .eq (this .head as Eq <a>))
				return false
			let ys = rest
			xs = xs .tail
			for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs .tail, ys = ys .tail)
				if (i === MAX_LIST_OPS)
					ERROR.MAX_LIST_OPS ('(.eq)', 'llprepend')
				else if (!xs .head .eq (ys .head as Eq <a>))
					return false
			return xs.CONS === ys.CONS
		},
		bind   (f)
		{
			const xs = f (this .head)
			return xs.CONS === 'Nil'
				? rest .bind (f)
				: Cons (() => xs .head) (() => xs .tail .link (rest .bind (f)))
		},
		fmap   (f)
		{
			return Cons (() => f (this .head)) (() => rest .fmap (f))
		},
		bindto (k, f)
		{
			const xs = f (this .head) .fmap (x => ({ ...this .head, [k] : x }) as any)
			return xs.CONS === 'Nil'
				? rest .bindto (k, f)
				: Cons (() => xs .head) (() => xs .tail .link (rest .bindto (k, f)))
		},
		fmapto (k, f)
		{
			return Cons (() => ({ ...this .head, [k] : f (this .head) }) as any) (() => rest .fmapto (k, f))
		},
		cast   (x)
		{
			return lprepend (x) (() => rest .cast (x))
		},
		link   (xs)
		{
			return Cons (() => this .head) (() => rest .link (xs))
		},
		get head () { return this.$head ??= lfirst () },
		tail  : rest,
		$tail : rest,
		$last : rest.$last,
		$len  : rest.$len === undefined ? undefined : 1 + rest.$len
	})

/**` repeat :: a -> List a `*/
const repeat = <a>(value : a) : List <a> =>
	({
		CONS   : 'Cons',
		pipe   (f) { return f (this) },
		eq     : xs =>
		{
			for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail)
				if (i === MAX_LIST_OPS)
					ERROR.MAX_LIST_OPS ('(.eq)', 'repeat')
				else if (!xs .head .eq (value as Eq <a>))
					return false
			return false
		},
		bind   : f =>
		{
			const xs = f (value)
			return xs.CONS === 'Nil'
				? ERROR.BINDING_NILS ('repeat', 'bind')
				: cycle (xs)
		},
		fmap   : f => repeat (f (value)),
		bindto : (k, f) =>
		{
			const xs = f (value) .fmap (x => ({ ...value, [k] : x }) as any)
			return xs.CONS === 'Nil'
				? ERROR.BINDING_NILS ('repeat', 'bindto')
				: cycle (xs)
		},
		fmapto : (k, f) => repeat ({ ...value, [k] : f (value) } as any),
		link   (_) { return this },
		cast   : repeat,
		head   : value,
		get tail  () { return this },
		$head  : value,
		get $tail () { return this },
		get $init () { return this }
	})

/**` cycle :: List a -> List a `*/
const cycle = <a>(pattern : List <a>) : List <a> =>
	pattern.CONS === 'Nil'
		? ERROR.ONLY_CONS ('cycle')
		:
			({
				CONS   : 'Cons',
				pipe   (f) { return f (this) },
				eq     (xs)
				{
					let ys : List <a> = this
					for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail, ys = ys .tail)
						if (i === MAX_LIST_OPS)
					ERROR.MAX_LIST_OPS ('(.eq)', 'cycle')
						else if (!xs .head .eq (ys .head as Eq <a>))
							return false
					return false
				},
				bind   (f)
				{
					const xs = pattern .bind (f)
					return xs.CONS === 'Nil'
						? ERROR.BINDING_NILS ('cycle', 'bind')
						: cycle (xs)
				},
				fmap   : f => cycle (pattern .fmap (f)),
				bindto (k, f)
				{
					const xs = pattern .bind ($ => f ($) .fmap (x => ({ ...$, [k] : x }) as any))
					return xs.CONS === 'Nil'
						? ERROR.BINDING_NILS ('cycle', 'bindto')
						: cycle (xs)
				},
				fmapto : (k, f) => cycle (pattern .fmap ($ => ({ ...$, [k] : f ($) }) as any)),
				link   (_) { return this },
				cast   : repeat,
				get head () { return this.$head ??= pattern .head },
				get tail () { return this.$tail ??= pattern .tail .link (this) },
				get $init () { return this }
			})

/**` iterate :: (a -> a) -> a -> List a `*/
const iterate = <a>(endomorphism : (value : a) => a) => (initial : a) : List <a> =>
	({
		CONS   : 'Cons',
		pipe   (f) { return f (this) },
		eq     (xs)
		{
			let ys : List <a> = this
			for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail, ys = ys .tail)
				if (i === MAX_LIST_OPS)
					ERROR.MAX_LIST_OPS ('(.eq)', 'iterate')
				else if (!xs .head .eq (ys .head as Eq <a>))
					return false
			return false
		},
		bind   (f)
		{
			const xs = f (initial)
			return xs.CONS === 'Nil'
				? this .tail .bind (f)
				: Cons (() => xs .head) (() => xs .tail .link (this .tail .bind (f)))
		},
		fmap   (f)
		{
			return Cons (() => f (initial)) (() => this .tail .fmap (f))
		},
		bindto (k, f)
		{
			const xs = f (initial) .fmap (x => ({ ...initial, [k] : x }) as any)
			return xs.CONS === 'Nil'
				? this .tail .bindto (k, f)
				: Cons (() => xs .head) (() => xs .tail .link (this .tail .bindto (k, f)))
		},
		fmapto (k, f)
		{
			return Cons (() => ({ ...initial, [k] : f (initial) }) as any) (() => this .fmapto (k, f))
		},
		link   (_) { return this },
		cast   : repeat,
		head   : initial,
		get tail () { return this.$tail ??= iterate (endomorphism) (endomorphism (initial)) },
		$head  : initial,
		get $init () { return this }
	})

/**` replicate :: Number -> a -> List a `*/
const replicate = (amount : number) => <a>(value : a) : List <a> =>
	Number.isInteger (value)
		? amount > 0
			?
				({
					CONS   : 'Cons',
					pipe   (f) { return f (this) },
					eq     : xs =>
					{
						for (let i = 0; i < amount; ++i, xs = xs .tail)
							if (xs.CONS === 'Nil' || !xs .head .eq (value as Eq <a>))
								return false
						return xs.CONS === 'Nil'
					},
					bind   : f => concat (replicate (amount) (f (value))),
					fmap   : f => replicate (amount) (f (value)),
					bindto : (k, f) => concat (replicate (amount) ({ ...value, [k] : f (value) } as any)),
					fmapto : (k, f) => replicate (amount) ({ ...value, [k] : f (value) } as any),
					cast   : replicate (amount),
					link   (xs)
					{
						return xs.CONS === 'Nil'
							? this
							: lprepend (value) (() => this .tail .link (xs))
					},
					head  : value,
					get tail () { return this.$tail ??= replicate (amount - 1) (value) },
					$head : value,
					$last : value,
					get $init    () { return this .tail },
					get $reverse () { return this },
					$len  : amount
				})
			: Nil
		: ERROR.ONLY_INTEGER ('replicate', amount)

/**` all :: (a -> Boolean) -> List a -> Boolean `*/
const all = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : boolean =>
{
	for (let i = 0; xs.CONS === 'Cons'; ++i)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('all')
		else if (predicate (xs .head))
			xs = xs .tail
		else
			return false
	return true
}

/**` any :: (a -> Boolean) -> List a -> Boolean `*/
const any = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : boolean =>
{
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('any')
		else if (predicate (xs .head))
			return true
	return false
}

/**` at :: Number -> List a -> a `*/
const at = (index : number) => <a>(xs : List <a>) : a =>
{
	if (index < 0 || !Number.isInteger (index))
		ERROR.ONLY_NATURAL ('at', index)
	for (let i = 0; i < index; ++i)
		if (xs.CONS === 'Nil')
			THROW (`'at' received an index beyond the list; stopped at index '${i}' with goal of '${index}'`)
		else xs = xs .tail
	if (xs.CONS === 'Nil')
		THROW (`'at' received an off-by-one error; cannot get index '${index}' in list of length ${index}`)
	return xs .head
}

/**` concat :: List (List a) -> List a `*/
const concat = <a>(xss : List <List <a>>) : List <a> =>
	xss.CONS === 'Nil'
		? Nil
		: xss .head.CONS === 'Nil'
			? concat (xss .tail)
			: Cons (() => xss .head .head) (() => xss .head .tail .link (concat (xss .tail)))

/**` drop :: Number -> List a -> List a `*/
const drop = (amount : number) => <a>(xs : List <a>) : List <a> =>
{
	if (!Number.isInteger (amount))
		ERROR.ONLY_INTEGER ('drop', amount)
	for (let i = 0; i < amount && xs.CONS === 'Cons'; ++i)
		xs = xs .tail
	return xs
}

/**` dropWhile :: (a -> Boolean) -> List a -> List a `*/
const dropWhile = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
{
	for (let i = 0; xs.CONS === 'Cons' && predicate (xs .head); ++i, xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('dropWhile')
	return xs
}

/**` elem :: (Eq a) => a -> List a -> Boolean `*/
const elem = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : boolean =>
{
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('elem')
		else if (xs .head .eq (value))
			return true
	return false
}

/**` elemIndices :: (Eq a) => a -> List a -> List Number `*/
const elemIndices = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : List <number> =>
	xs.CONS === 'Nil'
		? Nil
		: xs .head .eq (value)
			? lprepend (0) (() => elemIndices (value) (xs .tail) .fmap (x => x + 1))
			: elemIndices (value) (xs .tail) .fmap (x => x + 1)

/**` filter :: (a -> Boolean) -> List a -> List a `*/
const filter = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
	xs.CONS === 'Nil'
		? Nil
		: predicate (xs .head)
			? lprepend (xs .head) (() => filter (predicate) (xs .tail))
			: filter (predicate) (xs .tail)

/**` findIndices :: (a -> Boolean) -> List a -> List Number `*/
const findIndices = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <number> =>
	xs.CONS === 'Nil'
		? Nil
		: predicate (xs .head)
			? lprepend (0) (() => findIndices (predicate) (xs .tail) .fmap (x => x + 1))
			: findIndices (predicate) (xs .tail) .fmap (x => x + 1)

/**` foldl :: (b -> a -> b) -> b -> List a -> b `*/
const foldl = <a, b>(reducer : (leftside : b) => (rightside : a) => b) => (initial : b) => (xs : List <a>) : b =>
{
	let x = initial
	for (let i = 0; xs.CONS === 'Cons'; ++i, x = reducer (x) (xs .head), xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('foldl')
	return x
}

/**` foldl1 :: (a -> a -> a) -> List a -> a `*/
const foldl1 = <a>(reducer : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : a =>
{
	if (xs.CONS === 'Nil')
		ERROR.ONLY_CONS ('foldl1')
	let x = xs .head
	for (let i = 0; (xs = xs .tail).CONS === 'Cons'; ++i, x = reducer (x) (xs .head))
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('foldl1')
	return x
}

/**` foldr :: (a -> b -> b) -> b -> List a -> b `*/
const foldr = <a, b>(reducer : (leftside : a) => (rightside : b) => b) => (initial : b) => (xs : List <a>) : b =>
{
	xs = reverse (xs)
	let x = initial
	for (let i = 0; xs.CONS === 'Cons'; ++i, x = reducer (xs .head) (x), xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('foldr')
	return x
}

/**` foldr1 :: (a -> a -> a) -> List a -> a `*/
const foldr1 = <a>(reducer : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : a =>
{
	if (xs.CONS === 'Nil')
		ERROR.ONLY_CONS ('foldr1')
	xs = reverse (xs)
	let x = xs .head
	for (let i = 0; (xs = xs .tail).CONS === 'Cons'; ++i, x = reducer (xs .head) (x))
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('foldr1')
	return x
}

/**` init :: List a -> List a `*/
const init = <a>(xs : List <a>) : List <a> =>
	xs.$init ??
		xs.CONS === 'Nil'
			? ERROR.ONLY_CONS ('init')
			: xs .tail.CONS === 'Nil'
				? Nil
				: Cons (() => xs .head) (() => init (xs .tail))

/**` inits :: List a -> List (List a) `*/
const inits = <a>(xs : List <a>) : List <List <a>> =>
	xs.CONS === 'Nil'
		? singleton (Nil as List <a>)
		: lprepend (Nil as List <a>) (() => inits (xs .tail) .fmap (llprepend (() => xs .head)))

/**` intersperese :: a -> List a -> List a `*/
const intersperese = <a>(delimiter : a) => (xs : List <a>) : List <a> =>
	xs.CONS === 'Nil'
		? Nil
		: xs .tail.CONS === 'Nil'
			? xs
			: llprepend (() => xs .head) (lprepend (delimiter) (() => intersperese (delimiter) (xs .tail)))

/**` last :: List a -> a `*/
const last = <a>(xs : List <a>) : a =>
{
	if (xs.$last !== undefined)
		return xs.$last
	if (xs.CONS === 'Nil')
		ERROR.ONLY_CONS ('last')
	if (xs .tail.CONS === 'Nil')
		return xs .head
	for (let i = 0; (xs = xs .tail) .tail.CONS === 'Cons'; ++i)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('last')
	return xs .head
}

/**` len :: List a -> Number `*/
const len = <a>(xs : List <a>) : number =>
{
	if (xs.$len !== undefined)
		return xs.$len
	let i = 0
	while (xs.CONS === 'Cons')
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('len')
		else
			++i, xs = xs .tail
	return i
}

/**` map :: (a -> b) -> List a -> List b `*/
const map = <a, b>(morphism : (element : a) => b) => (xs : List <a>) : List <b> =>
	xs .fmap (morphism)

/**` nelem :: (Eq a) => a -> List a -> Boolean `*/
const nelem = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : boolean =>
	!elem (value) (xs)

/**` partition :: (a -> Boolean) -> List a -> (List a, List a) `*/
const partition = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <List <a>, List <a>> =>
{
	let ys : List <a> = Nil
	let zs : List <a> = Nil
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('partition')
		else if (predicate (xs .head))
			ys = prepend (xs .head) (ys)
		else
			zs = prepend (xs .head) (zs)
	return Pair (reverse (ys), reverse (zs))
}

/**` reverse :: List a -> List a `*/
const reverse = <a>(xs : List <a>) : List <a> =>
{
	if (xs.$reverse !== undefined)
		return xs.$reverse
	let ys : List <a> = Nil
	for (let i = 0; xs.CONS === 'Cons'; ++i, ys = prepend (xs .head) (ys), xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('reverse')
	return ys
}

/**` scanl :: (b -> a -> b) -> b -> List a -> List b `*/
const scanl = <a, b>(reducer : (leftside : b) => (rightside : a) => b) => (initial : b) => (xs : List <a>) : List <b> =>
	xs.CONS === 'Nil'
		? singleton (initial)
		: lprepend (initial) (() => scanl (reducer) (reducer (initial) (xs .head)) (xs .tail))

/**` scanl1 :: (a -> a -> b) -> List a -> List a `*/
const scanl1 = <a>(reducer : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : List <a> =>
	xs.CONS === 'Nil'
		? Nil
		: scanl (reducer) (xs .head) (xs .tail)

/**` scanr :: (a -> b -> b) -> b -> List a -> List b `*/
const scanr = <a, b>(reducer : (leftside : a) => (rightside : b) => b) => (initial : b) => (xs : List <a>) : List <b> =>
{
	xs = reverse (xs)
	let ys = singleton (initial)
	for (let i = 0; xs.CONS === 'Cons'; ++i, ys = prepend (reducer (xs .head) (ys .head)) (ys), xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('scanr')
	return ys
}

/**` scanr1 :: (a -> a -> a) -> List a -> List a `*/
const scanr1 = <a>(reducer : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : List <a> =>
{
	xs = reverse (xs)
	let ys = singleton (xs .head)
	for (let i = 0; (xs = xs .tail).CONS === 'Cons'; ++i, ys = prepend (reducer (xs .head) (ys .head)) (ys))
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('scanr1')
	return ys
}

/**` span :: (a -> Boolean) -> List a -> (List a, List a) `*/
const span = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <List <a>, List <a>> =>
{
	let ys : List <a> = Nil
	for (let i = 0; xs.CONS === 'Cons' && predicate (xs .head); ++i, ys = prepend (xs .head) (ys), xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('span')
	return Pair (reverse (ys), xs)
}

/**` splitAt :: Number -> List a -> (List a, List a) `*/
const splitAt = (amount : number) => <a>(xs : List <a>) : Pair <List <a>, List <a>> =>
{
	let ys : List <a> = Nil
	for (let i = 0; i < amount && xs.CONS === 'Cons'; ++i, ys = prepend (xs .head) (ys), xs = xs .tail)
		if (i === MAX_LIST_OPS)
			ERROR.MAX_LIST_OPS ('splitAt')
	return Pair (reverse (ys), xs)
}

/**` tails :: List a -> List (List a) `*/
const tails = <a>(xs : List <a>) : List <List <a>> =>
	xs.CONS === 'Nil'
		? singleton (Nil as List <a>)
		: lprepend (xs as List <a>) (() => tails (xs .tail))

/**` take :: Number -> List a -> List a `*/
const take = (amount : number) => <a>(xs : List <a>) : List <a> =>
	Number.isInteger (amount)
		? amount > 0
			? Cons (() => xs .head) (() => take (amount - 1) (xs .tail))
			: Nil
		: ERROR.ONLY_INTEGER ('take', amount)

/**` takeWhile :: (a -> Boolean) -> List a -> List a `*/
const takeWhile = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
	xs.CONS === 'Cons' && predicate (xs .head)
		? lprepend (xs .head) (() => takeWhile (predicate) (xs .tail))
		: Nil

/**` unzip :: List (Pair a b) -> Pair (List a) (List b) `*/
const unzip = <a, b>(xs : List <Pair <a, b>>) : Pair <List <a>, List <b>> =>
	Pair (xs .fmap (fst), xs .fmap (snd))

/**` unzipWith :: (c -> Pair a b) -> List c -> Pair (List a) (List b) `*/
const unzipWith = <a, b, c>(f : (element : c) => Pair <a, b>) => (xs : List <c>) : Pair <List <a>, List <b>> =>
	unzip (xs .fmap (f))

/**` zip :: List a -> List b -> List (Pair a b) `*/
const zip = <a>(xs : List <a>) => <b>(ys : List <b>) : List <Pair <a, b>> =>
	xs.CONS === 'Nil' || ys.CONS === 'Nil'
		? Nil
		: Cons (() => Pair (xs .head, ys .head)) (() => zip (xs .tail) (ys .tail))

/**` zipWith :: (a -> b -> c) -> List a -> List b -> List c `*/
const zipWith = <a, b, c>(zipper : (first : a) => (second : b) => c) => (xs : List <a>) => (ys : List <b>) : List <c> =>
	xs.CONS === 'Nil' || ys.CONS === 'Nil'
		? Nil
		: Cons (() => zipper (xs .head) (ys .head)) (() => zipWith (zipper) (xs .tail) (ys .tail))

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for 'Either' //

/**` isLeft :: Either a b -> Boolean `*/
const isLeft = <a, b>(either : Either <a, b>) : either is Left <a, b> => either.CONS === 'Left'

/**` isRight :: Either a b -> Boolean `*/
const isRight = <a, b>(either : Either <a, b>) : either is Right <a, b> => either.CONS === 'Right'

/**` eitherway :: (a -> c) -> (b -> c) -> Either a b -> c `*/
const eitherway = <a, c>(lf : (left : a) => c) => <b>(rf : (right : b) => c) => (either : Either <a, b>) : c =>
	either.CONS === 'Left'
		? lf (either.INFO)
		: rf (either.INFO)

/**` onLeft :: (a -> c) -> Either a b -> Either c b `*/
const onLeft = <a, c>(lf : (left : a) => c) => <b>(either : Either <a, b>) : Either <c, b> =>
	either.CONS === 'Left'
		? Left (lf (either.INFO))
		: either as Right <c, b>

/**` onRight :: (b -> c) -> Either a b -> Either a c `*/
const onRight = <b, c>(rf : (right : b) => c) => <a>(either : Either <a, b>) : Either <a, c> =>
	either.CONS === 'Right'
		? Right (rf (either.INFO))
		: either as Left <a, c>

/**` haveLeft :: a -> Either a b -> a `*/
const haveLeft = <a>(fallback : a) => <b>(either : Either <a, b>) : a =>
	either.CONS === 'Left'
		? either.INFO
		: fallback

/**` haveRight :: b -> Either a b -> b `*/
const haveRight = <b>(fallback : b) => <a>(either : Either <a, b>) : b =>
	either.CONS === 'Right'
		? either.INFO
		: fallback

/**` fromLeft :: Either a b -> a `*/
const fromLeft = <a, b>(either : Either <a, b>) : a =>
	either.CONS === 'Left'
		? either.INFO
		: THROW (`'fromLeft' was used on a right-value`)

/**` fromRight :: b -> Either a b -> b `*/
const fromRight = <a, b>(either : Either <a, b>) : b =>
	either.CONS === 'Right'
		? either.INFO
		: THROW (`'fromRight' was used on a left-value`)

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for Vectors and Matrices //

const V2 =
	{
		/**` V2.origin :: Vector2 `*/
		origin : Vector2 (0, 0),

		/**` V2.translate :: Number -> Number -> Vector2 -> Vector2 `*/
		translate : (dx : number) => (dy : number) => (v : Vector2) : Vector2 =>
			Vector2 (v.x + dx, v.y + dy),

		/**` V2.untranslate :: Number -> Number -> Vector2 -> Vector2 `*/
		untranslate : (dx : number) => (dy : number) => (v : Vector2) : Vector2 =>
			Vector2 (v.x - dx, v.y - dy),

		/**` V2.add :: Vector2 -> Vector2 -> Vector2 `*/
		add : (v : Vector2) => (w : Vector2) : Vector2 =>
			Vector2 (v.x + w.x, v.y + w.y),

		/**` V2.sub :: Vector2 -> Vector2 -> Vector2 `*/
		sub : (v : Vector2) => (w : Vector2) : Vector2 =>
			Vector2 (v.x - w.x, v.y - w.y),

		/**` V2.scale :: Number -> Vector2 -> Vector2 `*/
		scale : (k : number) => (v : Vector2) : Vector2 =>
			Vector2 (v.x * k, v.y * k),

		/**` V2.unscale :: Number -> Vector2 -> Vector2 `*/
		unscale : (k : number) => (v : Vector2) : Vector2 =>
			Vector2 (v.x / k, v.y / k),

		/**` V2.norm :: Vector2 -> Number `*/
		norm : (v : Vector2) : number =>
			Math.sqrt (v.x ** 2 + v.y ** 2),

		/**` V2.normalize :: Vector2 -> Vector2 `*/
		normalize : (v : Vector2) : Vector2 =>
			v .eq (V2.origin)
				? V2.origin
				: V2.unscale (Math.sqrt (v.x ** 2 + v.y ** 2)) (v),

		/**` V2.dot :: Vector2 -> Vector2 -> Number `*/
		dot : (v : Vector2) => (w : Vector2) : number =>
			v.x * w.x + v.y * w.y,

		/**` V2.transform :: Matrix2 -> Vector2 -> Vector2 `*/
		transform : (m : Matrix2) => (v : Vector2) : Vector2 =>
			Vector2 (
				m.ix * v.x + m.jx * v.y,
				m.iy * v.x + m.jy * v.y
			)
	}

const V3 =
	{
		/**` V3.origin :: Vector3 `*/
		origin : Vector3 (0, 0, 0),

		/**` V3.translate :: Number -> Number -> Number -> Vector3 -> Vector3 `*/
		translate : (dx : number) => (dy : number) => (dz : number) => (v : Vector3) : Vector3 =>
			Vector3 (v.x + dx, v.y + dy, v.z + dz),

		/**` V3.untranslate :: Number -> Number -> Number -> Vector3 -> Vector3 `*/
		untranslate : (dx : number) => (dy : number) => (dz : number) => (v : Vector3) : Vector3 =>
			Vector3 (v.x - dx, v.y - dy, v.z - dz),

		/**` V3.add :: Vector3 -> Vector3 -> Vector3 `*/
		add : (v : Vector3) => (w : Vector3) : Vector3 =>
			Vector3 (v.x + w.x, v.y + w.y, v.z + w.z),

		/**` V3.sub :: Vector3 -> Vector3 -> Vector3 `*/
		sub : (v : Vector3) => (w : Vector3) : Vector3 =>
			Vector3 (v.x - w.x, v.y - w.y, v.z - w.z),

		/**` V3.scale :: Number -> Vector3 -> Vector3 `*/
		scale : (k : number) => (v : Vector3) : Vector3 =>
			Vector3 (v.x * k, v.y * k, v.z * k),

		/**` V3.unscale :: Number -> Vector3 -> Vector3 `*/
		unscale : (k : number) => (v : Vector3) : Vector3 =>
			Vector3 (v.x / k, v.y / k, v.z / k),

		/**` V3.norm :: Vector3 -> Number `*/
		norm : (v : Vector3) : number =>
			Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2),

		/**` V3.normalize :: Vector3 -> Vector3 `*/
		normalize : (v : Vector3) : Vector3 =>
			v .eq (V3.origin)
				? V3.origin
				: V3.unscale (Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2)) (v),

		/**` V3.dot :: Vector3 -> Vector3 -> Number `*/
		dot : (v : Vector3) => (w : Vector3) : number =>
			v.x * w.x + v.y * w.y + v.z * w.z,

		/**` V3.cross :: Vector3 -> Vector3 -> Vector3 `*/
		cross : (v : Vector3) => (w : Vector3) : Vector3 =>
			Vector3 (v.y * w.z - v.z * w.y, v.z * w.x - v.x * w.z, v.x * w.y - v.y * w.x),

		/**` V3.transform :: Matrix3 -> Vector3 -> Vector3 `*/
		transform : (m : Matrix3) => (v : Vector3) : Vector3 =>
			Vector3 (
				m.ix * v.x + m.jx * v.y + m.kx * v.z,
				m.iy * v.x + m.jy * v.y + m.ky * v.z,
				m.iz * v.x + m.jz * v.y + m.kz * v.z
			)
	}

const V4 =
	{
		/**` V4.origin :: Vector4 `*/
		origin : Vector4 (0, 0, 0, 0),

		/**` V4.translate :: Number -> Number -> Number -> Vector4 -> Vector4 `*/
		translate :
			(dx : number) => (dy : number) =>
			(dz : number) => (dw : number) => (v : Vector4) : Vector4 =>
			Vector4 (v.x + dx, v.y + dy, v.z + dz, v.w + dw),

		/**` V4.untranslate :: Number -> Number -> Number -> Vector4 -> Vector4 `*/
		untranslate :
			(dx : number) => (dy : number) =>
			(dz : number) => (dw : number) => (v : Vector4) : Vector4 =>
			Vector4 (v.x - dx, v.y - dy, v.z - dz, v.w - dw),

		/**` V4.add :: Vector4 -> Vector4 -> Vector4 `*/
		add : (v : Vector4) => (w : Vector4) : Vector4 =>
			Vector4 (v.x + w.x, v.y + w.y, v.z + w.z, v.w + w.w),

		/**` V4.sub :: Vector4 -> Vector4 -> Vector4 `*/
		sub : (v : Vector4) => (w : Vector4) : Vector4 =>
			Vector4 (v.x - w.x, v.y - w.y, v.z - w.z, v.w - w.w),

		/**` V4.scale :: Number -> Vector4 -> Vector4 `*/
		scale : (k : number) => (v : Vector4) : Vector4 =>
			Vector4 (v.x * k, v.y * k, v.z * k, v.w * k),

		/**` V4.unscale :: Number -> Vector4 -> Vector4 `*/
		unscale : (k : number) => (v : Vector4) : Vector4 =>
			Vector4 (v.x / k, v.y / k, v.z / k, v.w / k),

		/**` V4.norm :: Vector4 -> Number `*/
		norm : (v : Vector4) : number =>
			Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2 + v.w ** 2),

		/**` V4.normalize :: Vector4 -> Vector4 `*/
		normalize : (v : Vector4) : Vector4 =>
			v .eq (V4.origin)
				? V4.origin
				: V4.unscale (Math.sqrt (v.x ** 2 + v.y ** 2 + v.z ** 2 + v.w ** 2)) (v),

		/**` V4.dot :: Vector4 -> Vector4 -> Number `*/
		dot : (v : Vector4) => (w : Vector4) : number =>
			v.x * w.x + v.y * w.y + v.z * w.z + v.w * w.w,

		/**` V4.transform :: Matrix4 -> Vector4 -> Vector4 `*/
		transform : (m : Matrix4) => (v : Vector4) : Vector4 =>
			Vector4 (
				m.ix * v.x + m.jx * v.y + m.kx * v.z + m.lx * v.w,
				m.iy * v.x + m.jy * v.y + m.ky * v.z + m.ly * v.w,
				m.iz * v.x + m.jz * v.y + m.kz * v.z + m.lz * v.w,
				m.iw * v.x + m.jw * v.y + m.kw * v.z + m.lw * v.w
			)
	}

const M2 =
	{
		/**` M2.id :: Matrix2 `*/
		id : Matrix2 (1, 0, 0, 1),

		/**` M2.fromBasis :: (Vector2, Vector2) -> Matrix2 `*/
		fromBasis : (i : Vector2, j : Vector2) : Matrix2 =>
			Matrix2 (i.x, j.x, i.y, j.y),

		/**` M2.mul :: Matrix2 -> Matrix2 -> Matrix2 `*/
		mul : (m : Matrix2) => (n : Matrix2) : Matrix2 =>
			Matrix2 (
				m.ix * n.ix + m.jx * n.iy,
				m.ix * n.jx + m.jx * n.jy,
				m.iy * n.ix + m.jy * n.iy,
				m.iy * n.jx + m.jy * n.jy
			)
	}

const M3 =
	{
		/**` M3.id :: Matrix3 `*/
		id : Matrix3 (1, 0, 0, 0, 1, 0, 0, 0, 1),

		/**` M3.fromBasis :: (Vector3, Vector3, Vector3) -> Matrix3 `*/
		fromBasis : (i : Vector3, j : Vector3, k : Vector3) : Matrix3 =>
			Matrix3 (i.x, j.x, k.x, i.y, j.y, k.y, i.z, j.z, k.z),

		/**` M3.mul :: Matrix3 -> Matrix3 -> Matrix3 `*/
		mul : (m : Matrix3) => (n : Matrix3) : Matrix3 =>
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

const M4 =
	{
		/**` M4.id :: Matrix4 `*/
		id : Matrix4 (1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1),

		/**` M4.fromBasis :: (Vector4, Vector4, Vector4, Vector4) -> Matrix4 `*/
		fromBasis : (i : Vector4, j : Vector4, k : Vector4, l : Vector4) : Matrix4 =>
			Matrix4 (i.x, j.x, k.x, l.x, i.y, j.y, k.y, l.y, i.z, j.z, k.z, l.z, i.w, j.w, k.w, l.w),

		/**` M4.mul :: Matrix4 -> Matrix4 -> Matrix4 `*/
		mul : (m : Matrix4) => (n : Matrix4) : Matrix4 =>
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
// Implementation of Micro-Functions|Constants for Multiple Algebraic Data Types //

/**` sequenceIOs :: List (IO a) -> IO (List a) `*/
const sequenceIOs = <a>(ios : List <IO <a>>) : IO <List <a>> =>
	IO (() => ios .fmap (io => io.INFO ()))

/**` executeIOs `*/
const executeIOs = <a>(ios : List <IO <a>>) : IO <null> =>
	IO (() => {
		for (let i = ios; i.CONS === 'Cons'; i = i .tail)
			i .head.INFO ()
		return null
	})

/**` maybeHead :: List a -> Maybe a `*/
const maybeHead = <a>(xs : List <a>) : Maybe <a> =>
	xs.CONS === 'Nil'
		? Nothing
		: Just (xs .head)

/**` maybeLast :: List a -> Maybe a `*/
const maybeLast = <a>(xs : List <a>) : Maybe <a> =>
	xs.CONS === 'Nil'
		? Nothing
		: Just (last (xs))

/**` maybeTail :: List a -> Maybe (List a) `*/
const maybeTail = <a>(xs : List <a>) : Maybe <List <a>> =>
	xs.CONS === 'Nil'
		? Nothing
		: Just (xs .tail)

/**` maybeInit :: List a -> Maybe (List a) `*/
const maybeInit = <a>(xs : List <a>) : Maybe <List <a>> =>
	xs.CONS === 'Nil'
		? Nothing
		: Just (init (xs))

/**` fromMaybes :: List (Maybe a) -> List a `*/
const fromMaybes = <a>(maybes : List <Maybe <a>>) : List <a> =>
	maybes.CONS === 'Nil'
		? Nil
		: maybes .head.CONS === 'Nothing'
			? fromMaybes (maybes .tail)
			: lprepend (maybes .head.INFO) (() => fromMaybes (maybes .tail))

/**` mapMaybe :: (a -> Maybe b) -> List a -> List b `*/
const mapMaybe = <a, b>(reaction : (element : a) => Maybe <b>) => (xs : List <a>) : List <b> =>
	fromMaybes (xs .fmap (reaction))

/**` maybeSingleton :: Maybe a -> List a `*/
const maybeSingleton = <a>(maybe : Maybe <a>) : List <a> =>
	maybe.CONS === 'Nothing'
		? Nil
		: singleton (maybe.INFO)

/**` maybeAt :: Number -> List a -> Maybe a `*/
const maybeAt = (index : number) => <a>(xs : List <a>) : Maybe <a> =>
{
	if (index < 0 || !Number.isInteger)
		return Nothing
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail)
		if (i === index)
			return Just (xs .head)
	return Nothing
}

/**` find :: (a -> Boolean) -> List a -> Maybe a `*/
const find = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Maybe <a> =>
{
	while (xs.CONS === 'Cons')
		if (predicate (xs .head)) return Just (xs .head)
		else xs = xs .tail
	return Nothing
}

/**` elemIndex :: (Eq a) => a -> List a -> Maybe Number `*/
const elemIndex = <a>(value : Eq <a>) => (xs : List <Eq <a>>) : Maybe <number> =>
{
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail)
		if (xs .head .eq (value))
			return Just (i)
	return Nothing
}

/**` findIndex :: (a -> Boolean) -> List a -> Maybe Number `*/
const findIndex = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Maybe <number> =>
{
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs .tail)
		if (predicate (xs .head))
			return Just (i)
	return Nothing
}

/**` lefts :: List (Either a b) -> List a `*/
const lefts = <a, b>(eithers : List <Either <a, b>>) : List <a> =>
	eithers.CONS === 'Nil'
		? Nil
		: eithers .head.CONS === 'Left'
			? lprepend (eithers .head.INFO) (() => lefts (eithers .tail))
			: lefts (eithers .tail)

/**` rights :: List (Either a b) -> List b `*/
const rights = <a, b>(eithers : List <Either <a, b>>) : List <b> =>
	eithers.CONS === 'Nil'
		? Nil
		: eithers .head.CONS === 'Right'
			? lprepend (eithers .head.INFO) (() => rights (eithers .tail))
			: rights (eithers .tail)

/********************************************************************************************************************************/
// Implementation of Micro-Functions|Constants for Enumerators //

/**` relaxX :: X -> X `*/
const relaxX = (direction : X) : X =>
	direction === X.LL ? X.L :
	direction === X.RR ? X.R :
	direction

/**` relaxY :: Y -> Y `*/
const relaxY = (direction : Y) : Y =>
	direction === Y.DD ? Y.D :
	direction === Y.UU ? Y.U :
	direction

/**` relaxZ :: Z -> Z `*/
const relaxZ = (direction : Z) : Z =>
	direction === Z.BB ? Z.B :
	direction === Z.FF ? Z.F :
	direction

/**` isL :: X -> Boolean `*/
const isL = (direction : X) : boolean => direction === X.L || direction === X.LL

/**` isR :: X -> Boolean `*/
const isR = (direction : X) : boolean => direction === X.R || direction === X.RR

/**` isD :: Y -> Boolean `*/
const isD = (direction : Y) : boolean => direction === Y.D || direction === Y.DD

/**` isU :: Y -> Boolean `*/
const isU = (direction : Y) : boolean => direction === Y.U || direction === Y.UU

/**` isB :: Z -> Boolean `*/
const isB = (direction : Z) : boolean => direction === Z.B || direction === Z.BB

/**` isF :: Z -> Boolean `*/
const isF = (direction : Z) : boolean => direction === Z.F || direction === Z.FF

/**` mappingLineCap :: Mapping LineCap CanvasLineCap `*/
const mappingLineCap : Mapping <LineCap, CanvasLineCap> =
	Mapping (
		[LineCap.Butt   , 'butt'  ],
		[LineCap.Round  , 'round' ],
		[LineCap.Square , 'square']
	)

/**` mappingLineJoin :: Mapping LineJoin CanvasLineJoin `*/
const mappingLineJoin : Mapping <LineJoin, CanvasLineJoin> =
	Mapping (
		[LineJoin.Round , 'round'],
		[LineJoin.Bevel , 'bevel'],
		[LineJoin.Miter , 'miter']
	)

/**` mappingTextAlign :: Mapping TextAlign CanvasTextAlign `*/
const mappingTextAlign : Mapping <TextAlign, CanvasTextAlign> =
	Mapping (
		[TextAlign.Center    , 'center'],
		[TextAlign.End       , 'end'   ],
		[TextAlign.Leftside  , 'left'  ],
		[TextAlign.Rightside , 'right' ],
		[TextAlign.Start     , 'start' ]
	)

/**` mappingTextBaseline :: Mapping TextBaseline CanvasTextBaseline `*/
const mappingTextBaseline : Mapping <TextBaseline, CanvasTextBaseline> =
	Mapping (
		[TextBaseline.Alphabetic  , 'alphabetic' ],
		[TextBaseline.Bottom      , 'bottom'     ],
		[TextBaseline.Hanging     , 'hanging'    ],
		[TextBaseline.Ideographic , 'ideographic'],
		[TextBaseline.Middle      , 'middle'     ],
		[TextBaseline.Top         , 'top'        ]
	)

/**` mappingComposition :: Mapping Composition String `*/
const mappingComposition : Mapping <Composition, string> =
	Mapping (
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
// Implementation of Monadic 'unit' and 'Do' //

const unit =
	{
		/**` unit.IO :: a -> IO a `*/
		IO : <a>(outcome : a) : IO <a> => IO (() => outcome),

		/**` unit.Maybe :: a -> Maybe a `*/
		Maybe : Just,

		/**` unit.Process :: a -> Process s a `*/
		Process : <a>(output : a) : Process <unknown, a> => Process (s => Pair (s, output)),

		/**` unit.List :: a -> List a `*/
		List : singleton
	}

const Do =
	{
		IO      : unit.IO      <{}> (Object.create (null)),
		Maybe   : unit.Maybe   <{}> (Object.create (null)),
		Process : unit.Process <{}> (Object.create (null)),
		List    : unit.List    <{}> (Object.create (null))
	}

/********************************************************************************************************************************/
// Implementation of IO Operations //

/**` KEYBOARD :: [String] `*/
const KEYBOARD =
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

type KeyboardKey = typeof KEYBOARD[number]

const λ =
	{
		ctxs            : [] as Array <CanvasRenderingContext2D>,
		ctx             : undefined as unknown as CanvasRenderingContext2D,
		resizeID        : undefined as unknown as number,
		isResized       : false,
		isPointerLocked : false,
		seed            : (Math.random() - 0.5) * Date.now(),
		debugCounter    : 0,
		image           : Object.create (null) as { [key : string] : HTMLImageElement },
		audio           : Object.create (null) as { [key : string] : HTMLAudioElement },
		mouseScreenX    : 0, mouseScreenY : 0,
		mouseWindowX    : 0, mouseWindowY : 0,
		mouseCanvasX    : 0, mouseCanvasY : 0,
		mouseDeltaX     : 0, mouseDeltaY  : 0,
		mouseScroll     : Y.Rest,
		mouseButtons    : Array (5) .fill (Y.U) as [Y, Y, Y, Y, Y],
		keyboard        : KEYBOARD .reduce (($, k) => ({ ...$, [k] : Y.U }), Object.create (null)) as { [x in KeyboardKey] : Y }
	}

const Input =
	{
		Norm :
			{
				/**` Input.Norm.mouseCanvasX :: IO Number `*/
				mouseCanvasX : IO (() => λ.mouseCanvasX / λ.ctx.canvas.width),

				/**` Input.Norm.mouseCanvasY :: IO Number `*/
				mouseCanvasY : IO (() => λ.mouseCanvasY / λ.ctx.canvas.height),

				/**` Input.Norm.mouseCanvasP :: IO (Pair Number Number) `*/
				mouseCanvasP : IO (() => Pair (λ.mouseCanvasX / λ.ctx.canvas.width, λ.mouseCanvasY / λ.ctx.canvas.height)),

				/**` Input.Norm.mouseCanvasV :: IO Vector2 `*/
				mouseCanvasV : IO (() => Vector2 (λ.mouseCanvasX / λ.ctx.canvas.width, λ.mouseCanvasY / λ.ctx.canvas.height)),

				/**` Input.Norm.mouseDeltaX :: IO Number `*/
				mouseDeltaX : IO (() => λ.mouseDeltaX / λ.ctx.canvas.width),

				/**` Input.Norm.mouseDeltaY :: IO Number `*/
				mouseDeltaY : IO (() => λ.mouseDeltaY / λ.ctx.canvas.height),

				/**` Input.Norm.mouseDeltaP :: IO (Pair Number Number) `*/
				mouseDeltaP : IO (() => Pair (λ.mouseDeltaX / λ.ctx.canvas.width, λ.mouseDeltaY / λ.ctx.canvas.height)),

				/**` Input.Norm.mouseDeltaV :: IO Vector2 `*/
				mouseDeltaV : IO (() => Vector2 (λ.mouseDeltaX / λ.ctx.canvas.width, λ.mouseDeltaY / λ.ctx.canvas.height)),

				/**` Input.Norm.lineThickness :: IO Number `*/
				lineThickness : IO (() => λ.ctx.lineWidth / λ.ctx.canvas.width),

				/**` Input.Norm.lineDashPattern :: IO (List Number) `*/
				lineDashPattern : IO (() => List (...λ.ctx.getLineDash() .map (x => x / λ.ctx.canvas.width))),

				/**` Input.Norm.lineDashOffset :: IO Number `*/
				lineDashOffset : IO (() => λ.ctx.lineDashOffset / λ.ctx.canvas.width),

				/**` Input.Norm.fontSize :: IO Number `*/
				fontSize : IO (() => parseFloat (λ.ctx.font) / λ.ctx.canvas.width),

				/**` Input.Norm.shadowOffsetX :: IO Number `*/
				shadowOffsetX : IO (() => λ.ctx.shadowOffsetX / λ.ctx.canvas.width),

				/**` Input.Norm.shadowOffsetY :: IO Number `*/
				shadowOffsetY : IO (() => λ.ctx.shadowOffsetY / λ.ctx.canvas.height),

				/**` Input.Norm.shadowOffsetP :: IO (Pair Number Number) `*/
				shadowOffsetP :
					IO (() => Pair (λ.ctx.shadowOffsetX / λ.ctx.canvas.width, λ.ctx.shadowOffsetY / λ.ctx.canvas.height)),

				/**` Input.Norm.shadowOffsetV :: IO Vector2 `*/
				shadowOffsetV :
					IO (() => Vector2 (λ.ctx.shadowOffsetX / λ.ctx.canvas.width, λ.ctx.shadowOffsetY / λ.ctx.canvas.height)),

				/**` Input.Norm.transformationMatrix :: IO Matrix3 `*/
				transformationMatrix :
					IO (() => {
						const m = λ.ctx.getTransform()
						return Matrix3 (m.a, m.c, m.e / λ.ctx.canvas.width, m.b, m.d, m.f / λ.ctx.canvas.height, 0, 0, 1)
					})
				},

		/**` Input.layer :: IO Number `*/
		layer : IO (() => λ.ctxs .findIndex (ctx => ctx === λ.ctx)),

		/**` Input.isWindowResized :: IO Boolean `*/
		isWindowResized : IO (() => λ.isResized),

		/**` Input.isPointerLocked :: IO Boolean `*/
		isPointerLocked : IO (() => λ.isPointerLocked),

		/**` Input.seed :: IO Number `*/
		seed : unit.IO (λ.seed),

		/**` Input.screenW :: IO Number `*/
		screenW : IO (() => screen.width),

		/**` Input.screenH :: IO Number `*/
		screenH : IO (() => screen.height),

		/**` Input.screenP :: IO (Pair Number Number) `*/
		screenP : IO (() => Pair (screen.width, screen.height)),

		/**` Input.screenV :: IO Vector2 `*/
		screenV : IO (() => Vector2 (screen.width, screen.height)),

		/**` Input.windowW :: IO Number `*/
		windowW : IO (() => innerWidth),

		/**` Input.windowH :: IO Number `*/
		windowH : IO (() => innerHeight),

		/**` Input.windowP :: IO (Pair Number Number) `*/
		windowP : IO (() => Pair (innerWidth, innerHeight)),

		/**` Input.windowV :: IO Vector2 `*/
		windowV : IO (() => Vector2 (innerWidth, innerHeight)),

		/**` Input.canvasW :: IO Number `*/
		canvasW : IO (() => λ.ctx.canvas.width),

		/**` Input.canvasH :: IO Number `*/
		canvasH : IO (() => λ.ctx.canvas.height),

		/**` Input.canvasP :: IO (Pair Number Number) `*/
		canvasP : IO (() => Pair (λ.ctx.canvas.width, λ.ctx.canvas.height)),

		/**` Input.canvasV :: IO Vector2 `*/
		canvasV : IO (() => Vector2 (λ.ctx.canvas.width, λ.ctx.canvas.height)),

		/**` Input.mouseScreenX :: IO Number `*/
		mouseScreenX : IO (() => λ.mouseScreenX),

		/**` Input.mouseScreenY :: IO Number `*/
		mouseScreenY : IO (() => λ.mouseScreenY),

		/**` Input.mouseScreenP :: IO (Pair Number Number) `*/
		mouseScreenP : IO (() => Pair (λ.mouseScreenX, λ.mouseScreenY)),

		/**` Input.mouseScreenV :: IO Vector2 `*/
		mouseScreenV : IO (() => Vector2 (λ.mouseScreenX, λ.mouseScreenY)),

		/**` Input.mouseWindowX :: IO Number `*/
		mouseWindowX : IO (() => λ.mouseWindowX),

		/**` Input.mouseWindowY :: IO Number `*/
		mouseWindowY : IO (() => λ.mouseWindowY),

		/**` Input.mouseWindowP :: IO (Pair Number Number) `*/
		mouseWindowP : IO (() => Pair (λ.mouseWindowX, λ.mouseWindowY)),

		/**` Input.mouseWindowV :: IO Vector2 `*/
		mouseWindowV : IO (() => Vector2 (λ.mouseWindowX, λ.mouseWindowY)),

		/**` Input.mouseCanvasX :: IO Number `*/
		mouseCanvasX : IO (() => λ.mouseCanvasX),

		/**` Input.mouseCanvasY :: IO Number `*/
		mouseCanvasY : IO (() => λ.mouseCanvasY),

		/**` Input.mouseCanvasP :: IO (Pair Number Number) `*/
		mouseCanvasP : IO (() => Pair (λ.mouseCanvasX, λ.mouseCanvasY)),

		/**` Input.mouseCanvasV :: IO Vector2 `*/
		mouseCanvasV : IO (() => Vector2 (λ.mouseCanvasX, λ.mouseCanvasY)),

		/**` Input.mouseDeltaX :: IO Number `*/
		mouseDeltaX : IO (() => λ.mouseDeltaX),

		/**` Input.mouseDeltaY :: IO Number `*/
		mouseDeltaY : IO (() => λ.mouseDeltaY),

		/**` Input.mouseDeltaP :: IO (Pair Number Number) `*/
		mouseDeltaP : IO (() => Pair (λ.mouseDeltaX, λ.mouseDeltaY)),

		/**` Input.mouseDeltaV :: IO Vector2 `*/
		mouseDeltaV : IO (() => Vector2 (λ.mouseDeltaX, λ.mouseDeltaY)),

		/**` Input.mouseScroll :: IO Y `*/
		mouseScroll : IO (() => λ.mouseScroll),

		/**` Input.mouseButtonLeft :: IO Y `*/
		mouseButtonLeft : IO (() => λ.mouseButtons[0]),

		/**` Input.mouseButtonMiddle :: IO Y `*/
		mouseButtonMiddle : IO (() => λ.mouseButtons[1]),

		/**` Input.mouseButtonRight :: IO Y `*/
		mouseButtonRight : IO (() => λ.mouseButtons[2]),

		/**` Input.mouseButtonEsotericX :: IO Y `*/
		mouseButtonEsotericX : IO (() => λ.mouseButtons[3]),

		/**` Input.mouseButtonEsotericY :: IO Y `*/
		mouseButtonEsotericY : IO (() => λ.mouseButtons[4]),

		/**` Input.keyboard :: KeyboardKey -> IO Y `*/
		keyboard : (key : KeyboardKey) : IO <Y> => IO (() => λ.keyboard[key]),

		/**` Input.time :: IO Number `*/
		time : IO (Date.now),

		/**` Input.textMeasurement :: String -> IO TextMeasurement `*/
		textMeasurement : (text : string) : IO <TextMeasurement> =>
			IO (() => {
				const metrics = λ.ctx.measureText(text)
				return TextMeasurement
					(text)
					(Math.abs (metrics.actualBoundingBoxLeft)   + Math.abs (metrics.actualBoundingBoxRight)  )
					(Math.abs (metrics.actualBoundingBoxAscent) + Math.abs (metrics.actualBoundingBoxDescent))
			}),

		/**` Input.lineThickness :: IO Number `*/
		lineThickness : IO (() => λ.ctx.lineWidth),

		/**` Input.lineCap :: IO LineCap `*/
		lineCap : IO (() => mappingLineCap .domain (λ.ctx.lineCap)),

		/**` Input.lineJoin :: IO LineJoin `*/
		lineJoin : IO (() => mappingLineJoin .domain (λ.ctx.lineJoin)),

		/**` Input.lineDashPattern :: IO (List Number) `*/
		lineDashPattern : IO (() => List (...λ.ctx.getLineDash())),

		/**` Input.lineDashOffset :: IO Number `*/
		lineDashOffset : IO (() => λ.ctx.lineDashOffset),

		/**` Input.miterLimit :: IO Number `*/
		miterLimit : IO (() => λ.ctx.miterLimit),

		/**` Input.font :: IO String `*/
		font : IO (() => λ.ctx.font),

		/**` Input.fontSize :: IO Number `*/
		fontSize : IO (() => parseFloat (λ.ctx.font)),

		/**` Input.fontFamily :: IO String `*/
		fontFamily : IO (() => λ.ctx.font .slice (λ.ctx.font .indexOf (" ") + 1)),

		/**` Input.textAlign :: IO TextAlign `*/
		textAlign : IO (() => mappingTextAlign .domain (λ.ctx.textAlign)),

		/**` Input.textBaseline :: IO TextBaseline `*/
		textBaseline : IO (() => mappingTextBaseline .domain (λ.ctx.textBaseline)),

		/**` Input.shadowBlurAmount :: IO Number `*/
		shadowBlurAmount : IO (() => λ.ctx.shadowBlur),

		/**` Input.shadowColor :: IO String `*/
		shadowColor : IO (() => λ.ctx.shadowColor),

		/**` Input.shadowOffsetX :: IO Number `*/
		shadowOffsetX : IO (() => λ.ctx.shadowOffsetX),

		/**` Input.shadowOffsetY :: IO Number `*/
		shadowOffsetY : IO (() => λ.ctx.shadowOffsetY),

		/**` Input.shadowOffsetP :: IO (Pair Number Number) `*/
		shadowOffsetP : IO (() => Pair (λ.ctx.shadowOffsetX, λ.ctx.shadowOffsetY)),

		/**` Input.shadowOffsetV :: IO Vector2 `*/
		shadowOffsetV : IO (() => Vector2 (λ.ctx.shadowOffsetX, λ.ctx.shadowOffsetY)),

		/**` Input.isInEvenOddPathP :: Number -> Number -> IO Boolean `*/
		isInEvenOddPathP : (x : number) => (y : number) : IO <boolean> => IO (() => λ.ctx.isPointInPath(x, y, 'evenodd')),

		/**` Input.isInEvenOddPathV :: Vector2 -> IO Boolean `*/
		isInEvenOddPathV : (v : Vector2) : IO <boolean> => IO (() => λ.ctx.isPointInPath(v.x, v.y, 'evenodd')),

		/**` Input.isInNonZeroPathP :: Number -> Number -> IO Boolean `*/
		isInNonZeroPathP : (x : number) => (y : number) : IO <boolean> => IO (() => λ.ctx.isPointInPath(x, y, 'nonzero')),

		/**` Input.isInNonZeroPathV :: Vector2 -> IO Boolean `*/
		isInNonZeroPathV : (v : Vector2) : IO <boolean> => IO (() => λ.ctx.isPointInPath(v.x, v.y, 'nonzero')),

		/**` Input.isInStrokeP :: Number -> Number -> IO Boolean `*/
		isInStrokeP : (x : number) => (y : number) : IO <boolean> => IO (() => λ.ctx.isPointInStroke(x, y)),

		/**` Input.isInStrokeV :: Vector2 -> IO Boolean `*/
		isInStrokeV : (v : Vector2) : IO <boolean> => IO (() => λ.ctx.isPointInStroke(v.x, v.y)),

		/**` Input.transformationMatrix :: IO Matrix3 `*/
		transformationMatrix :
			IO (() => {
				const m = λ.ctx.getTransform()
				return Matrix3 (m.a, m.c, m.e, m.b, m.d, m.f, 0, 0, 1)
			}),

		/**` Input.alpha :: IO Number `*/
		alpha : IO (() => λ.ctx.globalAlpha),

		/**` Input.composition :: IO Composition `*/
		composition : IO (() => mappingComposition .domain (λ.ctx.globalCompositeOperation))
	}

const Reput =
	{
		Norm :
			{
				/**` Reput.Norm.canvasW :: Number -> IO () `*/
				canvasW : (w : number) : IO <null> =>
					IO (() => (λ.ctx.canvas.width = w * innerWidth, null)),

				/**` Reput.Norm.canvasH :: Number -> IO () `*/
				canvasH : (h : number) : IO <null> =>
					IO (() => (λ.ctx.canvas.height = h * innerHeight, null)),

				/**` Reput.Norm.canvasWH :: Number -> Number -> IO () `*/
				canvasWH : (w : number) => (h : number) : IO <null> =>
					IO (() => (λ.ctx.canvas.width = w * innerWidth, λ.ctx.canvas.height = h * innerHeight, null)),

				/**` Reput.Norm.canvasP :: Pair Number Number -> IO () `*/
				canvasP : (p : Pair <number, number>) : IO <null> =>
					IO (() => (λ.ctx.canvas.width = p .fst * innerWidth, λ.ctx.canvas.height = p .snd * innerHeight, null)),

				/**` Reput.Norm.canvasV :: Vector2 -> IO () `*/
				canvasV : (v : Vector2) : IO <null> =>
					IO (() => (λ.ctx.canvas.width = v.x * innerWidth, λ.ctx.canvas.height = v.y * innerHeight, null)),

				/**` Reput.Norm.lineThickness :: Number -> IO () `*/
				lineThickness : (t : number) : IO <null> =>
					IO (() => (λ.ctx.lineWidth = t * λ.ctx.canvas.width, null)),

				/**` Reput.Norm.lineDashPattern :: List Number -> IO () `*/
				lineDashPattern : (pattern : List <number>) : IO <null> =>
					IO (() => (λ.ctx.setLineDash (array (pattern) .map (x => x * λ.ctx.canvas.width)), null)),

				/**` Reput.Norm.lineDashOffset :: Number -> IO () `*/
				lineDashOffset : (offset : number) : IO <null> =>
					IO (() => (λ.ctx.lineDashOffset = offset * λ.ctx.canvas.width, null)),

				/**` Reput.Norm.fontSize :: Number -> IO () `*/
				fontSize : (size : number) : IO <null> =>
					IO (() => (λ.ctx.font = `${size * λ.ctx.canvas.width}px${λ.ctx.font.slice(λ.ctx.font.indexOf(" "))}`, null)),

				/**` Reput.Norm.fillRGBA :: Number -> Number -> Number -> Number -> IO () `*/
				fillRGBA : (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
					IO (() => (λ.ctx.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null)),

				/**` Reput.Norm.fillRGBAV :: Vector4 -> IO () `*/
				fillRGBAV : (v : Vector4) : IO <null> =>
					IO (() => (λ.ctx.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null)),

				/**` Reput.Norm.strokeRGBA :: Number -> Number -> Number -> Number -> IO () `*/
				strokeRGBA : (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
					IO (() => (λ.ctx.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null)),

				/**` Reput.Norm.strokeRGBAV :: Vector4 -> IO () `*/
				strokeRGBAV : (v : Vector4) : IO <null> =>
					IO (() => (λ.ctx.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null)),

				/**` Reput.Norm.shadowRGBA :: Number -> Number -> Number -> Number -> IO () `*/
				shadowRGBA : (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
					IO (() => (λ.ctx.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null)),

				/**` Reput.Norm.shadowRGBAV :: Vector4 -> IO () `*/
				shadowRGBAV : (v : Vector4) : IO <null> =>
					IO (() => (λ.ctx.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null)),

				/**` Reput.Norm.shadowOffsetX :: Number -> IO () `*/
				shadowOffsetX : (x : number) : IO <null> =>
					IO (() => (λ.ctx.shadowOffsetX = x * λ.ctx.canvas.width, null)),

				/**` Reput.Norm.shadowOffsetY :: Number -> IO () `*/
				shadowOffsetY : (y : number) : IO <null> =>
					IO (() => (λ.ctx.shadowOffsetY = y * λ.ctx.canvas.height, null)),

				/**` Reput.Norm.shadowOffsetXY :: Number -> Number -> IO () `*/
				shadowOffsetXY : (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.shadowOffsetX = x * λ.ctx.canvas.width,
						λ.ctx.shadowOffsetY = y * λ.ctx.canvas.height,
						null
					)),

				/**` Reput.Norm.shadowOffsetP :: Pair Number Number -> IO () `*/
				shadowOffsetP : (p : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.shadowOffsetX = p .fst * λ.ctx.canvas.width,
						λ.ctx.shadowOffsetY = p .snd * λ.ctx.canvas.height,
						null
					)),

				/**` Reput.Norm.shadowOffsetV :: Vector2 -> IO () `*/
				shadowOffsetV : (v : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.shadowOffsetX = v.x * λ.ctx.canvas.width,
						λ.ctx.shadowOffsetY = v.y * λ.ctx.canvas.height,
						null
					)),

				/**` Reput.Norm.transformationMatrix :: Matrix3 -> IO () `*/
				transformationMatrix : (m : Matrix3) : IO <null> =>
					IO (() => (
						λ.ctx.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx * λ.ctx.canvas.width, m.ky * λ.ctx.canvas.height),
						null
					))
			},

		/**` Reput.layer :: Number -> IO () `*/
		layer : (index : number) : IO <null> =>
			IO (() => {
				if (λ.ctxs[index])
					λ.ctx = λ.ctxs[index]!
				else
					THROW (`'(Reput.layer)' only accepts integers in interval [0, ${λ.ctxs.length}); instead received '${index}'`)
				return null
			}),

		/**` Reput.canvasW :: Number -> IO () `*/
		canvasW : (w : number) : IO <null> =>
			IO (() => (λ.ctxs.forEach(ctx => ctx.canvas.width = w), null)),

		/**` Reput.canvasH :: Number -> IO () `*/
		canvasH : (h : number) : IO <null> =>
			IO (() => (λ.ctxs.forEach(ctx => ctx.canvas.height = h), null)),

		/**` Reput.canvasWH :: Number -> Number -> IO () `*/
		canvasWH : (w : number) => (h : number) : IO <null> =>
			IO (() => (λ.ctxs.forEach(ctx => (ctx.canvas.width = w, ctx.canvas.height = h)), null)),

		/**` Reput.canvasP :: Pair Number Number -> IO () `*/
		canvasP : (p : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctxs.forEach(ctx => (ctx.canvas.width = p .fst, ctx.canvas.height = p .snd)), null)),

		/**` Reput.canvasV :: Vector2 -> IO () `*/
		canvasV : (v : Vector2) : IO <null> =>
			IO (() => (λ.ctxs.forEach(ctx => (ctx.canvas.width = v.x, ctx.canvas.height = v.y)), null)),

		/**` Reput.lineThickness :: Number -> IO () `*/
		lineThickness : (t : number) : IO <null> =>
			IO (() => (λ.ctx.lineWidth = t, null)),

		/**` Reput.lineCap :: LineCap -> IO () `*/
		lineCap : (cap : LineCap) : IO <null> =>
			IO (() => (λ.ctx.lineCap = mappingLineCap .codomain (cap), null)),

		/**` Reput.lineJoin :: LineJoin -> IO () `*/
		lineJoin : (joining : LineJoin) : IO <null> =>
			IO (() => (λ.ctx.lineJoin = mappingLineJoin .codomain (joining), null)),

		/**` Reput.lineDashPattern :: List Number -> IO () `*/
		lineDashPattern : (pattern : List <number>) : IO <null> =>
			IO (() => (λ.ctx.setLineDash (array (pattern)), null)),

		/**` Reput.lineDashOffset :: Number -> IO () `*/
		lineDashOffset : (offset : number) : IO <null> =>
			IO (() => (λ.ctx.lineDashOffset = offset, null)),

		/**` Reput.miterLimit :: Number -> IO () `*/
		miterLimit : (limit : number) : IO <null> =>
			IO (() => (λ.ctx.miterLimit = limit, null)),

		/**` Reput.font :: String -> IO () `*/
		font : (fontInfo : string) : IO <null> =>
			IO (() => (λ.ctx.font = fontInfo, null)),

		/**` Reput.fontSize :: Number -> IO () `*/
		fontSize : (size : number) : IO <null> =>
			IO (() => (λ.ctx.font = `${size}px${λ.ctx.font.slice(λ.ctx.font.indexOf(" "))}`, null)),

		/**` Reput.fontFamily :: String -> IO () `*/
		fontFamily : (family : string) : IO <null> =>
			IO (() => (λ.ctx.font = `${parseFloat(λ.ctx.font)}px ${family}`, null)),

		/**` Reput.textAlign :: TextAlign -> IO () `*/
		textAlign : (align : TextAlign) : IO <null> =>
			IO (() => (λ.ctx.textAlign = mappingTextAlign .codomain (align), null)),

		/**` Reput.textBaseline :: TextBaseline -> IO () `*/
		textBaseline : (baseline : TextBaseline) : IO <null> =>
			IO (() => (λ.ctx.textBaseline = mappingTextBaseline .codomain (baseline), null)),

		/**` Reput.fillColor :: String -> IO () `*/
		fillColor : (color : string) : IO <null> =>
			IO (() => (λ.ctx.fillStyle = color, null)),

		/**` Reput.fillRGBA :: Number -> Number -> Number -> Number -> IO () `*/
		fillRGBA : (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
			IO (() => (λ.ctx.fillStyle = `rgba(${r},${g},${b},${a})`, null)),

		/**` Reput.fillRGBAV :: Vector4 -> IO () `*/
		fillRGBAV : (v : Vector4) : IO <null> =>
			IO (() => (λ.ctx.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null)),

		/**` Reput.strokeColor :: String -> IO () `*/
		strokeColor : (color : string) : IO <null> =>
			IO (() => (λ.ctx.strokeStyle = color, null)),

		/**` Reput.strokeRGBA :: Number -> Number -> Number -> Number -> IO () `*/
		strokeRGBA : (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
			IO (() => (λ.ctx.strokeStyle = `rgba(${r},${g},${b},${a})`, null)),

		/**` Reput.strokeRGBAV :: Vector4 -> IO () `*/
		strokeRGBAV : (v : Vector4) : IO <null> =>
			IO (() => (λ.ctx.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null)),

		/**` Reput.shadowBlurAmount :: Number -> IO () `*/
		shadowBlurAmount : (amount : number) : IO <null> =>
			IO (() => (λ.ctx.shadowBlur = amount, null)),

		/**` Reput.shadowColor :: String -> IO () `*/
		shadowColor : (color : string) : IO <null> =>
			IO (() => (λ.ctx.shadowColor = color, null)),

		/**` Reput.shadowRGBA :: Number -> Number -> Number -> Number -> IO () `*/
		shadowRGBA : (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
			IO (() => (λ.ctx.shadowColor = `rgba(${r},${g},${b},${a})`, null)),

		/**` Reput.shadowRGBAV :: Vector4 -> IO () `*/
		shadowRGBAV : (v : Vector4) : IO <null> =>
			IO (() => (λ.ctx.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`, null)),

		/**` Reput.shadowOffsetX :: Number -> IO () `*/
		shadowOffsetX : (x : number) : IO <null> =>
			IO (() => (λ.ctx.shadowOffsetX = x, null)),

		/**` Reput.shadowOffsetY :: Number -> IO () `*/
		shadowOffsetY : (y : number) : IO <null> =>
			IO (() => (λ.ctx.shadowOffsetY = y, null)),

		/**` Reput.shadowOffsetXY :: Number -> Number -> IO () `*/
		shadowOffsetXY : (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.shadowOffsetX = x, λ.ctx.shadowOffsetY = y, null)),

		/**` Reput.shadowOffsetV :: Vector2 -> IO () `*/
		shadowOffsetV : (v : Vector2) : IO <null> =>
			IO (() => (λ.ctx.shadowOffsetX = v.x, λ.ctx.shadowOffsetY = v.y, null)),

		/**` Reput.transformationMatrix :: Matrix3 -> IO () `*/
		transformationMatrix : (m : Matrix3) : IO <null> =>
			IO (() => (λ.ctx.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null)),

		/**` Reput.alpha :: Number -> IO () `*/
		alpha : (opacity : number) : IO <null> =>
			IO (() => (λ.ctx.globalAlpha = opacity, null)),

		/**` Reput.compositionOperation :: Composition -> IO () `*/
		compositionOperation : (composition : Composition) : IO <null> =>
			IO (() => (λ.ctx.globalCompositeOperation = mappingComposition .codomain (composition), null))
	}

const Output =
	{
		Norm :
			{
				/**` Output.Norm.drawImage :: String -> Number -> Number -> IO () `*/
				drawImage : (path : string) => (x : number) => (y : number) : IO <null> =>
					IO (() => (λ.ctx.drawImage(λ.image[path]!, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.drawImageP :: String -> Pair Number Number -> IO () `*/
				drawImageP : (path : string) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(λ.image[path]!, xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height
					), null)),

				/**` Output.Norm.drawImageV :: String -> Vector2 -> Vector2 -> IO () `*/
				drawImageV : (path : string) => (xy : Vector2) : IO <null> =>
					IO (() => (λ.ctx.drawImage(λ.image[path]!, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.drawCroppedImage :: String -> ...8 Number -> IO () `*/
				drawCroppedImage :
					(path : string) =>
					(cx   : number) => (cy : number) => (cw : number) => (ch : number) =>
					(x    : number) => (y  : number) => (w  : number) => (h  : number) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							cx * λ.image[path]!.width, cy * λ.image[path]!.height,
							cw * λ.image[path]!.width, ch * λ.image[path]!.height,
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							w * λ.ctx.canvas.width, h * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.drawCroppedImageP :: String -> ...4 Pair Number Number -> IO () `*/
				drawCroppedImageP :
					(path : string) =>
					(cxy  : Pair <number, number>) => (cwh  : Pair <number, number>) =>
					(xy   : Pair <number, number>) => (wh   : Pair <number, number>) : IO <null> =>
					IO (() => {
						λ.ctx.drawImage(
							λ.image[path]!,
							cxy .fst * λ.image[path]!.width, cxy .snd * λ.image[path]!.width,
							cwh .fst * λ.image[path]!.width, cwh .snd * λ.image[path]!.width,
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height
						)
						return null
					}),

				/**` Output.Norm.drawCroppedImageV :: String -> ...4 Vector2 -> IO () `*/
				drawCroppedImageV :
					(path : string ) =>
					(cxy  : Vector2) => (cwh : Vector2) =>
					(xy   : Vector2) => (wh  : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							cxy.x * λ.image[path]!.width,  cxy.y * λ.image[path]!.width,
							cwh.x * λ.image[path]!.height, cwh.y * λ.image[path]!.height,
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.drawFullImage :: String -> ...4 Number -> IO () `*/
				drawFullImage : (path : string) => (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							w * λ.ctx.canvas.width, h * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.drawFullImageP :: String -> Pair Number Number -> Pair Number Number -> IO () `*/
				drawFullImageP : (path : string) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.drawFullImageV :: String -> Vector2 -> Vector2 -> IO () `*/
				drawFullImageV : (path : string)  => (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.drawSquareImage :: String -> Number -> Number -> Number -> IO () `*/
				drawSquareImage : (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							k, k
						), null
					)),

				/**` Output.Norm.drawSquareImageP :: String -> Number -> Pair Number Number -> IO () `*/
				drawSquareImageP : (path : string) => (k : number) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							k, k
						), null
					)),

				/**` Output.Norm.drawSquareImageV :: String -> Number -> Vector2 -> IO () `*/
				drawSquareImageV : (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							k, k
						), null
					)),

				/**` Output.Norm.drawScaledImage :: String -> Number -> Number -> Number -> IO () `*/
				drawScaledImage : (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.drawImage(
							λ.image[path]!,
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							λ.image[path]!.width * k, λ.image[path]!.height * k
						), null
					)),

				/**` Output.Norm.drawScaledImageP :: String -> Number -> Pair Number Number -> IO () `*/
				drawScaledImageP : (path : string) => (k : number) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => {
						λ.ctx.drawImage(
							λ.image[path]!,
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							λ.image[path]!.width * k, λ.image[path]!.height * k
						)
						return null
					}),

				/**` Output.Norm.drawScaledImageV :: String -> Number -> Vector2 -> IO () `*/
				drawScaledImageV : (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
					IO (() => {
						λ.ctx.drawImage(
							λ.image[path]!,
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							λ.image[path]!.width * k, λ.image[path]!.height * k)
						return null
					}),

				/**` Output.Norm.clearRectangle :: Number -> Number -> Number -> Number -> IO () `*/
				clearRectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
					IO (() => (
						λ.ctx.clearRect(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.width,
							w * λ.ctx.canvas.width, h * λ.ctx.canvas.width
						), null
					)),

				/**` Output.Norm.clearRectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
				clearRectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.clearRect(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.clearRectangleV :: Vector2 -> Vector2 -> IO () `*/
				clearRectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.clearRect(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.rotate :: Number -> IO () `*/
				rotate : (angle : number) : IO <null> => IO (() => (λ.ctx.rotate(angle * TAU), null)),

				/**` Output.Norm.scaleAxisP :: Pair Number Number -> IO () `*/
				scaleAxisP : (kxy : Pair <number, number>) : IO <null> =>
					IO (() => (λ.ctx.scale(kxy .fst * λ.ctx.canvas.width, kxy .snd * λ.ctx.canvas.height), null)),

				/**` Output.Norm.scaleAxisV :: Vector2 -> IO () `*/
				scaleAxisV : (kxy : Vector2) : IO <null> =>
					IO (() => (λ.ctx.scale(kxy.x * λ.ctx.canvas.width, kxy.y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.translateX :: Number -> IO () `*/
				translateX : (dx : number) : IO <null> =>
					IO (() => (λ.ctx.translate(dx * λ.ctx.canvas.width, 0), null)),

				/**` Output.Norm.translateY :: Number -> IO () `*/
				translateY : (dy : number) : IO <null> =>
					IO (() => (λ.ctx.translate(0, dy * λ.ctx.canvas.height), null)),

				/**` Output.Norm.translate :: Number -> Number -> IO () `*/
				translate : (dx : number) => (dy : number) : IO <null> =>
					IO (() => (λ.ctx.translate(dx * λ.ctx.canvas.width, dy * λ.ctx.canvas.height), null)),

				/**` Output.Norm.translateP :: Pair Number Number -> IO () `*/
				translateP : (dxy : Pair <number, number>) : IO <null> =>
					IO (() => (λ.ctx.translate(dxy .fst * λ.ctx.canvas.width, dxy .snd * λ.ctx.canvas.height), null)),

				/**` Output.Norm.translateV :: Vector2 -> IO () `*/
				translateV : (dxy : Vector2) : IO <null> =>
					IO (() => (λ.ctx.translate(dxy.x * λ.ctx.canvas.width, dxy.y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.transformation :: Matrix3 -> IO () `*/
				transformation : (m : Matrix3) : IO <null> =>
					IO (() => (
						λ.ctx.transform(m.ix, m.iy, m.jx, m.jy, m.kx * λ.ctx.canvas.width, m.ky * λ.ctx.canvas.height),
						null
					)),

				/**` Output.Norm.moveTo :: Number -> Number -> IO () `*/
				moveTo : (x : number) => (y : number) : IO <null> =>
					IO (() => (λ.ctx.moveTo(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.moveToP :: Pair Number Number -> IO () `*/
				moveToP : (xy : Pair <number, number>) : IO <null> =>
					IO (() => (λ.ctx.moveTo(xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height), null)),

				/**` Output.Norm.moveToV :: Vector2 -> IO () `*/
				moveToV : (xy : Vector2) : IO <null> =>
					IO (() => (λ.ctx.moveTo(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.lineTo :: Number -> Number -> IO () `*/
				lineTo : (x : number) => (y : number) : IO <null> =>
					IO (() => (λ.ctx.lineTo(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.lineToP :: Pair Number Number -> IO () `*/
				lineToP : (xy : Pair <number, number>) : IO <null> =>
					IO (() => (λ.ctx.lineTo(xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height), null)),

				/**` Output.Norm.lineToV :: Vector2 -> IO () `*/
				lineToV : (xy : Vector2) : IO <null> =>
					IO (() => (λ.ctx.lineTo(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.bezierCurveTo :: ...6 Number -> IO () `*/
				bezierCurveTo :
					(cx0 : number) => (cy0 : number) =>
					(cx1 : number) => (cy1 : number) =>
					(x   : number) => (y   : number) : IO <null> =>
					IO (() => (
						λ.ctx.bezierCurveTo(
							cx0 * λ.ctx.canvas.width, cy0 * λ.ctx.canvas.height,
							cx1 * λ.ctx.canvas.width, cy1 * λ.ctx.canvas.height,
							x   * λ.ctx.canvas.width, y   * λ.ctx.canvas.height),
							null
						)),

				/**` Output.Norm.bezierCurveToP :: Pair Number Number -> Pair Number Number -> Pair Number Number -> IO () `*/
				bezierCurveToP :
					(cxy0 : Pair <number, number>) =>
					(cxy1 : Pair <number, number>) =>
					(xy   : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.bezierCurveTo(
							cxy0 .fst * λ.ctx.canvas.width, cxy0 .snd * λ.ctx.canvas.height,
							cxy1 .fst * λ.ctx.canvas.width, cxy1 .snd * λ.ctx.canvas.height,
							xy   .fst * λ.ctx.canvas.width, xy   .snd * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.bezierCurveToV :: Vector2 -> Vector2 -> Vector2 -> IO () `*/
				bezierCurveToV : (cxy0 : Vector2) => (cxy1 : Vector2) => (xy : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.bezierCurveTo(
							cxy0.x * λ.ctx.canvas.width, cxy0.y * λ.ctx.canvas.height,
							cxy1.x * λ.ctx.canvas.width, cxy1.y * λ.ctx.canvas.height,
							xy.x   * λ.ctx.canvas.width, xy.y   * λ.ctx.canvas.height
						), null)),

				/**` Output.Norm.quadraticCurveTo :: Number -> Number -> Number -> Number -> IO () `*/
				quadraticCurveTo : (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.quadraticCurveTo(
							cx * λ.ctx.canvas.width, cy * λ.ctx.canvas.width,
							x  * λ.ctx.canvas.width, y  * λ.ctx.canvas.width
						), null
					)),

				/**` Output.Norm.quadraticCurveToP :: Pair Number Number -> Pair Number Number -> IO () `*/
				quadraticCurveToP : (cxy : Pair <number, number>) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.quadraticCurveTo(
							cxy .fst * λ.ctx.canvas.width, cxy .snd * λ.ctx.canvas.height,
							xy  .fst * λ.ctx.canvas.width, xy  .snd * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.quadraticCurveToV :: Vector2 -> Vector2 -> IO () `*/
				quadraticCurveToV : (cxy : Vector2) => (xy : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.quadraticCurveTo(
							cxy.x * λ.ctx.canvas.width, cxy.y * λ.ctx.canvas.height,
							xy.x  * λ.ctx.canvas.width, xy.y  * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.arcTo :: ...5 Number -> IO () `*/
				arcTo : (r : number) => (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) : IO <null> =>
					IO (() => (
						λ.ctx.arcTo(
							cx0 * λ.ctx.canvas.width, cy0 * λ.ctx.canvas.height,
							cx1 * λ.ctx.canvas.width, cy1 * λ.ctx.canvas.height,
							r   * λ.ctx.canvas.width
						), null
					)),

				/**` Output.Norm.arcToP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
				arcToP : (r : number) => (cxy0 : Pair <number, number>) => (cxy1 : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.arcTo(
							cxy0 .fst * λ.ctx.canvas.width, cxy0 .snd * λ.ctx.canvas.height,
							cxy1 .fst * λ.ctx.canvas.width, cxy1 .snd * λ.ctx.canvas.height,
							r         * λ.ctx.canvas.width
						), null
					)),

				/**` Output.Norm.arcToV :: Number -> Vector2 -> Vector2 -> IO () `*/
				arcToV : (r : number) => (cxy0 : Vector2) => (cxy1 : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.arcTo(
							cxy0.x * λ.ctx.canvas.width, cxy0.y * λ.ctx.canvas.height,
							cxy1.x * λ.ctx.canvas.width, cxy1.y * λ.ctx.canvas.height,
							r      * λ.ctx.canvas.width
						), null
					)),

				/**` Output.Norm.rectangle :: Number -> Number -> Number -> Number -> IO () `*/
				rectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
					IO (() => (
						λ.ctx.rect(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							w * λ.ctx.canvas.width, h * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.rectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
				rectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.rect(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.width,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.width
						), null
					)),

				/**` Output.Norm.rectangleV :: Vector2 -> Vector2 -> IO () `*/
				rectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.rect(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.fillRectangle :: Number -> Number -> Number -> Number -> IO () `*/
				fillRectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
					IO (() => (
						λ.ctx.fillRect(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							w * λ.ctx.canvas.width, h * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.fillRectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
				fillRectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.fillRect(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.fillRectangleV :: Vector2 -> Vector2 -> IO () `*/
				fillRectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.fillRect(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height
						), null)),

				/**` Output.Norm.strokeRectangle :: Number -> Number -> Number -> Number -> IO () `*/
				strokeRectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
					IO (() => (
						λ.ctx.strokeRect(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							w * λ.ctx.canvas.width, h * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.strokeRectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
				strokeRectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.strokeRect(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.strokeRectangleV :: Vector2 -> Vector2 -> IO () `*/
				strokeRectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.strokeRect(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height
						), null
					)),

				/**` Output.Norm.arc :: ...5 Number -> IO () `*/
				arc : (r : number) => (a : number) => (b : number) => (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							r * λ.ctx.canvas.width, a * TAU, b * TAU
						), null
					)),

				/**` Output.Norm.arcP :: Number -> Number -> Number -> Pair Number Number -> IO () `*/
				arcP : (r : number) => (a : number) => (b : number) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.width, a * TAU, b * TAU
						), null
					)),

				/**` Output.Norm.arcV :: Number -> Number -> Number -> Vector2 -> IO () `*/
				arcV : (r : number) => (a : number) => (b : number) => (xy : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, a * TAU, b * 360
						), null
					)),

				/**` Output.Norm.circle :: Number -> Number -> Number -> IO () `*/
				circle : (r : number) => (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							r * λ.ctx.canvas.width, 0, TAU
						), null
					)),

				/**` Output.Norm.circleP :: Number -> Pair Number Number -> IO () `*/
				circleP : (r : number) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.width, 0, TAU
						), null
					)),

				/**` Output.Norm.circleV :: Number -> Vector2 -> IO () `*/
				circleV : (r : number) => (xy : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, 0, TAU
						), null
					)),

				/**` Output.Norm.strokeCircle :: Number -> Number -> Number -> IO () `*/
				strokeCircle : (r : number) => (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							r * λ.ctx.canvas.width, 0, TAU
						), λ.ctx.stroke(), null
					)),

				/**` Output.Norm.strokeCircleP :: Number -> Pair Number Number -> IO () `*/
				strokeCircleP : (r : number) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.width, 0, TAU
						), λ.ctx.stroke(), null
					)),

				/**` Output.Norm.strokeCircleV :: Number -> Vector2 -> IO () `*/
				strokeCircleV : (r : number) => (xy : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, 0, TAU
						), λ.ctx.stroke(), null
					)),

				/**` Output.Norm.fillCircle :: Number -> Number -> Number -> IO () `*/
				fillCircle : (r : number) => (x : number) => (y : number) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							x * λ.ctx.canvas.width, y * λ.ctx.canvas.height,
							r * λ.ctx.canvas.width, 0, TAU
						), λ.ctx.fill(), null
					)),

				/**` Output.Norm.fillCircleP :: Number -> Pair Number Number -> IO () `*/
				fillCircleP : (r : number) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.height, 0, TAU
						), λ.ctx.fill(), null
					)),

				/**` Output.Norm.fillCircleV :: Number -> Vector2 -> IO () `*/
				fillCircleV : (r : number) => (xy : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.arc(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, 0, TAU
						), λ.ctx.fill(), null
					)),

				/**` Output.Norm.elliptic :: ...7 Number -> IO () `*/
				elliptic :
					(r : number) => (a : number) => (b  : number) =>
					(x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
					IO (() => (
						λ.ctx.ellipse(
							x  * λ.ctx.canvas.width, y  * λ.ctx.canvas.height,
							kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height,
							r  * λ.ctx.canvas.width, a  * TAU, b * TAU
						), null
					)),

				/**` Output.Norm.ellipticP :: Number -> Number -> Number -> Pair Number Number -> Pair Number Number -> IO () `*/
				ellipticP :
					(r  : number) => (a : number) => (b  : number) =>
					(xy : Pair <number, number>)  => (wh : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.ellipse(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.width, a * TAU, b * TAU
						), null
					)),

				/**` Output.Norm.ellipticVector :: Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
				ellipticV :
					(r  : number)  => (a  : number ) => (b : number) =>
					(xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.ellipse(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, a * TAU, b * TAU
						), null
					)),

				/**` Output.Norm.ellipse :: ...5 Number -> IO () `*/
				ellipse : (r : number) => (x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
					IO (() => (
						λ.ctx.ellipse(
							x  * λ.ctx.canvas.width, y  * λ.ctx.canvas.height,
							kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height,
							r  * λ.ctx.canvas.width, 0, TAU
						), null
					)),

				/**` Output.Norm.ellipseP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
				ellipseP : (r : number) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => (
						λ.ctx.ellipse(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.width, 0, TAU
						), null
					)),

				/**` Output.Norm.ellipseV :: Number -> Vector2 -> Vector2 -> IO () `*/
				ellipseV : (r : number) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => (
						λ.ctx.ellipse(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, 0, TAU
						), null
					)),

				/**` Output.Norm.strokeEllipse :: ...5 Number -> IO () `*/
				strokeEllipse : (r : number) => (x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
					IO (() => (
						λ.ctx.ellipse(
							x  * λ.ctx.canvas.width, y  * λ.ctx.canvas.height,
							kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height,
							r  * λ.ctx.canvas.width, 0, TAU
						), λ.ctx.stroke(), null
					)),

				/**` Output.Norm.strokeEllipseP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
				strokeEllipseP : (r : number) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => {
						λ.ctx.ellipse(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.width, 0, TAU
						)
						λ.ctx.stroke()
						return null
					}),

				/**` Output.Norm.strokeEllipseV :: Number -> Vector2 -> Vector2 -> IO () `*/
				strokeEllipseV : (r : number) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => {
						λ.ctx.ellipse(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, 0, TAU
						)
						λ.ctx.stroke()
						return null
					}),

				/**` Output.Norm.fillEllipse :: ...5 Number -> IO () `*/
				fillEllipse : (r : number) => (x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
					IO (() => {
						λ.ctx.ellipse(
							x  * λ.ctx.canvas.width, y  * λ.ctx.canvas.height,
							kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height,
							r  * λ.ctx.canvas.width, 0, TAU
						)
						λ.ctx.fill()
						return null
					}),

				/**` Output.Norm.fillEllipseP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
				fillEllipseP : (r : number) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
					IO (() => {
						λ.ctx.ellipse(
							xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height,
							wh .fst * λ.ctx.canvas.width, wh .snd * λ.ctx.canvas.height,
							r       * λ.ctx.canvas.width, 0, TAU
						)
						λ.ctx.fill()
						return null
					}),

				/**` Output.Norm.fillEllipseV :: Number -> Vector2 -> Vector2 -> IO () `*/
				fillEllipseV : (r : number) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
					IO (() => {
						λ.ctx.ellipse(
							xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height,
							wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height,
							r    * λ.ctx.canvas.width, 0, TAU
						)
						λ.ctx.fill()
						return null
					}),

				/**` Output.Norm.strokeText :: String -> Number -> Number -> IO () `*/
				strokeText : (text : string) => (x : number) => (y : number) : IO <null> =>
					IO (() => (λ.ctx.strokeText(text, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.strokeTextP :: String -> Pair Number Number -> IO () `*/
				strokeTextP : (text : string) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (λ.ctx.strokeText(text, xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height), null)),

				/**` Output.Norm.strokeTextV :: String -> Vector2 -> IO () `*/
				strokeTextV : (text : string) => (xy : Vector2) : IO <null> =>
					IO (() => (λ.ctx.strokeText(text, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.fillText :: String -> Number -> Number -> IO () `*/
				fillText : (text : string) => (x : number) => (y : number) : IO <null> =>
					IO (() => (λ.ctx.fillText(text, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.fillTextP :: String -> Pair Number Number -> IO () `*/
				fillTextP : (text : string) => (xy : Pair <number, number>) : IO <null> =>
					IO (() => (λ.ctx.fillText(text, xy .fst * λ.ctx.canvas.width, xy .snd * λ.ctx.canvas.height), null)),

				/**` Output.Norm.fillTextV :: String -> Vector2 -> IO () `*/
				fillTextV : (text : string) => (xy : Vector2) : IO <null> =>
					IO (() => (λ.ctx.fillText(text, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),

				/**` Output.Norm.area :: Number -> Number -> Number -> Number -> IO () `*/
				area : (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
					IO (() => {
						λ.ctx.rect(
							x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height,
							(x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.height
						)
						return null
					}),

				/**` Output.Norm.areaP :: Pair Number Number -> Pair Number Number -> IO () `*/
				areaP : (xy0 : Pair <number, number>) => (xy1 : Pair <number, number>) : IO <null> =>
					IO (() => {
						λ.ctx.rect(
							xy0 .fst * λ.ctx.canvas.width, xy0 .snd * λ.ctx.canvas.height,
							(xy1 .fst - xy0 .fst) * λ.ctx.canvas.width, (xy1 .snd - xy0 .snd) * λ.ctx.canvas.height
						)
						return null
					}),

				/**` Output.Norm.areaV :: Vector2 -> Vector2 -> IO () `*/
				areaV : (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
					IO (() => {
						λ.ctx.rect(
							xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height,
							(xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height
						)
						return null
					}),

				/**` Output.Norm.strokeArea :: Number -> Number -> Number -> Number -> IO () `*/
				strokeArea : (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
					IO (() => {
						λ.ctx.strokeRect(
							x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height,
							(x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.width
						)
						return null
					}),

				/**` Output.Norm.strokeAreaP :: Pair Number Number -> Pair Number Number -> IO () `*/
				strokeAreaP : (xy0 : Pair <number, number>) => (xy1 : Pair <number, number>) : IO <null> =>
					IO (() => {
						λ.ctx.strokeRect(
							xy0 .fst * λ.ctx.canvas.width, xy0 .snd * λ.ctx.canvas.height,
							(xy1 .fst - xy0 .fst) * λ.ctx.canvas.width, (xy1 .snd - xy0 .snd) * λ.ctx.canvas.height
						)
						return null
					}),

				/**` Output.Norm.strokeAreaV :: Vector2 -> Vector2 -> IO () `*/
				strokeAreaV : (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
					IO (() => {
						λ.ctx.strokeRect(
							xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height,
							(xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height
						)
						return null
					}),

				/**` Output.Norm.fillArea :: Number -> Number -> Number -> Number -> IO () `*/
				fillArea : (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
					IO (() => {
						λ.ctx.fillRect(
							x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height,
							(x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.width
						)
						return null
					}),

				/**` Output.Norm.fillAreaP :: Pair Number Number -> Pair Number Number -> IO () `*/
				fillAreaP : (xy0 : Pair <number, number>) => (xy1 : Pair <number, number>) : IO <null> =>
					IO (() => {
						λ.ctx.fillRect(
							xy0 .fst * λ.ctx.canvas.width, xy0 .snd * λ.ctx.canvas.height,
							(xy1 .fst - xy0 .fst) * λ.ctx.canvas.width, (xy1 .snd - xy0 .snd) * λ.ctx.canvas.height
						)
						return null
					}),

				/**` Output.Norm.fillAreaV :: Vector2 -> Vector2 -> IO () `*/
				fillAreaV : (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
					IO (() => {
						λ.ctx.fillRect(
							xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height,
							(xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height
						)
						return null
					})
			},

		/**` Output.log :: a -> IO () `*/
		log : <a>(message : a) : IO <null> => IO (() => (console.log(message), null)),

		/**` Output.warn :: a -> IO () `*/
		warn : <a>(message : a) : IO <null> => IO (() => (console.warn(message), null)),

		/**` Output.flush :: IO () `*/
		flush : IO (() => (console.clear(), null)),

		/**` Output.debug :: Number -> a -> IO () `*/
		debug : (count : number) => <a>(message : a) : IO <null> =>
			IO (() => {
				if (--λ.debugCounter < 0)
				{
					λ.debugCounter = count
					console.debug (message)
				}
				return null
			}),

		/**` Output.queue :: IO a -> IO () `*/
		queue : <a>(io : IO <a>) : IO <null> => IO (() => (requestAnimationFrame(io.INFO), null)),

		/**` Output.tick :: IO () `*/
		tick :
			IO (() => {
				for (const k in λ.keyboard) λ.keyboard[k as KeyboardKey] = relaxY (λ.keyboard[k as KeyboardKey])
				for (const i in λ.mouseButtons) λ.mouseButtons[i] = relaxY (λ.mouseButtons[i]!)
				λ.mouseScroll = Y.Rest
				λ.mouseDeltaX = λ.mouseDeltaY = 0
				λ.isResized   = false
				return null
			}),

		/**` Output.activatePointerLock :: IO () `*/
		activatePointerLock :
			IO (() => (λ.ctx.canvas.onmouseup = () => λ.isPointerLocked && λ.ctx.canvas.requestPointerLock(), null)),

		/**` Output.deactivatePointerLock :: IO () `*/
		deactivatePointerLock : IO (() => λ.ctx.canvas.onmousedown = null),

		/**` Output.loadImage :: String -> IO () `*/
		loadImage : (path : string) : IO <null> =>
			IO (() => {
				λ.image[path]          = new Image
				λ.image[path]!.src     = path
				λ.image[path]!.onerror = () => THROW (`'Output.loadImage' failed; could not load image: '${path}'`)
				return null
			}),

		/**` Output.drawImage :: String -> Number -> Number -> IO () `*/
		drawImage : (path : string) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, x, y), null)),

		/**` Output.drawImageP :: String -> Pair Number Number -> IO () `*/
		drawImageP : (path : string) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, xy .fst, xy .snd), null)),

		/**` Output.drawImageV :: String -> Vector2 -> Vector2 -> IO () `*/
		drawImageV : (path : string) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, xy.x, xy.y), null)),

		/**` Output.drawCroppedImage :: String -> ...8 Number -> IO () `*/
		drawCroppedImage :
			(path : string) =>
			(cx   : number) => (cy : number) => (cw : number) => (ch : number) =>
			(x    : number) => (y  : number) => (w  : number) => (h  : number) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, cx, cy, cw, ch, x, y, w, h), null)),

		/**` Output.drawCroppedImageP :: String -> ...4 Pair Number Number -> IO () `*/
		drawCroppedImageP :
			(path : string) =>
			(cxy  : Pair <number, number>) => (cwh  : Pair <number, number>) =>
			(xy   : Pair <number, number>) => (wh   : Pair <number, number>) : IO <null> =>
			IO (() => {
				λ.ctx.drawImage(λ.image[path]!, cxy .fst, cxy .snd, cwh .fst, cwh .snd, xy .fst, xy .snd, wh .fst, wh .snd)
				return null
			}),

		/**` Output.drawCroppedImageV :: String -> ...4 Vector2 -> IO () `*/
		drawCroppedImageV :
			(path : string ) =>
			(cxy  : Vector2) => (cwh : Vector2) =>
			(xy   : Vector2) => (wh  : Vector2) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, cxy.x, cxy.y, cwh.x, cwh.y, xy.x, xy.y, wh.x, wh.y), null)),

		/**` Output.drawFullImage :: String -> ...4 Number -> IO () `*/
		drawFullImage : (path : string) => (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, x, y, w, h), null)),

		/**` Output.drawFullImageP :: String -> Pair Number Number -> Pair Number Number -> IO () `*/
		drawFullImageP : (path : string) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, xy .fst, xy .snd, wh .fst, wh .snd), null)),

		/**` Output.drawFullImageV :: String -> Vector2 -> Vector2 -> IO () `*/
		drawFullImageV : (path : string)  => (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, xy.x, xy.y, wh.x, wh.y), null)),

		/**` Output.drawSquareImage :: String -> Number -> Number -> Number -> IO () `*/
		drawSquareImage : (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, x, y, k, k), null)),

		/**` Output.drawSquareImageP :: String -> Number -> Pair Number Number -> IO () `*/
		drawSquareImageP : (path : string) => (k : number) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, xy .fst, xy .snd, k, k), null)),

		/**` Output.drawSquareImageV :: String -> Number -> Vector2 -> IO () `*/
		drawSquareImageV : (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, xy.x, xy.y, k, k), null)),

		/**` Output.drawScaledImage :: String -> Number -> Number -> Number -> IO () `*/
		drawScaledImage : (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.drawImage(λ.image[path]!, x, y, λ.image[path]!.width * k, λ.image[path]!.height * k), null)),

		/**` Output.drawScaledImageP :: String -> Number -> Pair Number Number -> IO () `*/
		drawScaledImageP : (path : string) => (k : number) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => {
				λ.ctx.drawImage(λ.image[path]!, xy .fst, xy .snd, λ.image[path]!.width * k, λ.image[path]!.height * k)
				return null
			}),

		/**` Output.drawScaledImageV :: String -> Number -> Vector2 -> IO () `*/
		drawScaledImageV : (path : string) => (k : number) => (xy : Vector2) : IO <null> =>
			IO (() => {
				λ.ctx.drawImage(λ.image[path]!, xy.x, xy.y, λ.image[path]!.width * k, λ.image[path]!.height * k)
				return null
			}),

		/**` Output.loadAudio :: String -> IO () `*/
		loadAudio : (path : string) : IO <null> =>
			IO (() => {
				λ.audio[path]          = new Audio(path)
				λ.audio[path]!.onerror = () => THROW (`'Output.loadAudio' failed; could not load audio: '${path}'`)
				return null
			}),

		/**` Output.playAudio :: String -> IO () `*/
		playAudio : (path : string) : IO <null> =>
			IO (() => ((λ.audio[path] ?? THROW (`'Output.playAudio' failed; audio not preloaded: '${path}'`) ).play(), null)),

		/**` Output.playSFX :: String -> IO () `*/
		playSFX : (path : string) : IO <null> =>
			IO (() => {
				((λ.audio[path] ?? THROW (`'Output.playSFX' failed; audio not preloaded: '${path}'`)).cloneNode() as any).play()
				return null
			}),

		/**` Output.loadFont :: String -> IO () `*/
		loadFont : (path : string) : IO <null> =>
			IO (() => {
				document.styleSheets[0]!.insertRule(
					`@font-face{font-family:"${
						path .slice (path .lastIndexOf ("/") + 1, path .lastIndexOf ("."))
					}";src:url("${path}")}`
				)
				return null
			}),

		/**` Output.clearRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		clearRectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
			IO (() => (λ.ctx.clearRect(x, y, w, h), null)),

		/**` Output.clearRectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
		clearRectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.clearRect(xy .fst, xy .snd, wh .fst, xy .snd), null)),

		/**` Output.clearRectangleV :: Vector2 -> Vector2 -> IO () `*/
		clearRectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => (λ.ctx.clearRect(xy.x, xy.y, wh.x, wh.y), null)),

		/**` Output.clearLayer :: IO () `*/
		clearLayer : IO (() => (λ.ctx.clearRect(0, 0, λ.ctx.canvas.width, λ.ctx.canvas.height), null)),

		/**` Output.clearCanvas :: IO () `*/
		clearCanvas : IO (() => (λ.ctxs.forEach(ctx => ctx.clearRect(0, 0, λ.ctx.canvas.width, λ.ctx.canvas.height), null))),

		/**` Output.fill :: IO () `*/
		fill : IO (() => (λ.ctx.fill(), null)),

		/**` Output.stroke :: IO () `*/
		stroke : IO (() => (λ.ctx.stroke(), null)),

		/**` Output.save :: IO () `*/
		save : IO (() => (λ.ctx.save(), null)),

		/**` Output.restore :: IO () `*/
		restore : IO (() => (λ.ctx.restore(), null)),

		/**` Output.clipEvenOdd :: IO () `*/
		clipEvenOdd : IO (() => (λ.ctx.clip('evenodd'), null)),

		/**` Output.clipNonZero :: IO () `*/
		clipNonZero : IO (() => (λ.ctx.clip('nonzero'), null)),

		/**` Output.rotate :: Number -> IO () `*/
		rotate : (angle : number) : IO <null> => IO (() => (λ.ctx.rotate(angle), null)),

		/**` Output.scale :: Number -> IO () `*/
		scale : (k : number) : IO <null> => IO (() => (λ.ctx.scale(k, k), null)),

		/**` Output.scaleAxisX :: Number -> IO () `*/
		scaleAxisX : (kx : number) : IO <null> => IO (() => (λ.ctx.scale(kx, 1), null)),

		/**` Output.scaleAxisY :: Number -> IO () `*/
		scaleAxisY : (ky : number) : IO <null> => IO (() => (λ.ctx.scale(1, ky), null)),

		/**` Output.scaleAxis :: Number -> Number -> IO () `*/
		scaleAxis : (kx : number) => (ky : number) : IO <null> => IO (() => (λ.ctx.scale(kx, ky), null)),

		/**` Output.scaleAxisP :: Pair Number Number -> IO () `*/
		scaleAxisP : (kxy : Pair <number, number>) : IO <null> => IO (() => (λ.ctx.scale(kxy .fst, kxy .snd), null)),

		/**` Output.scaleAxisV :: Vector2 -> IO () `*/
		scaleAxisV : (kxy : Vector2) : IO <null> => IO (() => (λ.ctx.scale(kxy.x, kxy.y), null)),

		/**` Output.translateX :: Number -> IO () `*/
		translateX : (dx : number) : IO <null> => IO (() => (λ.ctx.translate(dx, 0), null)),

		/**` Output.translateY :: Number -> IO () `*/
		translateY : (dy : number) : IO <null> => IO (() => (λ.ctx.translate(0, dy), null)),

		/**` Output.translate :: Number -> Number -> IO () `*/
		translate : (dx : number) => (dy : number) : IO <null> => IO (() => (λ.ctx.translate(dx, dy), null)),

		/**` Output.translateP :: Pair Number Number -> IO () `*/
		translateP : (dxy : Pair <number, number>) : IO <null> => IO (() => (λ.ctx.translate(dxy .fst, dxy .snd), null)),

		/**` Output.translateV :: Vector2 -> IO () `*/
		translateV : (dxy : Vector2) : IO <null> => IO (() => (λ.ctx.translate(dxy.x, dxy.y), null)),

		/**` Output.transformation :: Matrix3 -> IO () `*/
		transformation : (m : Matrix3) : IO <null> => IO (() => (λ.ctx.transform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null)),

		/**` Output.beginPath :: IO () `*/
		beginPath : IO (() => (λ.ctx.beginPath(), null)),

		/**` Output.closePath :: IO () `*/
		closePath : IO (() => (λ.ctx.closePath(), null)),

		/**` Output.moveTo :: Number -> Number -> IO () `*/
		moveTo : (x : number) => (y : number) : IO <null> => IO (() => (λ.ctx.moveTo(x, y), null)),

		/**` Output.moveToP :: Pair Number Number -> IO () `*/
		moveToP : (xy : Pair <number, number>) : IO <null> => IO (() => (λ.ctx.moveTo(xy .fst, xy .snd), null)),

		/**` Output.moveToV :: Vector2 -> IO () `*/
		moveToV : (xy : Vector2) : IO <null> => IO (() => (λ.ctx.moveTo(xy.x, xy.y), null)),

		/**` Output.lineTo :: Number -> Number -> IO () `*/
		lineTo : (x : number) => (y : number) : IO <null> => IO (() => (λ.ctx.lineTo(x, y), null)),

		/**` Output.lineToP :: Pair Number Number -> IO () `*/
		lineToP : (xy : Pair <number, number>) : IO <null> => IO (() => (λ.ctx.lineTo(xy .fst, xy .snd), null)),

		/**` Output.lineToV :: Vector2 -> IO () `*/
		lineToV : (xy : Vector2) : IO <null> => IO (() => (λ.ctx.lineTo(xy.x, xy.y), null)),

		/**` Output.bezierCurveTo :: ...6 Number -> IO () `*/
		bezierCurveTo :
			(cx0 : number) => (cy0 : number) =>
			(cx1 : number) => (cy1 : number) =>
			(x   : number) => (y   : number) : IO <null> =>
			IO (() => (λ.ctx.bezierCurveTo(cx0, cy0, cx1, cy1, x, y), null)),

		/**` Output.bezierCurveToP :: Pair Number Number -> Pair Number Number -> Pair Number Number -> IO () `*/
		bezierCurveToP :
			(cxy0 : Pair <number, number>) =>
			(cxy1 : Pair <number, number>) =>
			(xy   : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.bezierCurveTo(cxy0 .fst, cxy0 .snd, cxy1 .fst, cxy1 .snd, xy .fst, xy .snd), null)),

		/**` Output.bezierCurveToV :: Vector2 -> Vector2 -> Vector2 -> IO () `*/
		bezierCurveToV : (cxy0 : Vector2) => (cxy1 : Vector2) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.bezierCurveTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, xy.x, xy.y), null)),

		/**` Output.quadraticCurveTo :: Number -> Number -> Number -> Number -> IO () `*/
		quadraticCurveTo : (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.quadraticCurveTo(cx, cy, x, y), null)),

		/**` Output.quadraticCurveToP :: Pair Number Number -> Pair Number Number -> IO () `*/
		quadraticCurveToP : (cxy : Pair <number, number>) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.quadraticCurveTo(cxy .fst, cxy .snd, xy .fst, xy .snd), null)),

		/**` Output.quadraticCurveToV :: Vector2 -> Vector2 -> IO () `*/
		quadraticCurveToV : (cxy : Vector2) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.quadraticCurveTo(cxy.x, cxy.y, xy.x, xy.y), null)),

		/**` Output.arcTo :: ...5 Number -> IO () `*/
		arcTo : (r : number) => (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) : IO <null> =>
			IO (() => (λ.ctx.arcTo(cx0, cy0, cx1, cy1, r), null)),

		/**` Output.arcToP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
		arcToP : (r : number) => (cxy0 : Pair <number, number>) => (cxy1 : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.arcTo(cxy0 .fst, cxy0 .snd, cxy1 .fst, cxy1 .snd, r), null)),

		/**` Output.arcToV :: Number -> Vector2 -> Vector2 -> IO () `*/
		arcToV : (r : number) => (cxy0 : Vector2) => (cxy1 : Vector2) : IO <null> =>
			IO (() => (λ.ctx.arcTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, r), null)),

		/**` Output.rectangle :: Number -> Number -> Number -> Number -> IO () `*/
		rectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
			IO (() => (λ.ctx.rect(x, y, w, h), null)),

		/**` Output.rectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
		rectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.rect(xy .fst, xy .snd, wh .fst, wh .snd), null)),

		/**` Output.rectangleV :: Vector2 -> Vector2 -> IO () `*/
		rectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => (λ.ctx.rect(xy.x, xy.y, wh.x, wh.y), null)),

		/**` Output.fillRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		fillRectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
			IO (() => (λ.ctx.fillRect(x, y, w, h), null)),

		/**` Output.fillRectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
		fillRectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.fillRect(xy .fst, xy .snd, wh .fst, wh .snd), null)),

		/**` Output.fillRectangleV :: Vector2 -> Vector2 -> IO () `*/
		fillRectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => (λ.ctx.fillRect(xy.x, xy.y, wh.x, wh.y), null)),

		/**` Output.strokeRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		strokeRectangle : (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
			IO (() => (λ.ctx.strokeRect(x, y, w, h), null)),

		/**` Output.strokeRectangleP :: Pair Number Number -> Pair Number Number -> IO () `*/
		strokeRectangleP : (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.strokeRect(xy .fst, xy .snd, wh .fst, wh .snd), null)),

		/**` Output.strokeRectangleV :: Vector2 -> Vector2 -> IO () `*/
		strokeRectangleV : (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => (λ.ctx.strokeRect(xy.x, xy.y, wh.x, wh.y), null)),

		/**` Output.arc :: ...5 Number -> IO () `*/
		arc : (r : number) => (a : number) => (b : number) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.arc(x, y, r, a, b), null)),

		/**` Output.arcP :: Number -> Number -> Number -> Pair Number Number -> IO () `*/
		arcP : (r : number) => (a : number) => (b : number) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.arc(xy .fst, xy .snd, r, a, b), null)),

		/**` Output.arcV :: Number -> Number -> Number -> Vector2 -> IO () `*/
		arcV : (r : number) => (a : number) => (b : number) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.arc(xy.x, xy.y, r, a, b), null)),

		/**` Output.circle :: Number -> Number -> Number -> IO () `*/
		circle : (r : number) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.arc(x, y, r, 0, TAU), null)),

		/**` Output.circleP :: Number -> Pair Number Number -> IO () `*/
		circleP : (r : number) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.arc(xy .fst, xy .snd, r, 0, TAU), null)),

		/**` Output.circleV :: Number -> Vector2 -> IO () `*/
		circleV : (r : number) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), null)),

		/**` Output.strokeCircle :: Number -> Number -> Number -> IO () `*/
		strokeCircle : (r : number) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.arc(x, y, r, 0, TAU), λ.ctx.stroke(), null)),

		/**` Output.strokeCircleP :: Number -> Pair Number Number -> IO () `*/
		strokeCircleP : (r : number) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.arc(xy .fst, xy .snd, r, 0, TAU), λ.ctx.stroke(), null)),

		/**` Output.strokeCircleV :: Number -> Vector2 -> IO () `*/
		strokeCircleV : (r : number) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), λ.ctx.stroke(), null)),

		/**` Output.fillCircle :: Number -> Number -> Number -> IO () `*/
		fillCircle : (r : number) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.arc(x, y, r, 0, TAU), λ.ctx.fill(), null)),

		/**` Output.fillCircleP :: Number -> Pair Number Number -> IO () `*/
		fillCircleP : (r : number) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.arc(xy .fst, xy .snd, r, 0, TAU), λ.ctx.fill(), null)),

		/**` Output.fillCircleV :: Number -> Vector2 -> IO () `*/
		fillCircleV : (r : number) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), λ.ctx.fill(), null)),

		/**` Output.elliptic :: ...7 Number -> IO () `*/
		elliptic :
			(r : number) => (a : number) => (b  : number) =>
			(x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
			IO (() => (λ.ctx.ellipse(x, y, kx, ky, r, a, b), null)),

		/**` Output.ellipticP :: Number -> Number -> Number -> Pair Number Number -> Pair Number Number -> IO () `*/
		ellipticP :
			(r  : number) => (a : number) => (b  : number) =>
			(xy : Pair <number, number>)  => (wh : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.ellipse(xy .fst, xy .snd, wh .fst, wh .snd, r, a, b), null)),

		/**` Output.ellipticVector :: Number -> Number -> Number -> Vector2 -> Vector2 -> IO () `*/
		ellipticV :
			(r  : number)  => (a  : number ) => (b : number) =>
			(xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => (λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, a, b), null)),

		/**` Output.ellipse :: ...5 Number -> IO () `*/
		ellipse : (r : number) => (x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
			IO (() => (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), null)),

		/**` Output.ellipseP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
		ellipseP : (r : number) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.ellipse(xy .fst, xy .snd, wh .fst, wh .snd, r, 0, TAU), null)),

		/**` Output.ellipseV :: Number -> Vector2 -> Vector2 -> IO () `*/
		ellipseV : (r : number) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => (λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU), null)),

		/**` Output.strokeEllipse :: ...5 Number -> IO () `*/
		strokeEllipse : (r : number) => (x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
			IO (() => (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), λ.ctx.stroke(), null)),

		/**` Output.strokeEllipseP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
		strokeEllipseP : (r : number) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => {
				λ.ctx.ellipse(xy .fst, xy .snd, wh .fst, wh .snd, r, 0, TAU)
				λ.ctx.stroke()
				return null
			}),

		/**` Output.strokeEllipseV :: Number -> Vector2 -> Vector2 -> IO () `*/
		strokeEllipseV : (r : number) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => {
				λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU)
				λ.ctx.stroke()
				return null
			}),

		/**` Output.fillEllipse :: ...5 Number -> IO () `*/
		fillEllipse : (r : number) => (x : number) => (y : number) => (kx : number) => (ky : number) : IO <null> =>
			IO (() => (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), λ.ctx.fill(), null)),

		/**` Output.fillEllipseP :: Number -> Pair Number Number -> Pair Number Number -> IO () `*/
		fillEllipseP : (r : number) => (xy : Pair <number, number>) => (wh : Pair <number, number>) : IO <null> =>
			IO (() => {
				λ.ctx.ellipse(xy .fst, xy .snd, wh .fst, wh .snd, r, 0, TAU)
				λ.ctx.fill()
				return null
			}),

		/**` Output.fillEllipseV :: Number -> Vector2 -> Vector2 -> IO () `*/
		fillEllipseV : (r : number) => (xy : Vector2) => (wh : Vector2) : IO <null> =>
			IO (() => {
				λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU)
				λ.ctx.fill()
				return null
			}),

		/**` Output.strokeText :: String -> Number -> Number -> IO () `*/
		strokeText : (text : string) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.strokeText(text, x, y), null)),

		/**` Output.strokeTextP :: String -> Pair Number Number -> IO () `*/
		strokeTextP : (text : string) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.strokeText(text, xy .fst, xy .snd), null)),

		/**` Output.strokeTextV :: String -> Vector2 -> IO () `*/
		strokeTextV : (text : string) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.strokeText(text, xy.x, xy.y), null)),

		/**` Output.fillText :: String -> Number -> Number -> IO () `*/
		fillText : (text : string) => (x : number) => (y : number) : IO <null> =>
			IO (() => (λ.ctx.fillText(text, x, y), null)),

		/**` Output.fillTextP :: String -> Pair Number Number -> IO () `*/
		fillTextP : (text : string) => (xy : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.fillText(text, xy .fst, xy .snd), null)),

		/**` Output.fillTextV :: String -> Vector2 -> IO () `*/
		fillTextV : (text : string) => (xy : Vector2) : IO <null> =>
			IO (() => (λ.ctx.fillText(text, xy.x, xy.y), null)),

		/**` Output.area :: Number -> Number -> Number -> Number -> IO () `*/
		area : (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
			IO (() => (λ.ctx.rect(x0, y0, x1 - x0, y1 - y0), null)),

		/**` Output.areaP :: Pair Number Number -> Pair Number Number -> IO () `*/
		areaP : (xy0 : Pair <number, number>) => (xy1 : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.rect(xy0 .fst, xy0 .snd, xy1 .fst - xy0 .fst, xy1 .snd - xy0 .snd), null)),

		/**` Output.areaV :: Vector2 -> Vector2 -> IO () `*/
		areaV : (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
			IO (() => (λ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null)),

		/**` Output.strokeArea :: Number -> Number -> Number -> Number -> IO () `*/
		strokeArea : (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
			IO (() => (λ.ctx.strokeRect(x0, y0, x1 - x0, y1 - y0), null)),

		/**` Output.strokeAreaP :: Pair Number Number -> Pair Number Number -> IO () `*/
		strokeAreaP : (xy0 : Pair <number, number>) => (xy1 : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.strokeRect(xy0 .fst, xy0 .snd, xy1 .fst - xy0 .fst, xy1 .snd - xy0 .snd), null)),

		/**` Output.strokeAreaV :: Vector2 -> Vector2 -> IO () `*/
		strokeAreaV : (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
			IO (() => (λ.ctx.strokeRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null)),

		/**` Output.fillArea :: Number -> Number -> Number -> Number -> IO () `*/
		fillArea : (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
			IO (() => (λ.ctx.fillRect(x0, y0, x1 - x0, y1 - y0), null)),

		/**` Output.fillAreaP :: Pair Number Number -> Pair Number Number -> IO () `*/
		fillAreaP : (xy0 : Pair <number, number>) => (xy1 : Pair <number, number>) : IO <null> =>
			IO (() => (λ.ctx.fillRect(xy0 .fst, xy0 .snd, xy1 .fst - xy0 .fst, xy1 .snd - xy0 .snd), null)),

		/**` Output.fillAreaV :: Vector2 -> Vector2 -> IO () `*/
		fillAreaV : (xy0 : Vector2) => (xy1 : Vector2) : IO <null> =>
			IO (() => (λ.ctx.fillRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))
	}

onload = () =>
{
	λ.ctxs = Array.from (document.querySelectorAll('canvas')) .map (x => x.getContext('2d')!)
	λ.ctx = λ.ctxs[0]!

	onmousemove = ev =>
	{
		λ.mouseWindowX = ev.x,
		λ.mouseWindowY = ev.y,
		λ.mouseCanvasX = ev.clientX - λ.ctx.canvas.offsetLeft,
		λ.mouseCanvasY = ev.clientY - λ.ctx.canvas.offsetTop,
		λ.mouseScreenX = ev.screenX,
		λ.mouseScreenY = ev.screenY,
		λ.mouseDeltaX  = ev.movementX,
		λ.mouseDeltaY  = ev.movementY
	}

	onmousedown = ev => λ.mouseButtons[ev.button] = Y.DD
	onmouseup   = ev => λ.mouseButtons[ev.button] = Y.UU
	onkeyup     = ev => λ.keyboard[ev.code as KeyboardKey] = Y.UU
	onkeydown   = ev =>
		λ.keyboard[ev.code as KeyboardKey] =
			ev.repeat
				? λ.keyboard[ev.code as KeyboardKey]
				: Y.DD

	onwheel = ev =>
		λ.mouseScroll =
			ev.deltaY < 0 ? Y.U   :
			ev.deltaY > 0 ? Y.D : Y.Rest

	onresize = () => (clearTimeout(λ.resizeID), λ.resizeID = setTimeout(() => λ.isResized = true, 250))

	document.onpointerlockchange = () =>
		λ.isPointerLocked = document.pointerLockElement === λ.ctx.canvas
}
