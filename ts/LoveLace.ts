/* eslint-disable @typescript-eslint/no-unused-vars */

/** Throws am error via a function call. */
const THROW = (message : string) =>
	{ throw new Error(message) }

/** Throws a type error via a function call. */
const THROWTYPE = (message : string) =>
	{ throw new TypeError(message) }

/** Throws a range error via a function call. */
const THROWRANGE = (message : string) =>
	{ throw new RangeError(message) }

/********************************************************************************************************************************/

/**` E :: Number `*/
const E : number = 2.718281828459045

/**` LN2 :: Number `*/
const LN2 : number = 0.6931471805599453

/**` LN10 :: Number `*/
const LN10 : number = 2.302585092994046

/**` LOG2E :: Number `*/
const LOG2E : number = 1.4426950408889634

/**` LOG10E :: Number `*/
const LOG10E : number = 0.4342944819032518

/**` PI :: Number `*/
const PI : number = 3.141592653589793

/**` TAU :: Number `*/
const TAU : number = 6.283185307179586

/**` INVSQRT2 :: Number `*/
const INVSQRT2 : number = 0.7071067811865476

/**` SQRT2 :: Number `*/
const SQRT2 : number = 1.4142135623730951

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

/**` asin :: Number -> Number `*/
const asin = Math.asin

/**` asinh :: Number -> Number `*/
const asinh = Math.asinh

/**` atan :: Number -> Number `*/
const atan = Math.atan

/**` atan2 :: Number -> Number -> Number `*/
const atan2 = (y : number) => (x : number) : number => Math.atan2(y, x)

/**` ratan2 :: Number -> Number -> Number `*/
const ratan2 = (x : number) => (y : number) : number => Math.atan2(y, x)

/**` atanh :: Number -> Number `*/
const atanh = Math.atanh

/**` cbrt :: Number -> Number `*/
const cbrt = Math.cbrt

/**` ceil :: Number -> Number `*/
const ceil = Math.ceil

/**` CLZ32 :: Number -> Number `*/
const CLZ32 = Math.clz32

/**` cos :: Number -> Number `*/
const cos = Math.cos

/**` cosh :: Number -> Number `*/
const cosh = Math.cosh

/**` div :: Number -> Number -> Number `*/
const div = (x : number) => (y : number) : number => x / y

/**` rdiv :: Number -> Number -> Number `*/
const rdiv = (y : number) => (x : number) : number => x / y

/**` eq :: a -> a -> Boolean `*/
const eq = <a>(x : a) => (y : a) : boolean => x === y

/**` exp :: Number -> Number `*/
const exp = Math.exp

/**` expm1 :: Number -> Number `*/
const expm1 = Math.expm1

/**` floor :: Number -> Number `*/
const floor = Math.floor

/**` fround :: Number -> Number `*/
const fround = Math.fround

/**` gt :: Number -> Number -> Boolean `*/
const gt = (x : number) => (y : number) : boolean => x > y

/**` gte :: Number -> Number -> Boolean `*/
const gte = (x : number) => (y : number) : boolean => x >= y

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

/**` lt :: Number -> Number -> Boolean `*/
const lt = (x : number) => (y : number) : boolean => x < y

/**` lte :: Number -> Number -> Boolean `*/
const lte = (x : number) => (y : number) : boolean => x <= y

/**` max :: Number -> Number -> Number `*/
const max = (x : number) => (y : number) : number => Math.max(x, y)

/**` min :: Number -> Number -> Number `*/
const min = (x : number) => (y : number) : number => Math.min(x, y)

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
const negate = (x : number) => -x

/**` neq :: a -> a -> Boolean `*/
const neq = <a>(x : a) => (y : a) : boolean => x !== y

/**` NOR :: Number -> Number -> Number `*/
const NOR = (x : number) => (y : number) : number => ~(x | y)

/**` NOT :: Number -> Number `*/
const NOT = (x : number) : number => ~x

/**` not :: Boolean -> Boolean `*/
const not = (x : boolean) : boolean => !x

/**` OR :: Number -> Number -> Number `*/
const OR = (x : number) => (y : number) : number => x | y

/**` or :: Boolean -> Boolean -> Boolean `*/
const or = (x : boolean) => (y : boolean) : boolean => x || y

/**` nor :: Boolean -> Boolean -> Boolean `*/
const nor = (x : boolean) => (y : boolean) : boolean => !(x || y)

/**` pow :: Number -> Number -> Number `*/
const pow = (x : number) => (y : number) : number => x ** y

/**` rpow :: Number -> Number -> Number `*/
const rpow = (y : number) => (x : number) : number => x ** y

/**` pythagoras :: Number -> Number -> Number `*/
const pythagoras = (x : number) => (y : number) : number => Math.sqrt(x * x + y * y)

/**` round :: Number -> Number `*/
const round = Math.round

/**` RSHIFT :: Number -> Number `*/
const RSHIFT = (x : number) => (y : number) : number => x >> y

/**` rRSHIFT :: Number -> Number `*/
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

/**` trunc :: Number -> Number `*/
const trunc = Math.trunc

/**` URSHIFT :: Number -> Number `*/
const URSHIFT = (x : number) => (y : number) : number => x >>> y

/**` rURSHIFT :: Number -> Number `*/
const rURSHIFT = (y : number) => (x : number) : number => x >>> y

/**` XOR :: Number -> Number -> Number `*/
const XOR = (x : number) => (y : number) : number => x ^ y

/**` xor :: Boolean -> Boolean -> Boolean `*/
const xor = (x : boolean) => (y : boolean) : boolean => x !== y

/********************************************************************************************************************************/

/** The `IO` monad. */
type IO<a> =
	{
		readonly CONS : 'IO'
		readonly INFO : () => a

		/**` (IO).bind :: IO a -> (a -> IO b) -> IO b `*/
		readonly bind : <b>(reaction : (evaluation : a) => IO<b>) => IO<b>

		/**` (IO).bindto :: IO $ -> String -> ($ -> IO a) -> IO $ `*/
		readonly bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => IO<b>) => IO<a & { [x in k] : b }>

		/**` (IO).fmap :: IO a -> (a -> b) -> IO b `*/
		readonly fmap : <b>(computation : (evaluation : a) => b) => IO<b>

		/**` (IO).fmapto :: IO $ -> String -> ($ -> a) -> IO $ `*/
		readonly fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => IO<a & { [x in k] : b }>

		/**` (IO).then :: IO a -> IO b -> IO b `*/
		readonly then : <b>(successor : IO<b>) => IO<b>
	}

/** The `Maybe` monad.
 * ```
 * data (Monad) Maybe a = Nothing | Just a
 * ```
 */
type Maybe<a> =
	({
		readonly CONS   : 'Nothing'
	} | {
		readonly CONS   : 'Just'
		readonly INFO   : a
	}) & {

		/**` (Maybe).bind :: Maybe a -> (a -> Maybe b) -> Maybe b `*/
		readonly bind : <b>(reaction : (evaluation : a) => Maybe<b>) => Maybe<b>

		/**` (Maybe).bindto :: Maybe $ -> String -> ($ -> Maybe a) -> Maybe $ `*/
		readonly bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => Maybe<b>) => Maybe<a & { [x in k] : b }>

		/**` (Maybe).fmap :: Maybe a -> (a -> b) -> Maybe b `*/
		readonly fmap : <b>(computation : (evaluation : a) => b) => Maybe<b>

		/**` (Maybe).fmapto :: Maybe $ -> String -> ($ -> a) -> Maybe $ `*/
		readonly fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => Maybe<a & { [x in k] : b }>
	}

/** The `State` monad.
 * ```
 * data (Monad) State s a = State (s -> (s, a))
 * ```
 */
type State<s, a> =
	{
		readonly CONS : 'State'
		readonly INFO : (inputState : s) => [s, a]

		/**` (State).bind :: State s a -> (a -> State s b) -> State s b `*/
		readonly bind : <b>(reaction : (statefulComputationOutput : a) => State<s, b>) => State<s, b>

		/**` (State).fmap :: State s a -> (a -> b) -> State s b `*/
		readonly fmap : <b>(computation : (statefulComputationOutput : a) => b) => State<s, b>

		/**` (State).bindto :: State s $ -> String -> ($ -> State s a) -> State s $ `*/
		readonly bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => State<s, b>) => State<s, a & { [x in k] : b }>

		/**` (State).fmapto :: State s $ -> String -> ($ -> b) -> State s $ `*/
		readonly fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => State<s, a & { [x in k] : b }>

		/**` (State).then :: State s a -> State s b -> State s b `*/
		readonly then : <b>(successor : State<s, b>) => State<s, b>

		/**` (State).runState :: State s a -> s -> (s, a) `*/
		readonly runState : (initialState : s) => [s, a]

		/**` (State).evalState :: State s a -> s -> s `*/
		readonly evalState : (initialState : s) => s

		/**` (State).execState :: State s a -> s -> a `*/
		readonly execState : (initialState : s) => a
	}

/** The `List` monad.
 * ```
 * data (Monad) List a = List (...a)
 * ```
 */
type List<a> =
	{
		readonly CONS : 'List'
		readonly INFO : ReadonlyArray<a>

		/**` (List).all :: List a -> (a -> Boolean) -> Boolean `*/
		readonly all : (predicate : (element : a) => boolean) => boolean

		/**` (List).any :: List a -> (a -> Boolean) -> Boolean `*/
		readonly any : (predicate : (element : a) => boolean) => boolean

		/**` (List).append :: List a -> a -> List a `*/
		readonly append : (element : a) => List<a>

		/**` (List).at :: List a -> Number -> a `*/
		readonly at : (index : number) => a

		/**` (List).bind :: List a -> (a -> List b) -> List b `*/
		readonly bind : <b>(reaction : (element : a) => List<b>) => List<b>

		/**` (List).bindto :: List $ -> String -> ($ -> List b) -> List $ `*/
		readonly bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => List<b>) => List<a & { [x in k] : b }>

		/**` (List).break :: List a -> (a -> Boolean) -> (List a, List a) `*/
		readonly break : (predicate : (element : a) => boolean) => [List<a>, List<a>]

		/**` (List).concat :: List a -> List a -> List a `*/
		readonly concat : (succeeding : List<a>) => List<a>

		/**` (List).drop :: List a -> Number -> List a `*/
		readonly drop : (count : number) => List<a>

		/**` (List).dropWhile :: List a -> (a -> Boolean) -> List a `*/
		readonly dropWhile : (predicate : (element : a) => boolean) => List<a>

		/**` (List).elem :: List a -> a -> Boolean `*/
		readonly elem : (match : a) => boolean

		/**` (List).elemIndex :: List a -> a -> Maybe Number `*/
		readonly elemIndex : (match : a) => Maybe<number>

		/**` (List).elemIndices :: List a -> a -> List Number `*/
		readonly elemIndices : (match : a) => List<number>

		/**` (List).filter :: List a -> (a -> Boolean) -> List a `*/
		readonly filter : (predicate : (element : a) => boolean) => List<a>

		/**` (List).find :: List a -> (a -> Boolean) -> Maybe a `*/
		readonly find : (predicate : (element : a) => boolean) => Maybe<a>

		/**` (List).findIndex :: List a -> (a -> Boolean) -> Maybe Number `*/
		readonly findIndex : (predicate : (element : a) => boolean) => Maybe<number>

		/**` (List).findIndices :: List a -> (a -> Boolean) -> List Number `*/
		readonly findIndices : (predicate : (element : a) => boolean) => List<number>

		/**` (List).fmap :: List a -> (a -> b) -> List b `*/
		readonly fmap : <b>(computation : (element : a) => b) => List<b>

		/**` (List).fmapto :: List $ -> String -> ($ -> b) -> List $ `*/
		readonly fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => List<a & { [x in k] : b }>

		/**` (List).foldl :: List a -> (b -> a -> b) -> b -> b `*/
		readonly foldl : <b>(builder : (first : b) => (second : a) => b) => (initialValue : b) => b

		/**` (List).foldr :: List a -> (a -> b -> b) -> b -> b `*/
		readonly foldr : <b>(builder : (second : a) => (first : b) => b) => (initialValue : b) => b

		/**` (List).foldl1 :: List a -> (a -> a -> a) -> a `*/
		readonly foldl1 : (builder : (first : a) => (second : a) => a) => a

		/**` (List).foldr1 :: List a -> (a -> a -> a) -> a `*/
		readonly foldr1 : (builder : (second : a) => (first : a) => a) => a

		/**` (List).head :: List a -> a `*/
		readonly head : a

		/**` (List).init :: List a -> List a `*/
		readonly init : List<a>

		/**` (List).inits :: List a -> List (List a) `*/
		readonly inits : List<List<a>>

		/**` (List).intersperse :: List a -> a -> List a `*/
		readonly intersperse : (delimiter : a) => List<a>

		/**` (List).last :: List a -> a `*/
		readonly last : a

		/**` (List).notElem :: List a -> a -> Boolean `*/
		readonly notElem : (delimiter : a) => boolean

		/**` (List).prepend :: List a -> a -> List a `*/
		readonly prepend : (element : a) => List<a>

		/**` (List).reverse :: List a -> List a `*/
		readonly reverse : List<a>

		/**` (List).scanl :: List a -> (b -> a -> b) -> b -> List b `*/
		readonly scanl : <b>(builder : (first : b) => (second : a) => b) => (initialValue : b) => List<b>

		/**` (List).scanr :: List a -> (a -> b -> b) -> b -> List b `*/
		readonly scanr : <b>(builder : (second : a) => (first : b) => b) => (initialValue : b) => List<b>

		/**` (List).scanl1 :: List a -> (a -> a -> a) -> List a `*/
		readonly scanl1 : (builder : (first : a) => (second : a) => a) => List<a>

		/**` (List).scanr1 :: List a -> (a -> a -> a) -> List a `*/
		readonly scanr1 : (builder : (second : a) => (first : a) => a) => List<a>

		/**` (List).span :: List a -> (a -> Boolean) -> (List a, List a) `*/
		readonly span : (predicate : (element : a) => boolean) => [List<a>, List<a>]

		/**` (List).splitAt :: List a -> Number -> (List a, List a) `*/
		readonly splitAt : (index : number) => [List<a>, List<a>]

		/**` (List).tail :: List a -> List a `*/
		readonly tail : List<a>

		/**` (List).tails :: List a -> List (List a) `*/
		readonly tails : List<List<a>>

		/**` (List).take :: List a -> Number -> List a `*/
		readonly take : (count : number) => List<a>

		/**` (List).takeWhile :: List a -> (a -> Boolean) -> List a `*/
		readonly takeWhile : (predicate : (element : a) => boolean) => List<a>

		/**` (List).zip :: List a -> List b -> List (a, b) `*/
		readonly zip : <b>(postfixes : List<b>) => List<[a, b]>

		/**` (List).zipWith :: List a -> List b -> (a -> b -> c) -> List c `*/
		readonly zipWith : <b>(postfixes : List<b>) => <c>(builder : (firstElement : a) => (secondElement : b) => c) => List<c>
	}

/** `Vector2D` data-type used for two-dimensional linear algebra.
 * ```
 * data Vector2D = Vector2D Number Number
 * ```
 */
type Vector2D =
	{
		readonly CONS : 'Vector2D'

		/**` (Vector2D).x :: Vector2D -> Number `*/
		readonly x : number

		/**` (Vector2D).y :: Vector2D -> Number `*/
		readonly y : number
	}

/** `Vector3D` data-type used for three-dimensional linear algebra.
 * ```
 * data Vector3D = Vector3D Number Number Number
 * ```
 */
type Vector3D =
	{
		readonly CONS : 'Vector3D'

		/**` (Vector3D).x :: Vector3D -> Number `*/
		readonly x : number

		/**` (Vector3D).y :: Vector3D -> Number `*/
		readonly y : number

		/**` (Vector3D).z :: Vector3D -> Number `*/
		readonly z : number
	}

/** `Vector4D` data-type used for four-dimensional linear algebra.
 * ```
 * data Vector4D = Vector4D Number Number Number Number
 * ```
 */
type Vector4D =
	{
		readonly CONS : 'Vector4D'

		/**` (Vector4D).x :: Vector4D -> Number `*/
		readonly x : number

		/**` (Vector4D).y :: Vector4D -> Number `*/
		readonly y : number

		/**` (Vector4D).z :: Vector4D -> Number `*/
		readonly z : number

		/**` (Vector4D).w :: Vector4D -> Number `*/
		readonly w : number
	}

/** Data type for the representation of 2x2 matrices.
 * ```
 * data Matrix2x2 = Matrix2x2 (...4 Number)
 * ```
 */
type Matrix2x2 =
	{
		readonly CONS : 'Matrix2x2'

		/**` (Matrix2x2).ix :: Matrix2x2 -> Number `*/
		readonly ix : number

		/**` (Matrix2x2).jx :: Matrix2x2 -> Number `*/
		readonly jx : number

		/**` (Matrix2x2).iy :: Matrix2x2 -> Number `*/
		readonly iy : number

		/**` (Matrix2x2).jy :: Matrix2x2 -> Number `*/
		readonly jy : number

		/**` (Matrix2x2).i :: Matrix2x2 -> Vector2D `*/
		readonly i : Vector2D

		/**` (Matrix2x2).j :: Matrix2x2 -> Vector2D `*/
		readonly j : Vector2D

		/**` (Matrix2x2).x :: Matrix2x2 -> Vector2D `*/
		readonly x : Vector2D

		/**` (Matrix2x2).y :: Matrix2x2 -> Vector2D `*/
		readonly y : Vector2D
	}

/** Data type for the representation of 3x3 matrices.
 * ```
 * data Matrix3x3 = Matrix3x3 (...9 Number)
 * ```
 */
type Matrix3x3 =
	{
		readonly CONS : 'Matrix3x3'

		/**` (Matrix3x3).ix :: Matrix3x3 -> Number `*/
		readonly ix : number

		/**` (Matrix3x3).jx :: Matrix3x3 -> Number `*/
		readonly jx : number

		/**` (Matrix3x3).kx :: Matrix3x3 -> Number `*/
		readonly kx : number

		/**` (Matrix3x3).iy :: Matrix3x3 -> Number `*/
		readonly iy : number

		/**` (Matrix3x3).jy :: Matrix3x3 -> Number `*/
		readonly jy : number

		/**` (Matrix3x3).ky :: Matrix3x3 -> Number `*/
		readonly ky : number

		/**` (Matrix3x3).iz :: Matrix3x3 -> Number `*/
		readonly iz : number

		/**` (Matrix3x3).jz :: Matrix3x3 -> Number `*/
		readonly jz : number

		/**` (Matrix3x3).kz :: Matrix3x3 -> Number `*/
		readonly kz : number

		/**` (Matrix3x3).i :: Matrix3x3 -> Vector3D `*/
		readonly i : Vector3D

		/**` (Matrix3x3).j :: Matrix3x3 -> Vector3D `*/
		readonly j : Vector3D

		/**` (Matrix3x3).k :: Matrix3x3 -> Vector3D `*/
		readonly k : Vector3D

		/**` (Matrix3x3).x :: Matrix3x3 -> Vector3D `*/
		readonly x : Vector3D

		/**` (Matrix3x3).y :: Matrix3x3 -> Vector3D `*/
		readonly y : Vector3D

		/**` (Matrix3x3).z :: Matrix3x3 -> Vector3D `*/
		readonly z : Vector3D
	}

/** Data type for the representation of 4x4 matrices.
 * ```
 * data Matrix4x4 = Matrix4x4 (...16 Number)
 * ```
 */
type Matrix4x4 =
	{
		readonly CONS : 'Matrix4x4'

		/**` (Matrix4x4).ix :: Matrix4x4 -> Number `*/
		readonly ix : number

		/**` (Matrix4x4).jx :: Matrix4x4 -> Number `*/
		readonly jx : number

		/**` (Matrix4x4).kx :: Matrix4x4 -> Number `*/
		readonly kx : number

		/**` (Matrix4x4).lx :: Matrix4x4 -> Number `*/
		readonly lx : number

		/**` (Matrix4x4).iy :: Matrix4x4 -> Number `*/
		readonly iy : number

		/**` (Matrix4x4).jy :: Matrix4x4 -> Number `*/
		readonly jy : number

		/**` (Matrix4x4).ky :: Matrix4x4 -> Number `*/
		readonly ky : number

		/**` (Matrix4x4).ly :: Matrix4x4 -> Number `*/
		readonly ly : number

		/**` (Matrix4x4).iz :: Matrix4x4 -> Number `*/
		readonly iz : number

		/**` (Matrix4x4).jz :: Matrix4x4 -> Number `*/
		readonly jz : number

		/**` (Matrix4x4).kz :: Matrix4x4 -> Number `*/
		readonly kz : number

		/**` (Matrix4x4).lz :: Matrix4x4 -> Number `*/
		readonly lz : number

		/**` (Matrix4x4).iw :: Matrix4x4 -> Number `*/
		readonly iw : number

		/**` (Matrix4x4).jw :: Matrix4x4 -> Number `*/
		readonly jw : number

		/**` (Matrix4x4).kw :: Matrix4x4 -> Number `*/
		readonly kw : number

		/**` (Matrix4x4).lw :: Matrix4x4 -> Number `*/
		readonly lw : number

		/**` (Matrix4x4).i :: Matrix4x4 -> Vector4D `*/
		readonly i : Vector4D

		/**` (Matrix4x4).j :: Matrix4x4 -> Vector4D `*/
		readonly j : Vector4D

		/**` (Matrix4x4).k :: Matrix4x4 -> Vector4D `*/
		readonly k : Vector4D

		/**` (Matrix4x4).l :: Matrix4x4 -> Vector4D `*/
		readonly l : Vector4D

		/**` (Matrix4x4).x :: Matrix4x4 -> Vector4D `*/
		readonly x : Vector4D

		/**` (Matrix4x4).y :: Matrix4x4 -> Vector4D `*/
		readonly y : Vector4D

		/**` (Matrix4x4).z :: Matrix4x4 -> Vector4D `*/
		readonly z : Vector4D

		/**` (Matrix4x4).w :: Matrix4x4 -> Vector4D `*/
		readonly w : Vector4D
	}

/** A simple data type that stores information about the measurement of texts in rendering.
 * ```
 * data TextMeasurement = TextMeasurement String Number Number
 * ```
 */
type TextMeasurement =
	{
		readonly CONS : 'TextMeasurement'

		/**` (TextMeasurement).text :: TextMeasurement -> String `*/
		readonly text : string

		/**` (TextMeasurement).width :: TextMeasurement -> Number `*/
		readonly width : number

		/**` (TextMeasurement).height :: TextMeasurement -> Number `*/
		readonly height : number
	}

/** An inline version of the `switch` statement.
 * ```
 * data Switch a b = Switch.case a (() -> b)
 * ```
 */
type Switch<a, b> =
	{
		readonly CONS : 'Switch'

		/**` (.case) :: Switch a b -> a -> (() -> b) -> Switch a b `*/
		readonly case : (domain : a) => (codomain : () => b) => Switch<a, b>

		/**` (.else) :: Switch a b -> (() -> b) -> Switch a b `*/
		readonly else : (codomain : () => b) => Switch<a, b>

		/**` (.with) :: Switch a b -> a -> b `*/
		readonly with : (value : a) => b
	}

/** A data structure that maps values to other values supplied with an inverse.
 * ```
 * data Bijection a b = Bijection.of a b
 * ```
 */
type Bijection<a, b> =
	{
		readonly CONS : 'Bijection'
		readonly INFO : ReadonlyArray<[a, b]>

		/**` (.of) :: Bijection a b -> a -> b -> Bijection a b `*/
		readonly of : (domainValue : a) => (codomainValue : b) => Bijection<a, b>

		/**` (Bijection).domain :: Bijection a b -> a -> b `*/
		readonly domain : (domainValue   : a) => b

		/**` (Bijection).codomain :: Bijection a b -> b -> a `*/
		readonly codomain : (codomainValue : b) => a
	}

/** Clock data type that stores time information.
 * ```
 * data Clock = Clock Number Number Number
 * ```
 */
type Clock =
	{
		readonly CONS : 'Clock'

		/**` (Clock).time :: Clock -> Number `*/
		readonly time : number

		/**` (Clock).delta :: Clock -> Number `*/
		readonly delta : number

		/**` (Clock).counter :: Clock -> Number `*/
		readonly counter : number
	}

/********************************************************************************************************************************/

// -- Use only for creating new IO operations; otherwise, compose existing IO monads together.
const IO = <a>(sideeffect : () => a) : IO<a> =>
	({
		CONS : 'IO',
		INFO : sideeffect,
		bind : f => IO(() => f(sideeffect()).INFO()),
		bindto : x => f =>
			IO(() => {
				const $ = sideeffect()
				return { ...$, [x]: f($).INFO() } as any
			}),
		fmap : f => IO(() => f(sideeffect())),
		fmapto : x => f =>
			IO(() => {
				const $ = sideeffect()
				return { ...$, [x]: f($) } as any
			}),
		then : x => IO(() => (sideeffect(), x.INFO()))
	})

{
	/**` (IO).bind :: IO a -> (a -> IO b) -> IO b `*/
	IO.bind = <a>(io : IO<a>) => <b>(reaction : (evaluation : a) => IO<b>) : IO<b> =>
		io.bind(reaction)

	/**` (IO).bindto :: IO $ -> String -> ($ -> IO a) -> IO $ `*/
	IO.bindto =
		<a>(io : IO<a>) =>
		<k extends string>(name : k) =>
		<b>(reaction : ($ : a) => IO<b>) : IO<a & { [x in k] : b }> =>
		io.bindto(name)(reaction)

	/**` (IO).fmap :: IO a -> (a -> b) -> IO b `*/
	IO.fmap = <a>(io : IO<a>) => <b>(computation : (evaluation : a) => b) : IO<b> =>
		io.fmap(computation)

	/**` (IO).fmapto :: IO $ -> String -> ($ -> a) -> IO $ `*/
	IO.fmapto =
		<a>(io : IO<a>) =>
		<k extends string>(name : k) =>
		<b>(computation : ($ : a) => b) : IO<a & { [x in k] : b }> =>
		io.fmapto(name)(computation)

	/**` (IO).then :: IO a -> IO b -> IO b `*/
	IO.then = <a>(io : IO<a>) => <b>(successor : IO<b>) : IO<b> =>
		io.then(successor)
}

/**` Nothing :: Maybe a `*/
const Nothing : Maybe<any> =
	{
		CONS : 'Nothing',
		bind : _ => Nothing,
		bindto : _ => _ => Nothing,
		fmap : _ => Nothing,
		fmapto : _ => _ => Nothing
	}

/**` Just :: a -> Maybe a `*/
const Just = <a>(value : a) : Maybe<a> =>
	({
		CONS : 'Just',
		INFO : value,
		bind : f =>
		{
			const x = f(value)
			return x.CONS === 'Nothing' ? Nothing : x
		},
		bindto : x => f =>
		{
			const y = f(value)
			return y.CONS === 'Nothing' ? Nothing : Just({ ...value, [x]: y.INFO }) as any
		},
		fmap : f => Just(f(value)),
		fmapto : x => f => Just({ ...value, [x]: f(value) }) as any
	})

const Maybe =
{
	/**` (Maybe).bind :: Maybe a -> (a -> Maybe b) -> Maybe b `*/
	bind : <a>(maybe : Maybe<a>) => <b>(reaction : (evaluation : a) => Maybe<b>) : Maybe<b> =>
		maybe.bind(reaction),

	/**` (Maybe).bindto :: Maybe $ -> String -> ($ -> Maybe a) -> Maybe $ `*/
	bindto:
		<a>(maybe : Maybe<a>) =>
		<k extends string>(name : k) =>
		<b>(reaction : ($ : a) => Maybe<b>) : Maybe<a & { [x in k] : b }> =>
		maybe.bindto(name)(reaction),

	/**` (Maybe).fmap :: Maybe a -> (a -> b) -> Maybe b `*/
	fmap : <a>(maybe : Maybe<a>) => <b>(computation : (evaluation : a) => b) : Maybe<b> =>
		maybe.fmap(computation),

	/**` (Maybe).fmapto :: Maybe $ -> String -> ($ -> a) -> Maybe $ `*/
	fmapto:
		<a>(maybe : Maybe<a>) =>
		<k extends string>(name : k) =>
		<b>(computation : ($ : a) => b) : Maybe<a & { [x in k] : b }> =>
		maybe.fmapto(name)(computation)
}

/**` State :: (s -> (s, a)) -> State s a `*/
const State = <s, a>(statefulComputation : (inputState : s) => [s, a]) : State<s, a> =>
	({
		CONS      : 'State',
		INFO      : statefulComputation,
		bind      : f =>
			State(x => {
				const [y, z] = statefulComputation(x)
				return f(z).INFO(y)
			}),
		fmap      : f =>
			State(x => {
				const [y, z] = statefulComputation(x)
				return [y, f(z)]
			}),
		bindto    : k => f =>
			State(x => {
				const [y, $] = statefulComputation(x)
				const [z, w] = f($).INFO(y)
				return [z, { ...$, [k]: w }] as any
			}),
		fmapto    : k => f =>
			State(x => {
				const [y, $] = statefulComputation(x)
				return [y, { ...$, [k]: f($) }] as any
			}),
		then      : s => State(x => s.INFO(statefulComputation(x)[0])),
		runState  : s => statefulComputation(s),
		evalState : s => statefulComputation(s)[0],
		execState : s => statefulComputation(s)[1]
	})

{
	/**` (State).bind :: State s a -> (a -> State s b) -> State s b `*/
	State.bind = <s, a>(state : State<s, a>) => <b>(reaction : (evaluation : a) => State<s, b>) : State<s, b> =>
		state.bind(reaction)

	/**` (State).bindto :: State s $ -> String -> ($ -> State s a) -> State s $ `*/
	State.bindto =
		<s, a>(state : State<s, a>) =>
		<k extends string>(name : k) =>
		<b>(reaction : ($ : a) => State<s, b>) : State<s, a & { [x in k] : b }> =>
		state.bindto(name)(reaction)

	/**` (State).fmap :: State s a -> (a -> b) -> State s b `*/
	State.fmap = <s, a>(state : State<s, a>) => <b>(computation : (evaluation : a) => b) : State<s, b> =>
		state.fmap(computation)

	/**` (State).fmapto :: State s $ -> String -> ($ -> a) -> State s $ `*/
	State.fmapto =
		<s, a>(state : State<s, a>) =>
		<k extends string>(name : k) =>
		<b>(computation : ($ : a) => b) : State<s, a & { [x in k] : b }> =>
		state.fmapto(name)(computation)

	/**` (State).then :: State s a -> State s b -> State s b `*/
	State.then = <s, a>(state : State<s, a>) => <b>(successor : State<s, b>) : State<s, b> =>
		state.then(successor)
}

/**` List :: [a] -> List a `*/
const List = <a>(...elements : ReadonlyArray<a>) : List<a> =>
	({
		CONS : 'List',
		INFO : elements,
		all : f => elements.every(x => f(x)),
		any : f => elements.some(x => f(x)),
		append : x => List(...elements, x),
		at : i =>
			i < elements.length && i >= 0
				? elements[i] as a
				: THROWRANGE(`Cannot retrive element at index '${i}' in 'List' of length '${elements.length}'`),
		bind : f => List(...elements.flatMap(x => f(x).INFO)),
		bindto : k => f => List(...elements.flatMap($ => f($).INFO.map(x => ({ ...$, [k]: x })))) as any,
		break : f =>
		{
			const i = elements.findIndex(x => f(x))
			return ~i
				? [List(...elements.slice(0, i)), List(...elements.slice(i))]
				: [List(...elements), List()]
		},
		concat : xs => List(...elements, ...xs.INFO),
		drop : x => List(...elements.slice(Math.max(0, x))),
		dropWhile : f =>
		{
			const i = elements.findIndex(x => !f(x))
			return List(...(~i ? elements.slice(i) : []))
		},
		elem : x => elements.includes(x),
		elemIndex : x =>
		{
			const i = elements.indexOf(x)
			return ~i ? Just(i) : Nothing
		},
		elemIndices : x =>
		{
			const is : Array<number> = []
			elements.forEach((y, i) => {
				if (x === y) is.push(i)
			})
			return List(...is)
		},
		filter : f => List(...elements.filter(x => f(x))),
		find : f =>
		{
			const x = elements.find(y => f(y))
			return x === undefined ? Nothing : Just(x)
		},
		findIndex : f =>
		{
			const i = elements.findIndex(y => f(y))
			return ~i ? Just(i) : Nothing
		},
		findIndices : f =>
		{
			const is : Array<number> = []
			elements.forEach((y, i) => {
				if (f(y)) is.push(i)
			})
			return List(...is)
		},
		fmap : f => List(...elements.map(x => f(x))),
		fmapto : k => f => List(...elements.map($ => ({ ...$, [k]: f($) }))) as any,
		foldl : f => x => elements.reduce((y, z) => f(y)(z), x),
		foldr : f => x => elements.reduceRight((y, z) => f(z)(y), x),
		foldl1 : f =>
			elements.length
				? elements.reduce((y, z) => f(y)(z))
				: THROWRANGE(`Cannot 'foldl1' on an empty 'List'`),
		foldr1 : f =>
			elements.length
				? elements.reduceRight((y, z) => f(z)(y))
				: THROWRANGE(`Cannot 'foldr1' on an empty 'List'`),
		get head()
		{
			return elements.length
				? elements[0] as a
				: THROWRANGE(`Cannot get 'head' of an empty 'List'`)
		},
		get init()
		{
			return List(...elements.slice(0, -1))
		},
		get inits()
		{
			return List(...Array(elements.length + 1).fill(null).map((_, i) => List(...elements.slice(0, i))))
		},
		intersperse : x =>
			List(...Array(Math.max(0, elements.length * 2 - 1)).fill(null).map((_, i) => i % 2 ? x : elements[i / 2] as a)),
		get last()
		{
			return elements.length
				? elements[elements.length - 1] as a
				: THROWRANGE(`Cannot get 'last' of an empty 'List'`)
		},
		notElem : x => !elements.includes(x),
		prepend : x => List(x, ...elements),
		get reverse()
		{
			return List(...elements.slice().reverse())
		},
		scanl : f => x => List(...elements.reduce((y, z) => y.concat(f(y[y.length - 1]!)(z)), [x])),
		scanr : f => x => List(...elements.reduceRight((y, z) => [f(z)(y[0]!)].concat(y), [x])),
		scanl1 : f =>
			List(...elements.slice(1).reduce((x, y) => x.concat(f(x[x.length - 1]!)(y)), [elements[0] as a])),
		scanr1 : f =>
			List(...elements.slice(0, -1).reduceRight((x, y) => [f(y)(x[0]!)].concat(x), [elements[elements.length - 1] as a])),
		span : f =>
		{
			const i = elements.findIndex(x => !f(x))
			return ~i
				? [List(...elements.slice(0, i)), List(...elements.slice(i))]
				: [List(), List(...elements)]
		},
		splitAt : i => [List(...elements.slice(0, Math.max(0, i))), List(...elements.slice(Math.max(0, i)))],
		get tail()
		{
			return elements.length
				? List(...elements.slice(1))
				: THROWRANGE(`Cannot get 'tail' of an empty 'List'`)
		},
		get tails()
		{
			return List(...Array(elements.length + 1).fill(null).map((_, i) => List(...elements.slice(i))))
		},
		take : x => List(...elements.slice(0, Math.max(0, x))),
		takeWhile : f =>
		{
			const i = elements.findIndex(x => !f(x))
			return ~i
				? List(...elements.slice(0, i))
				: List(...elements)
		},
		zip : xs =>
			List(...Array(Math.min(elements.length, xs.INFO.length)).fill(null).map((_, i) => [elements[i], xs.INFO[i]] as any)),
		zipWith : xs => f =>
			List(...Array(Math.min(elements.length, xs.INFO.length)).fill(null).map((_, i) => f(elements[i]!)(xs.INFO[i]!)))
	})

{
	/**` (List).all :: List a -> (a -> Boolean) -> Boolean `*/
	List.all = <a>(list : List<a>) => (predicate : (element : a) => boolean) : boolean =>
		list.all(predicate)

	/**` (List).any :: List a -> (a -> Boolean) -> Boolean `*/
	List.any = <a>(list : List<a>) => (predicate : (element : a) => boolean) : boolean =>
		list.any(predicate)

	/**` (List).append :: List a -> a -> List a `*/
	List.append = <a>(list : List<a>) => (element : a) : List<a> =>
		list.append(element)

	/**` (List).at :: List a -> Number -> a `*/
	List.at = <a>(list : List<a>) => (index : number) : a =>
		list.at(index)

	/**` (List).bind :: List a -> (a -> List b) -> List b `*/
	List.bind = <a>(list : List<a>) => <b>(reaction : (element : a) => List<b>) : List<b> =>
		list.bind(reaction)

	/**` (List).bindto :: List $ -> String -> ($ -> List b) -> List $ `*/
	List.bindto =
		<a>(list : List<a>) =>
		<k extends string>(name : k) =>
		<b>(reaction : ($ : a) => List<b>) : List<a & { [x in k] : b }> =>
		list.bindto(name)(reaction)

	/**` (List).break :: List a -> (a -> Boolean) -> (List a, List a) `*/
	List.break = <a>(list : List<a>) => (predicate : (element : a) => boolean) : [List<a>, List<a>] =>
		list.break(predicate)

	/**` (List).concat :: List a -> List a -> List a `*/
	List.concat = <a>(list : List<a>) => (succeeding : List<a>) : List<a> =>
		list.concat(succeeding)

	/**` (List).drop :: List a -> Number -> List a `*/
	List.drop = <a>(list : List<a>) => (count : number) : List<a> =>
		list.drop(count)

	/**` (List).dropWhile :: List a -> (a -> Boolean) -> List a `*/
	List.dropWhile = <a>(list : List<a>) => (predicate : (element : a) => boolean) : List<a> =>
		list.dropWhile(predicate)

	/**` (List).elem :: List a -> a -> Boolean `*/
	List.elem = <a>(list : List<a>) => (match : a) : boolean =>
		list.elem(match)

	/**` (List).elemIndex :: List a -> a -> Maybe Number `*/
	List.elemIndex = <a>(list : List<a>) => (match : a) : Maybe<number> =>
		list.elemIndex(match)

	/**` (List).elemIndices :: List a -> a -> List Number `*/
	List.elemIndices = <a>(list : List<a>) => (match : a) : List<number> =>
		list.elemIndices(match)

	/**` (List).filter :: List a -> (a -> Boolean) -> List a `*/
	List.filter = <a>(list : List<a>) => (predicate : (element : a) => boolean) : List<a> =>
		list.filter(predicate)

	/**` (List).find :: List a -> (a -> Boolean) -> Maybe a `*/
	List.find = <a>(list : List<a>) => (predicate : (element : a) => boolean) : Maybe<a> =>
		list.find(predicate)

	/**` (List).findIndex :: List a -> (a -> Boolean) -> Maybe Number `*/
	List.findIndex = <a>(list : List<a>) => (predicate : (element : a) => boolean) : Maybe<number> =>
		list.findIndex(predicate)

	/**` (List).findIndices :: List a -> (a -> Boolean) -> List Number `*/
	List.findIndices = <a>(list : List<a>) => (predicate : (element : a) => boolean) : List<number> =>
		list.findIndices(predicate)

	/**` (List).fmap :: List a -> (a -> b) -> List b `*/
	List.fmap = <a>(list : List<a>) => <b>(computation : (element : a) => b) : List<b> =>
		list.fmap(computation)

	/**` (List).fmapto :: List $ -> String -> ($ -> b) -> List $ `*/
	List.fmapto =
		<a>(list : List<a>) =>
		<k extends string>(name : k) =>
		<b>(computation : ($ : a) => b) : List<a & { [x in k] : b }> =>
		list.fmapto(name)(computation)

	/**` (List).foldl :: List a -> (b -> a -> b) -> b -> b `*/
	List.foldl = <a>(list : List<a>) => <b>(builder : (first : b) => (second : a) => b) => (initialValue : b) : b =>
		list.foldl(builder)(initialValue)

	/**` (List).foldr :: List a -> (a -> b -> b) -> b -> b `*/
	List.foldr = <a>(list : List<a>) => <b>(builder : (second : a) => (first : b) => b) => (initialValue : b) : b =>
		list.foldr(builder)(initialValue)

	/**` (List).foldl1 :: List a -> (a -> a -> a) -> a `*/
	List.foldl1 = <a>(list : List<a>) => (builder : (first : a) => (second : a) => a) : a =>
		list.foldl1(builder)

	/**` (List).foldr1 :: List a -> (a -> a -> a) -> a `*/
	List.foldr1 = <a>(list : List<a>) => (builder : (second : a) => (first : a) => a) : a =>
		list.foldr1(builder)

	/**` (List).head :: List a -> a `*/
	List.head = <a>(list : List<a>) : a =>
		list.head

	/**` (List).init :: List a -> List a `*/
	List.init = <a>(list : List<a>) : List<a> =>
		list.init

	/**` (List).inits :: List a -> List (List a) `*/
	List.inits = <a>(list : List<a>) : List<List<a>> =>
		list.inits

	/**` (List).intersperse :: List a -> a -> List a `*/
	List.intersperse = <a>(list : List<a>) => (delimiter : a) : List<a> =>
		list.intersperse(delimiter)

	/**` (List).last :: List a -> a `*/
	List.last = <a>(list : List<a>) : a =>
		list.last

	/**` (List).notElem :: List a -> a -> Boolean `*/
	List.notElem = <a>(list : List<a>) => (delimiter : a) : boolean =>
		list.notElem(delimiter)

	/**` (List).prepend :: List a -> a -> List a `*/
	List.prepend = <a>(list : List<a>) => (element : a) : List<a> =>
		list.prepend(element)

	/**` (List).reverse :: List a -> List a `*/
	List.reverse = <a>(list : List<a>) : List<a> =>
		list.reverse

	/**` (List).scanl :: List a -> (b -> a -> b) -> b -> List b `*/
	List.scanl = <a>(list : List<a>) => <b>(builder : (first : b) => (second : a) => b) => (initialValue : b) : List<b> =>
		list.scanl(builder)(initialValue)

	/**` (List).scanr :: List a -> (a -> b -> b) -> b -> List b `*/
	List.scanr = <a>(list : List<a>) => <b>(builder : (second : a) => (first : b) => b) => (initialValue : b) : List<b> =>
		list.scanr(builder)(initialValue)

	/**` (List).scanl1 :: List a -> (a -> a -> a) -> List a `*/
	List.scanl1 = <a>(list : List<a>) => (builder : (first : a) => (second : a) => a) : List<a> =>
		list.scanl1(builder)

	/**` (List).scanr1 :: List a -> (a -> a -> a) -> List a `*/
	List.scanr1 = <a>(list : List<a>) => (builder : (second : a) => (first : a) => a) : List<a> =>
		list.scanr1(builder)

	/**` (List).span :: List a -> (a -> Boolean) -> (List a, List a) `*/
	List.span = <a>(list : List<a>) => (predicate : (element : a) => boolean) : [List<a>, List<a>] =>
		list.span(predicate)

	/**` (List).splitAt :: List a -> Number -> (List a, List a) `*/
	List.splitAt = <a>(list : List<a>) => (index : number) : [List<a>, List<a>] =>
		list.splitAt(index)

	/**` (List).tail :: List a -> List a `*/
	List.tail = <a>(list : List<a>) : List<a> =>
		list.tail

	/**` (List).tails :: List a -> List (List a) `*/
	List.tails = <a>(list : List<a>) : List<List<a>> =>
		list.tails

	/**` (List).take :: List a -> Number -> List a `*/
	List.take = <a>(list : List<a>) => (count : number) : List<a> =>
		list.take(count)

	/**` (List).takeWhile :: List a -> (a -> Boolean) -> List a `*/
	List.takeWhile = <a>(list : List<a>) => (predicate : (element : a) => boolean) : List<a> =>
		list.takeWhile(predicate)

	/**` (List).zip :: List a -> List b -> List (a, b) `*/
	List.zip = <a>(list : List<a>) => <b>(postfixes : List<b>) : List<[a, b]> =>
		list.zip(postfixes)

	/**` (List).zipWith :: List a -> List b -> (a -> b -> c) -> List c `*/
	List.zipWith =
		<a>(list : List<a>) =>
		<b>(postfixes : List<b>) =>
		<c>(builder : (firstElement : a) => (secondElement : b) => c) : List<c> =>
		list.zipWith(postfixes)(builder)
}

/**` Vector2D :: Number -> Number -> Vector2D `*/
const Vector2D = (x : number) => (y : number) : Vector2D =>
	(({
		CONS : 'Vector2D',
		x, y
	}))

{
	/**` (Vector2D).x :: Vector2D -> Number `*/
	Vector2D.x = (v : Vector2D) : number => v.x

	/**` (Vector2D).y :: Vector2D -> Number `*/
	Vector2D.y = (v : Vector2D) : number => v.y
}

/**` Vector3D :: Number -> Number -> Number -> Vector3D `*/
const Vector3D = (x : number) => (y : number) => (z : number) : Vector3D =>
	(({
		CONS : 'Vector3D',
		x, y, z
	}))

{
	/**` (Vector3D).x :: Vector3D -> Number `*/
	Vector3D.x = (v : Vector3D) : number => v.x

	/**` (Vector3D).y :: Vector3D -> Number `*/
	Vector3D.y = (v : Vector3D) : number => v.y

	/**` (Vector3D).z :: Vector3D -> Number `*/
	Vector3D.z = (v : Vector3D) : number => v.z
}

/**` Vector4D :: Number -> Number -> Number -> Number -> Vector4D `*/
const Vector4D = (x : number) => (y : number) => (z : number) => (w : number) : Vector4D =>
	(({
		CONS : 'Vector4D',
		x, y, z, w
	}))

{
	/**` (Vector4D).x :: Vector4D -> Number `*/
	Vector4D.x = (v : Vector4D) : number => v.x

	/**` (Vector4D).y :: Vector4D -> Number `*/
	Vector4D.y = (v : Vector4D) : number => v.y

	/**` (Vector4D).z :: Vector4D -> Number `*/
	Vector4D.z = (v : Vector4D) : number => v.z

	/**` (Vector4D).z :: Vector4D -> Number `*/
	Vector4D.w = (v : Vector4D) : number => v.w
}

/**` Matrix2x2 :: (4 Number...) -> Matrix2x2 `*/
const Matrix2x2 =
		(ix : number) => (jx : number) =>
		(iy : number) => (jy : number) : Matrix2x2 =>
	({
		CONS : 'Matrix2x2',
		ix, jx,
		iy, jy,
		i : Vector2D(ix)(iy), j : Vector2D(jx)(jy),
		x : Vector2D(ix)(jx), y : Vector2D(iy)(jy)
	})

{
	/**` (Matrix2x2).ix :: Matrix2x2 -> Number `*/
	Matrix2x2.ix = (matrix : Matrix2x2) : number => matrix.ix

	/**` (Matrix2x2).jx :: Matrix2x2 -> Number `*/
	Matrix2x2.jx = (matrix : Matrix2x2) : number => matrix.jx

	/**` (Matrix2x2).iy :: Matrix2x2 -> Number `*/
	Matrix2x2.iy = (matrix : Matrix2x2) : number => matrix.iy

	/**` (Matrix2x2).jy :: Matrix2x2 -> Number `*/
	Matrix2x2.jy = (matrix : Matrix2x2) : number => matrix.jy

	/**` (Matrix2x2).i :: Matrix2x2 -> Vector2D `*/
	Matrix2x2.i = (matrix : Matrix2x2) : Vector2D => matrix.i

	/**` (Matrix2x2).j :: Matrix2x2 -> Vector2D `*/
	Matrix2x2.j = (matrix : Matrix2x2) : Vector2D => matrix.j

	/**` (Matrix2x2).x :: Matrix2x2 -> Vector2D `*/
	Matrix2x2.x = (matrix : Matrix2x2) : Vector2D => matrix.x

	/**` (Matrix2x2).y :: Matrix2x2 -> Vector2D `*/
	Matrix2x2.y = (matrix : Matrix2x2) : Vector2D => matrix.y
}

/**` Matrix3x3 :: (9 Number...) -> Matrix3x3 `*/
const Matrix3x3 =
		(ix : number) => (jx : number) => (kx : number) =>
		(iy : number) => (jy : number) => (ky : number) =>
		(iz : number) => (jz : number) => (kz : number) : Matrix3x3 =>
	({
		CONS : 'Matrix3x3',
		ix, jx, kx,
		iy, jy, ky,
		iz, jz, kz,
		i : Vector3D(ix)(iy)(iz), j : Vector3D(jx)(jy)(jz), k : Vector3D(kx)(ky)(kz),
		x : Vector3D(ix)(jx)(kx), y : Vector3D(iy)(jy)(ky), z : Vector3D(iz)(jz)(kz)
	})

{
	/**` (Matrix3x3).ix :: Matrix3x3 -> Number `*/
	Matrix3x3.ix = (matrix : Matrix3x3) : number => matrix.ix

	/**` (Matrix3x3).jx :: Matrix3x3 -> Number `*/
	Matrix3x3.jx = (matrix : Matrix3x3) : number => matrix.jx

	/**` (Matrix3x3).kx :: Matrix3x3 -> Number `*/
	Matrix3x3.kx = (matrix : Matrix3x3) : number => matrix.kx

	/**` (Matrix3x3).iy :: Matrix3x3 -> Number `*/
	Matrix3x3.iy = (matrix : Matrix3x3) : number => matrix.iy

	/**` (Matrix3x3).jy :: Matrix3x3 -> Number `*/
	Matrix3x3.jy = (matrix : Matrix3x3) : number => matrix.jy

	/**` (Matrix3x3).ky :: Matrix3x3 -> Number `*/
	Matrix3x3.ky = (matrix : Matrix3x3) : number => matrix.ky

	/**` (Matrix3x3).iz :: Matrix3x3 -> Number `*/
	Matrix3x3.iz = (matrix : Matrix3x3) : number => matrix.iz

	/**` (Matrix3x3).jz :: Matrix3x3 -> Number `*/
	Matrix3x3.jz = (matrix : Matrix3x3) : number => matrix.jz

	/**` (Matrix3x3).kz :: Matrix3x3 -> Number `*/
	Matrix3x3.kz = (matrix : Matrix3x3) : number => matrix.kz

	/**` (Matrix3x3).i :: Matrix3x3 -> Vector3D `*/
	Matrix3x3.i = (matrix : Matrix3x3) : Vector3D => matrix.i

	/**` (Matrix3x3).j :: Matrix3x3 -> Vector3D `*/
	Matrix3x3.j = (matrix : Matrix3x3) : Vector3D => matrix.j

	/**` (Matrix3x3).k :: Matrix3x3 -> Vector3D `*/
	Matrix3x3.k = (matrix : Matrix3x3) : Vector3D => matrix.k

	/**` (Matrix3x3).x :: Matrix3x3 -> Vector3D `*/
	Matrix3x3.x = (matrix : Matrix3x3) : Vector3D => matrix.x

	/**` (Matrix3x3).y :: Matrix3x3 -> Vector3D `*/
	Matrix3x3.y = (matrix : Matrix3x3) : Vector3D => matrix.y

	/**` (Matrix3x3).z :: Matrix3x3 -> Vector3D `*/
	Matrix3x3.z = (matrix : Matrix3x3) : Vector3D => matrix.z
}

/**` Matrix4x4 :: (16 Number...) -> Matrix4x4 `*/
const Matrix4x4 =
		(ix : number) => (jx : number) => (kx : number) => (lx : number) =>
		(iy : number) => (jy : number) => (ky : number) => (ly : number) =>
		(iz : number) => (jz : number) => (kz : number) => (lz : number) =>
		(iw : number) => (jw : number) => (kw : number) => (lw : number) : Matrix4x4 =>
	({
		CONS : 'Matrix4x4',
		ix, jx, kx, lx,
		iy, jy, ky, ly,
		iz, jz, kz, lz,
		iw, jw, kw, lw,
		i : Vector4D(ix)(iy)(iz)(iw), j : Vector4D(jx)(jy)(jz)(jw), k : Vector4D(kx)(ky)(kz)(kw), l : Vector4D(lx)(ly)(lz)(lw),
		x : Vector4D(ix)(jx)(kx)(lx), y : Vector4D(iy)(jy)(ky)(ly), z : Vector4D(iz)(jz)(kz)(lz), w : Vector4D(iw)(jw)(kw)(lw)
	})

{
	/**` (Matrix4x4).ix :: Matrix4x4 -> Number `*/
	Matrix4x4.ix = (matrix : Matrix4x4) : number => matrix.ix

	/**` (Matrix4x4).jx :: Matrix4x4 -> Number `*/
	Matrix4x4.jx = (matrix : Matrix4x4) : number => matrix.jx

	/**` (Matrix4x4).kx :: Matrix4x4 -> Number `*/
	Matrix4x4.kx = (matrix : Matrix4x4) : number => matrix.kx

	/**` (Matrix4x4).lx :: Matrix4x4 -> Number `*/
	Matrix4x4.lx = (matrix : Matrix4x4) : number => matrix.lx

	/**` (Matrix4x4).iy :: Matrix4x4 -> Number `*/
	Matrix4x4.iy = (matrix : Matrix4x4) : number => matrix.iy

	/**` (Matrix4x4).jy :: Matrix4x4 -> Number `*/
	Matrix4x4.jy = (matrix : Matrix4x4) : number => matrix.jy

	/**` (Matrix4x4).ky :: Matrix4x4 -> Number `*/
	Matrix4x4.ky = (matrix : Matrix4x4) : number => matrix.ky

	/**` (Matrix4x4).ly :: Matrix4x4 -> Number `*/
	Matrix4x4.ly = (matrix : Matrix4x4) : number => matrix.ly

	/**` (Matrix4x4).iz :: Matrix4x4 -> Number `*/
	Matrix4x4.iz = (matrix : Matrix4x4) : number => matrix.iz

	/**` (Matrix4x4).jz :: Matrix4x4 -> Number `*/
	Matrix4x4.jz = (matrix : Matrix4x4) : number => matrix.jz

	/**` (Matrix4x4).kz :: Matrix4x4 -> Number `*/
	Matrix4x4.kz = (matrix : Matrix4x4) : number => matrix.kz

	/**` (Matrix4x4).lz :: Matrix4x4 -> Number `*/
	Matrix4x4.lz = (matrix : Matrix4x4) : number => matrix.lz

	/**` (Matrix4x4).iw :: Matrix4x4 -> Number `*/
	Matrix4x4.iw = (matrix : Matrix4x4) : number => matrix.iw

	/**` (Matrix4x4).jw :: Matrix4x4 -> Number `*/
	Matrix4x4.jw = (matrix : Matrix4x4) : number => matrix.jw

	/**` (Matrix4x4).kw :: Matrix4x4 -> Number `*/
	Matrix4x4.kw = (matrix : Matrix4x4) : number => matrix.kw

	/**` (Matrix4x4).lw :: Matrix4x4 -> Number `*/
	Matrix4x4.lw = (matrix : Matrix4x4) : number => matrix.lw

	/**` (Matrix4x4).i :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.i = (matrix : Matrix4x4) : Vector4D => matrix.i

	/**` (Matrix4x4).j :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.j = (matrix : Matrix4x4) : Vector4D => matrix.j

	/**` (Matrix4x4).k :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.k = (matrix : Matrix4x4) : Vector4D => matrix.k

	/**` (Matrix4x4).l :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.l = (matrix : Matrix4x4) : Vector4D => matrix.l

	/**` (Matrix4x4).x :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.x = (matrix : Matrix4x4) : Vector4D => matrix.x

	/**` (Matrix4x4).y :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.y = (matrix : Matrix4x4) : Vector4D => matrix.y

	/**` (Matrix4x4).z :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.z = (matrix : Matrix4x4) : Vector4D => matrix.z

	/**` (Matrix4x4).w :: Matrix4x4 -> Vector4D `*/
	Matrix4x4.w = (matrix : Matrix4x4) : Vector4D => matrix.w
}

/**` TextMeasurement :: String -> Number -> Number -> TextMeasurement `*/
const TextMeasurement = (text : string) => (width : number) => (height : number) : TextMeasurement =>
	(({
		CONS : 'TextMeasurement',
		text, width, height
	}))

{
	/**` (TextMeasurement).text :: TextMeasurement -> String `*/
	TextMeasurement.text = (measurement : TextMeasurement) : string => measurement.text

	/**` (TextMeasurement).width :: TextMeasurement -> Number `*/
	TextMeasurement.width = (measurement : TextMeasurement) : number => measurement.width

	/**` (TextMeasurement).height :: TextMeasurement -> Number `*/
	TextMeasurement.height = (measurement : TextMeasurement) : number => measurement.height
}

/**` Switch.case :: a -> (() -> b) -> Switch a b `*/
const Switch = <a, b>(f : (x : a) => b | undefined) : Switch<a, b> =>
	({
		CONS : 'Switch',
		case : x => y =>
			Switch(z => {
				const w = f(z)
				return w === undefined && z === x ? y() : w
			}),
		else : x => Switch(y => {
			const z = f(y)
			return z === undefined ? x() : z
		}),
		with : x => {
			const y = f(x)
			return y === undefined
				? THROWRANGE(`'Switch' did not cover all cases; missing case on value: '${x}'`)
				: y
		}
	})

Switch.case = <a>(domain : a) => <b>(codomain : () => b) : Switch<a, b> =>
	Switch<a, b>(x => x === domain ? codomain() : undefined)

/**` Bijection.of :: a -> b -> Bijection a b `*/
const Bijection = <a, b>(pairs : ReadonlyArray<[a, b]>) : Bijection<a, b> =>
	({
		CONS : 'Bijection',
		INFO : pairs,
		of : x => y => Bijection([...pairs, [x, y]]),
		domain : x => {
			const y = pairs.find(([z, _]) => z === x)
			return y === undefined
				? THROWRANGE(`'Bijection' did not have a well-defined enough domain for value: '${x}'`)
				: y[1]
		},
		codomain : x => {
			const y = pairs.find(([_, z]) => z === x)
			return y === undefined
				? THROWRANGE(`'Bijection' did not have a well-defined enough codomain for value: '${x}'`)
				: y[0]
		}
	})

Bijection.of = <a>(domainValue : a) => <b>(codomainValue : b) : Bijection<a, b> =>
	Bijection([[domainValue, codomainValue]])

{
	/**` (Bijection).domain :: Bijection a b -> a -> b `*/
	Bijection.domain = <a, b>(bijection : Bijection<a, b>) => (domain : a) : b =>
		bijection.domain(domain)

	/**` (Bijection).codomain :: Bijection a b -> b -> a `*/
	Bijection.codomain = <a, b>(bijection : Bijection<a, b>) => (codomain : b) : a =>
		bijection.codomain(codomain)
}

/**` Clock :: Number -> Number -> Number -> Clock `*/
const Clock = (time : number) => (delta : number) => (counter : number) : Clock =>
	(({ CONS : 'Clock', time, delta, counter }))

{
	/**` (Clock).time :: Clock -> Number `*/
	Clock.time = (clock : Clock) : number => clock.time

	/**` (Clock).delta :: Clock -> Number `*/
	Clock.delta = (clock : Clock) : number => clock.delta

	/**` (Clock).counter :: Clock -> Number `*/
	Clock.counter = (clock : Clock) : number => clock.counter
}

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
	CenterY  = 'CenterY :: Vertical',
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

/**` data TextAlign = Start | End | Left | Right | Center `*/
enum TextAlign
{
	Start  = 'Start :: TextAlign',
	End    = 'End :: TextAlign',
	Left   = 'Left :: TextAlign',
	Right  = 'Right :: TextAlign',
	Center = 'Center :: TextAlign'
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

/**
 * ```
 * data CompositionOperation = SourceOver      | SourceIn      | SourceOut      | SourceAtop      |
 *                             DestinationOver | DestinationIn | DestinationOut | DestinationAtop |
 *                             Lighter         | Copy          | Xor            | Multiply        |
 *                             Screen          | Overlay       | Darken         | Lighten         |
 *                             ColorDodge      | ColorBurn     | HardLight      | SoftLight       |
 *                             Difference      | Exclusion     | Hue            | Saturation      |
 *                             Color           | Luminosity
 * ```
 */
enum CompositionOperation
{
	SourceOver      = 'SourceOver :: CompositionOperation',
	SourceAtop      = 'SourceAtop :: CompositionOperation',
	SourceIn        = 'SourceIn :: CompositionOperation',
	SourceOut       = 'SourceOut :: CompositionOperation',
	DestinationOver = 'DestinationOver :: CompositionOperation',
	DestinationAtop = 'DestinationAtop :: CompositionOperation',
	DestinationIn   = 'DestinationIn :: CompositionOperation',
	DestinationOut  = 'DestinationOut :: CompositionOperation',
	Lighter         = 'Lighter :: CompositionOperation',
	Xor             = 'Xor :: CompositionOperation',
	Copy            = 'Copy :: CompositionOperation',
	Multiply        = 'Multiply :: CompositionOperation',
	Screen          = 'Screen :: CompositionOperation',
	Overlay         = 'Overlay :: CompositionOperation',
	Darken          = 'Darken :: CompositionOperation',
	Lighten         = 'Lighten :: CompositionOperation',
	ColorDodge      = 'ColorDodge :: CompositionOperation',
	ColorBurn       = 'ColorBurn :: CompositionOperation',
	HardLight       = 'HardLight :: CompositionOperation',
	SoftLight       = 'SoftLight :: CompositionOperation',
	Difference      = 'Difference :: CompositionOperation',
	Exclusion       = 'Exclusion :: CompositionOperation',
	Hue             = 'Hue :: CompositionOperation',
	Saturation      = 'Saturation :: CompositionOperation',
	Color           = 'Color :: CompositionOperation',
	Luminosity      = 'Luminosity :: CompositionOperation'
}

/********************************************************************************************************************************/

/**` relaxHorizontal :: Horizontal -> Horizontal `*/
const relaxHorizontal = (direction : Horizontal) : Horizontal =>
	Switch
		.case (Horizontal.Leftward)  (() => Horizontal.Left)
		.case (Horizontal.Rightward) (() => Horizontal.Right)
		.else (() => direction)
		.with (direction)

/**` relaxVertical :: Vertical -> Vertical `*/
const relaxVertical = (direction : Vertical) : Vertical =>
	Switch
		.case (Vertical.Downward) (() => Vertical.Down)
		.case (Vertical.Upward)   (() => Vertical.Up)
		.else (() => direction)
		.with (direction)

/**` relaxLateral :: Lateral -> Lateral `*/
const relaxLateral = (direction : Lateral) : Lateral =>
	Switch
		.case (Lateral.Backward) (() => Lateral.Back)
		.case (Lateral.Forward)  (() => Lateral.Fore)
		.else (() => direction)
		.with (direction)

/**` bijectionLineCap :: Bijection LineCap CanvasLineCap `*/
const bijectionLineCap : Bijection<LineCap, CanvasLineCap> =
	Bijection
		.of (LineCap.Butt)   ('butt' as CanvasLineCap)
		.of (LineCap.Round)  ('round')
		.of (LineCap.Square) ('square')

/**` bijectionLineJoin :: Bijection LineJoin CanvasLineJoin `*/
const bijectionLineJoin : Bijection<LineJoin, CanvasLineJoin> =
	Bijection
		.of (LineJoin.Round) ('round' as CanvasLineJoin)
		.of (LineJoin.Bevel) ('bevel')
		.of (LineJoin.Miter) ('miter')

/**` bijectionTextAlign :: Bijection TextAlign CanvasTextAlign `*/
const bijectionTextAlign : Bijection<TextAlign, CanvasTextAlign> =
	Bijection
		.of (TextAlign.Center) ('center' as CanvasTextAlign)
		.of (TextAlign.End)    ('end')
		.of (TextAlign.Left)   ('left')
		.of (TextAlign.Right)  ('right')
		.of (TextAlign.Start)  ('start')

/**` bijectionTextBaseline :: Bijection TextBaseline CanvasTextBaseline `*/
const bijectionTextBaseline : Bijection<TextBaseline, CanvasTextBaseline> =
	Bijection
		.of (TextBaseline.Alphabetic)  ('alphabetic' as CanvasTextBaseline)
		.of (TextBaseline.Bottom)      ('bottom')
		.of (TextBaseline.Hanging)     ('hanging')
		.of (TextBaseline.Ideographic) ('ideographic')
		.of (TextBaseline.Middle)      ('middle')
		.of (TextBaseline.Top)         ('top')

/**` bijectionCompositionOperation :: Bijection CompositionOperation String `*/
const bijectionCompositionOperation : Bijection<CompositionOperation, string> =
	Bijection
		.of(CompositionOperation.SourceOver)      ('source-over')
		.of(CompositionOperation.SourceIn)        ('source-in')
		.of(CompositionOperation.SourceOut)       ('source-out')
		.of(CompositionOperation.SourceAtop)      ('source-atop')
		.of(CompositionOperation.DestinationOver) ('destination-over')
		.of(CompositionOperation.DestinationIn)   ('destination-in')
		.of(CompositionOperation.DestinationOut)  ('destination-out')
		.of(CompositionOperation.DestinationAtop) ('destination-atop')
		.of(CompositionOperation.Lighter)         ('lighter')
		.of(CompositionOperation.Copy)            ('copy')
		.of(CompositionOperation.Xor)             ('xor')
		.of(CompositionOperation.Multiply)        ('multiply')
		.of(CompositionOperation.Screen)          ('screen')
		.of(CompositionOperation.Overlay)         ('overlay')
		.of(CompositionOperation.Darken)          ('darken')
		.of(CompositionOperation.Lighten)         ('lighten')
		.of(CompositionOperation.ColorDodge)      ('color-dodge')
		.of(CompositionOperation.ColorBurn)       ('color-burn')
		.of(CompositionOperation.HardLight)       ('hard-light')
		.of(CompositionOperation.SoftLight)       ('soft-light')
		.of(CompositionOperation.Difference)      ('difference')
		.of(CompositionOperation.Exclusion)       ('exclusion')
		.of(CompositionOperation.Hue)             ('hue')
		.of(CompositionOperation.Saturation)      ('saturation')
		.of(CompositionOperation.Color)           ('color')
		.of(CompositionOperation.Luminosity)      ('luminosity')

/**` Matrix2D :: Vector2D -> Vector2D -> Matrix2x2 `*/
const Matrix2D = (i : Vector2D) => (j : Vector2D) : Matrix2x2 =>
	Matrix2x2
		(i.x)(j.x)
		(i.y)(j.y)

/**` Matrix3D :: Vector3D -> Vector3D -> Vector3D -> Matrix3x3 `*/
const Matrix3D = (i : Vector3D) => (j : Vector3D) => (k : Vector3D) : Matrix3x3 =>
	Matrix3x3
		(i.x)(j.x)(k.x)
		(i.y)(j.y)(k.y)
		(i.z)(j.z)(k.z)

/**` Matrix4D :: Vector4D -> Vector4D -> Vector4D -> Vector4D -> Matrix4x4 `*/
const Matrix4D = (i : Vector4D) => (j : Vector4D) => (k : Vector4D) => (l : Vector4D) : Matrix4x4 =>
	Matrix4x4
		(i.x)(j.x)(k.x)(l.x)
		(i.y)(j.y)(k.y)(l.y)
		(i.z)(j.z)(k.z)(l.z)
		(i.w)(j.w)(k.w)(l.w)

/**` pseudoRandom :: State Number Number `*/
const pseudoRandom : State<number, number> =
	State(seed => [
		(-67 * seed * seed * seed + 23 * seed * seed - 91 * seed + 73) % 65536,
		Math.abs(97 * seed * seed * seed + 91 * seed * seed - 83 * seed + 79) % 65536 / 65536
	])

/********************************************************************************************************************************/

// The Do-Notation syntax where a monad stores an empty object.
const Do =
	{
		IO    : IO    <{}>      (() => Object.create(null)),
		Maybe : Just  <{}>      (Object.create(null)),
		State : State <any, {}> ((s : any) => [s, Object.create(null)]),
		List  : List  <{}>      (Object.create(null))
	} as const

/********************************************************************************************************************************/

/**` __KEYBOARD_KEYS_ARRAY__ :: [String] `*/
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
		context         : undefined as unknown as CanvasRenderingContext2D,
		resizeID        : undefined as unknown as number,
		isResized       : false,
		isPointerLocked : false,
		seed            : (Math.random() - 0.5) * Date.now(),
		image           : {} as { [key : string] : HTMLImageElement },
		audio           : {} as { [key : string] : HTMLAudioElement },
		mouse           :
			{
				screenX : 0, screenY : 0,
				windowX : 0, windowY : 0,
				canvasX : 0, canvasY : 0,
				deltaX  : 0, deltaY  : 0,
				scroll  : Vertical.CenterY,
				buttons : new Array(5).fill(Vertical.Up) as [Vertical, Vertical, Vertical, Vertical, Vertical]
			},
		keyboard        :
			__KEYBOARD_KEYS_ARRAY__.reduce(($, k) => ({ ...$, [k] : Vertical.Up }), {}) as { [key in KeyboardKey] : Vertical }
	}

/********************************************************************************************************************************/

namespace Import
{
	export namespace Norm
	{
		/**` Import.Norm.mouseCanvasPosition :: IO (Number, Number) `*/
		export const mouseCanvasPosition : IO<[number, number]> =
			IO(() => [
				__EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width,
				__EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height
			])

		/**` Import.Norm.mouseCanvasPositionVector :: IO Vector2D `*/
		export const mouseCanvasPositionVector : IO<Vector2D> =
			IO(() =>
				Vector2D
					(__EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width)
					(__EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height)
			)

		/**` Import.Norm.mouseCanvasPositionX :: IO Number `*/
		export const mouseCanvasPositionX : IO<number> =
			IO(() => __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width)

		/**` Import.Norm.mouseCanvasPositionY :: IO Number `*/
		export const mouseCanvasPositionY : IO<number> =
			IO(() => __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height)

		/**` Import.Norm.mouseVelocity :: IO (Number, Number) `*/
		export const mouseVelocity : IO<[number, number]> =
			IO(() => [
				__EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width,
				__EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height
			])

		/**` Import.Norm.mouseVelocityVector :: IO Vector2D `*/
		export const mouseVelocityVector : IO<Vector2D> =
			IO(() =>
				Vector2D
					(__EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width)
					(__EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height)
			)

		/**` Import.Norm.mouseVelocityX :: IO Number `*/
		export const mouseVelocityX : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width)

		/**` Import.Norm.mouseVelocityY :: IO Number `*/
		export const mouseVelocityY : IO<number> =
			IO(() => __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height)

		/**` Import.Norm.textMeasurement :: String -> IO TextMeasurement `*/
		export const textMeasurement = (text : string) : IO<TextMeasurement> =>
			IO(() => {
				const { width, height } = __EXTERNAL__.context.canvas
				const { actualBoundingBoxLeft, actualBoundingBoxRight, actualBoundingBoxAscent, actualBoundingBoxDescent } =
					__EXTERNAL__.context.measureText(text)
				return TextMeasurement
					(text)
					((Math.abs(actualBoundingBoxLeft) + Math.abs(actualBoundingBoxRight)) / width)
					((Math.abs(actualBoundingBoxAscent) + Math.abs(actualBoundingBoxDescent)) / height)
			})

		/**` Import.Norm.lineWidth :: IO Number `*/
		export const lineWidth : IO<number> =
			IO(() => __EXTERNAL__.context.lineWidth / __EXTERNAL__.context.canvas.width)

		/**` Import.Norm.lineDashPattern :: IO (List Number) `*/
		export const lineDashPattern : IO<List<number>> =
			IO(() => List(...__EXTERNAL__.context.getLineDash().map(n => n / __EXTERNAL__.context.canvas.width)))

		/**` Import.Norm.lineDashOffset :: IO Number `*/
		export const lineDashOffset : IO<number> =
			IO(() => __EXTERNAL__.context.lineDashOffset / __EXTERNAL__.context.canvas.width)

		/**` Import.Norm.fontSize :: IO Number `*/
		export const fontSize : IO<number> =
			IO(() => parseFloat(__EXTERNAL__.context.font) / __EXTERNAL__.context.canvas.width)

		/**` Import.Norm.shadowOffset :: IO (Number, Number) `*/
		export const shadowOffset : IO<[number, number]> =
			IO(() => [
				__EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width,
				__EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height
			])

		/**` Import.Norm.shadowOffsetVector :: IO Vector2D `*/
		export const shadowOffsetVector : IO<Vector2D> =
			IO(() =>
				Vector2D
					(__EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width)
					(__EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height)
			)

		/**` Import.Norm.shadowOffsetX :: IO Number `*/
		export const shadowOffsetX : IO<number> =
			IO(() => __EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width)

		/**` Import.Norm.shadowOffsetY :: IO Number `*/
		export const shadowOffsetY : IO<number> =
			IO(() => __EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height)

		/**` Import.Norm.isPointInEvenOddPath :: Number -> Number -> IO Boolean `*/
		export const isPointInEvenOddPath = (x : number) => (y : number) : IO<boolean> =>
			IO(() =>
				__EXTERNAL__.context.isPointInPath(
					x * __EXTERNAL__.context.canvas.width,
					y * __EXTERNAL__.context.canvas.height,
					'evenodd'
				)
			)

		/**` Import.Norm.isPointInNonZeroPath :: Number -> Number -> IO Boolean `*/
		export const isPointInNonZeroPath = (x : number) => (y : number) : IO<boolean> =>
			IO(() =>
				__EXTERNAL__.context.isPointInPath(
					x * __EXTERNAL__.context.canvas.width,
					y * __EXTERNAL__.context.canvas.height,
					'nonzero'
				)
			)

		/**` Import.Norm.isVectorInEvenOddPath :: Vector2D -> IO Boolean `*/
		export const isVectorInEvenOddPath = (v : Vector2D) : IO<boolean> =>
			IO(() =>
				__EXTERNAL__.context.isPointInPath(
					v.x * __EXTERNAL__.context.canvas.width,
					v.y * __EXTERNAL__.context.canvas.height,
					'evenodd'
				)
			)

		/**` Import.Norm.isVectorInNonZeroPath :: Vector2D -> IO Boolean `*/
		export const isVectorInNonZeroPath = (v : Vector2D) : IO<boolean> =>
			IO(() =>
				__EXTERNAL__.context.isPointInPath(
					v.x * __EXTERNAL__.context.canvas.width,
					v.y * __EXTERNAL__.context.canvas.height,
					'nonzero'
				)
			)

		/**` Import.Norm.isPointInStroke :: Number -> Number -> IO Boolean `*/
		export const isPointInStroke = (x : number) => (y : number) : IO<boolean> =>
			IO(() =>
				__EXTERNAL__.context.isPointInStroke(
					x * __EXTERNAL__.context.canvas.width,
					y * __EXTERNAL__.context.canvas.height
				)
			)

		/**` Import.Norm.isVectorInStroke :: Vector2D -> IO Boolean `*/
		export const isVectorInStroke = (v : Vector2D) : IO<boolean> =>
			IO(() =>
				__EXTERNAL__.context.isPointInStroke(
					v.x * __EXTERNAL__.context.canvas.width,
					v.y * __EXTERNAL__.context.canvas.height
				)
			)

		/**` Import.Norm.transformationMatrix :: IO Matrix3x3 `*/
		export const transformationMatrix : IO<Matrix3x3> =
			IO(() => {
				const m = __EXTERNAL__.context.getTransform()
				return Matrix3x3
					(m.a)(m.c)(m.e / __EXTERNAL__.context.canvas.width)
					(m.b)(m.d)(m.f / __EXTERNAL__.context.canvas.height)
					(0)  (0)  (1)
			})
	}

	/**` Import.timeSinceOpen :: IO Number `*/
	export const timeSinceOpen : IO<number> =
		IO(() => performance.now())

	/**` Import.timeSince1970 :: IO Number `*/
	export const timeSince1970 : IO<number> =
		IO(() => Date.now())

	/**` Import.universalSeed :: IO Number `*/
	export const universalSeed : IO<number> =
		IO(() => __EXTERNAL__.seed)

	/**` Import.isWindowResized :: IO Boolean `*/
	export const isWindowResized : IO<boolean> =
		IO(() => __EXTERNAL__.isResized)

	/**` Import.screenDimensions :: IO (Number, Number) `*/
	export const screenDimensions : IO<[number, number]> =
		IO(() => [screen.width, screen.height])

	/**` Import.screenDimensionsVector :: IO Vector2D `*/
	export const screenDimensionsVector : IO<Vector2D> =
		IO(() => Vector2D(screen.width)(screen.height))

	/**` Import.screenDimensionW :: IO Number `*/
	export const screenDimensionW : IO<number> =
		IO(() => screen.width)

	/**` Import.screenDimensionH :: IO Number `*/
	export const screenDimensionH : IO<number> =
		IO(() => screen.height)

	/**` Import.windowDimensions :: IO (Number, Number) `*/
	export const windowDimensions : IO<[number, number]> =
		IO(() => [innerWidth, innerHeight])

	/**` Import.windowDimensionsVector :: IO Vector2D `*/
	export const windowDimensionsVector : IO<Vector2D> =
		IO(() => Vector2D(innerWidth)(innerHeight))

	/**` Import.windowDimensionW :: IO Number `*/
	export const windowDimensionW : IO<number> =
		IO(() => innerWidth)

	/**` Import.windowDimensionH :: IO Number `*/
	export const windowDimensionH : IO<number> =
		IO(() => innerHeight)

	/**` Import.canvasDimensions :: IO (Number, Number) `*/
	export const canvasDimensions : IO<[number, number]> =
		IO(() => [__EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height])

	/**` Import.canvasDimensionsVector :: IO Vector2D `*/
	export const canvasDimensionsVector : IO<Vector2D> =
		IO(() => Vector2D(__EXTERNAL__.context.canvas.width)(__EXTERNAL__.context.canvas.height))

	/**` Import.canvasDimensionW :: IO Number `*/
	export const canvasDimensionW : IO<number> =
		IO(() => __EXTERNAL__.context.canvas.width)

	/**` Import.canvasDimensionH :: IO Number `*/
	export const canvasDimensionH : IO<number> =
		IO(() => __EXTERNAL__.context.canvas.height)

	/**` Import.mouseScreenPosition :: IO (Number, Number) `*/
	export const mouseScreenPosition : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.screenX, __EXTERNAL__.mouse.screenY])

	/**` Import.mouseScreenPositionVector :: IO Vector2D `*/
	export const mouseScreenPositionVector : IO<Vector2D> =
		IO(() => Vector2D(__EXTERNAL__.mouse.screenX)(__EXTERNAL__.mouse.screenY))

	/**` Import.mouseScreenPositionX :: IO Number `*/
	export const mouseScreenPositionX : IO<number> =
		IO(() => __EXTERNAL__.mouse.screenX)

	/**` Import.mouseScreenPositionY :: IO Number `*/
	export const mouseScreenPositionY : IO<number> =
		IO(() => __EXTERNAL__.mouse.screenY)

	/**` Import.mouseWindowPosition :: IO (Number, Number) `*/
	export const mouseWindowPosition : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.windowX, __EXTERNAL__.mouse.windowY])

	/**` Import.mouseWindowPositionVector :: IO Vector2D `*/
	export const mouseWindowPositionVector : IO<Vector2D> =
		IO(() => Vector2D(__EXTERNAL__.mouse.windowX)(__EXTERNAL__.mouse.windowY))

	/**` Import.mouseWindowPositionX :: IO Number `*/
	export const mouseWindowPositionX : IO<number> =
		IO(() => __EXTERNAL__.mouse.windowX)

	/**` Import.mouseWindowPositionY :: IO Number `*/
	export const mouseWindowPositionY : IO<number> =
		IO(() => __EXTERNAL__.mouse.windowY)

	/**` Import.mouseCanvasPosition :: IO (Number, Number) `*/
	export const mouseCanvasPosition : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.canvasX, __EXTERNAL__.mouse.canvasY])

	/**` Import.mouseCanvasPositionVector :: IO Vector2D `*/
	export const mouseCanvasPositionVector : IO<Vector2D> =
		IO(() => Vector2D(__EXTERNAL__.mouse.canvasX)(__EXTERNAL__.mouse.canvasY))

	/**` Import.mouseCanvasPositionX :: IO Number `*/
	export const mouseCanvasPositionX : IO<number> =
		IO(() => __EXTERNAL__.mouse.canvasX)

	/**` Import.mouseCanvasPositionY :: IO Number `*/
	export const mouseCanvasPositionY : IO<number> =
		IO(() => __EXTERNAL__.mouse.canvasY)

	/**` Import.mouseVelocity :: IO (Number, Number) `*/
	export const mouseVelocity : IO<[number, number]> =
		IO(() => [__EXTERNAL__.mouse.deltaX, __EXTERNAL__.mouse.deltaY])

	/**` Import.mouseVelocityVector :: IO Vector2D `*/
	export const mouseVelocityVector : IO<Vector2D> =
		IO(() => Vector2D(__EXTERNAL__.mouse.deltaX)(__EXTERNAL__.mouse.deltaY))

	/**` Import.mouseVelocityX :: IO Number `*/
	export const mouseVelocityX : IO<number> =
		IO(() => __EXTERNAL__.mouse.deltaX)

	/**` Import.mouseVelocityY :: IO Number `*/
	export const mouseVelocityY : IO<number> =
		IO(() => __EXTERNAL__.mouse.deltaY)

	/**` Import.mouseButtonLeft :: IO Vertical `*/
	export const mouseButtonLeft : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[0])

	/**` Import.mouseButtonMiddle :: IO Vertical `*/
	export const mouseButtonMiddle : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[1])

	/**` Import.mouseButtonRight :: IO Vertical `*/
	export const mouseButtonRight : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[2])

	/**` Import.mouseButtonA :: IO Vertical `*/
	export const mouseButtonA : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[3])

	/**` Import.mouseButtonB :: IO Vertical `*/
	export const mouseButtonB : IO<Vertical> =
		IO(() => __EXTERNAL__.mouse.buttons[4])

	/**` Import.keyboardKey :: KeyboardKey -> IO Vertical `*/
	export const keyboardKey = (key : KeyboardKey) : IO<Vertical> =>
		IO(() => __EXTERNAL__.keyboard[key])

	/**` Import.textMeasurement :: String -> IO TextMeasurement `*/
	export const textMeasurement = (text : string) : IO<TextMeasurement> =>
		IO(() => {
			const metrics = __EXTERNAL__.context.measureText(text)
			return TextMeasurement
				(text)
				(Math.abs(metrics.actualBoundingBoxLeft) + Math.abs(metrics.actualBoundingBoxRight))
				(Math.abs(metrics.actualBoundingBoxAscent) + Math.abs(metrics.actualBoundingBoxDescent))
		})

	/**` Import.lineWidth :: IO Number `*/
	export const lineWidth : IO<number> =
		IO(() => __EXTERNAL__.context.lineWidth)

	/**` Import.lineCap :: IO LineCap `*/
	export const lineCap : IO<LineCap> =
		IO(() => bijectionLineCap.codomain(__EXTERNAL__.context.lineCap))

	/**` Import.lineJoin :: IO LineJoin `*/
	export const lineJoin : IO<LineJoin> =
		IO(() => bijectionLineJoin.codomain(__EXTERNAL__.context.lineJoin))

	/**` Import.lineDashPattern :: IO (List Number) `*/
	export const lineDashPattern : IO<List<number>> =
		IO(() => List(...__EXTERNAL__.context.getLineDash()))

	/**` Import.lineDashOffset :: IO Number `*/
	export const lineDashOffset : IO<number> =
		IO(() => __EXTERNAL__.context.lineDashOffset)

	/**` Import.miterLimit :: IO Number `*/
	export const miterLimit : IO<number> =
		IO(() => __EXTERNAL__.context.miterLimit)

	/**` Import.font :: IO String `*/
	export const font : IO<string> =
		IO(() => __EXTERNAL__.context.font)

	/**` Import.fontSize :: IO Number `*/
	export const fontSize : IO<number> =
		IO(() => parseFloat(__EXTERNAL__.context.font))

	/**` Import.fontFamily :: IO String `*/
	export const fontFamily : IO<string> =
		IO(() => __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1))

	/**` Import.textAlign :: IO TextAlign `*/
	export const textAlign : IO<TextAlign> =
		IO(() => bijectionTextAlign.codomain(__EXTERNAL__.context.textAlign))

	/**` Import.textBaseline :: IO TextBaseline `*/
	export const textBaseline : IO<TextBaseline> =
		IO(() => bijectionTextBaseline.codomain(__EXTERNAL__.context.textBaseline))

	/**` Import.shadowBlurAmount :: IO Number `*/
	export const shadowBlurAmount : IO<number> =
		IO(() => __EXTERNAL__.context.shadowBlur)

	/**` Import.shadowColor :: IO String `*/
	export const shadowColor : IO<string> =
		IO(() => __EXTERNAL__.context.shadowColor)

	/**` Import.shadowOffset :: IO (Number, Number) `*/
	export const shadowOffset : IO<[number, number]> =
		IO(() => [__EXTERNAL__.context.shadowOffsetX, __EXTERNAL__.context.shadowOffsetY])

	/**` Import.shadowOffsetVector :: IO Vector2D `*/
	export const shadowOffsetVector : IO<Vector2D> =
		IO(() => Vector2D(__EXTERNAL__.context.shadowOffsetX)(__EXTERNAL__.context.shadowOffsetY))

	/**` Import.shadowOffsetX :: IO Number `*/
	export const shadowOffsetX : IO<number> =
		IO(() => __EXTERNAL__.context.shadowOffsetX)

	/**` Import.shadowOffsetY :: IO Number `*/
	export const shadowOffsetY : IO<number> =
		IO(() => __EXTERNAL__.context.shadowOffsetY)

	/**` Import.isPointInEvenOddPath :: Number -> Number -> IO Boolean `*/
	export const isPointInEvenOddPath = (x : number) => (y : number) : IO<boolean> =>
		IO(() => __EXTERNAL__.context.isPointInPath(x, y, 'evenodd'))

	/**` Import.isPointInNonZeroPath :: Number -> Number -> IO Boolean `*/
	export const isPointInNonZeroPath = (x : number) => (y : number) : IO<boolean> =>
		IO(() => __EXTERNAL__.context.isPointInPath(x, y, 'nonzero'))

	/**` Import.isVectorInEvenOddPath :: Vector2D -> IO Boolean `*/
	export const isVectorInEvenOddPath = (v : Vector2D) : IO<boolean> =>
		IO(() => __EXTERNAL__.context.isPointInPath(v.x, v.y, 'evenodd'))

	/**` Import.isVectorInNonZeroPath :: Vector2D -> IO Boolean `*/
	export const isVectorInNonZeroPath = (v : Vector2D) : IO<boolean> =>
		IO(() => __EXTERNAL__.context.isPointInPath(v.x, v.y, 'nonzero'))

	/**` Import.isPointInStroke :: Number -> Number -> IO Boolean `*/
	export const isPointInStroke = (x : number) => (y : number) : IO<boolean> =>
		IO(() => __EXTERNAL__.context.isPointInStroke(x, y))

	/**` Import.isVectorInStroke :: Vector2D -> IO Boolean `*/
	export const isVectorInStroke = (v : Vector2D) : IO<boolean> =>
		IO(() => __EXTERNAL__.context.isPointInStroke(v.x, v.y))

	/**` Import.transformationMatrix :: IO Matrix3x3 `*/
	export const transformationMatrix : IO<Matrix3x3> =
		IO(() => {
			const m = __EXTERNAL__.context.getTransform()
			return Matrix3x3
				(m.a)(m.c)(m.e)
				(m.b)(m.d)(m.f)
				(0)  (0)  (1)
		})

	/**` Import.alpha :: IO number `*/
	export const alpha : IO<number> =
		IO(() => __EXTERNAL__.context.globalAlpha)

	/**` Import.compositionOperation :: IO CompositionOperation `*/
	export const compositionOperation : IO<CompositionOperation> =
		IO(() => bijectionCompositionOperation.codomain(__EXTERNAL__.context.globalCompositeOperation))
}

namespace Mutate
{
	export namespace Norm
	{
		/**` Mutate.Norm.lineWidth :: Number -> IO () `*/
		export const lineWidth = (w : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.lineWidth = w * __EXTERNAL__.context.canvas.width
				return null
			})

		/**` Mutate.Norm.lineDashPattern :: List Number -> IO () `*/
		export const lineDashPattern = (pattern : List<number>) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.setLineDash(pattern.INFO.map(n => n * __EXTERNAL__.context.canvas.width))
				return null
			})

		/**` Mutate.Norm.lineDashOffset :: Number -> IO () `*/
		export const lineDashOffset = (offset : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.lineDashOffset = offset * __EXTERNAL__.context.canvas.width
				return null
			})

		/**` Mutate.Norm.fontSize :: Number -> IO () `*/
		export const fontSize = (size : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.font =
					`${size * __EXTERNAL__.context.canvas.width}px ` +
					`${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1)}`
				return null
			})

		/**` Mutate.Norm.fillRGBA :: Number -> Number -> Number -> Number -> IO ()  `*/
		export const fillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`
				return null
			})

		/**` Mutate.Norm.fillVector :: Vector4D -> IO ()  `*/
		export const fillVector = (v : Vector4D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`
				return null
			})

		/**` Mutate.Norm.strokeRGBA :: Number -> Number -> Number -> Number -> IO ()  `*/
		export const strokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a * 255})`
				return null
			})

		/**` Mutate.Norm.strokeVector :: Vector4D -> IO ()  `*/
		export const strokeVector = (v : Vector4D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`
				return null
			})

		/**` Mutate.Norm.shadowRGBA :: Number -> Number -> Number -> Number -> IO ()  `*/
		export const shadowRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`
				return null
			})

		/**` Mutate.Norm.shadowVector :: Vector4D -> IO ()  `*/
		export const shadowVector = (v : Vector4D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`
				return null
			})

		/**` Mutate.Norm.shadowOffset :: Number -> Number -> IO () `*/
		export const shadowOffset = (x : number) => (y : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width
				__EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height
				return null
			})

		/**` Mutate.Norm.shadowOffsetVector :: Vector2D -> IO () `*/
		export const shadowOffsetVector = (v : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.shadowOffsetX = v.x * __EXTERNAL__.context.canvas.width
				__EXTERNAL__.context.shadowOffsetY = v.y * __EXTERNAL__.context.canvas.height
				return null
			})

		/**` Mutate.Norm.shadowOffsetX :: Number -> IO () `*/
		export const shadowOffsetX = (x : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width
				return null
			})

		/**` Mutate.Norm.shadowOffsetY :: Number -> IO () `*/
		export const shadowOffsetY = (y : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height
				return null
			})

		/**` Mutate.Norm.transformationMatrix :: Matrix3x3 -> IO () `*/
		export const transformationMatrix = (m : Matrix3x3) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.setTransform(
					m.ix, m.iy, m.jx, m.jy,
					m.kx * __EXTERNAL__.context.canvas.width,
					m.ky * __EXTERNAL__.context.canvas.height
				)
				return null
			})
	}

	/**` Mutate.canvasDimensions :: Number -> Number -> IO () `*/
	export const canvasDimensions = (w : number) => (h : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.canvas.width  = w
			__EXTERNAL__.context.canvas.height = h
			return null
		})

	/**` Mutate.canvasDimensionVector :: Vector2D -> IO () `*/
	export const canvasDimensionVector = (v : Vector2D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.canvas.width  = v.x
			__EXTERNAL__.context.canvas.height = v.y
			return null
		})

	/**` Mutate.canvasDimensionW :: Number -> IO () `*/
	export const canvasDimensionW = (w : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.canvas.width = w
			return null
		})

	/**` Mutate.canvasDimensionH :: Number -> IO () `*/
	export const canvasDimensionH = (h : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.canvas.height = h
			return null
		})

	/**` Mutate.lineWidth :: Number -> IO () `*/
	export const lineWidth = (w : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineWidth = w
			return null
		})

	/**` Mutate.lineCap :: LineCap -> IO () `*/
	export const lineCap = (cap : LineCap) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineCap = bijectionLineCap.domain(cap)
			return null
		})

	/**` Mutate.lineJoin :: LineJoin -> IO () `*/
	export const lineJoin = (joining : LineJoin) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineJoin = bijectionLineJoin.domain(joining)
			return null
		})

	/**` Mutate.lineDashPattern :: List Number -> IO () `*/
	export const lineDashPattern = (pattern : List<number>) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.setLineDash(pattern.INFO.slice())
			return null
		})

	/**` Mutate.lineDashOffset :: Number -> IO () `*/
	export const lineDashOffset = (offset : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.lineDashOffset = offset
			return null
		})

	/**` Mutate.miterLimit :: Number -> IO () `*/
	export const miterLimit = (limit : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.miterLimit = limit
			return null
		})

	/**` Mutate.font :: String -> IO () `*/
	export const font = (fontDescription : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font = fontDescription
			return null
		})

	/**` Mutate.fontSize :: Number -> IO () `*/
	export const fontSize = (size : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font =
				`${size}px ${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1)}`
			return null
		})

	/**` Mutate.fontFamily :: String -> IO ()  `*/
	export const fontFamily = (family : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font = `${parseFloat(__EXTERNAL__.context.font)}px ${family}`
			return null
		})

	/**` Mutate.textAlign :: TextAlign -> IO ()  `*/
	export const textAlign = (align : TextAlign) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.textAlign = bijectionTextAlign.domain(align)
			return null
		})

	/**` Mutate.textBaseline :: TextBaseline -> IO ()  `*/
	export const textBaseline = (baseline : TextBaseline) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.textBaseline = bijectionTextBaseline.domain(baseline)
			return null
		})

	/**` Mutate.fillColor :: String -> IO ()  `*/
	export const fillColor = (color : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.fillStyle = color
			return null
		})

	/**` Mutate.fillRGBA :: Number -> Number -> Number -> Number -> IO ()  `*/
	export const fillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.fillStyle = `rgba(${r},${g},${b},${a})`
			return null
		})

	/**` Mutate.fillVector :: Vector4D -> IO ()  `*/
	export const fillVector = (v : Vector4D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`
			return null
		})

	/**` Mutate.strokeColor :: String -> IO ()  `*/
	export const strokeColor = (color : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.strokeStyle = color
			return null
		})

	/**` Mutate.strokeRGBA :: Number -> Number -> Number -> Number -> IO ()  `*/
	export const strokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.strokeStyle = `rgba(${r},${g},${b},${a})`
			return null
		})

	/**` Mutate.strokeVector :: Vector4D -> IO ()  `*/
	export const strokeVector = (v : Vector4D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`
			return null
		})

	/**` Mutate.shadowBlurAmount :: Number -> IO ()  `*/
	export const shadowBlurAmount = (amount : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowBlur = amount
			return null
		})

	/**` Mutate.shadowColor :: String -> IO ()  `*/
	export const shadowColor = (color : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowColor = color
			return null
		})

	/**` Mutate.shadowRGBA :: Number -> Number -> Number -> Number -> IO ()  `*/
	export const shadowRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowColor = `rgba(${r},${g},${b},${a})`
			return null
		})

	/**` Mutate.shadowVector :: Vector4D -> IO ()  `*/
	export const shadowVector = (v : Vector4D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`
			return null
		})

	/**` Mutate.shadowOffset :: Number -> Number -> IO () `*/
	export const shadowOffset = (x : number) => (y : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowOffsetX = x
			__EXTERNAL__.context.shadowOffsetY = y
			return null
		})

	/**` Mutate.shadowOffsetVector :: Vector2D -> IO () `*/
	export const shadowOffsetVector = (v : Vector2D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowOffsetX = v.x
			__EXTERNAL__.context.shadowOffsetY = v.y
			return null
		})

	/**` Mutate.shadowOffsetX :: Number -> IO () `*/
	export const shadowOffsetX = (x : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowOffsetX = x
			return null
		})

	/**` Mutate.shadowOffsetY :: Number -> IO () `*/
	export const shadowOffsetY = (y : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.shadowOffsetY = y
			return null
		})

	/**` Mutate.transformationMatrix :: Matrix3x3 -> IO () `*/
	export const transformationMatrix = (m : Matrix3x3) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky)
			return null
		})

	/**` Mutate.alpha :: Number -> IO () `*/
	export const alpha = (opacity : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.globalAlpha = opacity
			return null
		})

	/**` Mutate.compositionOperation :: CompositionOperation -> IO () `*/
	export const compositionOperation = (composition : CompositionOperation) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.globalCompositeOperation = bijectionCompositionOperation.domain(composition)
			return null
		})
}

namespace Effect
{
	export namespace Norm
	{
		/**` Effect.Norm.drawImage :: String -> (8 Number...) -> IO () `*/
		export const drawImage =
			(path  : string) =>
			(cropX : number) => (cropY : number) =>
			(cropW : number) => (cropH : number) =>
			(x     : number) => (y     : number) =>
			(w     : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path] as HTMLImageElement,
					cropX * __EXTERNAL__.image[path]!.width, cropY * __EXTERNAL__.image[path]!.height,
					cropW * __EXTERNAL__.image[path]!.width, cropH * __EXTERNAL__.image[path]!.height,
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.drawImageVector :: String -> Vector2D -> Vector2D -> Vector2D -> Vector2D -> IO () `*/
		export const drawImageVector =
			(path            : string)   =>
			(cropCoordinates : Vector2D) =>
			(cropDimensions  : Vector2D) =>
			(position        : Vector2D) =>
			(dimensions      : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path]!,
					cropCoordinates.x * __EXTERNAL__.image[path]!.width, cropCoordinates.y * __EXTERNAL__.image[path]!.height,
					cropDimensions.x  * __EXTERNAL__.image[path]!.width, cropDimensions.y  * __EXTERNAL__.image[path]!.height,
					position.x   * __EXTERNAL__.context.canvas.width, position.y   * __EXTERNAL__.context.canvas.height,
					dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.drawFullImage :: String -> Number -> Number -> Number -> Number -> IO () `*/
		export const drawFullImage =
			(path  : string) =>
			(x     : number) => (y : number) =>
			(w     : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path]!,
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.drawFullImageVector :: String -> Vector2D -> Vector2D -> IO () `*/
		export const drawFullImageVector =
			(path        : string)   =>
			(coordinates : Vector2D) =>
			(dimensions  : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path]!,
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.drawFixedImage :: String -> Number -> Number -> Number -> IO () `*/
		export const drawFixedImage = (path : string) => (x : number) => (y : number) => (k : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path]!,
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					k * __EXTERNAL__.context.canvas.width, k * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.drawFixedImageVector :: String -> Vector2D -> Number -> IO () `*/
		export const drawFixedImageVector = (path : string) => (coordinates : Vector2D) => (k : number) : IO<null> =>
			IO(() => {
				const l = k * __EXTERNAL__.context.canvas.width
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path]!,
					coordinates.x * __EXTERNAL__.context.canvas.width,
					coordinates.y * __EXTERNAL__.context.canvas.height,
					l, l
				)
				return null
			})

		/**` Effect.Norm.drawScaledImage :: String -> Number -> Number -> Number -> IO () `*/
		export const drawScaledImage = (path : string) => (x : number) => (y : number) => (k : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path]!,
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					k * __EXTERNAL__.context.canvas.width  * __EXTERNAL__.image[path]!.width,
					k * __EXTERNAL__.context.canvas.height * __EXTERNAL__.image[path]!.height
				)
				return null
			})

		/**` Effect.Norm.drawScaledImageVector :: String -> Vector2D -> Number -> IO () `*/
		export const drawScaledImageVector = (path : string) => (coordinates : Vector2D) => (k : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.drawImage(
					__EXTERNAL__.image[path]!,
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					k * __EXTERNAL__.context.canvas.width  * __EXTERNAL__.image[path]!.width,
					k * __EXTERNAL__.context.canvas.height * __EXTERNAL__.image[path]!.height
				)
				return null
			})


		/**` Effect.Norm.clearRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		export const clearRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.clearRect(
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.clearRectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const clearRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.clearRect(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.rotate :: Number -> IO () `*/
		export const rotate = (angle : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.rotate(angle * TAU)
				return null
			})

		/**` Effect.Norm.translate :: Number -> Number -> IO () `*/
		export const translate = (dx : number) => (dy : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.translate(dx * __EXTERNAL__.context.canvas.width, dy * __EXTERNAL__.context.canvas.height)
				return null
			})

		/**` Effect.Norm.translateVector :: Vector2D -> IO () `*/
		export const translateVector = (v : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.translate(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height)
				return null
			})

		/**` Effect.Norm.transformation :: Matrix3x3 -> IO () `*/
		export const transformation = (m : Matrix3x3) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.transform(
					m.ix, m.iy, m.jx, m.jy,
					m.kx * __EXTERNAL__.context.canvas.width,
					m.ky * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.moveTo :: Number -> Number -> IO () `*/
		export const moveTo = (x : number) => (y : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.moveTo(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height)
				return null
			})

		/**` Effect.Norm.moveToVector :: Vector2D -> IO () `*/
		export const moveToVector = (v : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.moveTo(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height)
				return null
			})

		/**` Effect.Norm.lineTo :: Number -> Number -> IO () `*/
		export const lineTo = (x : number) => (y : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.lineTo(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height)
				return null
			})

		/**` Effect.Norm.lineToVector :: Vector2D -> IO () `*/
		export const lineToVector = (v : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.lineTo(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height)
				return null
			})

		/**` Effect.Norm.bezierCurveTo :: (6 Number...) -> IO () `*/
		export const bezierCurveTo =
			(ix : number) => (iy : number) =>
			(jx : number) => (jy : number) =>
			(x  : number) => (y  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.bezierCurveTo(
					ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height,
					jx * __EXTERNAL__.context.canvas.width, jy * __EXTERNAL__.context.canvas.height,
					x  * __EXTERNAL__.context.canvas.width, y  * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.bezierCurveToVector :: Vector2D -> Vector2D -> Vector2D -> IO () `*/
		export const bezierCurveToVector = (i : Vector2D) => (j : Vector2D) => (v : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.bezierCurveTo(
					i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height,
					j.x * __EXTERNAL__.context.canvas.width, j.y * __EXTERNAL__.context.canvas.height,
					v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.quadraticCurveTo :: Number -> Number -> Number -> Number -> IO () `*/
		export const quadraticCurveTo =
			(ix : number) => (iy : number) =>
			(x  : number) => (y  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.quadraticCurveTo(
					ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height,
					x  * __EXTERNAL__.context.canvas.width, y  * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.quadraticCurveToVector :: Vector2D -> Vector2D -> IO () `*/
		export const quadraticCurveToVector = (i : Vector2D) => (v : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.quadraticCurveTo(
					i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height,
					v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height
				)
				return null
			})


		/**` Effect.Norm.arcTo :: (5 Number...) -> IO () `*/
		export const arcTo =
			(ix : number) => (iy : number) =>
			(jx : number) => (jy : number) =>
			(r  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arcTo(
					ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height,
					jx * __EXTERNAL__.context.canvas.width, jy * __EXTERNAL__.context.canvas.height,
					r  * __EXTERNAL__.context.canvas.width
				)
				return null
			})

		/**` Effect.Norm.arcToVector :: Vector2D -> Vector2D -> Number -> IO () `*/
		export const arcToVector = (i : Vector2D) => (j : Vector2D) => (r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arcTo(
					i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height,
					j.x * __EXTERNAL__.context.canvas.width, j.y * __EXTERNAL__.context.canvas.height,
					r   * __EXTERNAL__.context.canvas.width
				)
				return null
			})

		/**` Effect.Norm.rectangle :: Number -> Number -> Number -> Number -> IO () `*/
		export const rectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.rect(
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.rectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const rectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.rect(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.fillRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		export const fillRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillRect(
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.fillRectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const fillRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillRect(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.strokeRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		export const strokeRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeRect(
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.strokeRectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const strokeRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeRect(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.arc :: (5 Number...) -> IO () `*/
		export const arc = (x : number) => (y : number) => (r : number) => (a : number) => (b : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					x * __EXTERNAL__.context.canvas.width,
					y * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width,
					a * TAU, b * TAU
				)
				return null
			})

		/**` Effect.Norm.arcVector :: Vector2D -> Number -> Number -> Number -> IO () `*/
		export const arcVector = (v : Vector2D) => (r : number) => (a : number) => (b : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					v.x * __EXTERNAL__.context.canvas.width,
					v.y * __EXTERNAL__.context.canvas.height,
					r   * __EXTERNAL__.context.canvas.width,
					a   * TAU, b   * TAU
				)
				return null
			})

		/**` Effect.Norm.circle :: Number -> Number -> Number -> IO () `*/
		export const circle = (x : number) => (y : number) => (r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				return null
			})

		/**` Effect.Norm.circleVector :: Vector2D -> Number -> IO () `*/
		export const circleVector = (coordinates : Vector2D) => (r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				return null
			})

		/**` Effect.Norm.strokeCircle :: Number -> Number -> Number -> IO () `*/
		export const strokeCircle = (x : number) => (y : number) => (r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.stroke()
				return null
			})

		/**` Effect.Norm.strokeCircleVector :: Vector2D -> Number -> IO () `*/
		export const strokeCircleVector = (coordinates : Vector2D) => (r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.stroke()
				return null
			})

		/**` Effect.Norm.fillCircle :: Number -> Number -> Number -> IO () `*/
		export const fillCircle = (x : number) => (y : number) => (r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.fill()
				return null
			})

		/**` Effect.Norm.fillCircleVector :: Vector2D -> Number -> IO () `*/
		export const fillCircleVector = (coordinates : Vector2D) => (r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.arc(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.fill()
				return null
			})

		/**` Effect.Norm.elliptic :: (7 Number...) -> IO () `*/
		export const elliptic =
			(x  : number) => (y  : number) =>
			(kx : number) => (ky : number) =>
			(a  : number) => (b  : number) =>
			(r  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					x  * __EXTERNAL__.context.canvas.width, y  * __EXTERNAL__.context.canvas.height,
					kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height,
					r * TAU, a * TAU, b * TAU
				)
				return null
			})

		/**` Effect.Norm.ellipticVector :: Vector2D -> Vector2D -> Number -> Number -> Number -> IO () `*/
		export const ellipticVector =
			(coordinates : Vector2D) => (dimensions : Vector2D) =>
			(a : number) => (b : number) => (r  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					coordinates.x * __EXTERNAL__.context.canvas.width,
					coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width,
					dimensions.y  * __EXTERNAL__.context.canvas.height,
					r * TAU, a * TAU, b * TAU
				)
				return null
			})

		/**` Effect.Norm.ellipse :: (5 Number...) -> IO () `*/
		export const ellipse =
			(x  : number) => (y  : number) =>
			(kx : number) => (ky : number) =>
			(r  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					x  * __EXTERNAL__.context.canvas.width, y  * __EXTERNAL__.context.canvas.height,
					kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height,
					r  * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				return null
			})

		/**` Effect.Norm.ellipseVector :: Vector2D -> Vector2D -> Number -> IO () `*/
		export const ellipseVector =
			(coordinates : Vector2D) =>
			(dimensions  : Vector2D) =>
			(r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				return null
			})

		/**` Effect.Norm.strokeEllipse :: (5 Number...) -> IO () `*/
		export const strokeEllipse =
			(x  : number) => (y  : number) =>
			(kx : number) => (ky : number) =>
			(r  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					x  * __EXTERNAL__.context.canvas.width, y  * __EXTERNAL__.context.canvas.height,
					kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height,
					r  * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.stroke()
				return null
			})

		/**` Effect.Norm.strokeEllipseVector :: Vector2D -> Vector2D -> Number -> IO () `*/
		export const strokeEllipseVector =
			(coordinates : Vector2D) =>
			(dimensions  : Vector2D) =>
			(r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.stroke()
				return null
			})

		/**` Effect.Norm.fillEllipse :: (5 Number...) -> IO () `*/
		export const fillEllipse =
			(x  : number) => (y  : number) =>
			(kx : number) => (ky : number) =>
			(r  : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					x  * __EXTERNAL__.context.canvas.width, y  * __EXTERNAL__.context.canvas.height,
					kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height,
					r  * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.fill()
				return null
			})

		/**` Effect.Norm.fillEllipseVector :: Vector2D -> Vector2D -> Number -> IO () `*/
		export const fillEllipseVector =
			(coordinates : Vector2D) =>
			(dimensions  : Vector2D) =>
			(r : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.ellipse(
					coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height,
					dimensions.x  * __EXTERNAL__.context.canvas.width, dimensions.y  * __EXTERNAL__.context.canvas.height,
					r * __EXTERNAL__.context.canvas.width, 0, TAU
				)
				__EXTERNAL__.context.fill()
				return null
			})

		/**` Effect.Norm.strokeText :: String -> Number -> Number -> IO () `*/
		export const strokeText = (text : string) => (x : number) => (y : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeText(
					text, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.strokeTextVector :: String -> Vector2D -> IO () `*/
		export const strokeTextVector = (text : string) => (coordinates : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeText(
					text, coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.width
				)
				return null
			})

		/**` Effect.Norm.fillText :: String -> Number -> Number -> IO () `*/
		export const fillText = (text : string) => (x : number) => (y : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillText(
					text, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height
				)
				return null
			})

		/**` Effect.Norm.fillTextVector :: String -> Vector2D -> IO () `*/
		export const fillTextVector = (text : string) => (coordinates : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillText(
					text, coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.width
				)
				return null
			})

		/**` Effect.Norm.area :: Number -> Number -> Number -> Number -> IO () `*/
		export const area = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.rect(
					ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height,
					(jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height
				), null
			))

		/**` Effect.Norm.areaVector :: Vector2D -> Vector2D -> IO () `*/
		export const areaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.rect(
					i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height,
					(j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height
				), null
			))

		/**` Effect.Norm.strokeArea :: Number -> Number -> Number -> Number -> IO () `*/
		export const strokeArea = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.strokeRect(
					ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height,
					(jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height
				), null
			))

		/**` Effect.Norm.strokeAreaVector :: Vector2D -> Vector2D -> IO () `*/
		export const strokeAreaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.strokeRect(
					i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height,
					(j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height
				), null
			))

		/**` Effect.Norm.fillArea :: Number -> Number -> Number -> Number -> IO () `*/
		export const fillArea = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.fillRect(
					ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height,
					(jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height
				), null
			))

		/**` Effect.Norm.fillAreaVector :: Vector2D -> Vector2D -> IO () `*/
		export const fillAreaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.fillRect(
					i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height,
					(j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height
				), null
			))
	}

	/**` Effect.log :: String -> IO () `*/
	export const log = (message : string) : IO<null> =>
		IO(() => (console.log(message), null))

	/**` Effect.flush :: IO () `*/
	export const flush : IO<null> =
		IO(() => (console.clear(), null))

	/**` Effect.queue :: IO a -> IO () `*/
	export const queue = (io : IO<any>) : IO<null> =>
		IO(() => (io.INFO(), null))

	/**` Effect.tick :: IO () `*/
	export const tick : IO<null> =
		IO(() => {
			for (const k in __EXTERNAL__.keyboard)
				__EXTERNAL__.keyboard[k as KeyboardKey] = relaxVertical(__EXTERNAL__.keyboard[k as KeyboardKey])
			for (const i in __EXTERNAL__.mouse.buttons)
				__EXTERNAL__.mouse.buttons[i] = relaxVertical(__EXTERNAL__.mouse.buttons[i]!)
			__EXTERNAL__.mouse.scroll = Vertical.CenterY
			__EXTERNAL__.mouse.deltaX = 0
			__EXTERNAL__.mouse.deltaY = 0
			return null
		})

	/**` Effect.activatePointerLock :: IO () `*/
	export const activatePointerLock : IO<null> =
		IO(() => {
			__EXTERNAL__.context.canvas.onmousedown = () =>
			{
				if (!__EXTERNAL__.isPointerLocked) __EXTERNAL__.context.canvas.requestPointerLock()
			}
			return null
		})

	/**` Effect.deactivatePointerLock :: IO () `*/
	export const deactivatePointerLock : IO<null> =
		IO(() => {
			__EXTERNAL__.context.canvas.onmousedown = null
			return null
		})

	/**` Effect.loadImage :: String -> IO () `*/
	export const loadImage = (path : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.image[path]          = new Image
			__EXTERNAL__.image[path]!.src     = path
			__EXTERNAL__.image[path]!.onerror = () => THROW(`Could not load image: '${path}'`)
			return null
		})

	/**` Effect.drawImage :: String -> (8 Number...) -> IO () `*/
	export const drawImage =
		(path  : string) =>
		(cropX : number) => (cropY : number) =>
		(cropW : number) => (cropH : number) =>
		(x     : number) => (y     : number) =>
		(w     : number) => (h : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path] as HTMLImageElement, cropX, cropY, cropW, cropH, x, y, w, h)
			return null
		})

	/**` Effect.drawImageVector :: String -> Vector2D -> Vector2D -> Vector2D -> Vector2D -> IO () `*/
	export const drawImageVector =
		(path            : string)   =>
		(cropCoordinates : Vector2D) =>
		(cropDimensions  : Vector2D) =>
		(position        : Vector2D) =>
		(dimensions      : Vector2D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.drawImage(
				__EXTERNAL__.image[path]!,
				cropCoordinates.x, cropCoordinates.y, cropDimensions.x, cropDimensions.y,
				position.x, position.y, dimensions.x, dimensions.y
			)
			return null
		})

	/**` Effect.drawFullImage :: String -> Number -> Number -> Number -> Number -> IO () `*/
	export const drawFullImage =
		(path  : string) =>
		(x     : number) => (y : number) =>
		(w     : number) => (h : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path]!, x, y, w, h), null))

	/**` Effect.drawFullImageVector :: String -> Vector2D -> Vector2D -> IO () `*/
	export const drawFullImageVector =
		(path        : string)   =>
		(coordinates : Vector2D) =>
		(dimensions  : Vector2D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path]!, coordinates.x, coordinates.y, dimensions.x, dimensions.y)
			return null
		})

	/**` Effect.drawFixedImage :: String -> Number -> Number -> Number -> IO () `*/
	export const drawFixedImage = (path : string) => (x : number) => (y : number) => (k : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path]!, x, y, k, k), null))

	/**` Effect.drawFixedImageVector :: String -> Vector2D -> Number -> IO () `*/
	export const drawFixedImageVector = (path : string) => (coordinates : Vector2D) => (k : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path]!, coordinates.x, coordinates.y, k, k), null))

	/**` Effect.drawScaledImage :: String -> Number -> Number -> Number -> IO () `*/
	export const drawScaledImage = (path : string) => (x : number) => (y : number) => (k : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.drawImage(
				__EXTERNAL__.image[path]!, x, y,
				k * __EXTERNAL__.image[path]!.width,
				k * __EXTERNAL__.image[path]!.height
			)
			return null
		})

	/**` Effect.drawScaledImageVector :: String -> Vector2D -> Number -> IO () `*/
	export const drawScaledImageVector = (path : string) => (coordinates : Vector2D) => (k : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.drawImage(
				__EXTERNAL__.image[path]!,
				coordinates.x, coordinates.y,
				k * __EXTERNAL__.image[path]!.width,
				k * __EXTERNAL__.image[path]!.height
			)
			return null
		})

	/**` Effect.loadAudio :: String -> IO () `*/
	export const loadAudio = (path : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.audio[path]          = new Audio(path)
			__EXTERNAL__.audio[path]!.onerror = () => THROW(`Could not load audio: '${path}'`)
			return null
		})

	/**` Effect.playAudio :: String -> IO () `*/
	export const playAudio = (path : string) : IO<null> =>
		IO(() => ((__EXTERNAL__.audio[path] || THROW(`Audio not preloaded: '${path}'`) ).play(), null))

	/**` Effect.playSFX :: String -> IO () `*/
	export const playSFX = (path : string) : IO<null> =>
		IO(() => (((__EXTERNAL__.audio[path] || THROW(`Audio not preloaded: '${path}'`)).cloneNode() as any).play(), null))

	/**` Effect.loadFont :: String -> IO () `*/
	export const loadFont = (path : string) : IO<null> =>
		IO(() => {
			document.styleSheets[0]!.insertRule(
				`@font-face{font-family:"${path.slice(path.lastIndexOf("/") + 1, path.lastIndexOf("."))}";src:url("${path}")}`
			)
			return null
		})

	/**` Effect.clearRectangle :: Number -> Number -> Number -> Number -> IO () `*/
	export const clearRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.clearRect(x, y, w, h), null))

	/**` Effect.clearRectangleVector :: Vector2D -> Vector2D -> IO () `*/
	export const clearRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.clearRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null))

	/**` Effect.clearCanvas :: IO () `*/
	export const clearCanvas : IO<null> =
		IO(() => {
			__EXTERNAL__.context.clearRect(0, 0, __EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height)
			return null
		})

	/**` Effect.fill :: IO () `*/
	export const fill : IO<null> =
		IO(() => (__EXTERNAL__.context.fill(), null))

	/**` Effect.stroke :: IO () `*/
	export const stroke : IO<null> =
		IO(() => (__EXTERNAL__.context.stroke(), null))

	/**` Effect.save :: IO () `*/
	export const save : IO<null> =
		IO(() => (__EXTERNAL__.context.save(), null))

	/**` Effect.restore :: IO () `*/
	export const restore : IO<null> =
		IO(() => (__EXTERNAL__.context.restore(), null))

	/**` Effect.clipEvenOdd :: IO () `*/
	export const clipEvenOdd : IO<null> =
		IO(() => (__EXTERNAL__.context.clip('evenodd'), null))

	/**` Effect.clipNonZero :: IO () `*/
	export const clipNonZero : IO<null> =
		IO(() => (__EXTERNAL__.context.clip('nonzero'), null))

	/**` Effect.rotate :: Number -> IO () `*/
	export const rotate = (angle : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.rotate(angle), null))

	/**` Effect.scale :: Number -> IO () `*/
	export const scale = (k : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.scale(k, k), null))

	/**` Effect.scaleAxis :: Number -> Number -> IO () `*/
	export const scaleAxis = (kx : number) => (ky : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.scale(kx, ky), null))

	/**` Effect.scaleAxisVector :: Vector2D -> IO () `*/
	export const scaleAxisVector = (v : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.scale(v.x, v.y), null))

	/**` Effect.translate :: Number -> Number -> IO () `*/
	export const translate = (dx : number) => (dy : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.translate(dx, dy), null))

	/**` Effect.translateVector :: Vector2D -> IO () `*/
	export const translateVector = (v : Vector2D) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.translate(v.x, v.y)
			return null
		})

	/**` Effect.transformation :: Matrix3x3 -> IO () `*/
	export const transformation = (m : Matrix3x3) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.transform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky)
			return null
		})

	/**` Effect.beginPath :: IO () `*/
	export const beginPath : IO<null> =
		IO(() => (__EXTERNAL__.context.beginPath(), null))

	/**` Effect.closePath :: IO () `*/
	export const closePath : IO<null> =
		IO(() => (__EXTERNAL__.context.closePath(), null))

	/**` Effect.moveTo :: Number -> Number -> IO () `*/
	export const moveTo = (x : number) => (y : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.moveTo(x, y), null))

	/**` Effect.moveToVector :: Vector2D -> IO () `*/
	export const moveToVector = (v : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.moveTo(v.x, v.y), null))

	/**` Effect.lineTo :: Number -> Number -> IO () `*/
	export const lineTo = (x : number) => (y : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.lineTo(x, y), null))

	/**` Effect.lineToVector :: Vector2D -> IO () `*/
	export const lineToVector = (v : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.lineTo(v.x, v.y), null))

	/**` Effect.bezierCurveTo :: (6 Number...) -> IO () `*/
	export const bezierCurveTo =
		(ix : number) => (iy : number) =>
		(jx : number) => (jy : number) =>
		(x  : number) => (y  : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.bezierCurveTo(ix, iy, jx, jy, x, y), null))

	/**` Effect.bezierCurveToVector :: Vector2D -> Vector2D -> Vector2D -> IO () `*/
	export const bezierCurveToVector = (i : Vector2D) => (j : Vector2D) => (v : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.bezierCurveTo(i.x, i.y, j.x, j.y, v.x, v.y), null))

	/**` Effect.quadraticCurveTo :: Number -> Number -> Number -> Number -> IO () `*/
	export const quadraticCurveTo =
		(ix : number) => (iy : number) =>
		(x  : number) => (y  : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.quadraticCurveTo(ix, iy, x, y), null))

	/**` Effect.quadraticCurveToVector :: Vector2D -> Vector2D -> IO () `*/
	export const quadraticCurveToVector = (i : Vector2D) => (v : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.quadraticCurveTo(i.x, i.y, v.x, v.y), null))

	/**` Effect.arcTo :: (5 Number...) -> IO () `*/
	export const arcTo =
		(ix : number) => (iy : number) =>
		(jx : number) => (jy : number) =>
		(r  : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arcTo(ix, iy, jx, jy, r), null))

	/**` Effect.arcToVector :: Vector2D -> Vector2D -> Number -> IO () `*/
	export const arcToVector = (i : Vector2D) => (j : Vector2D) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arcTo(i.x, i.y, j.x, j.y, r), null))

	/**` Effect.rectangle :: Number -> Number -> Number -> Number -> IO () `*/
	export const rectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.rect(x, y, w, h), null))

	/**` Effect.rectangleVector :: Vector2D -> Vector2D -> IO () `*/
	export const rectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.rect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null))

	/**` Effect.fillRectangle :: Number -> Number -> Number -> Number -> IO () `*/
	export const fillRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillRect(x, y, w, h), null))

	/**` Effect.fillRectangleVector :: Vector2D -> Vector2D -> IO () `*/
	export const fillRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null))

	/**` Effect.strokeRectangle :: Number -> Number -> Number -> Number -> IO () `*/
	export const strokeRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeRect(x, y, w, h), null))

	/**` Effect.strokeRectangleVector :: Vector2D -> Vector2D -> IO () `*/
	export const strokeRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null))

	/**` Effect.arc :: (5 Number...) -> IO () `*/
	export const arc = (x : number) => (y : number) => (r : number) => (a : number) => (b : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(x, y, r, a, b), null))

	/**` Effect.arcVector :: Vector2D -> Number -> Number -> Number -> IO () `*/
	export const arcVector = (v : Vector2D) => (r : number) => (a : number) => (b : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(v.x, v.y, r, a, b), null))

	/**` Effect.circle :: Number -> Number -> Number -> IO () `*/
	export const circle = (x : number) => (y : number) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), null))

	/**` Effect.circleVector :: Vector2D -> Number -> IO () `*/
	export const circleVector = (coordinates : Vector2D) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), null))

	/**` Effect.strokeCircle :: Number -> Number -> Number -> IO () `*/
	export const strokeCircle = (x : number) => (y : number) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.stroke(), null))

	/**` Effect.strokeCircleVector :: Vector2D -> Number -> IO () `*/
	export const strokeCircleVector = (coordinates : Vector2D) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.stroke(), null))

	/**` Effect.fillCircle :: Number -> Number -> Number -> IO () `*/
	export const fillCircle = (x : number) => (y : number) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.fill(), null))

	/**` Effect.fillCircleVector :: Vector2D -> Number -> IO () `*/
	export const fillCircleVector = (coordinates : Vector2D) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.fill(), null))

	/**` Effect.elliptic :: (7 Number...) -> IO () `*/
	export const elliptic =
		(x  : number) => (y  : number) =>
		(kx : number) => (ky : number) =>
		(a  : number) => (b  : number) =>
		(r  : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, a, b), null))

	/**` Effect.ellipticVector :: Vector2D -> Vector2D -> Number -> Number -> Number -> IO () `*/
	export const ellipticVector =
		(coordinates : Vector2D) => (dimensions : Vector2D) =>
		(a : number) => (b : number) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, a, b), null))

	/**` Effect.ellipse :: (5 Number...) -> IO () `*/
	export const ellipse = (x : number) => (y : number) => (kx : number) => (ky : number) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), null))

	/**` Effect.ellipseVector :: Vector2D -> Vector2D -> Number -> IO () `*/
	export const ellipseVector = (coordinates : Vector2D) => (dimensions : Vector2D) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU), null))

	/**` Effect.strokeEllipse :: (5 Number...) -> IO () `*/
	export const strokeEllipse = (x : number) => (y : number) => (kx : number) => (ky : number) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), __EXTERNAL__.context.stroke(), null))

	/**` Effect.strokeEllipseVector :: Vector2D -> Vector2D -> Number -> IO () `*/
	export const strokeEllipseVector = (coordinates : Vector2D) => (dimensions : Vector2D) => (r : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU)
			__EXTERNAL__.context.stroke()
			return null
		})

	/**` Effect.fillEllipse :: (5 Number...) -> IO () `*/
	export const fillEllipse = (x : number) => (y : number) => (kx : number) => (ky : number) => (r : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), __EXTERNAL__.context.fill(), null))

	/**` Effect.fillEllipseVector :: Vector2D -> Vector2D -> Number -> IO () `*/
	export const fillEllipseVector = (coordinates : Vector2D) => (dimensions : Vector2D) => (r : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU)
			__EXTERNAL__.context.fill()
			return null
		})

	/**` Effect.strokeText :: String -> Number -> Number -> IO () `*/
	export const strokeText = (text : string) => (x : number) => (y : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeText(text, x, y), null))

	/**` Effect.strokeTextVector :: String -> Vector2D -> IO () `*/
	export const strokeTextVector = (text : string) => (coordinates : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeText(text, coordinates.x, coordinates.y), null))

	/**` Effect.fillText :: String -> Number -> Number -> IO () `*/
	export const fillText = (text : string) => (x : number) => (y : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillText(text, x, y), null))

	/**` Effect.fillTextVector :: String -> Vector2D -> IO () `*/
	export const fillTextVector = (text : string) => (coordinates : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillText(text, coordinates.x, coordinates.y), null))

	/**` Effect.area :: Number -> Number -> Number -> Number -> IO () `*/
	export const area = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.rect(ix, iy, jx - ix, jy - iy), null))

	/**` Effect.areaVector :: Vector2D -> Vector2D -> IO () `*/
	export const areaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.rect(i.x, i.y, j.x - i.x, j.y - i.y), null))

	/**` Effect.strokeArea :: Number -> Number -> Number -> Number -> IO () `*/
	export const strokeArea = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeRect(ix, iy, jx - ix, jy - iy), null))

	/**` Effect.strokeAreaVector :: Vector2D -> Vector2D -> IO () `*/
	export const strokeAreaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeRect(i.x, i.y, j.x - i.x, j.y - i.y), null))

	/**` Effect.fillArea :: Number -> Number -> Number -> Number -> IO () `*/
	export const fillArea = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillRect(ix, iy, jx - ix, jy - iy), null))

	/**` Effect.fillAreaVector :: Vector2D -> Vector2D -> IO () `*/
	export const fillAreaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillRect(i.x, i.y, j.x - i.x, j.y - i.y), null))
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

	document.onpointerlockchange = () =>
	{
		__EXTERNAL__.isPointerLocked = document.pointerLockElement === __EXTERNAL__.context.canvas
	}
}
