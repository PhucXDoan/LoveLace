/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable no-return-assign                  */
/* eslint-disable no-plusplus                       */
/* eslint-disable no-param-reassign                 */
/* eslint-disable no-console                        */
/* eslint-disable no-multi-assign                   */

/********************************************************************************************************************************/

/*
class Eq e where
	(.eq) :: e -> e -> Boolean

class Pipeable p where
	(.pipe) :: p -> (p -> a) -> a

class Chainable c where
	then :: c a -> c b -> c b
	side :: c a -> c b -> c a
	also :: c a -> (a -> c b) -> c a

class Monoid m where
	(.plus) :: m a -> m a -> m a

class Monad m where
	(.bind)   :: m a -> (a -> m b) -> m b
	(.bindto) :: m $ -> String -> ($ -> m a) -> m $
	(.fmap)   :: m a -> (a -> b) -> m b
	(.fmapto) :: m $ -> String -> ($ -> a) -> m $
	(.cast)   :: m a -> b -> m b
*/

/********************************************************************************************************************************/

// -- Throws am error via a function call.
const THROW = (message : string) =>
	{ throw new Error(message) }

// -- Throws a type error via a function call.
const THROWTYPE = (message : string) =>
	{ throw new TypeError(message) }

// -- Throws a range error via a function call.
const THROWRANGE = (message : string) =>
	{ throw new RangeError(message) }

// -- Used at the end of pattern matching if it is exhuastive.
const never = undefined as never

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

/**` apply :: a -> (a -> b) -> b `*/
const apply = <a>(x : a) => <b>(f : (x : a) => b) : b => f(x)

/**` approx :: Number -> Number -> Number -> Boolean `*/
const approx = (x : number) => (y : number) => (error : number) : boolean =>
	Math.abs(x - y) < error

/**` napprox :: Number -> Number -> Number -> Boolean `*/
const napprox = (x : number) => (y : number) => (error : number) : boolean =>
	Math.abs(x - y) > error

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

/**` BIT :: Boolean -> Number `*/
const BIT = (x : boolean) : (0 | 1) => x ? 1 : 0

/**` rboth :: (a -> b) -> (a, a) -> (b, b) `*/
const rboth = <a>(pair : [a, a]) => <b>(f : (x : a) => b) : [b, b] =>
	[f(pair[0]), f(pair[1])]

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

/**` diff :: Number -> Number -> Number `*/
const diff = (x : number) => (y : number) : number => Math.abs(x - y)

/**` div :: Number -> Number -> Number `*/
const div = (x : number) => (y : number) : number => x / y

/**` rdiv :: Number -> Number -> Number `*/
const rdiv = (y : number) => (x : number) : number => x / y

/**` eq :: a -> a -> Boolean `*/
const eq = <a>(x : a) => (y : a) : boolean => x === y

/**` even :: Number -> Boolean `*/
const even = (x : number) : boolean => x % 2 === 0

/**` exp :: Number -> Number `*/
const exp = Math.exp

/**` expm1 :: Number -> Number `*/
const expm1 = Math.expm1

/**` floor :: Number -> Number `*/
const floor = Math.floor

/**` fround :: Number -> Number `*/
const fround = Math.fround

/**` fst :: (a, b) -> a `*/
const fst = <a, b>(pair : [a, b]) : a => pair[0]

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
const isInsideExclusive = (n : number) => (lower : number) => (upper : number) : boolean =>
	lower < n && n < upper

/**` isInsideInclusive :: Number -> Number -> Number -> Boolean `*/
const isInsideInclusive = (n : number) => (lower : number) => (upper : number) : boolean =>
	lower <= n && n <= upper

/**` isOutsideExclusive :: Number -> Number -> Number -> Boolean `*/
const isOutsideExclusive = (n : number) => (lower : number) => (upper : number) : boolean =>
	n < lower || upper < n

/**` isOutsideInclusive :: Number -> Number -> Number -> Boolean `*/
const isOutsideInclusive = (n : number) => (lower : number) => (upper : number) : boolean =>
	n <= lower && upper <= n

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

/**` odd :: Number -> Boolean `*/
const odd = (x : number) : boolean => Math.abs(x) % 2 === 1

/**` OR :: Number -> Number -> Number `*/
const OR = (x : number) => (y : number) : number => x | y

/**` or :: Boolean -> Boolean -> Boolean `*/
const or = (x : boolean) => (y : boolean) : boolean => x || y

/**` nor :: Boolean -> Boolean -> Boolean `*/
const nor = (x : boolean) => (y : boolean) : boolean => !(x || y)

/**` pick :: Boolean -> (a, a) -> a `*/
const pick = (condition : boolean) : <a>(pair : [a, a]) => a =>
	condition ? fst : snd

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

/**` snd :: (a, b) -> b `*/
const snd = <a, b>(pair : [a, b]) : b => pair[1]

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
const toHexColor = (decimal : number) : string =>
	decimal >= 0 && decimal <= 16777215 && Number.isInteger(decimal)
		? `#${decimal.toString(16).padStart(6, '0')}`
		: THROWTYPE (`'toHexColor' requires a non-negative integer below '0xffffff' ('16777215'); received '${decimal}' instead`)

/**` trunc :: Number -> Number `*/
const trunc = Math.trunc

/**` qtrunc :: Number -> Number `*/
const qtrunc = (x : number) : number => ~~x

/**` URSHIFT :: Number -> Number `*/
const URSHIFT = (x : number) => (y : number) : number => x >>> y

/**` rURSHIFT :: Number -> Number `*/
const rURSHIFT = (y : number) => (x : number) : number => x >>> y

/**` XOR :: Number -> Number -> Number `*/
const XOR = (x : number) => (y : number) : number => x ^ y

/**` xor :: Boolean -> Boolean -> Boolean `*/
const xor = (x : boolean) => (y : boolean) : boolean => x !== y

/********************************************************************************************************************************/

/**` Boolean (Eq, Pipeable) `*/
interface Boolean
{
	eq   : (value : boolean) => boolean
	pipe : <a>(morphism : (bool : boolean) => a) => a
}

Boolean.prototype.eq = function(x)
	{ return this === x }

Boolean.prototype.pipe = function(f)
	{ return f(this as boolean) }

/**` Boolean (Eq, Pipeable) `*/
interface Number
{
	eq   : (value : number) => boolean
	pipe : <a>(morphism : (num : number) => a) => a
}

Number.prototype.eq = function(x)
	{ return this === x }

Number.prototype.pipe = function(f)
	{ return f(this as number) }

/**` String (Eq, Pipeable) `*/
interface String
{
	eq   : (value : string) => boolean
	pipe : <a>(morphism : (str : string) => a) => a
}

String.prototype.eq = function(x)
	{ return this === x }

String.prototype.pipe = function(f)
	{ return f(this as string) }

/********************************************************************************************************************************/

/**` IO (Pipeable, Chainable, Monad) `*/
type IO<a> =
	{
		CONS : 'IO'
		INFO : () => a

		/**` (.pipe) :: IO a -> (IO a -> b) -> b `*/
		pipe : <b>(morphism : (io : IO<a>) => b) => b

		/**` (.then) :: IO a -> IO b -> IO b `*/
		then : <b>(successor : IO<b>) => IO<b>

		/**` (.side) :: IO a -> IO b -> IO a `*/
		side : <b>(effect : IO<b>) => IO<a>

		/**` (.also) :: IO a -> (a -> IO b) -> IO a `*/
		also : <b>(branch : (evaluation : a) => IO<b>) => IO<a>

		/**` (.bind) :: IO a -> (a -> IO b) -> IO b `*/
		bind : <b>(reaction : (evaluation : a) => IO<b>) => IO<b>

		/**` (.bindto) :: IO $ -> String -> ($ -> IO a) -> IO $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => IO<b>) => IO<a & { [x in k] : b }>

		/**` (.fmap) :: IO a -> (a -> b) -> IO b `*/
		fmap : <b>(computation : (evaluation : a) => b) => IO<b>

		/**` (.fmapto) :: IO $ -> String -> ($ -> a) -> IO $ `*/
		fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => IO<a & { [x in k] : b }>

		/**` (.cast) :: IO a -> b -> IO b `*/
		cast : <b>(replacement : b) => IO<b>
	}

/**` IO :: (() ~> a) -> IO a `*/
const IO = <a>(sideeffect : () => a) : IO<a> =>
	({
		CONS : 'IO',
		INFO : sideeffect,
		get pipe() { return (f : any) => f (this) },
		then : x => IO (() => (sideeffect (), x.INFO ())),
		side : x => IO (() => {
			const y = sideeffect ()
			return x.INFO (), y
		}),
		also : x => IO (() => {
			const y = sideeffect ()
			return x (y).INFO (), y
		}),
		bind : f => IO (() => f (sideeffect ()).INFO ()),
		bindto : x => f =>
			IO (() => {
				const $ = sideeffect ()
				return { ...$, [x]: f ($).INFO () } as any
			}),
		fmap : f => IO (() => f (sideeffect ())),
		fmapto : x => f =>
			IO (() => {
				const $ = sideeffect ()
				return { ...$, [x]: f ($) } as any
			}),
		cast : x => IO (() => (sideeffect(), x))
	})

/**` idle :: IO () `*/
const idle : IO<null> =
	IO(() => null)

/**` when :: Boolean -> IO a -> IO () `*/
const when = (condition : boolean) => <a>(io : IO<a>) : IO<null> =>
	condition ? io .fmap (_ => null) : idle

/**` sequenceIOs :: List (IO a) -> IO (List a) `*/
const sequenceIOs = <a>(ios : List<IO<a>>) : IO<List<a>> =>
	IO(() => ios .fmap (io => io.INFO()))

/**` executeIOs :: List (IO a) -> IO () `*/
const executeIOs = <a>(ios : List<IO<a>>) : IO<null> =>
	IO(() => {
		while (ios.CONS === 'Cons') ios.INFO.head.INFO(), ios = ios.INFO.tail
		return null
	})

/********************************************************************************************************************************/

/** `Maybe (Eq, Pipeable, Monad)` */
type Maybe<a> =
	({
		CONS : 'Nothing'
	} | {
		CONS : 'Just'
		INFO : a
	}) & {
		/**` (.eq) :: Maybe a -> Maybe a -> Boolean `*/
		eq   : (value : Maybe<a>) => boolean

		/**` (.pipe) :: Maybe a -> (Maybe a -> b) -> b `*/
		pipe : <b>(morphism : (maybe : Maybe<a>) => b) => b

		/**` (.bind) :: Maybe a -> (a -> Maybe b) -> Maybe b `*/
		bind : <b>(reaction : (evaluation : a) => Maybe<b>) => Maybe<b>

		/**` (.bindto) :: Maybe $ -> String -> ($ -> Maybe a) -> Maybe $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => Maybe<b>) => Maybe<a & { [x in k] : b }>

		/**` (.fmap) :: Maybe a -> (a -> b) -> Maybe b `*/
		fmap : <b>(computation : (evaluation : a) => b) => Maybe<b>

		/**` (.fmapto) :: Maybe $ -> String -> ($ -> a) -> Maybe $ `*/
		fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => Maybe<a & { [x in k] : b }>

		/**` (.cast) :: Maybe a -> b -> Maybe b `*/
		cast : <b>(replacement : b) => Maybe<b>
	}

/**` Nothing :: Maybe a `*/
const Nothing : Maybe<any> =
	{
		CONS   : 'Nothing',
		eq     : x => x === Nothing,
		pipe   : f => f (Nothing),
		bind   : _ => Nothing,
		bindto : _ => _ => Nothing,
		fmap   : _ => Nothing,
		fmapto : _ => _ => Nothing,
		cast   : _ => Nothing
	}

/**` Just :: a -> Maybe a `*/
const Just = <a>(value : a) : Maybe<a> =>
	({
		CONS : 'Just',
		INFO : value,
		eq : x => x.CONS === 'Just' && (x.INFO as any) .eq (value),
		get pipe() { return (f : any) => f (this) },
		bind : f =>
		{
			const x = f (value)
			return x.CONS === 'Nothing' ? Nothing : x
		},
		bindto : x => f =>
		{
			const y = f (value)
			return y.CONS === 'Nothing' ? Nothing : Just ({ ...value, [x]: y.INFO }) as any
		},
		fmap : f => Just (f (value)),
		fmapto : x => f => Just ({ ...value, [x]: f (value) }) as any,
		cast : x => Just (x)
	})

/********************************************************************************************************************************/

/** `State (Pipeable, Chainable, Monad)` */
type State<s, a> =
	{
		CONS : 'State'
		INFO : (inputState : s) => [s, a]

		/**` (.pipe) :: State s a -> (State s a -> b) -> b `*/
		pipe : <b>(morphism : (state : State<s, a>) => b) => b

		/**` (.then) :: State s a -> State s b -> State s b `*/
		then : <b>(successor : State<s, b>) => State<s, b>

		/**` (.side) :: State s a -> State s b -> State s a `*/
		side : <b>(effect : State<s, b>) => State<s, a>

		/**` (.also) :: State s a -> (a -> State s b) -> State s a `*/
		also : <b>(branch : (stateComputationOutput : a) => State<s, b>) => State<s, a>

		/**` (.bind) :: State s a -> (a -> State s b) -> State s b `*/
		bind : <b>(reaction : (stateComputationOutput : a) => State<s, b>) => State<s, b>

		/**` (.bindto) :: State s $ -> String -> ($ -> State s a) -> State s $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => State<s, b>) => State<s, a & { [x in k] : b }>

		/**` (.fmap) :: State s a -> (a -> b) -> State s b `*/
		fmap : <b>(computation : (stateComputationOutput : a) => b) => State<s, b>

		/**` (.fmapto) :: State s $ -> String -> ($ -> b) -> State s $ `*/
		fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => State<s, a & { [x in k] : b }>

		/**` (.cast) :: State s a -> b -> State s b `*/
		cast : <b>(replacement : b) => State<s, b>
	}

/**` State :: (s -> (s, a)) -> State s a `*/
const State = <s, a>(statefulComputation : (inputState : s) => [s, a]) : State<s, a> =>
	({
		CONS : 'State',
		INFO : statefulComputation,
		get pipe() { return (f : any) => f (this) },
		then : s => State (x => s.INFO (statefulComputation (x)[0])),
		side : s => State (x => {
			const y = statefulComputation (x)
			return [s.INFO (y[0])[0], y[1]]
		}),
		also : f => State (x => {
			const y = statefulComputation (x)
			return [f (y[1]).INFO (y[0])[0], y[1]]
		}),
		bind : f =>
			State (x => {
				const [y, z] = statefulComputation (x)
				return f (z).INFO (y)
			}),
		bindto : k => f =>
			State (x => {
				const [y, $] = statefulComputation (x)
				const [z, w] = f ($).INFO (y)
				return [z, { ...$, [k]: w }] as any
			}),
		fmap : f =>
			State (x => {
				const [y, z] = statefulComputation (x)
				return [y, f (z)]
			}),
		fmapto : k => f =>
			State (x => {
				const [y, $] = statefulComputation (x)
				return [y, { ...$, [k]: f ($) }] as any
			}),
		cast : x => State (y => [statefulComputation (y)[0], x])
	})

/**` pseudoRandom :: State Number Number `*/
const pseudoRandom : State<number, number> =
	State(seed => [
		(-67 * seed * seed * seed + 23 * seed * seed - 91 * seed + 73) % 65536,
		Math.abs(97 * seed * seed * seed + 91 * seed * seed - 83 * seed + 79) % 65536 / 65536
	])

/********************************************************************************************************************************/

/** Stores cached information about a linked list. */
type LIST_CACHE<a> =
	{
		head    : a
		tail    : List<a>
		last    : a
		init    : List<a>
		len     : number
		reverse : List<a>
	}

/** `List` (Eq, Pipeable, Monoid, Monad) */
type List<a> =
	({
		CONS : 'Nil'
	} | {
		CONS : 'Cons'
	}) & {
		INFO : { CACHE : Partial<LIST_CACHE<a>> } & LIST_CACHE<a>

		/**` (.eq) :: List a -> List a -> Boolean `*/
		eq : (xs : List<a>) => boolean

		/**` (.pipe) :: List a -> (List a -> b) -> b `*/
		pipe : <b>(morphism : (list : List<a>) => b) => b

		/**` (.plus) :: List a -> List a -> List a `*/
		plus : (successor : List<a>) => List<a>

		/**` (.bind) :: List a -> (a -> List b) -> List b `*/
		bind : <b>(reaction : (element : a) => List<b>) => List<b>

		/**` (.bindto) :: List $ -> String -> List $ -> ($ -> List a) -> List $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => List<b>) => List<a & { [x in k] : b }>

		/**` (.fmap) :: List a -> (a -> b) -> List b `*/
		fmap : <b>(computation : (element : a) => b) => List<b>

		/**` (.fmapto) :: List $ -> String -> List $ -> ($ -> a) -> List $ `*/
		fmapto : <k extends string>(name : k) => <b>(computation : ($ : a) => b) => List<a & { [x in k] : b }>

		/**` (.cast) :: List a -> b -> List b `*/
		cast : <b>(replacement : b) => List<b>
	}

/**` Nil :: List a `*/
const Nil : List<any> =
	(() => {
		const self : List<any> =
			{
				CONS : 'Nil',
				INFO :
				{
					CACHE :
					{
						len     : 0,
						reverse : undefined as any
					},
					len     : 0,
					reverse : undefined as any,
					get head () { return THROWRANGE (`'head' cannot be used on 'Nil' (an empty 'List')`) },
					get tail () { return THROWRANGE (`'tail' cannot be used on 'Nil' (an empty 'List')`) },
					get last () { return THROWRANGE (`'last' cannot be used on 'Nil' (an empty 'List')`) },
					get init () { return THROWRANGE (`'init' cannot be used on 'Nil' (an empty 'List')`) }
				},
				eq     : xs => xs === Nil,
				pipe   : f  => f (Nil),
				plus   : id,
				bind   : _  => Nil,
				bindto : _  => _ => Nil,
				fmap   : _  => Nil,
				fmapto : _  => _ => Nil,
				cast   : _  => Nil
			}

		return self.INFO.reverse = self.INFO.CACHE.reverse = self
	})()

/**` Cons :: (() -> a) -> (() -> List a) -> List a `*/
const Cons = <a>(lazyFirst : () => a) => (lazyRest : () => List<a>) : List<a> =>
{
	const self : List<a> =
	{
		CONS : 'Cons',
		INFO :
		{
			CACHE : {},
			get head() { return this.CACHE.head ??= lazyFirst () },
			get tail() { return this.CACHE.tail ??= lazyRest  () },
			get last()
			{
				return this.CACHE.last ??= (() => {
					if (this.CACHE.reverse) return this.CACHE.reverse.INFO.head

					let xs = self
					if (this.CACHE.len)
						while (xs.INFO.tail.CONS === 'Cons') xs = xs.INFO.tail
					else
					{
						let i = 1
						while (xs.INFO.tail.CONS === 'Cons') xs = xs.INFO.tail, ++i
						this.CACHE.len = i
					}
					return xs.INFO.head
				})()
			},
			get init()
			{
				return this.CACHE.init ??=
					this.tail.CONS === 'Cons'
						? Cons (() => this.head) (() => this.tail.INFO.tail.CONS === 'Cons' ? this.tail.INFO.init : Nil)
						: Nil
			},
			get len()
			{
				return this.CACHE.len ??= (() => {
					let i = 1, xs = this.tail
					while (xs.CONS === 'Cons') ++i, xs = xs.INFO.tail
					return i
				})()
			},
			get reverse()
			{
				return this.CACHE.reverse ??= (() => {
					let xs = singleton (this.head), ys = this.tail
					if (this.CACHE.len)
						while (ys.CONS === 'Cons') xs = prepend (ys.INFO.head) (xs), ys = ys.INFO.tail
					else
					{
						let i = 1
						while (ys.CONS === 'Cons') xs = prepend (ys.INFO.head) (xs), ys = ys.INFO.tail, ++i
						this.CACHE.len = xs.INFO.CACHE.len = i
					}
					xs.INFO.CACHE.last    = this.head
					xs.INFO.CACHE.reverse = self
					return xs
				})()
			}
		},

		eq : xs =>
		{
			if (self === xs) return true
			let ys = self, i = 0
			while (xs.CONS === 'Cons' && ys.CONS === 'Cons')
			{
				if (i >= 256)
					THROWRANGE (`(.eq) checked the max amount of elements ('256') in a possible 'List'`)
				if (!(xs.INFO.head as any).eq(ys.INFO.head)) return false
				xs = xs.INFO.tail, ys = ys.INFO.tail, ++i
			}
			return xs.CONS === ys.CONS
		},
		pipe   : f  => f (self),
		plus   : xs => xs.CONS === 'Cons' ? Cons (() => self.INFO.head) (() => self.INFO.tail .plus (xs)) : self,
		bind   : f  => concat (self .fmap (f)),
		bindto : k  => f => self .bind ($ => f ($) .fmap (x => ({ ...$, [k] : x }) as any)),
		fmap   : f  => Cons (() => f (self.INFO.head)) (() => self.INFO.tail .fmap (f)),
		fmapto : k  => f => self .fmap ($ => ({ ...$, [k] : f ($) }) as any),
		cast   : x  =>
		{
			if (self.INFO.CACHE.len) return replicate (self.INFO.CACHE.len) (x)
			const xs = Cons (() => x) (() => self.INFO.tail .cast (x))
			xs.INFO.CACHE.head = xs.INFO.CACHE.last = x
			return xs.INFO.CACHE.reverse = xs
		}
	}

	return self
}

/**` List :: (...a) -> List a `*/
const List = <a = any>(...xs : Array<a>) : List<a> =>
{
	let ys : List<a> = Nil
	for (let i = xs.length - 1; ~i; --i) ys = prepend (xs[i] as a) (ys)
	ys.INFO.CACHE.len = xs.length
	return ys
}

/**` all :: (a -> Boolean) -> List a -> Boolean `*/
const all = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : boolean =>
	!any ((x : a) => !predicate (x)) (xs)

/**` any :: (a -> Boolean) -> List a -> Boolean `*/
const any = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : boolean =>
{
	while (xs.CONS === 'Cons')
		if (predicate (xs.INFO.head)) return true
		else xs = xs.INFO.tail
	return false
}

/**` append :: a -> List a -> List a `*/
const append = <a>(element : a) => (xs : List<a>) : List<a> =>
	xs.CONS === 'Nil' ? singleton (element) :
	Cons (() => xs.INFO.head) (() => append (element) (xs.INFO.tail))

/**` array :: List a -> [a] `*/
const array = <a>(xs : List<a>) : Array<a> =>
{
	const ys : Array<a> = []
	for (let i = 256; i && xs.CONS === 'Cons'; ys.push(xs.INFO.head), xs = xs.INFO.tail)
		if (!--i)
			console.warn(`'array' reached the max lengthed array of 'List' ('256') which could suggest infinity.`)
	return ys
}

/**` at :: Number -> List a -> a `*/
const at = (index : number) => <a>(xs : List<a>) : a =>
{
	if (index < 0)
		THROWRANGE (`'at' only accepts non-negatives as an index; received '${index}' as an input`)
	if (xs.INFO.CACHE.len! <= index)
		THROWRANGE (`'at' cannot get element at index '${index}' in 'List' of length '${xs.INFO.len}'`)

	let i = index
	while (xs.CONS === 'Cons')
		if (--i < 0) return xs.INFO.head
		else xs = xs.INFO.tail

	return THROWRANGE (`'at' cannot get element at index '${index}' in 'List' `)
}

/**` concat :: List (List a) -> List a `*/
const concat = <a>(xxs : List<List<a>>) : List<a> =>
	xxs           .CONS === 'Nil' ? Nil                    :
	xxs.INFO.head .CONS === 'Nil' ? concat (xxs.INFO.tail) :
	Cons (() => xxs.INFO.head.INFO.head) (() => xxs.INFO.head.INFO.tail .plus (concat (xxs.INFO.tail)))

/**` countBy :: Number -> Number -> List Number `*/
const countBy = (delta : number) => (start : number) : List<number> =>
	Cons (() => start) (() => countBy (delta) (start + delta))

/**` countDownFrom :: Number -> List Number `*/
const countDownFrom = (start : number) : List<number> =>
	Cons (() => start) (() => countDownFrom (start - 1))

/**` countUpFrom :: Number -> List Number `*/
const countUpFrom = (start : number) : List<number> =>
	Cons (() => start) (() => countUpFrom (start + 1))

/**` cycle :: List a -> List a `*/
const cycle = <a>(xs : List<a>) : List<a> =>
{
	const self : List<a> =
	{
		CONS : 'Cons',
		INFO :
		{
			CACHE : {},
			head : xs.INFO.head,
			tail : undefined as any,
			init : undefined as any,
			get last    () { return THROWRANGE (`'last' cannot be used on infinite 'List's from 'cycle'`) },
			get len     () { return THROWRANGE (`'len' cannot be used on infinite 'List's from 'cycle'`) },
			get reverse () { return THROWRANGE (`'reverse' cannot be used on infinite 'List's from 'cycle'`) }
		},

		eq : ys =>
		{
			if (self === ys) return true
			let zs = self, i = 0
			while (ys.CONS === 'Cons' && zs.CONS === 'Cons')
			{
				if (i >= 256)
					THROWRANGE (`(.eq) checked the max amount of elements ('256') in a infinite 'List' from 'cycle'`)
				if (!(ys.INFO.head as any).eq(zs.INFO.head)) return false
				ys = ys.INFO.tail, zs = zs.INFO.tail, ++i
			}
			return ys.CONS === zs.CONS
		},
		pipe   : f => f (self),
		plus   : _ => self,
		bind   : f => cycle (xs .bind (f)),
		bindto : _ => THROWTYPE (`'.bindto' should only be used on monads coming from 'Do', not 'repeat'`),
		fmap   : f => cycle (xs .fmap (f)),
		fmapto : _ => THROWTYPE (`'.fmapto' should only be used on monads coming from 'Do', not 'repeat'`),
		cast   : repeat
	}

	self.INFO.tail = xs.INFO.tail .plus (self)
	self.INFO.init = self
	return self
}

/**` drop :: Number -> List a -> List a `*/
const drop = (amount : number) => <a>(xs : List<a>) : List<a> =>
{
	if (xs.INFO.CACHE.len! <= amount) return Nil
	while (xs.CONS === 'Cons' && amount >= 1) xs = xs.INFO.tail, --amount
	return xs
}

/**` dropWhile :: (a -> Boolean) -> List a -> List a `*/
const dropWhile = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : List<a> =>
{
	while (xs.CONS === 'Cons')
		if (predicate (xs.INFO.head)) xs = xs.INFO.tail
		else break
	return xs
}

/**` elem :: a -> List a -> Boolean`*/
const elem = <a>(value : a) => (xs : List<a>) : boolean =>
{
	while (xs.CONS === 'Cons')
		if (xs.INFO.head === value) return true
		else xs = xs.INFO.tail
	return false
}

/**` elemIndex :: a -> List a -> Maybe Number `*/
const elemIndex = <a>(value : a) => (xs : List<a>) : Maybe<number> =>
{
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.INFO.tail)
		if (xs.INFO.head === value) return Just (i)
	return Nothing
}

/**` elemIndices :: a -> List a -> List Number `*/
const elemIndices = <a>(value : a) => (xs : List<a>) : List<number> =>
	xs.CONS === 'Nil' ? Nil :
	xs.INFO.head === value
		? Cons (() => 0) (() => elemIndices (value) (xs.INFO.tail) .fmap (x => x + 1))
		: elemIndices (value) (xs.INFO.tail) .fmap (x => x + 1)

/**` filter :: (a -> Boolean) -> List a -> List a `*/
const filter = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : List<a> =>
	xs.CONS === 'Nil' ? Nil :
		predicate (xs.INFO.head)
			? Cons (() => xs.INFO.head) (() => filter (predicate) (xs.INFO.tail))
			: filter (predicate) (xs.INFO.tail)

/**` findIndex :: (a -> Boolean) -> List a -> Maybe Number `*/
const findIndex = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : Maybe<number> =>
{
	for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.INFO.tail)
		if (predicate (xs.INFO.head)) return Just (i)
	return Nothing
}

/**` findIndices :: (a -> Boolean) -> List a -> List Number `*/
const findIndices = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : List<number> =>
	xs.CONS === 'Nil' ? Nil :
	predicate (xs.INFO.head)
		? Cons (() => 0) (() => findIndices (predicate) (xs.INFO.tail) .fmap (x => x + 1))
		: findIndices (predicate) (xs.INFO.tail) .fmap (x => x + 1)

/**` foldl :: (b -> a -> b) -> b -> List a -> b `*/
const foldl = <a, b>(operation : (previous : b) => (next : a) => b) => (initial : b) => (xs : List<a>) : b =>
{
	if (xs.INFO.CACHE.len)
		while (xs.CONS === 'Cons')
			initial = operation (initial) (xs.INFO.head), xs = xs.INFO.tail
	else
	{
		let i = 0, ys = xs
		while (ys.CONS === 'Cons')
			initial = operation (initial) (ys.INFO.head), ys = ys.INFO.tail, ++i
		xs.INFO.CACHE.len = i
	}
		return initial
}

/**` foldl1 :: (a -> a -> a) -> List a -> a `*/
const foldl1 = <a>(operation : (previous : a) => (next : a) => a) => (xs : List<a>) : a =>
	xs.CONS === 'Nil' ? THROWRANGE (`'foldl1' cannot be used on 'Nil' (an empty 'List')`) :
	foldl (operation) (xs.INFO.head) (xs.INFO.tail)

/**` foldr :: (a -> b -> b) -> b -> List a -> b `*/
const foldr = <a, b>(operation : (previous : a) => (next : b) => b) => (initial : b) => (xs : List<a>) : b =>
	foldl<a, b>(x => y => operation (y) (x)) (initial) (xs.INFO.reverse)

/**` foldr1 :: (a -> a -> a) -> List a -> a `*/
const foldr1 = <a>(operation : (previous : a) => (next : a) => a) => (xs : List<a>) : a =>
	xs.CONS === 'Nil' ? THROWRANGE (`'foldr1' cannot be used on 'Nil' (an empty 'List')`) :
	foldl<a, a>(x => y => operation (y) (x)) (xs.INFO.reverse.INFO.head) (xs.INFO.reverse.INFO.tail)

/**` head :: List a -> a `*/
const head = <a>(xs : List<a>) : a => xs.INFO.head

/**` init :: List a -> List a `*/
const init = <a>(xs : List<a>) : List<a> => xs.INFO.init

/**` inits :: List a -> List (List a) `*/
const inits = <a>(xs : List<a>) : List<List<a>> =>
	xs.CONS === 'Nil' ? singleton (Nil as List<a>) :
	Cons (() => Nil as List<a>) (() => inits (xs.INFO.tail) .fmap (prepend (xs.INFO.head)))

/**` isEmpty :: List a -> Boolean `*/
const isEmpty = <a>(xs : List<a>) : boolean =>
	xs.CONS === 'Nil'

/**` iterate :: (a -> a) -> a -> List a `*/
const iterate = <a>(endomorphism : (input : a) => a) => (initial : a) : List<a> =>
	Cons (() => initial) (() => iterate (endomorphism) (endomorphism (initial)))

/**` intersperse :: a -> List a -> List a `*/
const intersperse = <a>(delimiter : a) => (xs : List<a>) : List<a> =>
	xs .bind (x => List(delimiter, x)).INFO.tail

/**` last :: List a -> a `*/
const last = <a>(xs : List<a>) : a => xs.INFO.last

/**` len :: List a -> Number `*/
const len = <a>(xs : List<a>) : number => xs.INFO.len

/**` map :: (a -> b) -> List a -> List b `*/
const map = <a, b>(morphism : (element : a) => b) => (xs : List<a>) : List<b> =>
	xs .fmap (morphism)

/**` partition :: (a -> Boolean) -> List a -> (List a, List a) `*/
const partition = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : [List<a>, List<a>] =>
	[filter (predicate) (xs), filter ((x : a) => !predicate(x)) (xs)]

/**` prepend :: a -> List a -> List a `*/
const prepend = <a>(element : a) => (xs : List<a>) : List<a> =>
	xs.CONS === 'Nil' ? singleton (element) :
	(() => {
		const self : List<a> =
		{
			CONS : 'Cons',
			INFO :
			{
				CACHE :
				{
					head : element,
					tail : xs
				},
				head : element,
				tail : xs,
				get len  () { return this.CACHE.len  ??= xs.INFO.len + 1 },
				get last () { return this.CACHE.last ??= xs.CONS === 'Cons' ? xs.INFO.last : element },
				get init () { return this.CACHE.init ??= xs.CONS === 'Cons' ? xs.INFO.init : Nil },
				get reverse()
				{
					return this.CACHE.reverse ??=
						xs.CONS === 'Cons'
							? append (element) (xs.INFO.reverse)
							: self
				}
			},

			eq : ys =>
			{
				if (self === ys) return true
				let zs = self
				while (ys.CONS === 'Cons' && zs.CONS === 'Cons')
				{
					if (!(ys.INFO.head as any).eq(zs.INFO.head)) return false
					ys = ys.INFO.tail, zs = zs.INFO.tail
				}
				return true
			},
			pipe   : f  => f (self),
			plus   : ys => ys.CONS === 'Cons' ? Cons (() => self.INFO.head) (() => self.INFO.tail .plus (ys)) : self,
			bind   : f  => concat (self .fmap (f)),
			bindto : k  => f => self .bind ($ => f ($) .fmap (x => ({ ...$, [k] : x }) as any)),
			fmap   : f  => Cons (() => f (self.INFO.head)) (() => self.INFO.tail .fmap (f)),
			fmapto : k  => f => self .fmap ($ => ({ ...$, [k] : f ($) }) as any),
			cast   : x  =>
			{
				if (self.INFO.CACHE.len) return replicate (self.INFO.CACHE.len) (x)
				const ys = Cons (() => x) (() => xs .cast (x))
				ys.INFO.CACHE.head = ys.INFO.CACHE.last = x
				return ys.INFO.CACHE.reverse = ys
			}
		}

		return self
	})()

/**` repeat :: a -> List a `*/
const repeat = <a>(value : a) : List<a> =>
{
	const self : List<a> =
	{
		CONS : 'Cons',
		INFO :
		{
			CACHE :
			{
				head : value
			},
			head : value,
			tail : undefined as any,
			init : undefined as any,
			get last    () { return THROWRANGE (`'last' cannot be used on infinite 'List's from 'repeat'`) },
			get len     () { return THROWRANGE (`'len' cannot be used on infinite 'List's from 'repeat'`) },
			get reverse () { return THROWRANGE (`'reverse' cannot be used on infinite 'List's from 'repeat'`) }
		},
		eq : xs =>
		{
			if (self === xs) return true
			let ys = self, i = 0
			while (xs.CONS === 'Cons' && ys.CONS === 'Cons')
			{
				if (i >= 256)
					THROWRANGE (`(.eq) checked the max amount of elements ('256') in a infinite 'List' from 'repeat'`)
				if (!(xs.INFO.head as any).eq(ys.INFO.head)) return false
				xs = xs.INFO.tail, ys = ys.INFO.tail, ++i
			}
			return xs.CONS === ys.CONS
		},
		pipe   : f => f (self),
		plus   : _ => self,
		bind   : f => concat (repeat (f (value))),
		bindto : _ => THROWTYPE (`'.bindto' should only be used on monads coming from 'Do', not 'repeat'`),
		fmap   : f => repeat (f (value)),
		fmapto : _ => THROWTYPE (`'.fmapto' should only be used on monads coming from 'Do', not 'repeat'`),
		cast   : repeat
	}

	return self.INFO.tail = self.INFO.init = self
}

/**` replicate :: Number -> a -> List a `*/
const replicate = (amount : number) => <a>(value : a) : List<a> =>
	amount < 1 ? Nil :
	Cons (() => value) (() => replicate (amount - 1) (value))

/**` reverse :: List a -> List a `*/
const reverse = <a>(xs : List<a>) : List<a> => xs.INFO.reverse

/**` scanl :: (b -> a -> b) -> List a -> List b `*/
const scanl = <a, b>(operation : (previous : b) => (next : a) => b) => (initial : b) => (xs : List<a>) : List<b> =>
	xs.CONS === 'Nil' ? singleton (initial) :
	Cons (() => initial) (() => scanl (operation) (operation (initial) (xs.INFO.head)) (xs.INFO.tail))

/**` scanl1 :: (a -> a -> a) -> List a -> List a `*/
const scanl1 = <a>(operation : (previous : a) => (next : a) => a) => (xs : List<a>) : List<a> =>
	xs.CONS === 'Nil' ? Nil :
	scanl (operation) (xs.INFO.head) (xs.INFO.tail)

/**` scanr :: (a -> b -> b) -> b -> List a -> List b `*/
const scanr = <a, b>(operation : (previous : a) => (next : b) => b) => (initial : b) => (xs : List<a>) : List<b> =>
{
	xs = xs.INFO.reverse
	let ys = singleton (initial)
	if (xs.INFO.CACHE.len)
		while (xs.CONS === 'Cons')
			ys = prepend (operation (xs.INFO.head) (ys.INFO.head)) (ys), xs = xs.INFO.tail
	else
	{
		let i = 1
		while (xs.CONS === 'Cons')
			ys = prepend (operation (xs.INFO.head) (ys.INFO.head)) (ys), xs = xs.INFO.tail, ++i
		ys.INFO.CACHE.len = i
	}
	return ys
}

/**` scanr1 :: (a -> a -> a) -> List a -> List a `*/
const scanr1 = <a>(operation : (previous : a) => (next : a) => a) => (xs : List<a>) : List<a> =>
{
	if (xs.CONS === 'Nil') return Nil
	xs = xs.INFO.reverse
	let ys = singleton (xs.INFO.head)
	xs = xs.INFO.tail
	while (xs.CONS === 'Cons')
		ys = prepend (operation (xs.INFO.head) (ys.INFO.head)) (ys), xs = xs.INFO.tail
	return ys
}

/**` singleton :: a -> List a `*/
const singleton = <a>(value : a) : List<a> =>
{
	const self : List<a> =
		{
			CONS : 'Cons',
			INFO :
			{
				CACHE :
				{
					head    : value,
					tail    : Nil,
					last    : value,
					init    : Nil,
					len     : 1,
					reverse : undefined as any
				},
				head    : value,
				tail    : Nil,
				last    : value,
				init    : Nil,
				len     : 1,
				reverse : undefined as any
			},

			eq : xs =>
			{
				if (self === xs) return true
				if (xs.CONS === 'Nil' || xs.INFO.tail.CONS === 'Cons') return false
				return (xs.INFO.head as any) .eq (value)
			},
			pipe   : f  => f (self),
			plus   : xs => xs.CONS === 'Cons' ? prepend (value) (xs) : self,
			bind   : f  => f (value),
			bindto : _  => THROWTYPE (`'.bindto' should be used in monads coming from 'Do', not 'singleton'`),
			fmap   : f  => singleton (f (value)),
			fmapto : _  => THROWTYPE (`'.fmapto' should be used in monads coming from 'Do', not 'singleton'`),
			cast   : singleton
		}

	self.INFO.reverse = self.INFO.CACHE.reverse = self
	return self
}

/**` span :: (a -> Boolean) -> List a -> (List a, List a) `*/
const span = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : [List<a>, List<a>] =>
	[takeWhile (predicate) (xs), dropWhile (predicate) (xs)]

/**` splitAt :: Number -> List a -> (List a, List a) `*/
const splitAt = (index : number) => <a>(xs : List<a>) : [List<a>, List<a>] =>
	[take (index) (xs), drop (index) (xs)]

/**` string :: String -> List String `*/
const string = (str : string) : List<string> =>
	str
		? (() => {
			const self : List<string> =
			{
				CONS : 'Cons',
				INFO :
				{
					CACHE :
					{
						head : str[0]!,
						last : str.slice(-1),
						len  : str.length
					},
					head : str[0]!,
					last : str.slice(-1),
					len  : str.length,
					get tail()    { return this.CACHE.tail    ??= string (str.slice(1)) },
					get init()    { return this.CACHE.init    ??= string (str.slice(0, str.length - 1)) },
					get reverse() { return this.CACHE.reverse ??= string (str.split("").reverse().join("")) }
				},

				eq : xs =>
				{
					if (self === xs) return true
					let ys = self, i = 0
					while (xs.CONS === 'Cons' && ys.CONS === 'Cons')
					{
						if (i >= 256)
							THROWRANGE (`(.eq) checked the max amount of characters ('256') in a possible infinite 'List'`)
						if (!(xs.INFO.head as any).eq(ys.INFO.head)) return false
						xs = xs.INFO.tail, ys = ys.INFO.tail, ++i
					}
					return xs.CONS === ys.CONS
				},
				get pipe() { return (f : any) => f (this) },
				plus (xs) { return xs.CONS === 'Cons' ? Cons (() => this.INFO.head) (() => this.INFO.tail .plus (xs)) : Nil },
				bind (f)  { return concat (this .fmap (f)) },
				fmap (f)  { return List (...str.split("").map(x => f(x))) },
				bindto : _ => THROWTYPE (`'.bindto' should be used in monads coming from 'Do', not 'string'`),
				fmapto : _ => THROWTYPE (`'.fmapto' should be used in monads coming from 'Do', not 'string'`),
				cast   : x => replicate (str.length) (x)
			}
			return self
		})()
		: Nil

/**` tail :: List a -> List a `*/
const tail = <a>(xs : List<a>) : List<a> => xs.INFO.tail

/**` tails :: List a -> List (List a) `*/
const tails = <a>(xs : List<a>) : List<List<a>> =>
	xs.CONS === 'Nil' ? singleton (Nil as List<a>) :
	Cons (() => xs as List<a>) (() => tails (xs.INFO.tail))

/**` take :: Number -> List a -> List a `*/
const take = (amount : number) => <a>(xs : List<a>) : List<a> =>
	amount < 1                   ? Nil :
	xs.INFO.CACHE.len! <= amount ? xs  :
	Cons (() => xs.INFO.head) (() => take (amount - 1) (xs.INFO.tail))

/**` takeWhile :: (a -> Boolean) -> List a -> List a `*/
const takeWhile = <a>(predicate : (element : a) => boolean) => (xs : List<a>) : List<a> =>
	xs.CONS === 'Nil'        ? xs :
	predicate (xs.INFO.head)
		? Cons (() => xs.INFO.head) (() => takeWhile (predicate) (xs.INFO.tail))
		: Nil

/**` unstring :: List String -> String `*/
const unstring = (xs : List<string>) : string =>
{
	let s = ""
	for (let i = 256; i && xs.CONS === 'Cons'; s += xs.INFO.head, xs = xs.INFO.tail)
		if (!--i)
			console.warn(`'unstring' reached the max lengthed string of 'List' ('256') which could suggest infinity.`)
	return s
}

/**` unzip :: List (a, b) -> (List a, List b) `*/
const unzip = <a, b>(xs : List<[a, b]>) : [List<a>, List<b>] =>
	[xs .fmap (x => x[0]), xs .fmap (x => x[1])]

/**` zip :: List a -> List b -> List (a, b) `*/
const zip = <a>(xs : List<a>) => <b>(ys : List<b>) : List<[a, b]> =>
	xs.CONS === 'Nil' || ys.CONS === 'Nil' ? Nil :
	Cons (() => [xs.INFO.head, ys.INFO.head] as [a, b]) (() => zip (xs.INFO.tail) (ys.INFO.tail))

/**` zipWith :: (a -> b -> c) -> List a -> List b -> List c `*/
const zipWith = <a, b, c> (zipper : (x : a) => (y : b) => c) => (xs : List<a>) => (ys : List<b>) : List<c> =>
	xs.CONS === 'Nil' || ys.CONS === 'Nil' ? Nil :
	Cons (() => zipper (xs.INFO.head) (ys.INFO.head)) (() => zipWith (zipper) (xs.INFO.tail) (ys.INFO.tail))

/********************************************************************************************************************************/

/**` Vector2D (Eq, Pipeable) `*/
type Vector2D =
	{
		CONS : 'Vector2D'

		/**` (.eq) :: Vector2D -> Vector2D -> Boolean `*/
		eq : (value : Vector2D) => boolean

		/**` (.pipe) :: Vector2D -> (Vector2D -> a) -> a `*/
		pipe : <a>(morphism : (vector : Vector2D) => a) => a

		/**` (.x) :: Vector2D -> Number `*/
		x : number

		/**` (.y) :: Vector2D -> Number `*/
		y : number
	}

/**` Vector2D :: Number -> Number -> Vector2D `*/
const Vector2D = (x : number) => (y : number) : Vector2D =>
	({
		CONS : 'Vector2D',
		eq : v => v.x === x && v.y === y,
		get pipe() { return (f : any) => f (this) },
		x, y
	})

/**` Vector3D (Eq, Pipeable) `*/
type Vector3D =
	{
		CONS : 'Vector3D'

		/**` (.eq) :: Vector3D -> Vector3D -> Boolean `*/
		eq : (value : Vector3D) => boolean

		/**` (.pipe) :: Vector3D -> (Vector3D -> a) -> a `*/
		pipe : <a>(morphism : (vector : Vector3D) => a) => a

		/**` (.x) :: Vector3D -> Number `*/
		x : number

		/**` (.y) :: Vector3D -> Number `*/
		y : number

		/**` (.z) :: Vector3D -> Number `*/
		z : number
	}

/**` Vector3D :: Number -> Number -> Number -> Vector3D `*/
const Vector3D = (x : number) => (y : number) => (z : number) : Vector3D =>
	({
		CONS : 'Vector3D',
		eq : v => v.x === x && v.y === y && v.z === z,
		get pipe() { return (f : any) => f (this) },
		x, y, z
	})

/**` Vector4D `*/
type Vector4D =
	{
		CONS : 'Vector4D'

		/**` (.eq) :: Vector4D -> Vector4D -> Boolean `*/
		eq : (value : Vector4D) => boolean

		/**` (.pipe) :: Vector4D -> (Vector4D -> a) -> a `*/
		pipe : <a>(morphism : (vector : Vector4D) => a) => a

		/**` (.x) :: Vector4D -> Number `*/
		x : number

		/**` (.y) :: Vector4D -> Number `*/
		y : number

		/**` (.z) :: Vector4D -> Number `*/
		z : number

		/**` (.w) :: Vector4D -> Number `*/
		w : number
	}

/**` Vector4D :: Number -> Number -> Number -> Number -> Vector4D `*/
const Vector4D = (x : number) => (y : number) => (z : number) => (w : number) : Vector4D =>
	({
		CONS : 'Vector4D',
		eq : v => v.x === x && v.y === y && v.z === z && v.w === w,
		get pipe() { return (f : any) => f (this) },
		x, y, z, w
	})

/********************************************************************************************************************************/

/**` Matrix2x2 (Eq, Pipeable) `*/
type Matrix2x2 =
	{
		CONS : 'Matrix2x2'

		/**` eq :: Matrix2x2 -> Matrix2x2 -> boolean `*/
		eq : (value : Matrix2x2) => boolean

		/**` pipe :: Matrix2x2 -> (Matrix2x2 -> a) -> a `*/
		pipe : <a>(morphism : (matrix : Matrix2x2) => a) => a

		/**` (.ix) :: Matrix2x2 -> Number `*/
		ix : number

		/**` (.jx) :: Matrix2x2 -> Number `*/
		jx : number

		/**` (.iy) :: Matrix2x2 -> Number `*/
		iy : number

		/**` (.jy) :: Matrix2x2 -> Number `*/
		jy : number

		/**` (.i) :: Matrix2x2 -> Vector2D `*/
		i : Vector2D

		/**` (.j) :: Matrix2x2 -> Vector2D `*/
		j : Vector2D

		/**` (.x) :: Matrix2x2 -> Vector2D `*/
		x : Vector2D

		/**` (.y) :: Matrix2x2 -> Vector2D `*/
		y : Vector2D
	}

/**` Matrix2x2 :: (4 Number...) -> Matrix2x2 `*/
const Matrix2x2 =
		(ix : number) => (jx : number) =>
		(iy : number) => (jy : number) : Matrix2x2 =>
	({
		CONS : 'Matrix2x2',
		eq : m => m.ix === ix && m.jx === jx && m.iy === iy && m.jy === jy,
		get pipe() { return (f : any) => f (this) },
		ix, jx,
		iy, jy,
		i : Vector2D(ix)(iy), j : Vector2D(jx)(jy),
		x : Vector2D(ix)(jx), y : Vector2D(iy)(jy)
	})

/**` Matrix2D :: Vector2D -> Vector2D -> Matrix2x2 `*/
const Matrix2D = (i : Vector2D) => (j : Vector2D) : Matrix2x2 =>
	Matrix2x2
		(i.x)(j.x)
		(i.y)(j.y)

/**` Matrix3x3 (Eq, Pipeable) `*/
type Matrix3x3 =
	{
		CONS : 'Matrix3x3'

		/**` eq :: Matrix3x3 -> Matrix3x3 -> boolean `*/
		eq : (value : Matrix3x3) => boolean

		/**` pipe :: Matrix3x3 -> (Matrix3x3 -> a) -> a `*/
		pipe : <a>(morphism : (matrix : Matrix3x3) => a) => a

		/**` (.ix) :: Matrix3x3 -> Number `*/
		ix : number

		/**` (.jx) :: Matrix3x3 -> Number `*/
		jx : number

		/**` (.kx) :: Matrix3x3 -> Number `*/
		kx : number

		/**` (.iy) :: Matrix3x3 -> Number `*/
		iy : number

		/**` (.jy) :: Matrix3x3 -> Number `*/
		jy : number

		/**` (.ky) :: Matrix3x3 -> Number `*/
		ky : number

		/**` (.iz) :: Matrix3x3 -> Number `*/
		iz : number

		/**` (.jz) :: Matrix3x3 -> Number `*/
		jz : number

		/**` (.kz) :: Matrix3x3 -> Number `*/
		kz : number

		/**` (.i) :: Matrix3x3 -> Vector3D `*/
		i : Vector3D

		/**` (.j) :: Matrix3x3 -> Vector3D `*/
		j : Vector3D

		/**` (.k) :: Matrix3x3 -> Vector3D `*/
		k : Vector3D

		/**` (.x) :: Matrix3x3 -> Vector3D `*/
		x : Vector3D

		/**` (.y) :: Matrix3x3 -> Vector3D `*/
		y : Vector3D

		/**` (.z) :: Matrix3x3 -> Vector3D `*/
		z : Vector3D
	}

/**` Matrix3x3 :: (9 Number...) -> Matrix3x3 `*/
const Matrix3x3 =
		(ix : number) => (jx : number) => (kx : number) =>
		(iy : number) => (jy : number) => (ky : number) =>
		(iz : number) => (jz : number) => (kz : number) : Matrix3x3 =>
	({
		CONS : 'Matrix3x3',
		eq : m =>
			m.ix === ix && m.jx === jx && m.kx === kx &&
			m.iy === iy && m.jy === jy && m.ky === ky &&
			m.iz === iz && m.jz === jz && m.kz === kz,
		get pipe() { return (f : any) => f (this) },
		ix, jx, kx,
		iy, jy, ky,
		iz, jz, kz,
		i : Vector3D(ix)(iy)(iz), j : Vector3D(jx)(jy)(jz), k : Vector3D(kx)(ky)(kz),
		x : Vector3D(ix)(jx)(kx), y : Vector3D(iy)(jy)(ky), z : Vector3D(iz)(jz)(kz)
	})

/**` Matrix3D :: Vector3D -> Vector3D -> Vector3D -> Matrix3x3 `*/
const Matrix3D = (i : Vector3D) => (j : Vector3D) => (k : Vector3D) : Matrix3x3 =>
	Matrix3x3
		(i.x)(j.x)(k.x)
		(i.y)(j.y)(k.y)
		(i.z)(j.z)(k.z)

/**` Matrix4x4 (Eq, Pipeable) `*/
type Matrix4x4 =
	{
		CONS : 'Matrix4x4'

		/**` eq :: Matrix4x4 -> Matrix4x4 -> boolean `*/
		eq : (value : Matrix4x4) => boolean

		/**` pipe :: Matrix4x4 -> (Matrix4x4 -> a) -> a `*/
		pipe : <a>(morphism : (matrix : Matrix4x4) => a) => a

		/**` (.i)x ::Matrix4x4 ->  Number `*/
		ix : number

		/**` (.j)x ::Matrix4x4 ->  Number `*/
		jx : number

		/**` (.k)x ::Matrix4x4 ->  Number `*/
		kx : number

		/**` (.l)x ::Matrix4x4 ->  Number `*/
		lx : number

		/**` (.i)y ::Matrix4x4 ->  Number `*/
		iy : number

		/**` (.j)y ::Matrix4x4 ->  Number `*/
		jy : number

		/**` (.k)y ::Matrix4x4 ->  Number `*/
		ky : number

		/**` (.l)y ::Matrix4x4 ->  Number `*/
		ly : number

		/**` (.i)z ::Matrix4x4 ->  Number `*/
		iz : number

		/**` (.j)z ::Matrix4x4 ->  Number `*/
		jz : number

		/**` (.k)z ::Matrix4x4 ->  Number `*/
		kz : number

		/**` (.l)z ::Matrix4x4 ->  Number `*/
		lz : number

		/**` (.i)w ::Matrix4x4 ->  Number `*/
		iw : number

		/**` (.j)w ::Matrix4x4 ->  Number `*/
		jw : number

		/**` (.k)w ::Matrix4x4 ->  Number `*/
		kw : number

		/**` (.l)w ::Matrix4x4 ->  Number `*/
		lw : number

		/**` (.i) :: Matrix4x4 -> Vector4D `*/
		i : Vector4D

		/**` (.j) :: Matrix4x4 -> Vector4D `*/
		j : Vector4D

		/**` (.k) :: Matrix4x4 -> Vector4D `*/
		k : Vector4D

		/**` (.l) :: Matrix4x4 -> Vector4D `*/
		l : Vector4D

		/**` (.x) :: Matrix4x4 -> Vector4D `*/
		x : Vector4D

		/**` (.y) :: Matrix4x4 -> Vector4D `*/
		y : Vector4D

		/**` (.z) :: Matrix4x4 -> Vector4D `*/
		z : Vector4D

		/**` (.w) :: Matrix4x4 -> Vector4D `*/
		w : Vector4D
	}

/**` Matrix4x4 :: (16 Number...) -> Matrix4x4 `*/
const Matrix4x4 =
		(ix : number) => (jx : number) => (kx : number) => (lx : number) =>
		(iy : number) => (jy : number) => (ky : number) => (ly : number) =>
		(iz : number) => (jz : number) => (kz : number) => (lz : number) =>
		(iw : number) => (jw : number) => (kw : number) => (lw : number) : Matrix4x4 =>
	({
		CONS : 'Matrix4x4',
		eq : m =>
			m.ix === ix && m.jx === jx && m.kx === kx && m.lx === lx &&
			m.iy === iy && m.jy === jy && m.ky === ky && m.ly === ly &&
			m.iz === iz && m.jz === jz && m.kz === kz && m.lz === lz &&
			m.iw === iw && m.jw === jw && m.kw === kw && m.lw === lw,
		get pipe() { return (f : any) => f (this) },
		ix, jx, kx, lx,
		iy, jy, ky, ly,
		iz, jz, kz, lz,
		iw, jw, kw, lw,
		i : Vector4D(ix)(iy)(iz)(iw), j : Vector4D(jx)(jy)(jz)(jw), k : Vector4D(kx)(ky)(kz)(kw), l : Vector4D(lx)(ly)(lz)(lw),
		x : Vector4D(ix)(jx)(kx)(lx), y : Vector4D(iy)(jy)(ky)(ly), z : Vector4D(iz)(jz)(kz)(lz), w : Vector4D(iw)(jw)(kw)(lw)
	})

/**` Matrix4D :: Vector4D -> Vector4D -> Vector4D -> Vector4D -> Matrix4x4 `*/
const Matrix4D = (i : Vector4D) => (j : Vector4D) => (k : Vector4D) => (l : Vector4D) : Matrix4x4 =>
	Matrix4x4
		(i.x)(j.x)(k.x)(l.x)
		(i.y)(j.y)(k.y)(l.y)
		(i.z)(j.z)(k.z)(l.z)
		(i.w)(j.w)(k.w)(l.w)

/********************************************************************************************************************************/

/** A simple data type that stores information about the measurement of texts in rendering. */
type TextMeasurement =
	{
		CONS : 'TextMeasurement'

		/**` (.text) :: TextMeasurement -> String `*/
		text : string

		/**` (.width) :: TextMeasurement -> Number `*/
		width : number

		/**` (.height) :: TextMeasurement -> Number `*/
		height : number
	}

/**` TextMeasurement :: String -> Number -> Number -> TextMeasurement `*/
const TextMeasurement = (text : string) => (width : number) => (height : number) : TextMeasurement =>
	({
		CONS : 'TextMeasurement',
		text, width, height
	})

/********************************************************************************************************************************/

/** An inline version of the `switch` statement. */
type Switch<a, b> =
	{
		CONS : 'Switch'

		/**` (.case) :: Switch a b -> a -> (() -> b) -> Switch a b `*/
		case : (domain : a) => (codomain : () => b) => Switch<a, b>

		/**` (.else) :: Switch a b -> (() -> b) -> Switch a b `*/
		else : (codomain : () => b) => Switch<a, b>

		/**` (.with) :: Switch a b -> a -> b `*/
		with : (value : a) => b
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

/********************************************************************************************************************************/

/** A data structure that maps values to other values supplied with an inverse. */
type Bijection<a, b> =
	{
		CONS : 'Bijection'
		INFO : ReadonlyArray<[a, b]>

		/**` (.of) :: Bijection a b -> a -> b -> Bijection a b `*/
		of : (domainValue : a) => (codomainValue : b) => Bijection<a, b>

		/**` (.domain) :: Bijection a b -> a -> b `*/
		domain : (domainValue : a) => b

		/**` (.codomain) :: Bijection a b -> b -> a `*/
		codomain : (codomainValue : b) => a
	}

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

/********************************************************************************************************************************/

/**` unit :: (Monad m) => a -> m a `*/
const unit =
	{
		IO    : <a>(output : a) : IO<a> => IO(() => output),
		Maybe : Just,
		State : <a>(output : a) : State<null, a> => State(_ => [null, output]),
		List  : <a>(element : a) : List<a> => Cons (() => element) (() => Nil)
	} as const

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

/**` bijectionLineCap :: Bijection LineCap String `*/
const bijectionLineCap : Bijection<LineCap, string> =
	Bijection
		.of (LineCap.Butt)   ('butt')
		.of (LineCap.Round)  ('round')
		.of (LineCap.Square) ('square')

/**` bijectionLineJoin :: Bijection LineJoin String `*/
const bijectionLineJoin : Bijection<LineJoin, string> =
	Bijection
		.of (LineJoin.Round) ('round')
		.of (LineJoin.Bevel) ('bevel')
		.of (LineJoin.Miter) ('miter')

/**` bijectionTextAlign :: Bijection TextAlign String `*/
const bijectionTextAlign : Bijection<TextAlign, string> =
	Bijection
		.of (TextAlign.Center) ('center')
		.of (TextAlign.End)    ('end')
		.of (TextAlign.Left)   ('left')
		.of (TextAlign.Right)  ('right')
		.of (TextAlign.Start)  ('start')

/**` bijectionTextBaseline :: Bijection TextBaseline String `*/
const bijectionTextBaseline : Bijection<TextBaseline, string> =
	Bijection
		.of (TextBaseline.Alphabetic)  ('alphabetic')
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

/********************************************************************************************************************************/

// The Do-Notation syntax where a monad stores an empty object.
const Do =
	{
		IO    : IO <{}> (() => Object.create(null)),
		Maybe : Just <{}> (Object.create(null)),
		State : State <any, {}> ((s : any) => [s, Object.create(null)]),
		List  : singleton <{}> (Object.create(null))
	} as const


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
			IO(() => (__EXTERNAL__.context.lineWidth = w * __EXTERNAL__.context.canvas.width, null))

		/**` Mutate.Norm.lineDashPattern :: List Number -> IO () `*/
		export const lineDashPattern = (pattern : List<number>) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.setLineDash(array(pattern.fmap(n => n * __EXTERNAL__.context.canvas.width)))
				return null
			})

		/**` Mutate.Norm.lineDashOffset :: Number -> IO () `*/
		export const lineDashOffset = (offset : number) : IO<null> =>
			IO(() => (__EXTERNAL__.context.lineDashOffset = offset * __EXTERNAL__.context.canvas.width, null))

		/**` Mutate.Norm.fontSize :: Number -> IO () `*/
		export const fontSize = (size : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.font =
					`${size * __EXTERNAL__.context.canvas.width}px` +
					`${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" "))}`
				return null
			})

		/**` Mutate.Norm.fillRGBA :: Number -> Number -> Number -> Number -> IO () `*/
		export const fillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
			IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

		/**` Mutate.Norm.fillVector :: Vector4D -> IO () `*/
		export const fillVector = (v : Vector4D) : IO<null> =>
			IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

		/**` Mutate.Norm.strokeRGBA :: Number -> Number -> Number -> Number -> IO () `*/
		export const strokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
			IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a * 255})`, null))

		/**` Mutate.Norm.strokeVector :: Vector4D -> IO () `*/
		export const strokeVector = (v : Vector4D) : IO<null> =>
			IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

		/**` Mutate.Norm.shadowRGBA :: Number -> Number -> Number -> Number -> IO () `*/
		export const shadowRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
			IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

		/**` Mutate.Norm.shadowVector :: Vector4D -> IO () `*/
		export const shadowVector = (v : Vector4D) : IO<null> =>
			IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

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
			IO(() => (__EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width, null))

		/**` Mutate.Norm.shadowOffsetY :: Number -> IO () `*/
		export const shadowOffsetY = (y : number) : IO<null> =>
			IO(() => (__EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height, null))

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
		IO(() => (__EXTERNAL__.context.canvas.width = w, null))

	/**` Mutate.canvasDimensionH :: Number -> IO () `*/
	export const canvasDimensionH = (h : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.canvas.height = h, null))

	/**` Mutate.lineWidth :: Number -> IO () `*/
	export const lineWidth = (w : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.lineWidth = w, null))

	/**` Mutate.lineCap :: LineCap -> IO () `*/
	export const lineCap = (cap : LineCap) : IO<null> =>
		IO(() => (__EXTERNAL__.context.lineCap = bijectionLineCap.domain(cap) as any, null))

	/**` Mutate.lineJoin :: LineJoin -> IO () `*/
	export const lineJoin = (joining : LineJoin) : IO<null> =>
		IO(() => (__EXTERNAL__.context.lineJoin = bijectionLineJoin.domain(joining) as any, null))

	/**` Mutate.lineDashPattern :: List Number -> IO () `*/
	export const lineDashPattern = (pattern : List<number>) : IO<null> =>
		IO(() => (__EXTERNAL__.context.setLineDash(array(pattern)), null))

	/**` Mutate.lineDashOffset :: Number -> IO () `*/
	export const lineDashOffset = (offset : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.lineDashOffset = offset, null))

	/**` Mutate.miterLimit :: Number -> IO () `*/
	export const miterLimit = (limit : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.miterLimit = limit, null))

	/**` Mutate.font :: String -> IO () `*/
	export const font = (fontDescription : string) : IO<null> =>
		IO(() => (__EXTERNAL__.context.font = fontDescription, null))

	/**` Mutate.fontSize :: Number -> IO () `*/
	export const fontSize = (size : number) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font =
				`${size}px ${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1)}`
			return null
		})

	/**` Mutate.fontFamily :: String -> IO () `*/
	export const fontFamily = (family : string) : IO<null> =>
		IO(() => {
			__EXTERNAL__.context.font = `${parseFloat(__EXTERNAL__.context.font)}px ${family}`
			return null
		})

	/**` Mutate.textAlign :: TextAlign -> IO () `*/
	export const textAlign = (align : TextAlign) : IO<null> =>
		IO(() => (__EXTERNAL__.context.textAlign = bijectionTextAlign.domain(align) as any, null))

	/**` Mutate.textBaseline :: TextBaseline -> IO () `*/
	export const textBaseline = (baseline : TextBaseline) : IO<null> =>
		IO(() => (__EXTERNAL__.context.textBaseline = bijectionTextBaseline.domain(baseline) as any, null))

	/**` Mutate.fillColor :: String -> IO () `*/
	export const fillColor = (color : string) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillStyle = color, null))

	/**` Mutate.fillRGBA :: Number -> Number -> Number -> Number -> IO () `*/
	export const fillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${r},${g},${b},${a})`, null))

	/**` Mutate.fillVector :: Vector4D -> IO () `*/
	export const fillVector = (v : Vector4D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

	/**` Mutate.strokeColor :: String -> IO () `*/
	export const strokeColor = (color : string) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeStyle = color, null))

	/**` Mutate.strokeRGBA :: Number -> Number -> Number -> Number -> IO () `*/
	export const strokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${r},${g},${b},${a})`, null))

	/**` Mutate.strokeVector :: Vector4D -> IO () `*/
	export const strokeVector = (v : Vector4D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

	/**` Mutate.shadowBlurAmount :: Number -> IO () `*/
	export const shadowBlurAmount = (amount : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.shadowBlur = amount, null))

	/**` Mutate.shadowColor :: String -> IO () `*/
	export const shadowColor = (color : string) : IO<null> =>
		IO(() => (__EXTERNAL__.context.shadowColor = color, null))

	/**` Mutate.shadowRGBA :: Number -> Number -> Number -> Number -> IO () `*/
	export const shadowRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${r},${g},${b},${a})`, null))

	/**` Mutate.shadowVector :: Vector4D -> IO () `*/
	export const shadowVector = (v : Vector4D) : IO<null> =>
		IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

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
		IO(() => (__EXTERNAL__.context.shadowOffsetX = x, null))

	/**` Mutate.shadowOffsetY :: Number -> IO () `*/
	export const shadowOffsetY = (y : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.shadowOffsetY = y, null))

	/**` Mutate.transformationMatrix :: Matrix3x3 -> IO () `*/
	export const transformationMatrix = (m : Matrix3x3) : IO<null> =>
		IO(() => (__EXTERNAL__.context.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null))

	/**` Mutate.alpha :: Number -> IO () `*/
	export const alpha = (opacity : number) : IO<null> =>
		IO(() => (__EXTERNAL__.context.globalAlpha = opacity, null))

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
					x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5,
					w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1
				)
				return null
			})

		/**` Effect.Norm.clearRectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const clearRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.clearRect(
					coordinates.x * __EXTERNAL__.context.canvas.width - 0.5,
					coordinates.y * __EXTERNAL__.context.canvas.height - 0.5,
					dimensions.x * __EXTERNAL__.context.canvas.width + 1,
					dimensions.y * __EXTERNAL__.context.canvas.height + 1
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
					x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5,
					w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1
				)
				return null
			})

		/**` Effect.Norm.rectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const rectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.rect(
					coordinates.x * __EXTERNAL__.context.canvas.width - 0.5,
					coordinates.y * __EXTERNAL__.context.canvas.height - 0.5,
					dimensions.x  * __EXTERNAL__.context.canvas.width + 1,
					dimensions.y  * __EXTERNAL__.context.canvas.height + 1
				)
				return null
			})

		/**` Effect.Norm.fillRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		export const fillRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillRect(
					x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5,
					w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1
				)
				return null
			})

		/**` Effect.Norm.fillRectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const fillRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.fillRect(
					coordinates.x * __EXTERNAL__.context.canvas.width - 0.5,
					coordinates.y * __EXTERNAL__.context.canvas.height - 0.5,
					dimensions.x  * __EXTERNAL__.context.canvas.width + 1,
					dimensions.y  * __EXTERNAL__.context.canvas.height + 1
				)
				return null
			})

		/**` Effect.Norm.strokeRectangle :: Number -> Number -> Number -> Number -> IO () `*/
		export const strokeRectangle = (x : number) => (y : number) => (w : number) => (h : number) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeRect(
					x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5,
					w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1
				)
				return null
			})

		/**` Effect.Norm.strokeRectangleVector :: Vector2D -> Vector2D -> IO () `*/
		export const strokeRectangleVector = (coordinates : Vector2D) => (dimensions : Vector2D) : IO<null> =>
			IO(() => {
				__EXTERNAL__.context.strokeRect(
					coordinates.x * __EXTERNAL__.context.canvas.width - 0.5,
					coordinates.y * __EXTERNAL__.context.canvas.height - 0.5,
					dimensions.x  * __EXTERNAL__.context.canvas.width + 1,
					dimensions.y  * __EXTERNAL__.context.canvas.height + 1
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
			(a : number) => (b : number) => (r : number) : IO<null> =>
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
					ix * __EXTERNAL__.context.canvas.width - 0.5, iy * __EXTERNAL__.context.canvas.height - 0.5,
					(jx - ix) * __EXTERNAL__.context.canvas.width + 1, (jy - iy) * __EXTERNAL__.context.canvas.height + 1
				), null
			))

		/**` Effect.Norm.areaVector :: Vector2D -> Vector2D -> IO () `*/
		export const areaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.rect(
					i.x * __EXTERNAL__.context.canvas.width - 0.5, i.y * __EXTERNAL__.context.canvas.height - 0.5,
					(j.x - i.x) * __EXTERNAL__.context.canvas.width + 1, (j.y - i.y) * __EXTERNAL__.context.canvas.height + 1
				), null
			))

		/**` Effect.Norm.strokeArea :: Number -> Number -> Number -> Number -> IO () `*/
		export const strokeArea = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.strokeRect(
					ix * __EXTERNAL__.context.canvas.width - 0.5, iy * __EXTERNAL__.context.canvas.height - 0.5,
					(jx - ix) * __EXTERNAL__.context.canvas.width + 1, (jy - iy) * __EXTERNAL__.context.canvas.height + 1
				), null
			))

		/**` Effect.Norm.strokeAreaVector :: Vector2D -> Vector2D -> IO () `*/
		export const strokeAreaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.strokeRect(
					i.x * __EXTERNAL__.context.canvas.width - 0.5, i.y * __EXTERNAL__.context.canvas.height - 0.5,
					(j.x - i.x) * __EXTERNAL__.context.canvas.width + 1, (j.y - i.y) * __EXTERNAL__.context.canvas.height + 1
				), null
			))

		/**` Effect.Norm.fillArea :: Number -> Number -> Number -> Number -> IO () `*/
		export const fillArea = (ix : number) => (iy : number) => (jx : number) => (jy : number) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.fillRect(
					ix * __EXTERNAL__.context.canvas.width - 0.5, iy * __EXTERNAL__.context.canvas.height - 0.5,
					(jx - ix) * __EXTERNAL__.context.canvas.width + 1, (jy - iy) * __EXTERNAL__.context.canvas.height + 1
				), null
			))

		/**` Effect.Norm.fillAreaVector :: Vector2D -> Vector2D -> IO () `*/
		export const fillAreaVector = (i : Vector2D) => (j : Vector2D) : IO<null> =>
			IO(() => (
				__EXTERNAL__.context.fillRect(
					i.x * __EXTERNAL__.context.canvas.width - 0.5, i.y * __EXTERNAL__.context.canvas.height - 0.5,
					(j.x - i.x) * __EXTERNAL__.context.canvas.width + 1, (j.y - i.y) * __EXTERNAL__.context.canvas.height + 1
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
	export const queue = <a>(io : IO<a>) : IO<null> =>
		IO(() => (requestAnimationFrame(() => io.INFO()), null))

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
			__EXTERNAL__.isResized    = false
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
	__EXTERNAL__.context = document.querySelector('canvas')!.getContext('2d')!

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
