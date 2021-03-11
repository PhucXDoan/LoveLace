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

/**` TAU :: Number `*/
const TAU : number = 6.283185307179586

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

/** `Vector2D` data-type used for two-dimensional linear algebra.
 * ```
 * data Vector2D = Vector2D Number Number
 * (.x) :: Number
 * (.y) :: Number
 * ```
 */
type Vector2D =
	{
		readonly CONS : 'Vector2D'
		readonly x    : number
		readonly y    : number
	}

/** `Vector3D` data-type used for three-dimensional linear algebra.
 * ```
 * data Vector3D = Vector3D Number Number Number
 * (.x) :: Number
 * (.y) :: Number
 * (.z) :: Number
 * ```
 */
type Vector3D =
	{
		readonly CONS : 'Vector3D'
		readonly x    : number
		readonly y    : number
		readonly z    : number
	}

/** `Vector4D` data-type used for four-dimensional linear algebra.
 * ```
 * data Vector4D = Vector4D Number Number Number Number
 * (.x) :: Number
 * (.y) :: Number
 * (.z) :: Number
 * (.w) :: Number
 * ```
 */
type Vector4D =
	{
		readonly CONS : 'Vector4D'
		readonly x    : number
		readonly y    : number
		readonly z    : number
		readonly w    : number
	}

/** Data type for the representation of 3x3 matrices.
 * ```
 * data Matrix2x2 = Matrix2x2 (4 Number...)
 * (.ix). (.jx) :: Number
 * (.iy), (.jy) :: Number
 * (.i) , (.j)  :: Vector2D
 * (.x) , (.y)  :: Vector2D
 * ```
 */
type Matrix2x2 =
	{
		readonly CONS : 'Matrix2x2'
		readonly ix   : number
		readonly jx   : number
		readonly iy   : number
		readonly jy   : number
		readonly i    : Vector2D
		readonly j    : Vector2D
		readonly x    : Vector2D
		readonly y    : Vector2D
	}

/** Data type for the representation of 3x3 matrices.
 * ```
 * data Matrix3x3 = Matrix3x3 (9 Number...)
 * (.ix), (.jx), (.kx) :: Number
 * (.iy), (.jy), (.ky) :: Number
 * (.iz), (.jz), (.kz) :: Number
 * (.i) , (.j) , (.k)  :: Vector3D
 * (.x) , (.y) , (.z)  :: Vector3D
 * ```
 */
type Matrix3x3 =
	{
		readonly CONS : 'Matrix3x3'
		readonly ix   : number
		readonly jx   : number
		readonly kx   : number
		readonly iy   : number
		readonly jy   : number
		readonly ky   : number
		readonly iz   : number
		readonly jz   : number
		readonly kz   : number
		readonly i    : Vector3D
		readonly j    : Vector3D
		readonly k    : Vector3D
		readonly x    : Vector3D
		readonly y    : Vector3D
		readonly z    : Vector3D
	}

/** Data type for the representation of 4x4 matrices.
 * ```
 * data Matrix4x4 = Matrix4x4 (16 Number...)
 * (.ix), (.jx), (.kx), (.lx) :: Number
 * (.iy), (.jy), (.ky), (.ly) :: Number
 * (.iz), (.jz), (.kz), (.lz) :: Number
 * (.iw), (.jw), (.kw), (.lw) :: Number
 * (.i) , (.j) , (.k) , (.l)  :: Vector4D
 * (.x) , (.y) , (.z) , (.w)  :: Vector4D
 * ```
 */
type Matrix4x4 =
	{
		readonly CONS : 'Matrix4x4'
		readonly ix   : number
		readonly jx   : number
		readonly kx   : number
		readonly lx   : number
		readonly iy   : number
		readonly jy   : number
		readonly ky   : number
		readonly ly   : number
		readonly iz   : number
		readonly jz   : number
		readonly kz   : number
		readonly lz   : number
		readonly iw   : number
		readonly jw   : number
		readonly kw   : number
		readonly lw   : number
		readonly i    : Vector4D
		readonly j    : Vector4D
		readonly k    : Vector4D
		readonly l    : Vector4D
		readonly x    : Vector4D
		readonly y    : Vector4D
		readonly z    : Vector4D
		readonly w    : Vector4D
	}

/** A simple data type that stores information about the measurement of texts in rendering.
 * ```
 * data TextMeasurement = TextMeasurement String Number Number Number
 * (.text)   :: String
 * (.width)  :: Number
 * (.height) :: Number
 * ```
 */
type TextMeasurement =
	{
		readonly CONS   : 'TextMeasurement'
		readonly text   : string
		readonly width  : number
		readonly height : number
	}

/** An inline version of the `switch` statement.
 * ```
 * data Switch a b = Switch.case a (() -> b)
 * (.case) :: Switch a b -> a -> (() -> b) -> Switch a b
 * (.fall) :: Switch a b ->      (() -> b) -> Switch a b
 * (.with) :: Switch a b -> a -> b
 * (.thru) :: Switch a a -> a -> a
 * ```
 */
type Switch<a, b> =
	{
		readonly CONS : 'Switch'
		readonly case : (domain : a)   => (codomain : () => b) => Switch<a, b>
		readonly fall : (codomain : () => b) => Switch<a, b>
		readonly with : (value : a)    => b
		readonly thru : (value : a)    => a
	}

/** A data structure that maps values to other values supplied with an inverse.
 * ```
 * data Bijection a b = Bijection.of a b
 * (.of)       :: Bijection a b -> a -> b -> Bijection a b
 * (.domain)   :: Bijection a b -> a -> b
 * (.codomain) :: Bijection a b -> b -> a
 * ```
 */
type Bijection<a, b> =
	{
		readonly CONS     : 'Bijection'
		readonly INFO     : ReadonlyArray<[a, b]>
		readonly of       : (domainValue : a) => (codomainValue : b) => Bijection<a, b>
		readonly domain   : (domainValue   : a) => b
		readonly codomain : (codomainValue : b) => a
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
				? THROWRANGE(`Out of bounds index (${i}) occured with 'List' monad; indexing returned 'undefined'`)
				: elements[i] as a
	})

/**` Vector2D :: Number -> Number -> Vector2D `*/
const Vector2D = (x : number) => (y : number) : Vector2D =>
	({
		CONS : 'Vector2D',
		x, y
	})

/**` Vector3D :: Number -> Number -> Number -> Vector3D `*/
const Vector3D = (x : number) => (y : number) => (z : number) : Vector3D =>
	({
		CONS : 'Vector3D',
		x, y, z
	})

/**` Vector4D :: Number -> Number -> Number -> Number -> Vector4D `*/
const Vector4D = (x : number) => (y : number) => (z : number) => (w : number) : Vector4D =>
	({
		CONS : 'Vector4D',
		x, y, z, w
	})

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

/**` Matrix2D :: Vector2D -> Vector2D -> Matrix2x2 `*/
const Matrix2D = (i : Vector2D) => (j : Vector2D) : Matrix2x2 =>
	Matrix2x2
		(i.x)(j.x)
		(i.y)(j.y)

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

/**` Matrix3D :: Vector3D -> Vector3D -> Vector3D -> Matrix3x3 `*/
const Matrix3D = (i : Vector3D) => (j : Vector3D) => (k : Vector3D) : Matrix3x3 =>
	Matrix3x3
		(i.x)(j.x)(k.x)
		(i.y)(j.y)(k.y)
		(i.z)(j.z)(k.z)

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

/**` Matrix4D :: Vector4D -> Vector4D -> Vector4D -> Vector4D -> Matrix4x4 `*/
const Matrix4D = (i : Vector4D) => (j : Vector4D) => (k : Vector4D) => (l : Vector4D) : Matrix4x4 =>
	Matrix4x4
		(i.x)(j.x)(k.x)(l.x)
		(i.y)(j.y)(k.y)(l.y)
		(i.z)(j.z)(k.z)(l.z)
		(i.w)(j.w)(k.w)(l.w)

/**` TextMeasurement :: String -> Number -> Number -> TextMeasurement `*/
const TextMeasurement = (text : string) => (width : number) => (height : number) : TextMeasurement =>
	({
		CONS : 'TextMeasurement',
		text, width, height
	})

/**` Switch.case :: a -> (() -> b) -> Switch a b `*/
const Switch = <a, b>(f : (x : a) => b | undefined) : Switch<a, b> =>
	({
		CONS : 'Switch',
		case : x => y =>
			Switch(z => {
				const w = f(z)
				return w === undefined && z === x ? y() : w
			}),
		fall : x => Switch(y => {
			const z = f(y)
			return z === undefined ? x() : z
		}),
		with : x => {
			const y = f(x)
			return y === undefined
				? THROWRANGE(`'Switch' did not cover all cases; missing case on value: '${x}'`)
				: y
		},
		thru : x => {
			const y = f(x)
			return y === undefined ? x : y as unknown as a
		}
	})

Switch.case = <a>(domain : a) => <b>(codomain : () => b) : Switch<a, b> =>
	Switch<a, b>(x => x === domain ? codomain() : undefined)

/**` Bijection.of :: a -> b -> Bijection a b `*/
const Bijection = <a, b>(pairs : ReadonlyArray<[a, b]>) : Bijection<a, b> =>
	({
		CONS     : 'Bijection',
		INFO     : pairs,
		of       : x => y => Bijection([...pairs, [x, y]]),
		domain   : x => {
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
const relaxHorizontal : (direction : Horizontal) => Horizontal =
	Switch
		.case(Horizontal.Leftward)  (() => Horizontal.Left)
		.case(Horizontal.Rightward) (() => Horizontal.Right)
		.thru

/**` relaxVertical :: Vertical -> Vertical `*/
const relaxVertical : (direction : Vertical) => Vertical =
	Switch
		.case (Vertical.Downward) (() => Vertical.Down)
		.case (Vertical.Upward)   (() => Vertical.Up)
		.thru

/**` relaxLateral :: Lateral -> Lateral `*/
const relaxLateral : (direction : Lateral) => Lateral =
	Switch
		.case (Lateral.Backward) (() => Lateral.Back)
		.case (Lateral.Forward)  (() => Lateral.Fore)
		.thru

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

/********************************************************************************************************************************/

// The Do-Notation syntax where a monad stores an empty object.
const Do =
	{
		IO    : IO    (() => Object.create(null)),
		Maybe : Just  (Object.create(null)),
		State : State ((s : any) => [s, Object.create(null)]),
		List  : List  (Object.create(null))
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
