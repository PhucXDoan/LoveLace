/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable no-return-assign                  */
/* eslint-disable no-plusplus                       */
/* eslint-disable no-console                        */
/* eslint-disable no-multi-assign                   */
/* eslint-disable no-loop-func                      */
/* eslint-disable max-len                           */
/* eslint-disable no-alert                          */

/********************************************************************************************************************************/
// Overhead //

const MAX   = 2048
const SCOPE = Object.create(null)

type Variation <a, b> = a & { variation : b }

/**` halt : a `*/
declare const halt : never
Object.defineProperty(this, "halt", { get() { throw `| LoveLace halted due to an error` } })

/**` never : a `*/
declare const never : never
Object.defineProperty(this, "never", { get() { throw `| Unexhuastive pattern matching reaches 'never'` } })

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

type KeyboardKey = typeof keyboardKeysArray [number]

const __MACRO__ =
	{
		clickx   : (i : number) : IO <Maybe <number>> => IO (() => Ψ.mouseButtons[i] === 'toDown' ? Just (Ψ.mouseCX) : Nothing),
		clicky   : (i : number) : IO <Maybe <number>> => IO (() => Ψ.mouseButtons[i] === 'toDown' ? Just (Ψ.mouseCY) : Nothing),
		clickv   : (i : number) : IO <Maybe <V2>>     => IO (() => Ψ.mouseButtons[i] === 'toDown' ? Just (V2 (Ψ.mouseCX, Ψ.mouseCY)) : Nothing),
		n_clickx : (i : number) : IO <Maybe <number>> => IO (() => Ψ.mouseButtons[i] === 'toDown' ? Just (Ψ.mouseCX / Ψ.ctx.canvas.width) : Nothing),
		n_clicky : (i : number) : IO <Maybe <number>> => IO (() => Ψ.mouseButtons[i] === 'toDown' ? Just (Ψ.mouseCY / Ψ.ctx.canvas.height) : Nothing),
		n_clickv : (i : number) : IO <Maybe <V2>> =>
			IO (() =>
				Ψ.mouseButtons[i] === 'toDown'
					? Just (V2 (Ψ.mouseCX / Ψ.ctx.canvas.width, Ψ.mouseCY / Ψ.ctx.canvas.height))
					: Nothing
			),

		err_nonexisting_image_path : (funcName : string, path : string) : never =>
		{
			console.error(`'${funcName}' received an unloaded (possibly non-existing) image path`)
			console.dir(`Signature : ${funcName} (<PATH>)`)
			console.dir(`<PATH> =`, path)
			return halt
		},

		err_nonexisting_audio_path : (funcName : string, path : string) : never =>
		{
			console.error(`'${funcName}' received an unloaded (possibly non-existing) audio path`)
			console.dir(`Signature : ${funcName} (<PATH>)`)
			console.dir(`<PATH> =`, path)
			return halt
		},

		err_layer_index : (funcName : string, index : number) : never =>
		{
			console.error(`'${funcName}' only takes an integer in the interval [0, ${Ψ.ctxs.length}) for an index`)
			console.dir(`Signature : ${clearLayer} (<INDEX>)`)
			console.dir(`<INDEX> =`, index)
			return halt
		},
		default_implementation_list_eq : <a>(xs : List <a>) => (ys : List <a>) : boolean =>
		{
			if (xs.variation === ys.variation)
				if (xs.variation === 'Nil')
					return true
				else if ((xs.head as any) ?.eq === undefined)
				{
					console.error(`'.eq' cannot compare the elements of the given Lists that does not implement '.eq'`)
					console.dir(`Signature : <LEFTSIDE> .eq (<RIGHTSIDE>) | Error originated from Macro`)
					console.dir(`<LEFTSIDE>  =`, xs)
					console.dir(`<RIGHTSIDE> =`, ys)
					return halt
				}
				else
				{
					let xs_ : List <a> = xs
					let ys_ : List <a> = ys
					for (let i = 0; xs_.variation === 'Cons' && ys_.variation === 'Cons'; ++i)
						if (i === MAX)
						{
							console.error(`'.eq' traversed too many elements`)
							console.dir(`Signature : <LEFTSIDE> .eq (<RIGHTSIDE>) | Error originated from Macro'`)
							console.dir(`<LEFTSIDE>  =`, xs)
							console.dir(`<RIGHTSIDE> =`, ys)
							return halt
						}
						else if (!(xs_.head as any) .eq (ys_.head))
							return false
						else
							xs_ = xs_.tail,
							ys_ = ys_.tail
					return xs_.variation === ys_.variation
				}
			else return false
		},
		default_implementation_list_show : <a>(xs : List <a>) : string =>
		{
			if (xs.$show === undefined)
				if ((xs.head as any) ?.eq === undefined)
				{
					console.error(`'.show' cannot stringify the elements of the List that does not implement '.show'`)
					console.dir(`Signature : <VALUE> .show | Error orignated from Macro`)
					console.dir(`<VALUE> =`, xs)
					return halt
				}
				else
				{
					let str            = "List ("
					let xs_ : List <a> = xs
					for (let i = 0; xs_.variation === 'Cons'; ++i)
						if (i === MAX)
						{
							console.error(`'.show' traversed too many elements`)
							console.dir(`Signature : <VALUE> .show | Error originated from Macro`)
							console.dir(`<VALUE>  =`, xs)
							return halt
						}
						else
							str += (xs_.head as any).show + ", ",
							xs_ = xs_.tail
					return str.slice(0, -2) + ")"
				}
			else return xs.$show
		}
	}

/********************************************************************************************************************************/
// Typeclasses //

type Eq <a>   =
	{
		/**` (a).eq : a -> Boolean `*/
		eq : (value : a) => boolean
	}

type Show <a> =
	{
		/**` (a).show : String `*/
		show : string
	}

/********************************************************************************************************************************/
// Algebraic Data Types //

interface Boolean
	{
		variation  : 'Boolean'

		/**` (Boolean).pipe : (Boolean -> a) -> a `*/
		pipe : <a>(morphism : (bool : boolean) => a) => a

		/**` (Boolean).eq : Boolean -> Boolean `*/
		eq : (bool : boolean) => boolean

		/**` (Boolean).show : String `*/
		show : string
	}

interface Number
	{
		variation  : 'Number'

		/**` (Number).pipe : (Number -> a) -> a `*/
		pipe : <a>(morphism : (num : number) => a) => a

		/**` (Number).eq : Number -> Boolean `*/
		eq : (num : number) => boolean

		/**` (Number).show : String `*/
		show : string
	}

interface String
	{
		variation  : 'String'

		/**` (String).pipe : (String -> a) -> a `*/
		pipe : <a>(morphism : (str : string) => a) => a

		/**` (String).eq : String -> Boolean `*/
		eq : (str : string) => boolean

		/**` (String).show : String `*/
		show : string
	}

interface Array<T>
	{
		variation  : 'Array'

		/**` ([a]).pipe : ([a] -> b) -> b `*/
		pipe : <a>(morphism : (array : Array <T>) => a) => a

		/**` ([a]).eq : [a] -> Boolean `*/
		eq : (array : Array <T>) => boolean

		/**` ([a]).show : String `*/
		show : string
	}

type IO <a> =
	{
		variation : 'IO'

		/**` (IO a).pipe : (IO a -> b) -> b `*/
		pipe : <b>(morphism : (io : IO <a>) => b) => b

		/**` (IO a).bind : (a -> IO b) -> IO b `*/
		bind : <b>(reaction : (output : a) => IO <b>) => IO <b>

		/**` (IO a).fmap : (a -> b) -> IO b `*/
		fmap : <b>(morphism : (output : a) => b) => IO <b>

		/**` (IO $).bindto : String -> ($ -> IO b) -> IO $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => IO <b>) => IO <a & { [x in k] : b }>

		/**` (IO $).fmapto : String -> ($ -> b) -> IO $ `*/
		fmapto : <k extends string>(name : k) => <b>(morphism : ($ : a) => b) => IO <a & { [x in k] : b }>

		/**` (IO a).then : IO b -> IO b `*/
		then : <b>(successor : IO <b>) => IO <b>

		/**` (IO a).cast : b -> IO b `*/
		cast : <b>(replacement : b) => IO <b>

		/**` (IO a).call : (a -> IO b) -> IO a `*/
		call : <b>(reaction : (output : a) => IO <b>) => IO <a>

		/**` (IO a).side : IO b -> IO a `*/
		side : <b>(effect : IO <b>) => IO <a>

		/**` (IO a).effect : () -> a `*/
		effect : () => a
	}

type Process <s, a> =
	{
		variation : 'Process'

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

		/**` (Process s a).cast : b -> Process s b `*/
		cast : <b>(replacement : b) => Process <s, b>

		/**` (Process s a).call : (a -> Process s b) -> Process s a `*/
		call : <b>(reaction : (output : a) => Process <s, b>) => Process <s, a>

		/**` (Process s a).side : Process s b -> Process s a `*/
		side : <b>(effect : Process <s, b>) => Process <s, a>

		/**` (Process s a).computation : s -> Pair s a `*/
		computation : (state : s) => Pair <s, a>
	}

type Maybe <a> =
	{
		$show ?: string

		/**` (Maybe a).pipe : (Maybe a -> b) -> b `*/
		pipe : <b>(morphism : (maybe : Maybe <a>) => b) => b

		/**` (Maybe a).eq : Maybe a -> Boolean `*/
		eq : (maybe : Maybe <a>) => boolean

		/**` (Maybe a).show : String `*/
		show : string

		/**` (Maybe a).bind : (a -> Maybe b) -> Maybe b `*/
		bind : <b>(reaction : (value : a) => Maybe <b>) => Maybe <b>

		/**` (Maybe a).fmap : (a -> b) -> Maybe b `*/
		fmap : <b>(morphism : (value : a) => b) => Maybe <b>

		/**` (Maybe $).bindto : String -> ($ -> Maybe b) -> Maybe $ `*/
		bindto : <k extends string>(name : k) => <b>(reaction : ($ : a) => Maybe <b>) => Maybe <a & { [x in k] : b }>

		/**` (Maybe $).fmapto : String -> ($ -> b) -> Maybe $ `*/
		fmapto : <k extends string>(name : k) => <b>(reaction : ($ : a) => b) => Maybe <a & { [x in k] : b }>
	} & ({
		variation : 'Nothing'
	} | {
		variation : 'Just'

		/**` (Maybe a).value : a `*/
		value : a
	})

type List <a> =
	{
		/**` (List a).pipe : (List a -> b) -> b `*/
		pipe : <b>(morphism : (xs : List <a>) => b) => b

		/**` (List a).eq : List a -> Boolean `*/
		eq : (xs : List <a>) => boolean

		/**` (List a).show : String `*/
		show : string

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
	} & ({
		variation : 'Nil'
		$reverse  : Variation <List <a>, 'Nil'>
		$len      : 0
		$show     : 'List ()'
	} | {
		variation : 'Cons'
		$head    ?: a
		$tail    ?: List <a>
		$last    ?: a
		$init    ?: List <a>
		$reverse ?: Variation <List <a>, 'Cons'>
		$len     ?: number
		$show    ?: string

		/**` (List a).head : a `*/
		head : a

		/**` (List a).tail : List a `*/
		tail : List <a>
	})

type Pair <a, b> =
	{
		variation : 'Pair'
		$show    ?: string

		/**` (Pair a b).pipe : (Pair a b -> c) -> c `*/
		pipe : <c>(morphism : (pair : Pair <a, b>) => c) => c

		/**` (Pair a b).eq : Pair a b -> Boolean `*/
		eq : (pair : Pair <a, b>) => boolean

		/**` (Pair a b).show : String `*/
		show : string

		/**` (Pair a b).fst : a `*/
		fst : a

		/**` (Pair a b).snd : b `*/
		snd : b
	}

type Either <a, b> =
	{
		$show ?: string

		/**` (Either a b).pipe : (Either a b -> c) -> c `*/
		pipe : <c>(morphism : (either : Either <a, b>) => c) => c

		/**` (Either a b).eq : Either a b -> Boolean `*/
		eq : (either : Either <a, b>) => boolean

		/**` (Either a b).show : String `*/
		show : string
	} & ({
		variation : 'Left'

		/**` (Either a b).value : a `*/
		value : a
	} | {
		variation : 'Right'

		/**` (Either a b).value : b `*/
		value : b
	})

type V2 =
	{
		variation : 'V2'

		/**` (V2).pipe : (V2 -> a) -> a `*/
		pipe : <a>(morphism : (vector : V2) => a) => a

		/**` (V2).eq : V2 -> Boolean `*/
		eq : (vector : V2) => boolean

		/**` (V2).show : String `*/
		show : string

		/**` (V2).x : Number `*/
		x : number

		/**` (V2).y : Number `*/
		y : number
	}

type V3 =
	{
		variation : 'V3'

		/**` (V3).pipe : (V3 -> a) -> a `*/
		pipe : <a>(morphism : (vector : V3) => a) => a

		/**` (V3).eq : V3 -> Boolean `*/
		eq : (vector : V3) => boolean

		/**` (V3).show : String `*/
		show : string

		/**` (V3).x : Number `*/
		x : number

		/**` (V3).y : Number `*/
		y : number

		/**` (V3).y : Number `*/
		z : number
	}

type V4 =
	{
		variation : 'V4'

		/**` (V4).pipe : (V4 -> a) -> a `*/
		pipe : <a>(morphism : (vector : V4) => a) => a

		/**` (V4).eq : V4 -> Boolean `*/
		eq : (vector : V4) => boolean

		/**` (V4).show : String `*/
		show : string

		/**` (V4).x : Number `*/
		x : number

		/**` (V4).y : Number `*/
		y : number

		/**` (V4).y : Number `*/
		z : number

		/**` (V4).w : Number `*/
		w : number
	}

type Axis =
	| 'Positive' | 'Negative' | 'Zero'

type ButtonState =
	| 'Up'   | 'Down'
	| 'toUp' | 'toDown'

type LineCap =
	| 'butt' | 'round' | 'square'

type LineJoin =
	| 'round' | 'bevel' | 'miter'

type TextAlign =
	| 'start'
	| 'end'
	| 'left'
	| 'right'
	| 'center'

type TextBaseline =
	| 'top'
	| 'hanging'
	| 'middle'
	| 'alphabetic'
	| 'ideographic'
	| 'bottom'

type Composition =
	| 'source-over'      | 'source-in'        | 'source-out'     | 'source-atop'
	| 'destination-over' | 'destination-atop' | 'destination-in' | 'destination-out'
	| 'color-dodge'      | 'color-burn'       | 'hard-light'     | 'soft-light'
	| 'lighten'          | 'lighter'          | 'darken'         | 'copy'
	| 'xor'              | 'multiply'         | 'screen'         | 'overlay'
	| 'color'            | 'hue'              | 'saturation'     | 'luminosity'
	| 'difference'       | 'exclusion'

/********************************************************************************************************************************/
// Primitive Functions

/**` e : Number `*/
const e : number = Math.E

/**` pi : Number `*/
const pi : number = Math.PI

/**` tau : Number `*/
const tau : number = 2 * Math.PI

/**` ln2 : Number `*/
const ln2 : number = Math.LN2

/**` ln10 : Number `*/
const ln10 : number = Math.LN10

/**` log2e : Number `*/
const log2e : number = Math.LOG2E

/**` log10e : Number `*/
const log10e : number = Math.LOG10E

/**` sqrt2 : Number `*/
const sqrt2 : number = Math.SQRT2

/**` invsqrt2 : Number `*/
const invsqrt2 : number = Math.SQRT1_2

/********************************************************************************************************************************/
// Primitive Functions

/**` id : a -> a `*/
const id = <a>(value : a) : a => value

/**` notf : (a -> Boolean) -> a -> Boolean `*/
const notf = <a>(predicate : (value : a) => boolean) => (value : a) : boolean => !predicate (value)

/**` eqeqeq : a -> a -> Boolean `*/
const eqeqeq = <a>(leftside : a) => (rightside : a) : boolean => leftside === rightside

/**` toStr : $ -> String `*/
const toStr = (obj : Object) : string => obj.toString()

/********************************************************************************************************************************/
// Globalization of Typeclass Functions //

/**` eq : (Eq a) => a -> a -> Boolean `*/
const eq = <a extends Eq <a>>(leftside : a) : (rightside : a) => boolean => leftside.eq

/**` show : (Show a) => a -> String `*/
const show = <a extends Show <a>>(value : a) : string => value.show

/********************************************************************************************************************************/
// Implementation of Algebraic Data Types //

Object.defineProperties(Boolean.prototype, {
	variation : { value : 'Boolean'                           },
	pipe      : { get() { return (f : any) => f (!!+this)   } },
	eq        : { get() { return (b : any) => !!+this === b } },
	show      : { get() { return this.toString()            } }
})

Object.defineProperties(Number.prototype, {
	variation : { value : 'Number'                          },
	pipe      : { get() { return (f : any) => f (+this)   } },
	eq        : { get() { return (n : any) => +this === n } },
	show      : { get() { return this.toString()          } }
})

Object.defineProperties(String.prototype, {
	variation : { value : 'String'                                    },
	pipe      : { get() { return (f : any) => f (this.toString())   } },
	eq        : { get() { return (s : any) => this.toString() === s } },
	show      : { get() { return `"${this.toString()}"`             } }
})

Object.defineProperties(Array.prototype, {
	variation : { value : 'Array'                },
	pipe      : { get() { return (f : any) => f (this) } },
	eq        :
		{
			get()
			{
				return (array : any) =>
				{
					if (this.length === array.length)
						if (this.length === 0)
							return true
						else if (this [0] ?.eq === undefined)
						{
							console.error(`'.eq' cannot compare the elements of the given arrays that does not implement '.eq'`)
							console.dir(`Signature : (<LEFTSIDE>) .eq  (<RIGHTSIDE>)`)
							console.dir(`<LEFTSIDE>  =`, this)
							console.dir(`<RIGHTSIDE> =`, array)
							return halt
						}
						else
						{
							for (let i = 0; i < this.length; ++i)
								if (!this [i] .eq (array [i]))
									return false
							return true
						}
					else return false
				}
			}
		},
	show      :
		{
			get()
			{
				if (this.length === 0)
					return "[]"
				else if (this [0] ?.show === undefined)
				{
					console.error(`'.show' cannot stringify the elements of an array that does not implement '.show'`)
					console.dir(`Signature : (<VALUE>) .show`)
					console.dir(`<VALUE> =`, this)
					return halt
				}
				else return `[${this.map((element : any) => element.show).join(", ")}]`
			}
		}
})

/**` IO : (() -> a) -> IO a `*/
const IO = <a>(effect : () => a) : IO <a> =>
	({
		variation : 'IO',
		effect,
		pipe   (f) { return f (this) },
		bind   : f => IO (() => f (effect ()).effect ()),
		fmap   : f => IO (() => f (effect ())),
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
		then   : io => IO (() => (effect (), io.effect ())),
		cast   : x  => IO (() => (effect (), x)),
		call   : f  =>
			IO (() => {
				const x = effect ()
				return f (x).effect (), x
			}),
		side   : io =>
			IO (() => {
				const x = effect ()
				return io.effect (), x
			})
	})

/**` Process : (s -> Pair s a) -> Process s a `*/
const Process = <s, a>(computation : (state : s) => Pair <s, a>) : Process <s, a> =>
	({
		variation : 'Process',
		pipe (f) { return f (this) },
		bind   : f =>
			Process (s => {
				const p = computation (s)
				return f (p.snd).computation (p.fst)
			}),
		fmap   : f =>
			Process (s => {
				const p = computation (s)
				return Pair (p.fst, f (p.snd))
			}),
		bindto : k => f =>
			Process (s => {
				const p0 = computation (s)
				const p1 = f (p0.snd).computation (p0.fst)
				return Pair (p1.fst, { ...p0.snd, [k] : p1.snd } as any)
			}),
		fmapto : k => f =>
			Process (s => {
				const p = computation (s)
				return Pair (p.fst, { ...p.snd, [k] : f (p.snd) } as any)
			}),
		then   : p => Process (s => p.computation (computation (s).fst)),
		cast   : x => Process (s => Pair (computation (s).fst, x)),
		call   : f =>
			Process (s => {
				const p = computation (s)
				return Pair (f (p.snd).computation (p.fst).fst, p.snd)
			}),
		side   : p =>
			Process (s => {
				const pr = computation (s)
				return Pair (p.computation (pr.fst).fst, pr.snd)
			}),
		computation
	})

/**` Nothing : Maybe a `*/
const Nothing : Maybe <any> =
	{
		variation : 'Nothing',
		$show     : 'Nothing',
		show      : 'Nothing',
		eq        : m => m === Nothing,
		pipe      : f => f (Nothing),
		bind      : _ => Nothing,
		fmap      : _ => Nothing,
		bindto    : _ => _ => Nothing,
		fmapto    : _ => _ => Nothing
	} as Maybe <any>

/**` Just : a -> Maybe a `*/
const Just = <a>(value : a) : Maybe <a> =>
	({
		variation : 'Just',
		pipe (f) { return f (this) },
		eq : m =>
		{
			if (m.variation === 'Nothing')
				return false
			else if ((value as any) ?.eq === undefined)
			{
				console.error(`'.eq' cannot compare the two given Maybe values that does not implement '.eq'`)
				console.dir(`Signature : Just (<LEFTSIDE>) .eq (Just (<RIGHTSIDE>))`)
				console.dir(`<LEFTSIDE>  =`, value)
				console.dir(`<RIGHTSIDE> =`, m.value)
				return halt
			}
			else return (value as any) .eq (m.value)
		},
		get show()
		{
			if (this.$show === undefined)
			{
				if ((value as any) ?.show === undefined)
				{
					console.error(`'.show' cannot stringify the given Maybe value as it does not implement '.show'`)
					console.dir(`Signature : Just (<VALUE>) .show`)
					console.dir(`<VALUE> =`, value)
					return halt
				}
				else return this.$show = `Just (${(value as any) .show})`
			}
			else return this.$show
		},
		bind      : f => f (value),
		fmap      : f => Just (f (value)),
		bindto    : k => f => f (value) .fmap (x => ({ ...value, [k] : x } as any)),
		fmapto    : k => f => Just ({ ...value, [k] : f (value) } as any),
		value
	})

/**` Nil : List a `*/
const Nil : List <any> =
	{
		variation : 'Nil',
		$show     : 'List ()',
		show      : 'List ()',
		pipe      : f  => f (Nil),
		eq        : xs => xs === Nil,
		get $reverse () { return this },
		$len   : 0,
		bind   : _  => Nil,
		fmap   : _  => Nil,
		fmapto : _  => _ => Nil,
		bindto : _  => _ => Nil,
		link   : id
	} as List <any>

/**` Cons : (() -> a) -> (() -> List a) -> List a `*/
const Cons = <a>(lvalue : () => a) => (lxs : () => List <a>) : List <a> =>
	({
		variation : 'Cons',
		pipe (f) { return f (this) },
		eq (xs)
		{
			if (xs.variation === 'Nil')
				return false
			else if ((this.head as any) ?.eq === undefined)
			{
				console.error(`'.eq' cannot compare the elements of the given Lists that does not implement '.eq'`)
				console.dir(`Signature : <LEFTSIDE> .eq (<RIGHTSIDE>) | <LEFTSIDE> originated from 'Cons'`)
				console.dir(`<LEFTSIDE>  =`, this)
				console.dir(`<RIGHTSIDE> =`, xs)
				return halt
			}
			else
			{
				let this_ : List <a> = this
				let xs_   : List <a> = xs
				for (let i = 0; this_.variation === 'Cons' && xs_.variation === 'Cons'; ++i)
					if (i === MAX)
					{
						console.error(`'.eq' traversed too many elements`)
						console.dir(`Signature : <LEFTSIDE> .eq (<RIGHTSIDE>) | <LEFTSIDE> originated from 'Cons'`)
						console.dir(`<LEFTSIDE>  =`, this)
						console.dir(`<RIGHTSIDE> =`, xs)
						return halt
					}
					else if (!(this_.head as any) .eq (xs_.head))
						return false
					else
						this_ = this_.tail,
						xs_   = xs_.tail
				return this_.variation === xs_.variation
			}
		},
		get show()
		{
			if (this.$show === undefined)
				if ((this.head as any) ?.eq === undefined)
				{
					console.error(`'.show' cannot stringify the elements of the List that does not implement '.show'`)
					console.dir(`Signature : <VALUE> .show | <VALUE> originated from 'Cons'`)
					console.dir(`<VALUE> =`, this)
					return halt
				}
				else
				{
					let str              = "List ("
					let this_ : List <a> = this
					for (let i = 0; this_.variation === 'Cons'; ++i)
						if (i === MAX)
						{

						}
						else
							str += (this_.head as any).show + ", ",
							this_ = this_.tail
					return this.$show = str.slice(0, -2) + ")"
				}
			else return this.$show
		},
		get head () { return this.$head ??= lvalue () },
		get tail () { return this.$tail ??= lxs    () },
		bind (f)
		{
			let xs : List <a> = this
			for (let i = 0; xs.variation === 'Cons'; ++i)
				if (i === MAX)
				{
					console.error(`'.bind' traversed too many elements (${MAX})`)
					console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> orignated from 'Cons'`)
					console.dir(`<OBJECT>   =`, this)
					console.dir(`<REACTION> =`, f)
					return halt
				}
				else
				{
					const ys = f (xs.head)
					if (ys.variation === 'Cons')
						return Cons (() => ys.head) (() => ys.tail .link ((xs as any).tail .bind (f)))
					else
						xs = xs.tail
				}
			return Nil
		},
		fmap (f)
		{
			return Cons (() => f (this.head)) (() => this.tail .fmap (f))
		},
		bindto (k)
		{
			return f =>
			{
				let xs : List <a> = this
				for (let i = 0; xs.variation === 'Cons'; ++i)
					if (i === MAX)
					{
						console.error(`'.bindto' traversed too many elements (${MAX})`)
						console.dir(`Signature : (<OBJECT>) .bindto (<NAME>) (<REACTION>) | <OBJECT> originated from 'Cons'`)
						console.dir(`<OBJECT>   =`, this)
						console.dir(`<NAME>     =`, k)
						console.dir(`<REACTION> =`, f)
						return halt
					}
					else
					{
						const ys = f (xs.head)
						if (ys.variation === 'Cons')
						{
							const zs = ys .fmap (y => ({ ...(xs as any).head, [k] : y } as any))
							return Cons (() => (zs as any).head) (() => (zs as any).tail .link ((xs as any).tail .bindto (k) (f)))
						}
						else
							xs = xs.tail
					}
				return Nil
			}
		},
		fmapto (k)
		{
			return f =>
				Cons (() => ({ ...this.head, [k] : f (this.head) } as any)) (() => this.tail .fmapto (k) (f))
		},
		link (xs)
		{
			return xs.variation === 'Nil'
				? this
				: Cons (() => this.head) (() => this.tail .link (xs))
		}
	})

/**` Pair : (a, b) -> Pair a b `*/
const Pair = <a, b>(fst : a, snd : b) : Pair <a, b> =>
	({
		variation  : 'Pair',
		pipe (f) { return f (this) },
		eq : p =>
		{
			if ((fst as any) ?.eq === undefined || (snd as any) ?.eq === undefined)
			{
				console.error(`'.eq' cannot compare Pair values that do not implement '.eq'`)
				console.dir(`Signature : Pair (<LEFTSIDE_FST>, <LEFTSIDE_SND>) .eq (Pair (<RIGHTSIDE_FST, RIGHTSIDE_SND>))`)
				console.dir(`<LEFTSIDE_FST> =`, fst)
				console.dir(`<LEFTSIDE_SND> =`, snd)
				console.dir(`<RIGHTSIDE_FST> =`, p.fst)
				console.dir(`<RIGHTSIDE_SND> =`, p.snd)
				return halt
			}
			else return (fst as any) .eq (p.fst) && (snd as any) .eq (p.snd)
		},
		get show()
		{
			if (this.$show === undefined)
				if ((fst as any) ?.show === undefined || (snd as any) ?.show === undefined)
				{
					console.error(`'.show' cannot stringify Pair values that do not implement '.show'`)
					console.dir(`Signature : Pair (<FST>, <SND>) .show`)
					console.dir(`<FST> =`, fst)
					console.dir(`<SND> =`, snd)
					return halt
				}
				else return this.$show = `Pair (${(fst as any) .show}, ${(snd as any) .show})`
			else return this.$show
		},
		fst, snd
	})

/**` Left : a -> Either a b `*/
const Left = <a, b>(value : a) : Either <a, b> =>
	({
		variation : 'Left',
		pipe (f) { return f (this) },
		eq : er =>
		{
			if (er.variation === 'Left')
				return false
			else if ((value as any) ?.eq === undefined)
			{
				console.error(`'.eq' cannot compare Left values that do not implement '.eq'`)
				console.dir(`Signature : Left (<LEFTSIDE>) .eq (Left (<RIGHTSIDE>))`)
				console.dir(`<LEFTSIDE> =`, value)
				console.dir(`<RIGHTSIDE> =`, er.value)
				return halt
			}
			else return (value as any) .eq (er.value)
		},
		get show()
		{
			if (this.$show === undefined)
				if ((value as any) ?.show === undefined)
				{
					console.error(`'.show' cannot stringify a Left value that does not implement '.show'`)
					console.dir(`Signature : Left (<VALUE>) .show`)
					console.dir(`<VALUE> =`, value)
					return halt
				}
				else return this.$show = `Left (${(value as any) .show})`
			else return this.$show
		},
		value
	})

/**` Right : b -> Either a b `*/
const Right = <a, b>(value : b) : Either <a, b> =>
	({
		variation : 'Right',
		pipe (f) { return f (this) },
		eq : er =>
		{
			if (er.variation === 'Left')
				return false
			else if ((value as any) ?.eq === undefined)
			{
				console.error(`'.eq' cannot compare Right values that do not implement '.eq'`)
				console.dir(`Signature : Right (<LEFTSIDE>) .eq (Right (<RIGHTSIDE>))`)
				console.dir(`<LEFTSIDE> =`, value)
				console.dir(`<RIGHTSIDE> =`, er.value)
				return halt
			}
			else return (value as any) .eq (er.value)
		},
		get show()
		{
			if (this.$show === undefined)
				if ((value as any) ?.show === undefined)
				{
					console.error(`'.show' cannot stringify a Right value that does not implement '.show'`)
					console.dir(`Signature : Right (<VALUE>) .show`)
					console.dir(`<VALUE> =`, value)
					return halt
				}
				else return this.$show = `Right (${(value as any) ?.show})`
			else return this.$show
		},
		value
	})

/**` V2 : (Number, Number) -> V2 `*/
const V2 = (x : number, y : number) : V2 =>
	({
		variation : 'V2',
		pipe (f) { return f (this) },
		eq : v => v.x === x && v.y === y,
		get show() { return `V3 (${x}, ${y})` },
		x, y
	})

/**` V3 : (Number, Number, Number) -> V3 `*/
const V3 = (x : number, y : number, z : number) : V3 =>
	({
		variation : 'V3',
		pipe (f) { return f (this) },
		eq : v => v.x === x && v.y === y && v.z === z,
		get show() { return `V3 (${x}, ${y}, ${z})` },
		x, y, z
	})

/**` V4 : (Number, Number, Number, Number) -> V4 `*/
const V4 = (x : number, y : number, z : number, w : number) : V4 =>
	({
		variation : 'V4',
		pipe (f) { return f (this) },
		eq : v => v.x === x && v.y === y && v.z === z && v.w === w,
		get show() { return `V4 (${x}, ${y}, ${z}, ${w})` },
		x, y, z, w
	})

/********************************************************************************************************************************/
// Implementation of Constants and Functions of IO //

/**` send : a -> IO () `*/
const send = <a>(value : a) : IO <a> =>
	({
		variation : 'IO',
		pipe (f) { return f (this) },
		bind   : f  =>       f (value) ,
		fmap   : f  => send (f (value)),
		bindto : k  => f => IO (() => ({ ...value, [k] : f (value).effect () } as any)),
		fmapto : k  => f => IO (() => ({ ...value, [k] : f (value)           } as any)),
		then   : id,
		cast   : send,
		call   : f  => IO (() => (f (value).effect (), value)),
		side   : io => IO (() => (io       .effect (), value)),
		effect : () => value
	})

/**` idle : IO () `*/
const idle : IO <null> = send (null)

/**` executing : (...IO *) -> IO () `*/
const executing = (...ios : Array <IO <any>>) : IO <null> =>
	IO (() => (ios.forEach(io => io.effect ()), null))

/********************************************************************************************************************************/
// Implementation of Constants and Functions of Process //

/**` runProcess : Process s a -> s -> Pair s a `*/
const runProcess = <s, a>(process : Process <s, a>) =>
	process.computation

/**` execProcess : Process s a -> s -> s `*/
const execProcess = <s, a>(process : Process <s, a>) => (state : s) : s =>
	process.computation (state).fst

/**` evalProcess : Process s a -> s -> a `*/
const evalProcess = <s, a>(process : Process <s, a>) => (state : s) : a =>
	process.computation (state).snd

/**` put : s -> Process s a -> Process s a `*/
const put = <s>(replacement : s) => <a>(process : Process <s, a>) : Process <s, a> =>
	Process (s => Pair (replacement, process.computation (s).snd))

/**` get : Process s a -> Process s s `*/
const get = <s, a>(process : Process <s, a>) : Process <s, s> =>
	Process (s => same (process.computation (s).fst))

/**` mapProcess : (Pair s a -> Pair s b) -> Process s a -> Process s b `*/
const mapProcess = <s, a, b>(morphism : (result : Pair <s, a>) => Pair <s, b>) => (process : Process <s, a>) : Process <s, b> =>
	Process (s => morphism (process.computation (s)))

/**` mapState : (s -> s) -> Process s a -> Process s a `*/
const mapState = <s, a>(endomorphism : (state : s) => s) => (process : Process <s, a>) : Process <s, a> =>
	Process (s => mapFst (endomorphism) (process.computation (s)))

/**` random : Process Number Number `*/
const random : Process <number, number> =
	Process (s =>
		Pair (
			Math.abs(161 * s ** 3        - 91 * s ** 2      + 177 * s - 901) % 0xffffff,
			Math.abs(13  * s ** 3 % 203 + 248 * s ** 2 % 34 + 112 * s - 528) % 2048 / 2048
		)
	)

/**` randomFloatRange : Number -> Number -> Process Number Number `*/
const randomFloatRange = (lower : number) => (upper : number) : Process <number, number> =>
	Process (s =>
		Pair (
			Math.abs(698 * s ** 3       - 471 * s ** 2       + 295 * s - 77 ) % 0xffffff,
			Math.abs(-13 * s ** 3 % 196 + 989 * s ** 2 % 786 + 534 * s - 571) % 2048 / 2048 * (upper - lower) + lower
		)
	)

/**` randomIntRange : Number -> Number -> Process Number Number `*/
const randomIntRange = (lower : number) => (upper : number) : Process <number, number> =>
	Process (s =>
		Pair (
			Math.abs(852 * s ** 3 - 274 * s ** 2 + 345 * s - 558) % 0xffffff,
			~~(Math.abs(71  * s ** 3 % 71 + 570 * s ** 2 % 39 + 509 * s - 72 ) % 2048 / 2048 * (upper - lower)) + lower
		)
	)

/**` randomV2 : Process Number V2 `*/
const randomV2 : Process <number, V2> =
	Process (s =>
		Pair (
			Math.abs(84 * s ** 3 + 729 * s ** 2 + 215 * s + 1015) % 0xffffff,
			V2 (
				Math.abs(304 * s ** 3 % 582 + 204 * s ** 2 % 288 + 254 * s - 617) % 2048 / 2048,
				Math.abs(906 * s ** 3 % 717 + 518 * s ** 2 % 38  + 112 * s - 581) % 2048 / 2048
			)
		)
	)

/**` randomV3 : Process Number V3 `*/
const randomV3 : Process <number, V3> =
	Process (s =>
		Pair (
			Math.abs(390 * s ** 3 - 329 * s ** 2 + 22 * s + 41) % 0xffffff,
			V3 (
				Math.abs(407 * s ** 3 % 594 + 70 * s ** 2 % 61 +         s - 5283) % 2048 / 2048,
				Math.abs(109 * s ** 3 % 200 + 23 * s ** 2 % 8  + 69940 * s - 558 ) % 2048 / 2048,
				Math.abs(273 * s ** 3 % 286 + 23 * s ** 2 % 60 + 36    * s - 184 ) % 2048 / 2048
			)
		)
	)

/**` randomV4 : Process Number V4 `*/
const randomV4 : Process <number, V4> =
	Process (s =>
		Pair (
			Math.abs(350 * s ** 3 - 7527 * s ** 2 + 639 * s - 1011) % 0xffffff,
			V4 (
				Math.abs(881  * s ** 3 % 461 + 213   * s ** 2 % 16 +      s - 519 ) % 2048 / 2048,
				Math.abs(75   * s ** 3 % 516 + 75459 * s ** 2 % 19 + 67 * s - 693 ) % 2048 / 2048,
				Math.abs(3498 * s ** 3 % 242 + 590   * s ** 2 % 27 + 50 * s - 3039) % 2048 / 2048,
				Math.abs(8641 * s ** 3 % 256 + 613   * s ** 2 % 28 + 12 * s - 62  ) % 2048 / 2048
			)
		)
	)

/**` randomDirectionV2 : Process Number V2 `*/
const randomDirectionV2 : Process <number, V2> =
	Process (s => {
		const angle = Math.abs(2474 * s ** 3 % 2676 + 369 * s ** 2 % 3871 + 267628 * s % 6048 + 744)
		return Pair (
			Math.abs(372 * s ** 3 - 566 * s ** 2 + 21713 * s + 36769) % 0xffffff,
			V2 (Math.cos(angle), Math.sin(angle))
		)
	})

/**` randomDirectionV3 : Process Number V3 `*/
const randomDirectionV3 : Process <number, V3> =
	Process (s => {
		const angle0 = Math.abs(      s ** 3 % 198 + 378  * s ** 2 % 86 +       s - 16 )
		const angle1 = Math.abs(116 * s ** 3       - 3168 * s ** 2      + 258 * s - 901)
		const c      = Math.sin(angle0)
		return Pair (
			Math.abs(414 * s ** 3 - 607 * s ** 2 + 889 * s - 888) % 0xffffff,
			V3 (Math.cos(angle0), c * Math.cos(angle1), c * Math.sin(angle1))
		)
	})

/**` randomDirectionV4 : Process Number V4 `*/
const randomDirectionV4 : Process <number, V4> =
	Process (s => {
		const angle0 = Math.abs(905 * s ** 3 % 2312 + 633 * s ** 2 % 94975 + 208 * s - 250)
		const angle1 = Math.abs(189 * s ** 3 % 2641 - 466 * s ** 2 % 44291 + 224 * s - 917)
		const angle2 = Math.abs(417 * s ** 3 % 2354 - 262 * s ** 2 % 29516 + 41  * s - 529)
		const c0     = Math.sin(angle0)
		const c1     = Math.sin(angle1) * c0
		return Pair (
			Math.abs(161 * s ** 3 - 91 * s ** 2 + 177 * s - 901) % 0xffffff,
			V4 (Math.cos(angle0), c0 * Math.cos(angle1), c1 * Math.cos(angle2), c1 * Math.sin(angle2))
		)
	})

/**` randomLowercase : Process Number String `*/
const randomLowercase : Process <number, string> =
	Process (s =>
		Pair (
			Math.abs(28721 * s ** 3 % 2999 - 712 * s ** 2 + 3778 * s - 558) % 0xffffff,
			String.fromCharCode(Math.abs(3252 * s ** 3 % 359 + 945 * s ** 2 % 878 + 503 * s % 379 + 826) % 1028 / 1028 * 26 + 97)
		)
	)

/**` randomUppercase : Process Number String `*/
const randomUppercase : Process <number, string> =
	Process (s =>
		Pair (
			Math.abs(363 * s ** 3 % 384 - 31 * s ** 2 + 67 * s - 793) % 0xffffff,
			String.fromCharCode(Math.abs(3252 * s ** 3 % 398 + 778 * s ** 2 % 128 + 13 * s % 1701 + 871) % 1028 / 1028 * 26 + 65)
		)
	)

/**` randomChance : Number -> Process Number Boolean `*/
const randomChance = (probability : number) : Process <number, boolean> =>
	Process (s =>
		Pair (
			Math.abs(1800 * s ** 3        + 6031 * s ** 2       - 23462 * s       - 1105) % 0xffffff,
			Math.abs(1587 * s ** 3 % 9228 - 8604 * s ** 2 % 568 + 51310 * s % 234 + 2278) % 2048 / 2048 < probability
		)
	)

/**` randomElem : List a -> Process Number a `*/
const randomElem = <a>(xs : List <a>) : Process <number, a> =>
{
	if (xs === Nil)
	{
		console.error(`'randomElem' cannot pseudo-randomly retrieve an element from Nil (an empty List)`)
		console.dir(`Signature : randomElem (<XS>)`)
		console.dir(`<XS> =`, xs)
		return halt
	}
	else
	{
		const length = xs.$len ??= (() => {
			let xs_ = xs
			let i   = 0
			while (xs_.variation === 'Cons')
				if (i === MAX)
				{
					console.error(`'randomElem' traversed too many elements (${MAX})`)
					console.dir(`Signature : randomElem (<XS>)`)
					console.dir(`<XS> =`, xs)
					return halt
				}
				else
					xs_ = xs_.tail,
					++i
			return xs.$len = i
		})()

		return Process (s => {
			let xs_ = xs
			for (let i = Math.abs(4170 * s ** 3 % 92310 - 2410 * s ** 2 % 1568 + 8562752 * s % 34282 + 862) % 2048 / 2048 * length; ~~i; --i)
				xs_ = (xs_ as any).tail
			return Pair (
				Math.abs(462  * s ** 3 + 261 * s ** 2 - 778 * s - 1510) % 0xffffff,
				(xs_ as any).head
			)
		})
	}
}

/**` randomChar : String -> Process Number String `*/
const randomChar = (str : string) : Process <number, string> =>
{
	if (str === "")
	{
		console.error(`'randomChar' cannot pseudo-randomly retrieve a character from an empty string`)
		console.dir(`Signature : randomChar (<STRING>)`)
		console.dir(`<STRING> =`, str)
		return halt
	}
	else return Process (s =>
			Pair (
				Math.abs(488030 * s ** 3 + 47546031 * s ** 2 - 23462 * s - 13105) % 0xffffff,
				str [Math.trunc(Math.abs(8588 * s ** 3 % 5685 - 8645 * s ** 2 % 4993 + 5168 * s % 2313 + 2210) % 2048 / 2048 * str.length)]
			)
		)
}

/**` randomPick : List a -> Process s (Pair a (List a)) `*/
const randomPick = <a>(xs : List <a>) : Process <number, Pair <a, List <a>>> =>
{
	if (xs === Nil)
	{
		console.error(`'randomPick' cannot pseudo-randomly retrieve an element from Nil (an empty List)`)
		console.dir(`Signature : randomPick (<XS>)`)
		console.dir(`<XS> =`, xs)
		return halt
	}
	else
	{
		const length = xs.$len ??= (() => {
			let xs_ = xs
			let i   = 0
			while (xs_.variation === 'Cons')
				if (i === MAX)
				{
					console.error(`'randomPick' traversed too many elements (${MAX})`)
					console.dir(`Signature : randomPick (<XS>)`)
					console.dir(`<XS> =`, xs)
					return halt
				}
				else
					xs_ = xs_.tail,
					++i
			return xs.$len = i
		})()

		return Process (s => {
			let xs_             = xs
			let rxs0 : List <a> = Nil
			let xs0  : List <a> = Nil
			for (let i = Math.abs(4170 * s ** 3 % 92310 - 2410 * s ** 2 % 1568 + 8562752 * s % 34282 + 862) % 2048 / 2048 * length; ~~i; --i)
				rxs0 = prepend ((xs_ as any).head) (rxs0),
				xs_ = (xs_ as any).tail

			while (rxs0.variation === 'Cons')
				xs0  = prepend ((rxs0 as any).head) (xs0),
				rxs0 = (rxs0 as any).tail

			return Pair (
				Math.abs(462  * s ** 3 + 261 * s ** 2 - 778 * s - 1510) % 0xffffff,
				Pair ((xs_ as any).head, xs0 .link ((xs_ as any).tail))
			)
		})
	}
}

/**` randomShuffle : List a -> Process s (List a) `*/
const randomShuffle = <a>(xs : List <a>) : Process <number, List <a>> =>
{
	if (xs === Nil)
		return Process (s => Pair (s ** 3 / 1000, Nil))
	else
	{
		const length = xs.$len ??= (() => {
			let xs_ = xs
			let i   = 0
			while (xs_.variation === 'Cons')
				if (i === MAX)
				{
					console.error(`'randomShuffle' traversed too many elements (${MAX})`)
					console.dir(`Signature : randomShuffle (<XS>)`)
					console.dir(`<XS> =`, xs)
					return halt
				}
				else
					xs_ = xs_.tail,
					++i
			return xs.$len = i
		})()

		return Process (s => {
			let xs_             = xs
			let rxs0 : List <a> = Nil
			let xs0  : List <a> = Nil
			for (let i = Math.abs(3885 * s ** 3 % 1281 - 2310 * s ** 2 % 2410 + 2752 * s % 3428 + 686) % 2048 / 2048 * length; ~~i; --i)
				rxs0 = prepend ((xs_ as any).head) (rxs0),
				xs_ = (xs_ as any).tail

			while (rxs0.variation === 'Cons')
				xs0  = prepend ((rxs0 as any).head) (xs0),
				rxs0 = (rxs0 as any).tail

			return Pair (
				Math.abs(2296  * s ** 3 + 26191 * s ** 2 - 62778 * s - 56885) % 0xffffff,
				lprepend ((xs_ as any).head) (() => randomShuffle (xs0 .link ((xs_ as any).tail)) .computation (s / 41510 + 13).snd)
			)
		})
	}
}

/**` strictRandomShuffle : List a -> Process Number (List a) `*/
const strictRandomShuffle = <a>(xs : List <a>) : Process <number, List <a>> =>
{
	if (xs === Nil)
		return Process (s => Pair (s ** 3 / 1000, Nil))
	else
	{
		const array = (() => {
			let xs_             = xs
			let arr : Array <a> = []
			for (let i = 0; xs_.variation === 'Cons'; ++i)
				if (i === MAX)
				{
					console.error(`'strictRandomShuffle' traversed too many elements (${MAX})`)
					console.dir(`Signature : strictRandomShuffle (<XS>)`)
					console.dir(`<XS> =`, xs)
					return halt
				}
				else
					arr.push(xs_.head),
					xs_ = xs_.tail
			return arr
		})()

		return Process (s => {
			let array_ = array.slice()
			let j = 0
			let i = array_.length
			while (i !== 0)
				j = Math.trunc((3885 * s ** 3 % 1281 - 2310 * s ** 2 % 2410 + 2752 * s % 3428 + 686) % 2048 / 2048 * i),
				--i,
				[array_ [i], array_ [j]] = [array_ [j], array_ [i]]
			let ys : List <a> = Nil
			for (let i = 0; i < array_.length; ++i)
				ys = prepend (array_ [i]) (ys)
			return Pair (Math.abs(2296  * s ** 3 + 26191 * s ** 2 - 62778 * s - 56885) % 0xffffff, ys)
		})
	}
}

/********************************************************************************************************************************/
// Implementation of Constants and Functions of Maybe //

/**` extractJust : Maybe a -> a `*/
const extractJust = <a>(maybe : Maybe <a>) : a =>
{
	if (maybe.variation === 'Just')
		return maybe.value
	else
	{
		console.error(`'extractJust' expected a Just value`)
		console.dir(`Signature : extractJust (<MAYBE>)`)
		console.dir(`<MAYBE> =`, maybe)
		return halt
	}
}

/**` fromMaybe : a -> Maybe a -> a `*/
const fromMaybe = <a>(fallback : a) => (maybe : Maybe <a>) : a =>
	maybe.variation === 'Just'
		? maybe.value
		: fallback

/**` ensure : (a -> Boolean) -> a -> Maybe a `*/
const ensure = <a>(predicate : (value : a) => boolean) => (value : a) : Maybe <a> =>
	predicate (value)
		? Just (value)
		: Nothing

/**` testMaybe : (a -> Boolean) -> Maybe a -> Boolean `*/
const testMaybe = <a>(predicate : (value : a) => boolean) => (maybe : Maybe <a>) : boolean =>
	maybe.variation === 'Just' && predicate (maybe.value)

/********************************************************************************************************************************/
// Implementation of Constants and Functions of List //

/**` head : List a -> a `*/
const head = <a>(xs : List <a>) : a =>
{
	if (xs.variation === 'Cons')
		return xs.head
	else
	{
		console.error(`'head' expected a Cons (non-empty List)`)
		console.dir(`Signature : head (<XS>)`)
		console.dir(`<XS> =`, xs)
		return halt
	}
}

/**` tail : List a -> List a `*/
const tail = <a>(xs : List <a>) : List <a> =>
{
	if (xs.variation === 'Cons')
		return xs.tail
	else
	{
		console.error(`'tail' expected a Cons (non-empty List)`)
		console.dir(`Signature : tail (<XS>)`)
		console.dir(`<XS> =`, xs)
		return halt
	}
}

/**` link : List a -> List a -> List a `*/
const link = <a>(xs : List <a>) : (ys : List <a>) => List <a> =>
	xs .link

/**` listToArray : List a -> [a] `*/
const listToArray = <a>(xs : List <a>) : Array <a> =>
{
	const array : Array <a> = []
	let xs_ = xs

	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'listToArray' traversed too many elements (${MAX}) when coverting the given List to a primitive Array`)
			console.dir(`Signature : listToArray (<XS>)`)
			console.dir(`<XS> =`, xs)
			return halt
		}
		else
			array.push(xs_.head),
			xs_ = xs_.tail

	return array
}

/**` safeListToArray : List a -> [a] `*/
const safeListToArray = <a>(xs : List <a>) : Array <a> =>
{
	const array : Array <a> = []
	let xs_ = xs

	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.warn(
				`'safeListToArray' traversed too many elements (${MAX}) when coverting`,
				`the given List to a primitive Array; resulting array is truncated`
			)
			console.dir(`Signature : safeListToArray (<XS>)`)
			console.dir(`<XS> =`, xs)
			return array
		}
		else
			array.push(xs_.head),
			xs_ = xs_.tail

	return array
}

/**` unchars : List String -> String `*/
const unchars = (xs : List <string>) : string =>
{
	let str = ""
	let xs_ = xs

	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'unchars' traversed too many elements (${MAX}) when converting the given List to a primitive String`)
			console.dir(`Signature : unchars (<XS>)`)
			console.dir(`<XS> =`, xs)
			return halt
		}
		else
			str += xs_.head,
			xs_ = xs_.tail

	return str
}

/**` safeUnchars : List String -> String `*/
const safeUnchars = (xs : List <string>) : string =>
{
	let str = ""
	let xs_ = xs

	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.warn(
				`'unchars' traversed too many elements (${MAX}) when converting the`,
				`given List to a primitive String; resulting string is truncated`
			)
			console.dir(`Signature : unchars (<XS>)`)
			console.dir(`<XS> =`, xs)
			return str
		}
		else
			str += xs_.head,
			xs_ = xs_.tail

	return str
}

/**` chars : String -> List String `*/
const chars = (str : string) : List <string> =>
	str === ""
		? Nil
		:
			{
				variation : 'Cons',
				$head : str [0],
				$last : str [str.length - 1],
				$len  : str.length,
				head  : str [0],
				get tail () { return this.$tail ??= chars (str.slice(1)) },
				pipe (f) { return f (this) },
				eq (xs)
				{
					let xs_  : List <string> = xs
					for (let i = 0; i < str.length; ++i)
						if (xs_.variation === 'Nil')
							return false
						else if (str [i] === xs_.head)
							xs_ = xs_.tail
						else return false
					return xs_.variation === 'Nil'
				},
				get show() { return this.$show ??= `List (${str.split('').map(c => `"${c}"`).join(", ")})` },
				bind (f)
				{
					let xs : List <string> = this
					while (xs.variation === 'Cons')
					{
						const ys = f (xs.head)
						if (ys.variation === 'Cons')
							return Cons (() => ys.head) (() => ys.tail .link ((xs as any).tail .bind (f)))
						else
							xs = xs.tail
					}
					return Nil
				},
				fmap (f)
				{
					return Cons (() => f (str [0])) (() => this.tail .fmap (f))
				},
				bindto (k)
				{
					return f =>
					{
						console.error(`'.bindto' shouldn't be used on a list of strings`)
						console.dir(`Signature : (<OBJECT>) .bindto (<NAME>) (<REACTION>) | <OBJECT> originated from 'chars'`)
						console.dir(`<OBJECT>   = `, this)
						console.dir(`<NAME>     = `, k)
						console.dir(`<REACTION> = `, f)
						return halt
					}
				},
				fmapto (k)
				{
					return f =>
					{
						console.error(`'.fmapto' shouldn't be used on a list of strings`)
						console.dir(`Signature : (<OBJECT>) .fmapto (<NAME>) (<REACTION>) | <OBJECT> originated from 'chars'`)
						console.dir(`<OBJECT>   = `, this)
						console.dir(`<NAME>     = `, k)
						console.dir(`<MORPHISM> = `, f)
						return halt
					}
				},
				link (xs)
				{
					return xs.variation === 'Nil'
						? this
						: lprepend (str [0]) (() => this.tail .link (xs))
				}
			}

/**` List : (...a) -> List a `*/
const List = <a>(...elements : Array <a>) : List <a> =>
{
	let xs : List <a> = Nil
	for (let i = elements.length - 1; ~i; --i)
		xs = prepend (elements [i]) (xs)
	return xs
}

/**` prepend : a -> List a -> List a `*/
const prepend = <a>(value : a) => (xs : List <a>) : List <a> =>
	xs.variation === 'Nil'
		? singleton (value)
		:
			{
				variation : 'Cons',
				pipe (f) { return f (this) },
				$head : value,
				$tail : xs,
				$last : xs.$last,
				$len  : xs.$len! + 1 || undefined,
				head  : value,
				tail  : xs,
				eq (xs)
				{
					if (xs.variation === 'Nil')
						return false
					else if ((this.head as any) ?.eq === undefined)
					{
						console.error(`'.eq' cannot compare the elements of the given Lists that does not implement '.eq'`)
						console.dir(`Signature : <LEFTSIDE> .eq (<RIGHTSIDE>) | <LEFTSIDE> originated from 'prepend'`)
						console.dir(`<LEFTSIDE>  =`, this)
						console.dir(`<RIGHTSIDE> =`, xs)
						return halt
					}
					else
					{
						let this_ : List <a> = this
						let xs_   : List <a> = xs
						for (let i = 0; this_.variation === 'Cons' && xs_.variation === 'Cons'; ++i)
							if (i === MAX)
							{
								console.error(`'.eq' traversed too many elements`)
								console.dir(`Signature : <LEFTSIDE> .eq (<RIGHTSIDE>) | <LEFTSIDE> originated from 'prepend'`)
								console.dir(`<LEFTSIDE>  =`, this)
								console.dir(`<RIGHTSIDE> =`, xs)
								return halt
							}
							else if (!(this_.head as any) .eq (xs_.head))
								return false
							else
								this_ = this_.tail,
								xs_   = xs_.tail
						return this_.variation === xs_.variation
					}
				},
				get show()
				{
					if (this.$show === undefined)
						if ((this.head as any) ?.eq === undefined)
						{
							console.error(`'.show' cannot stringify the elements of the List that does not implement '.show'`)
							console.dir(`Signature : <VALUE> .show | <VALUE> originated from 'prepend'`)
							console.dir(`<VALUE> =`, this)
							return halt
						}
						else
						{
							let str              = "List ("
							let this_ : List <a> = this
							for (let i = 0; this_.variation === 'Cons'; ++i)
								if (i === MAX)
								{

								}
								else
									str += (this_.head as any).show + ", ",
									this_ = this_.tail
							return this.$show = str.slice(0, -2) + ")"
						}
					else return this.$show
				},
				bind (f)
				{
					let ys = f (value)
					if (ys.variation === 'Cons')
						return Cons
							(() => (ys as any).head)
							(() => (ys as any).tail .link (xs .bind (f)))
					else
					{
						let xs_ : List <a> = xs
						for (let i = 0; xs_.variation === 'Cons'; ++i)
							if (i === MAX)
							{
								console.error(`'.bind' traversed too many elements (${MAX})`)
								console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> originated from 'prepend'`)
								console.dir(`<OBJECT>   =`, this)
								console.dir(`<REACTION> =`, f)
								return halt
							}
							else
							{
								ys = f (xs_.head)
								if (ys.variation === 'Cons')
									return Cons
										(() => (ys as any).head)
										(() => (ys as any).tail .link ((xs_ as any).tail .bind (f)))
								else
									xs_ = xs_.tail
							}
						return Nil
					}
				},
				fmap   : f => Cons (() => f (value)) (() => xs .fmap (f)),
				bindto (k)
				{
					return f =>
					{
						let ys = f (value)
						if (ys.variation === 'Cons')
						{
							const zs = ys .fmap (y => ({ ...value, [k] : y } as any))
							return Cons
								(() => (zs as any).head)
								(() => (zs as any).tail .link (xs .bindto (k) (f)))
						}
						else
						{
							let xs_ : List <a> = xs
							for (let i = 0; xs_.variation === 'Cons'; ++i)
								if (i === MAX)
								{
									console.error(`'.bindto' traversed too many elements (${MAX})`)
									console.dir(
										`Signature : (<OBJECT>) .bindto (<NAME>) (<REACTION>) | <OBJECT> originated from 'prepend'`
									)
									console.dir(`<OBJECT>   =`, this)
									console.dir(`<NAME>     =`, k)
									console.dir(`<REACTION> =`, f)
									return halt
								}
								else
								{
									ys = f (xs_.head)
									if (ys.variation === 'Cons')
									{
										const zs = ys .fmap (y => ({ ...value, [k] : y } as any))
										return Cons
											(() => (ys as any).head)
											(() => (ys as any).tail .link ((xs_ as any).tail .bind (f)))
									}
									else
										xs_ = xs_.tail
								}
							return Nil
						}
					}
				},
				fmapto : k => f => Cons (() => ({ ...value, [k] : f (value) } as any)) (() => xs .fmapto (k) (f)),
				link   : ys => lprepend (value) (() => xs .link (ys))
			}

/**` lprepend : a -> (() -> List a) -> List a `*/
const lprepend = <a>(value : a) => (lxs : () => List <a>) : List <a> =>
	({
		variation : 'Cons',
		pipe (f) { return f (this) },
		$head : value,
		head  : value,
		get tail () { return this.$tail ??= lxs () },
		eq (xs)     { return __MACRO__.default_implementation_list_eq   (this) (xs) },
		get show () { return __MACRO__.default_implementation_list_show (this)      },
		bind (f)
		{
			let ys = f (value)
			if (ys.variation === 'Cons')
				return Cons (() => (ys as any).head) (() => (ys as any).tail .link (this.tail .bind (f)))
			else
			{
				let xs : List <a> = this.tail
				for (let i = 0; xs.variation === 'Cons'; ++i)
					if (i === MAX)
					{
						console.error(`'.bind' traversed too many elements (${MAX})`)
						console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> originated from 'lprepend'`)
						console.dir(`<OBJECT>   =`, this)
						console.dir(`<REACTION> =`, f)
						return halt
					}
					else
					{
						ys = f (xs.head)
						if (ys.variation === 'Cons')
							return Cons
								(() => (ys as any).head)
								(() => (ys as any).tail .link ((xs as any).tail .bind (f)))
						else
							xs = xs.tail
					}
				return Nil
			}
		},
		fmap (f)
		{
			return Cons (() => f (value)) (() => this.tail .fmap (f))
		},
		bindto (k)
		{
			return f =>
			{
				let ys = f (value)
				if (ys.variation === 'Cons')
				{
					const zs = ys .fmap (y => ({ ...value, [k] : y } as any))
					return Cons
						(() => (zs as any).head)
						(() => (zs as any).tail .link (this.tail .bindto (k) (f)))
				}
				else
				{
					let xs : List <a> = this.tail
					for (let i = 0; xs.variation === 'Cons'; ++i)
						if (i === MAX)
						{
							console.error(`'.bindto' traversed too many elements (${MAX})`)
							console.dir(
								`Signature : (<OBJECT>) .bindto (<NAME>) (<REACTION>) | <OBJECT> originated from 'lprepend'`
							)
							console.dir(`<OBJECT>   =`, this)
							console.dir(`<NAME>     =`, k)
							console.dir(`<REACTION> =`, f)
							return halt
						}
						else
						{
							ys = f (xs.head)
							if (ys.variation === 'Cons')
							{
								const zs = ys .fmap (y => ({ ...value, [k] : y } as any))
								return Cons
									(() => (zs as any).head)
									(() => (zs as any).tail .link ((zs as any).tail .bind (f)))
							}
							else
								xs = xs.tail
						}
					return Nil
				}
			}
		},
		fmapto (k)
		{
			return f =>
				Cons (() => ({ ...value, [k] : f (value) } as any)) (() => this.tail .fmapto (k) (f))
		},
		link (ys)
		{
			return lprepend (value) (() => this.tail .link (ys))
		}
	})

/**` postpend : a -> List a -> List a `*/
const postpend = <a>(value : a) => (xs : List <a>) : List <a> =>
	xs.variation === 'Nil'
		? singleton (value)
		:
			{
				variation : 'Cons',
				pipe (f) { return f (this) },
				$head : xs.$head,
				$init : xs,
				$last : value,
				$len  : xs.$len! + 1 || undefined,
				get head () { return this.$head ??= (xs as any).head },
				get tail () { return this.$tail ??= postpend (value) ((xs as any).tail) },
				eq (xs)     { return __MACRO__.default_implementation_list_eq   (this) (xs) },
				get show () { return __MACRO__.default_implementation_list_show (this)      },
				bind (f)
				{
					let xs_ : List <a> = xs
					for (let i = 0; xs_.variation === 'Cons'; ++i)
						if (i === MAX)
						{
							console.error(`'.bind' traversed too many elements (${MAX})`)
							console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> originated from 'postpend'`)
							console.dir(`<OBJECT>   =`, this)
							console.dir(`<REACTION> =`, f)
							return halt
						}
						else
						{
							const ys = f (xs_.head)
							if (ys.variation === 'Cons')
								return Cons
									(() => ys.head)
									(() => ys.tail .link (postpend (value) ((xs_ as any).tail) .bind (f)))
							else
								xs_ = xs_.tail
						}
					return f (value)
				},
				fmap : f => lpostpend (f (value)) (() => xs .fmap (f)),
				bindto (k)
				{
					return f =>
					{
						let xs_ : List <a> = xs
						for (let i = 0; xs_.variation === 'Cons'; ++i)
							if (i === MAX)
							{
								console.error(`'.bind' traversed too many elements (${MAX})`)
								console.dir(
									`Signature : (<OBJECT>) .bindto (<NAME>) (<REACTION>)`,
									`| <OBJECT> originated from 'postpend'`
								)
								console.dir(`<OBJECT>   =`, this)
								console.dir(`<NAME>     =`, k)
								console.dir(`<REACTION> =`, f)
								return halt
							}
							else
							{
								const ys = f (xs_.head)
								if (ys.variation === 'Cons')
								{
									const zs = ys .fmap (y => ({ ...(xs_ as any).head, [k] : y } as any))
									return Cons
										(() => (zs as any).head)
										(() => (zs as any).tail .link (postpend (value) ((xs_ as any).tail) .bind (f)))
								}
								else
									xs_ = xs_.tail
							}
						return f (value) .fmap (x => ({ ...value,  [k] : x } as any))
					}
				},
				fmapto : k => f => lpostpend ({ ...value, [k] : f (value) } as any) (() => xs .fmapto (k) (f)),
				link (ys)
				{
					return this.$head === undefined
						? Cons (() => this.head) (() => this.tail .link (ys))
						: lprepend (this.$head) (() => this.tail .link (ys))
				}
			}

/**` lpostpend : a -> (() -> List a) -> List a `*/
const lpostpend = <a>(value : a) => (lxs : () => List <a>) : List <a> =>
	({
		variation : 'Cons',
		pipe (f) { return f (this) },
		$last : value,
		get head ()
		{
			return this.$head ??=
				(this.$init ??= lxs ()).variation === 'Nil'
					? value
					: this.$init.head
		},
		get tail ()
		{
			return this.$tail ??=
				(this.$init ??= lxs ()).variation === 'Nil'
					? singleton (value)
					: postpend (value) (this.$init.tail)
		},
		eq (xs)     { return __MACRO__.default_implementation_list_eq   (this) (xs) },
		get show () { return __MACRO__.default_implementation_list_show (this)      },
		bind (f)
		{
			let xs_ : List <a> = this.tail
			for (let i = 0; xs_.variation === 'Cons'; ++i)
				if (i === MAX)
				{
					console.error(`'.bind' traversed too many elements (${MAX})`)
					console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> originated from 'lpostpend'`)
					console.dir(`<OBJECT>   =`, this)
					console.dir(`<REACTION> =`, f)
					return halt
				}
				else
				{
					const ys = f (xs_.head)
					if (ys.variation === 'Cons')
						return Cons (() => ys.head) (() => ys.tail .link (postpend (value) ((xs_ as any).tail) .bind (f)))
					else
						xs_ = xs_.tail
				}
			return f (value)
		},
		fmap (f)
		{
			return lpostpend (f (value)) (() => this.tail .fmap (f))
		},
		bindto (k)
		{
			return f =>
			{
				let xs_ : List <a> = this.tail
				for (let i = 0; xs_.variation === 'Cons'; ++i)
					if (i === MAX)
					{
						console.error(`'.bind' traversed too many elements (${MAX})`)
						console.dir(
							`Signature : (<OBJECT>) .bindto (<NAME>) (<REACTION>)`,
							`| <OBJECT> originated from 'lpostpend'`
						)
						console.dir(`<OBJECT>   =`, this)
						console.dir(`<NAME>     =`, k)
						console.dir(`<REACTION> =`, f)
						return halt
					}
					else
					{
						const ys = f (xs_.head)
						if (ys.variation === 'Cons')
						{
							const zs = ys .fmap (y => ({ ...(xs_ as any).head, [k] : y } as any))
							return Cons
								(() => (zs as any).head)
								(() => (zs as any).tail .link (postpend (value) ((xs_ as any).tail) .bind (f)))
						}
						else
							xs_ = xs_.tail
					}
				return f (value) .fmap (x => ({ ...value,  [k] : x } as any))
			}
		},
		fmapto (k)
		{
			return f =>
				lpostpend ({ ...value, [k] : f (value) } as any) (() => this.tail .fmapto (k) (f))
		},
		link (ys)
		{
			return this.$head === undefined
				? Cons (() => this.head) (() => this.tail .link (ys))
				: lprepend (this.$head) (() => this.tail .link (ys))
		}
	})

/**` singleton : a -> List a `*/
const singleton = <a>(value : a) : List <a> =>
	({
		variation : 'Cons',
		pipe (f) { return f (this) },
		$head  : value,
		$tail  : Nil,
		$last  : value,
		$init  : Nil,
		get $reverse () { return this },
		$len   : 1,
		head   : value,
		tail   : Nil,
		eq (xs)     { return __MACRO__.default_implementation_list_eq   (this) (xs) },
		get show () { return __MACRO__.default_implementation_list_show (this)      },
		bind   : f => f (value),
		fmap   : f => singleton (f (value)),
		bindto : k => f => f (value) .fmap (x => ({ ...value, [k] : x } as any)),
		fmapto : k => f => singleton ({ ...value, [k] : f (value) } as any),
		link   : prepend (value)
	})

/**` repeat : a -> List a `*/
const repeat = <a>(value : a) : List <a> =>
	({
		variation : 'Cons',
		$head : value,
		get $tail () { return this },
		get $init () { return this },
		head : value,
		get tail () { return this },
		eq (xs)     { return __MACRO__.default_implementation_list_eq   (this) (xs) },
		get show () { return __MACRO__.default_implementation_list_show (this)      },
		pipe (f) { return f (this) },
		bind   : f => cycle  (f (value)),
		fmap   : f => repeat (f (value)),
		bindto : k => f => cycle  ({ ...value, [k] : f (value) } as any),
		fmapto : k => f => repeat ({ ...value, [k] : f (value) } as any),
		link (_) { return this }
	})

/**` cycle : List a -> List a `*/
const cycle = <a>(pattern : List <a>) : List <a> =>
	pattern.variation === 'Nil'
		? Nil
		:
			{
				variation : 'Cons',
				pipe (f) { return f (this) },
				get head () { return this.$head ??= (pattern as any).head },
				get tail () { return this.$tail ??= (pattern as any).tail .link (this) },
				eq (xs)     { return __MACRO__.default_implementation_list_eq   (this) (xs) },
				get show () { return __MACRO__.default_implementation_list_show (this)      },
				bind (f)
				{
					let xs : List <a> = pattern
					for (let i = 0; xs.variation === 'Cons'; ++i)
						if (i === MAX)
						{
							console.error(`'.bind' traversed too many elements (${MAX})`)
							console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> orignated from 'cycle'`)
							console.dir(`<OBJECT>   =`, this)
							console.dir(`<REACTION> =`, f)
							return halt
						}
						else
						{
							const ys = f (xs.head)
							if (ys.variation === 'Cons')
								return cycle (Cons (() => ys.head) (() => ys.tail .link ((xs as any).tail .bind (f))))
							else
								xs = xs.tail
						}
					return Nil
				},
				fmap : f => cycle (pattern .fmap (f)),
				bindto (k)
				{
					return f =>
					{
						let xs : List <a> = pattern
						for (let i = 0; xs.variation === 'Cons'; ++i)
							if (i === MAX)
							{
								console.error(`'.bind' traversed too many elements (${MAX})`)
								console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> orignated from 'cycle'`)
								console.dir(`<OBJECT>   =`, this)
								console.dir(`<REACTION> =`, f)
								return halt
							}
							else
							{
								const ys = f (xs.head)
								if (ys.variation === 'Cons')
								{
									const zs = ys .fmap (y => ({ ...(xs as any).head, [k] : y } as any))
									return cycle (
										Cons
											(() => (zs as any).head)
											(() => (zs as any).tail .link ((xs as any).tail .bind (f)))
									)
								}
								else
									xs = xs.tail
							}
						return Nil
					}
				},
				fmapto : k => f => cycle (pattern .fmap (x => ({ ...x, [k] : f (x) } as any))),
				link (_) { return this }
			}

/**` range : Number -> Number -> List Number `*/
const range = (start : number) => (end : number) : List <number> =>
{
	if (Number.isFinite(start) && Number.isFinite(end))
		if (start > end)
			return Nil
		else if (start === end)
			return singleton (end)
		else if (Number.isInteger(start))
			return ({
				variation  : 'Cons',
				pipe (f) { return f (this) },
				$head : start,
				$len  : end - start,
				$last : end,
				head  : start,
				get tail () { return this.$tail ??= range (start + 1) (end) },
				eq (xs)     { return __MACRO__.default_implementation_list_eq   (this) (xs) },
				get show () { return __MACRO__.default_implementation_list_show (this)      },
				bind (f)
				{
					let xs : List <number> = this
					for (let i = 0; xs.variation === 'Cons'; ++i)
						if (i === MAX)
						{
							console.error(`'.bind' traversed too many elements (${MAX})`)
							console.dir(`Signature : (<OBJECT>) .bind (<REACTION>) | <OBJECT> orignated from 'range'`)
							console.dir(`<OBJECT>   =`, this)
							console.dir(`<REACTION> =`, f)
							return halt
						}
						else
						{
							const ys = f (xs.head)
							if (ys.variation === 'Cons')
								return Cons (() => ys.head) (() => ys.tail .link ((xs as any).tail .bind (f)))
							else
								xs = xs.tail
						}
					return Nil
				},
				fmap (f)
				{
					return Cons (() => f (start)) (() => this.tail .fmap (f))
				},
				bindto (k)
				{
					return f =>
					{
						console.error(`'.bindto' shouldn't be used on a list of numbers`)
						console.dir(`Signature : (<OBJECT>) .bindto (<NAME>) (<REACTION>) | <OBJECT> originated from 'range'`)
						console.dir(`<OBJECT>   = `, this)
						console.dir(`<NAME>     = `, k)
						console.dir(`<REACTION> = `, f)
						return halt
					}
				},
				fmapto (k)
				{
					return f =>
					{
						console.error(`'.fmapto' shouldn't be used on a list of numbers`)
						console.dir(`Signature : (<OBJECT>) .fmapto (<NAME>) (<REACTION>) | <OBJECT> originated from 'range'`)
						console.dir(`<OBJECT>   = `, this)
						console.dir(`<NAME>     = `, k)
						console.dir(`<MORPHISM> = `, f)
						return halt
					}
				},
				link (xs) { return lprepend (start) (() => this.tail .link (xs)) }
			})
		else return range (~~start) (end)
	else
	{
		console.error(`'range' expected integer inputs`)
		console.dir(`Signature : range (<START>) (<END>)`)
		console.dir(`<START> =`, start)
		console.dir(`<END>   =`, end)
		return halt
	}
}

/**` iterate : (a -> a) -> a -> List a `*/
const iterate = <a>(endomorphism : (value : a) => a) => (initial : a) : List <a> =>
	lprepend (initial) (() => iterate (endomorphism) (endomorphism (initial)))

/**` replicate : Number -> a -> List a `*/
const replicate = (amount : number) => <a>(value : a) : List <a> =>
	amount > 0
		? lprepend (value) (() => replicate (amount - 1) (value))
		: Nil

/**` countBy : Number -> Number -> List Number `*/
const countBy = (step : number) => (start : number) : List <number> =>
	lprepend (start) (() => countBy (step) (start + step))

/**` countDown : Number -> List Number `*/
const countDown : (start : number) => List <number> =
	countBy (-1)

/**` countUp : Number -> List Number `*/
const countUp : (start : number) => List <number> =
	countBy (1)

/**` naturals : List Number `*/
const naturals : List <number> =
	countUp (0)

/**` joinList : List (List a) -> List a `*/
const joinList = <a>(xss : List <List <a>>) : List <a> =>
{
	let xss_ = xss
	for (let i = 0; xss_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'joinList' traversed too many elements (${MAX})`)
			console.dir(`Signature : joinList (<XSS>)`)
			console.dir(`<XSS> =`, xss)
			return halt
		}
		else if (xss_.head.variation === 'Cons')
			return Cons (() => (xss_ as any).head.head) (() => (xss_ as any).head.tail .link (joinList ((xss_ as any).tail)))
		else
			xss_ = xss_.tail
	return Nil
}

/**` all : (a -> Boolean) -> List a -> Boolean `*/
const all = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : boolean =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'all' traversed too many elements (${MAX})`)
			console.dir(`Signature : all (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			xs_ = xs_.tail
		else return false
	return true
}

/**` any : (a -> Boolean) -> List a -> Boolean `*/
const any = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : boolean =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'any' traversed too many elements (${MAX})`)
			console.dir(`Signature : any (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			return true
		else
			xs_ = xs_.tail
	return false
}

/**` at : Number -> List a -> a `*/
const at = (index : number) => <a>(xs : List <a>) : a =>
{
	let i   = 0
	let xs_ = xs
	while (xs_.variation === 'Cons')
		if (i === MAX)
		{
			console.error(`'at' traversed too many elements (${MAX})`)
			console.dir(`Signature : at (<INDEX>) (<XS>)`)
			console.dir(`<INDEX> =`, index)
			console.dir(`<XS>    =`, xs)
			return halt
		}
		else if (i < index)
			xs_ = xs_.tail,
			++i
		else return xs_.head

	console.error(`'at' received an out-of-bounds index of ${index} for a list of length ${i}`)
	console.dir(`Signature : at (<INDEX>) (<XS>)`)
	console.dir(`<INDEX> =`, index)
	console.dir(`<XS>    =`, xs)
	return halt
}

/**` indexing : List a -> Number -> a `*/
const indexing = <a>(xs : List <a>) => (index : number) : a =>
{
	let i   = 0
	let xs_ = xs
	while (xs_.variation === 'Cons')
		if (i < index)
			xs_ = xs_.tail,
			++i
		else return xs_.head
	console.error(`'indexing' received an out-of-bounds index of ${index} for a list of length ${i}`)
	console.dir(`Signature : indexing (<XS>) (<INDEX>)`)
	console.dir(`<XS>    =`, xs)
	console.dir(`<INDEX> =`, index)
	return halt
}

/**` len : List a -> Number `*/
const len = <a>(xs : List <a>) : number =>
{
	if (xs.$len === undefined)
	{
		let xs_ : List <a> = xs
		let i              = 0
		while (xs_.variation === 'Cons')
			if (i === MAX)
			{
				console.error(`'len' traversed too many elements (${MAX})`)
				console.dir(`Signature : len (<XS>)`)
				console.dir(`<XS> =`, xs)
				return halt
			}
			else
				xs_ = xs_.tail,
				++i
		return xs.$len = i
	}
	else return xs.$len
}

/**` last : List a -> a `*/
const last = <a>(xs : List <a>) : a =>
{
	if (xs.variation === 'Nil')
	{
		console.error(`'last' expected a Cons (non-empty List)`)
		console.dir(`Signature : last (<XS>)`)
		console.dir(`<XS> =`, xs)
		return halt
	}
	else if (xs.$last === undefined)
	{
		let xs_ : List <a> = xs
		for (let i = 0; (xs_ as any).tail.variation === 'Cons'; ++i)
			if (i === MAX)
			{
				console.error(`'last' traversed too many elements (${MAX})`)
				console.dir(`Signature : last (<XS>)`)
				console.dir(`<XS> =`, xs)
				return halt
			}
			else
				xs_ = (xs_ as any).tail
		return xs.$last = (xs_ as any).head
	}
	else return xs.$last
}

/**` init : List a -> List a `*/
const init = <a>(xs : List <a>) : List <a> =>
{
	if (xs.variation === 'Nil')
	{
		console.error(`'init' expected a Cons (non-empty List)`)
		console.dir(`Signature : init (<XS>)`)
		console.dir(`<XS> =`, xs)
		return halt
	}
	else if (xs.$init === undefined)
		return xs.$init =
			xs.tail.variation === 'Nil'
				? Nil
				: Cons (() => xs.head) (() => init (xs.tail))
	else return xs.$init
}

/**` reverse : List a -> List a `*/
const reverse = <a>(xs : List <a>) : List <a> =>
{
	if (xs.$reverse === undefined)
	{
		let xs_           = xs
		let ys : List <a> = Nil
		for (let i = 0; xs_.variation === 'Cons'; ++i)
			if (i === MAX)
			{
				console.error(`'reverse' traversed too many elements (${MAX})`)
				console.dir(`Signature : reverse (<XS>)`)
				console.dir(`<XS> =`, xs)
				return halt
			}
			else
				ys  = prepend (xs_.head) (ys),
				xs_ = xs_.tail
		ys.$reverse = xs
		return xs.$reverse = ys
	}
	else return xs.$reverse
}

/**` map : (a -> b) -> List a -> List b `*/
const map = <a, b>(morphism : (element : a) => b) => (xs : List <a>) : List <b> =>
	xs .fmap (morphism)

/**` imap_tail : Number -> (Number -> a -> b) -> List a -> List b `*/
const imap_tail = (start : number) => <a, b>(imorphism : (index : number) => (element : a) => b) => (xs : List <a>) : List <b> =>
	xs.variation === 'Nil'
		? Nil
		: Cons (() => imorphism (start) (xs.head)) (() => imap_tail (start + 1) (imorphism) (xs.tail))

/**` imap : (Number -> a -> b) -> List a -> List b `*/
const imap : <a, b>(morphism : (index : number) => (element : a) => b) => (xs : List <a>) => List <b> = imap_tail (0)

/**` intersperse : a -> List a -> List a `*/
const intersperse = <a>(delimiter : a) => (xs : List <a>) : List <a> =>
	xs.variation === 'Nil' || xs.tail.variation === 'Nil'
		? xs
		: Cons (() => xs.head) (() => prepend (delimiter) (intersperse (delimiter) (xs.tail)))

/**` foldl : (b -> a -> b) -> b -> List a -> b `*/
const foldl = <a, b>(operation : (leftside : b) => (rightside : a) => b) => (initial : b) => (xs : List <a>) : b =>
{
	let out = initial
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'foldl' traversed too many elements (${MAX})`)
			console.dir(`Signature : foldl (<OPERATION>) (<INITIAL>) (<XS>)`)
			console.dir(`<OPERATION> =`, operation)
			console.dir(`<INITIAL>   =`, initial)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else
			out = operation (out) (xs_.head),
			xs_ = xs_.tail
	return out
}

/**` foldl1 : (a -> a -> a) -> List a -> a `*/
const foldl1 = <a>(operation : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : a =>
{
	if (xs.variation === 'Nil')
	{
		console.error(`'foldl1' expected a Cons (non-empty List)`)
		console.dir(`Signature : foldl1 (<OPERATION>) (<XS>)`)
		console.dir(`<OPERATION> =`, operation)
		console.dir(`<XS> =`, xs)
		return halt
	}
	else
	{
		let out = xs.head
		let xs_ = xs.tail
		for (let i = 0; xs_.variation === 'Cons'; ++i)
			if (i === MAX)
			{
				console.error(`'foldl1' traversed too many elements (${MAX})`)
				console.dir(`Signature : foldl1 (<OPERATION>) (<XS>)`)
				console.dir(`<OPERATION> =`, operation)
				console.dir(`<XS>        =`, xs)
			}
			else
				out = operation (out) (xs_.head),
				xs_ = xs_.tail
		return out
	}
}

/**` foldr : (a -> b -> b) -> b -> List a -> b `*/
const foldr = <a, b>(operation : (leftside : a) => (rightside : b) => b) => (initial : b) => (xs : List <a>) : b =>
{
	const rxs = xs.$reverse ??
		(() => {
			let xs_           = xs
			let ys : List <a> = Nil
			for (let i = 0; xs_.variation === 'Cons'; ++i)
				if (i === MAX)
				{
					console.error(`'foldr' traversed too many elements (${MAX})`)
					console.dir(`Signature : foldr (<OPERATION>) (<INITIAL>) (<XS>)`)
					console.dir(`<OPERATION> =`, operation)
					console.dir(`<INITIAL>   =`, initial)
					console.dir(`<XS>        =`, xs)
					return halt
				}
				else
					ys  = prepend (xs_.head) (ys),
					xs_ = xs_.tail
			ys.$reverse = xs
			return xs.$reverse = ys
		})()

	let out  = initial
	let rxs_ = rxs
	while (rxs_.variation === 'Cons')
		out  = operation (rxs_.head) (out),
		rxs_ = rxs_.tail
	return out
}

/**` foldr1 : (a -> b -> a) -> List a -> a `*/
const foldr1 = <a>(operation : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : a =>
{
	if (xs.variation === 'Nil')
	{
		console.error(`'foldr1' expected a Cons (non-empty List)`)
		console.dir(`Signature : foldr1 (<OPERATION>) (<XS>)`)
		console.dir(`<OPERATION> =`, operation)
		console.dir(`<XS> =`, xs)
		return halt
	}
	else
	{
		const rxs : Variation <List <a>, 'Cons'> = xs.$reverse ??
			(() => {
				let xs_ : List <a> = xs
				let ys  : List <a> = Nil
				for (let i = 0; xs_.variation === 'Cons'; ++i)
					if (i === MAX)
					{
						console.error(`'foldr1' traversed too many elements (${MAX})`)
						console.dir(`Signature : foldr1 (<OPERATION>) (<XS>)`)
						console.dir(`<OPERATION> =`, operation)
						console.dir(`<XS>        =`, xs)
						return halt
					}
					else
						ys  = prepend (xs_.head) (ys),
						xs_ = xs_.tail
				ys.$reverse = xs
				return xs.$reverse = ys as any
			})()

		let out   = rxs.head
		let trxs_ = rxs.tail
		while (trxs_.variation === 'Cons')
			out   = operation (trxs_.head) (out),
			trxs_ = trxs_.tail
		return out
	}
}

/**` scanl : (b -> a -> b) -> b -> List b `*/
const scanl = <a, b>(operation : (leftside : b) => (rightside : a) => b) => (initial : b) => (xs : List <a>) : List <b> =>
	xs.variation === 'Nil'
		? singleton (initial)
		: lprepend  (initial) (() => scanl (operation) (operation (initial) (xs.head)) (xs.tail))

/**` scanl1 : (a -> a -> a) -> List a `*/
const scanl1 = <a>(operation : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : List <a> =>
	xs.variation === 'Nil' || xs.tail.variation === 'Nil'
		? xs
		: Cons
			(() => xs.head)
			(() => scanl (operation) (operation (xs.head) ((xs as any).tail.head)) ((xs as any).tail.tail))

/**` scanr : (a -> b -> b) -> b -> List a -> List b `*/
const scanr = <a, b>(operation : (leftside : a) => (rightside : b) => b) => (initial : b) => (xs : List <a>) : List <b> =>
{
	const rxs = xs.$reverse ??
		(() => {
			let ys  : List <a> = Nil
			let xs_            = xs
			for (let i = 0; xs_.variation === 'Cons'; ++i)
				if (i === MAX)
				{
					console.error(`'scanr' traversed too many elements (${MAX})`)
					console.dir(`Signature : scanr (<OPERATION>) (<INITIAL>) (<XS>)`)
					console.dir(`<OPERATION> =`, operation)
					console.dir(`<INITIAL>   =`, initial)
					console.dir(`<XS>        =`, xs)
					return halt
				}
				else
					ys  = prepend (xs_.head) (ys),
					xs_ = xs_.tail
			ys.$reverse = xs
			return xs.$reverse = ys
		})()

	let rxs_ = rxs
	let outs = singleton (initial)
	while (rxs_.variation === 'Cons')
		outs = prepend (operation (rxs_.head) ((outs as any).head)) (outs),
		rxs_ = rxs_.tail
	return outs
}

/**` scanr1 : (a -> a -> a) -> List a -> List a `*/
const scanr1 = <a>(operation : (leftside : a) => (rightside : a) => a) => (xs : List <a>) : List <a> =>
{
	if (xs.variation === 'Nil')
		return Nil
	else
	{
		const rxs = xs.$reverse ??
			(() => {
				let ys  : List <a> = Nil
				let xs_ : List <a> = xs
				for (let i = 0; xs_.variation === 'Cons'; ++i)
					if (i === MAX)
					{
						console.error(`'scanr1' traversed too many elements (${MAX})`)
						console.dir(`Signature : scanr1 (<OPERATION>) (<XS>)`)
						console.dir(`<OPERATION> =`, operation)
						console.dir(`<XS>        =`, xs)
						return halt
					}
					else
						ys  = prepend (xs_.head) (ys),
						xs_ = xs_.tail
				ys.$reverse = xs
				return xs.$reverse = ys as any
			})()

		let rxs_ = (rxs as any).tail
		let outs = singleton ((rxs as any).head)
		while (rxs_.variation === 'Cons')
			outs = prepend (operation (rxs_.head) ((outs as any).head)) (outs),
			rxs_ = rxs_.tail
		return outs
	}
}

/**` take : Number -> List a -> List a `*/
const take = (amount : number) => <a>(xs : List <a>) : List <a> =>
	amount >= xs.$len!
		? xs
		: xs.variation === 'Nil' || amount < 1
			? Nil
			: Cons (() => xs.head) (() => take (amount - 1) (xs.tail))

/**` drop : Number -> List a -> List a `*/
const drop = (amount : number) => <a>(xs : List <a>) : List <a> =>
{
	if (amount >= xs.$len!)
		return Nil
	else if (amount >= MAX)
	{
		console.error(`'drop' would be traversing too many elements (${MAX})`)
		console.dir(`Signature : drop (<AMOUNT>) (<XS>)`)
		console.dir(`<AMOUNT> =`, amount)
		console.dir(`<XS>     =`, xs)
		return halt
	}
	else
	{
		let xs_ = xs
		let am_ = amount
		while (xs_.variation === 'Cons' && ~~am_)
			xs_ = xs_.tail,
			--am_
		return xs_
	}
}

/**` splitAt : Number -> List a -> Pair (List a) (List a) `*/
const splitAt = (index : number) => <a>(xs : List <a>) : Pair <List <a>, List <a>> =>
{
	if (index >= xs.$len!)
		return Pair (Nil, xs)
	else if (index < 1)
		return Pair (xs, Nil)
	else if (index >= MAX)
	{
		console.error(`'splitAt' would be traversing too many elements (${MAX})`)
		console.dir(`Signature : splitAt (<INDEX>) (<XS>)`)
		console.dir(`<INDEX> =`, index)
		console.dir(`<XS>     =`, xs)
		return halt
	}
	else
	{
		let xs_ = xs

		let rxs0 : List <a> = Nil
		for (let i = 1; i <= index && xs_.variation === 'Cons'; ++i)
			rxs0 = prepend (xs_.head) (rxs0),
			xs_  = xs_.tail

		let xs0 : List <a> = Nil
		while (rxs0.variation === 'Cons')
			xs0   = prepend (rxs0.head) (xs0),
			rxs0 = rxs0.tail

		return Pair (xs0, xs_)
	}
}

/**` takeWhile : (a -> Boolean) -> List a -> List a `*/
const takeWhile = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
	xs.variation === 'Cons' && predicate (xs.head)
		? lprepend (xs.head) (() => takeWhile (predicate) (xs.tail))
		: Nil

/**` dropWhile : (a -> Boolean) -> List a -> List a `*/
const dropWhile = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'dropWhile' traversed too many elements (${MAX})`)
			console.dir(`Signature : dropWhile (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			xs_ = xs_.tail
		else return xs_
	return Nil
}

/**` span : (a -> Boolean) -> List a -> Pair (List a) (List a) `*/
const span = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <List <a>, List <a>> =>
{
	let xs_ = xs

	let rxs0 : List <a> = Nil
	for (let i = 0; xs_.variation === 'Cons' && predicate (xs_.head); ++i)
		if (i === MAX)
		{
			console.error(`'span' traversed too many elements (${MAX})`)
			console.dir(`Signature : span (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
		}
		else
			rxs0 = prepend (xs_.head) (rxs0),
			xs_  = xs_.tail

	let xs0 : List <a> = Nil
	while (rxs0.variation === 'Cons')
		xs0   = prepend (rxs0.head) (xs0),
		rxs0 = rxs0.tail

	return Pair (xs0, xs_)
}

/**` elem : (Eq a) => a -> List a -> Boolean `*/
const elem = <a extends Eq <a>>(value : a) => (xs : List <a>) : boolean =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'elem' traversed too many elements`)
			console.dir(`Signature : elem (<VALUE>) (<XS>)`)
			console.dir(`<VALUE> =`, value)
			console.dir(`<XS>    =`, xs)
			return halt
		}
		else if (xs_.head .eq (value))
			return true
		else
			xs_ = xs_.tail
	return false
}

/**` filter : (a -> Boolean) -> List a -> List a `*/
const filter = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <a> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'filter' traversed too many elements (${MAX})`)
			console.dir(`Signature : filter (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			return lprepend (xs_.head) (() => filter (predicate) ((xs_ as any).tail))
		else
			xs_ = xs_.tail
	return Nil
}

/**` partition : (a -> Boolean) -> List a -> Pair (List a) (List a) `*/
const partition = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <List <a>, List <a>> =>
	Pair (filter (predicate) (xs), filter (notf (predicate)) (xs))

/**` strictPartition : (a -> Boolean) -> List a -> Pair (List a) (List a) `*/
const strictPartition = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <List <a>, List <a>> =>
{
	let xs_             = xs
	let rxs0 : List <a> = Nil
	let rxs1 : List <a> = Nil
	for (let i = 0; xs_.variation === 'Cons'; ++i, xs_ = xs_.tail)
		if (i === MAX)
		{
			console.error(`'strictPartition' traversed too many elements (${MAX})`)
			console.dir(`Signature : strictPartition (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			rxs0 = prepend (xs_.head) (rxs0)
		else
			rxs1 = prepend (xs_.head) (rxs1)

	let xs0 : List <a> = Nil
	while (rxs0.variation === 'Cons')
		xs0  = prepend (rxs0.head) (xs0),
		rxs0 = rxs0.tail

	let xs1 : List <a> = Nil
	while (rxs1.variation === 'Cons')
		xs1  = prepend (rxs1.head) (xs1),
		rxs1 = rxs1.tail

	return Pair (xs0, xs1)
}

/**` tail_elemIndices : (Eq a) => Number -> a -> List a -> List Number `*/
const tail_elemIndices = (start : number) => <a extends Eq <a>>(value : a) => (xs : List <a>) : List <number> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'tail_elemIndices' traversed too many elements`)
			console.dir(`Signature : tail_elemIndices .eq (<START>) (<VALUE>) (<XS>) | This function is the tail version of 'elemIndices'`)
			console.dir(`<START> =`, start)
			console.dir(`<VALUE> =`, value)
			console.dir(`<XS>    =`, xs)
			return halt
		}
		else if (xs_.head .eq (value))
			return lprepend (i + start) (() => tail_elemIndices (i + start + 1) (value) ((xs_ as any).tail))
		else
			xs_ = xs_.tail
	return Nil
}

/**` elemIndices : (Eq a) => a -> List a -> List Number `*/
const elemIndices = <a extends Eq <a>>(value : a) => (xs : List <a>) : List <number> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'elemIndices' traversed too many elements`)
			console.dir(`Signature : elemIndices (<VALUE>) (<XS>)`)
			console.dir(`<VALUE> =`, value)
			console.dir(`<XS>    =`, xs)
			return halt
		}
		else if (xs_.head .eq (value))
			return lprepend (i) (() => tail_elemIndices (i + 1) (value) ((xs_ as any).tail))
		else
			xs_ = xs_.tail
	return Nil
}

/**` findIndices : (a -> Boolean) -> List a -> List Number `*/
const findIndices = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : List <number> =>
{
	let xs_                  = xs
	let rins : List <number> = Nil
	for (let i = 0; xs_.variation === 'Cons'; ++i, xs_ = xs_.tail)
		if (i === MAX)
		{
			console.error(`'findIndices' traversed too many elements (${MAX})`)
			console.dir(`Signature : findIndices (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			rins = prepend (i) (rins)

	let ins : List <number> = Nil
	while (rins.variation === 'Cons')
		ins  = prepend (rins.head) (ins),
		rins = rins.tail

	return ins
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

/**` strictUnzip : List (Pair a b) -> Pair (List a) (List b) `*/
const strictUnzip = <a, b>(pairs : List <Pair <a, b>>) : Pair <List <a>, List <b>> =>
{
	let pairs_           = pairs
	let rfsts : List <a> = Nil
	let rsnds : List <b> = Nil

	for (let i = 0; pairs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'strictUnzip' traversed too many elements (${MAX})`)
			console.dir(`Signature : strictUnzip (<PAIRS>)`)
			console.dir(`<PAIRS> =`, pairs)
			return halt
		}
		else
			rfsts = prepend (pairs_.head.fst) (rfsts),
			rsnds = prepend (pairs_.head.snd) (rsnds),
			pairs_ = pairs_.tail

	let fsts : List <a> = Nil
	while (rfsts.variation === 'Cons')
		fsts  = prepend (rfsts.head) (fsts),
		rfsts = rfsts.tail

	let snds : List <b> = Nil
	while (rsnds.variation === 'Cons')
		snds  = prepend (rsnds.head) (snds),
		rsnds = rsnds.tail

	return Pair (fsts, snds)
}

/**` lowercases : List String `*/
const lowercases : List <string> = chars ('abcdefghijklmnopqrstuvwxyz')

/**` uppercases : List String `*/
const uppercases : List <string> = chars ('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

/********************************************************************************************************************************/
// Implementation of Constants and Functions of Pair //

/**` fst : Pair a b -> a `*/
const fst = <a, b>(pair : Pair <a, b>) : a =>
	pair.fst

/**` snd : Pair a b -> b `*/
const snd = <a, b>(pair : Pair <a, b>) : b =>
	pair.snd

/**` mapPair : (a -> b) -> Pair a a -> Pair b b `*/
const mapPair = <a, b>(morphism : (value : a) => b) => (pair : Pair <a, a>) : Pair <b, b> =>
	Pair (morphism (pair.fst), morphism (pair.snd))

/**` mapFst : (a -> c) -> Pair a b -> Pair c b `*/
const mapFst = <a, c>(morphism : (value : a) => c) => <b>(pair : Pair <a, b>) : Pair <c, b> =>
	Pair (morphism (pair.fst), pair.snd)

/**` mapSnd : (b -> c) -> Pair a b -> Pair a c `*/
const mapSnd = <b, c>(morphism : (value : b) => c) => <a>(pair : Pair <a, b>) : Pair <a, c> =>
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
// Implementation of Constants and Functions of Either //

/**` extractLeft : Either a b -> a `*/
const extractLeft = <a, b>(either : Either <a, b>) : a =>
{
	if (either.variation === 'Left')
		return either.value
	else
	{
		console.error(`'extractLeft' expected a Left value`)
		console.dir(`Signature : extractLeft (<EITHER>)`)
		console.dir(`<EITHER> =`, either)
		return halt
	}
}

/**` extractRight : Either a b -> b `*/
const extractRight = <a, b>(either : Either <a, b>) : b =>
{
	if (either.variation === 'Right')
		return either.value
	else
	{
		console.error(`'extractRight' expected a Right value`)
		console.dir(`Signature : extractRight (<EITHER>)`)
		console.dir(`<EITHER> =`, either)
		return halt
	}
}

/**` fromLeft : a -> Either a b -> a `*/
const fromLeft = <a>(fallback : a) => <b>(either : Either <a, b>) : a =>
	either.variation === 'Left'
		? either.value
		: fallback

/**` fromRight : b -> Either a b -> b `*/
const fromRight = <b>(fallback : b) => <a>(either : Either <a, b>) : b =>
	either.variation === 'Right'
		? either.value
		: fallback

/**` collapseEither : Either a a -> a `*/
const collapseEither = <a>(either : Either <a, a>) : a =>
	either.value

/**` mapLeft : (a -> c) -> Either a b -> Either c b `*/
const mapLeft = <a, c>(morphism : (left : a) => c) => <b>(either : Either <a, b>) : Either <c, b> =>
	either.variation === 'Left'
		? Left (morphism (either.value))
		: either as Either <c, b>

/**` mapRight : (b -> c) -> Either a b -> Either a c `*/
const mapRight = <b, c>(morphism : (right : b) => c) => <a>(either : Either <a, b>) : Either <a, c> =>
	either.variation === 'Right'
		? Right (morphism (either.value))
		: either as Either <a, c>

/**` mapEither : (a -> c) -> (b -> d) -> Either a b -> Either c d `*/
const mapEither =
	<a, c>(leftMorphism  : (left  : a) => c) =>
	<b, d>(rightMorphism : (right : b) => d) => (either : Either <a, b>) : Either <c, d> =>
		either.variation === 'Left'
			? Left  (leftMorphism  (either.value))
			: Right (rightMorphism (either.value))

/**` reduceEither : (a -> c) -> (b -> c) -> Either a b -> c `*/
const reduceEither =
	<a, c>(leftMorphism  : (left  : a) => c) =>
	<b   >(rightMorphism : (right : b) => c) => (either : Either <a, b>) : c =>
		either.variation === 'Left'
			? leftMorphism  (either.value)
			: rightMorphism (either.value)

/********************************************************************************************************************************/
// Implementation of Constants and Functions of Misc. Algebraic Data Types //

/**` zeroV2 : V2 `*/
const zeroV2 : V2 = V2 (0, 0)

/**` zeroV3 : V3 `*/
const zeroV3 : V3 = V3 (0, 0, 0)

/**` zeroV4 : V4 `*/
const zeroV4 : V4 = V4 (0, 0, 0, 0)

/**` isButtonUp : ButtonState -> Boolean `*/
const isButtonUp = (state : ButtonState) : boolean =>
	state === 'Up' || state === 'toUp'

/**` isButtonDown : ButtonState -> Boolean `*/
const isButtonDown = (state : ButtonState) : boolean =>
	state === 'Down' || state === 'toDown'

/**` isButtonInTransition : ButtonState -> Boolean `*/
const isButtonInTransition = (state : ButtonState) : boolean =>
	state === 'toUp' || state === 'toDown'

/**` iterateButtonState : ButtonState -> ButtonState `*/
const iterateButtonState = (state : ButtonState) : ButtonState =>
	state === 'toUp'   ? 'Up'   :
	state === 'toDown' ? 'Down' : state

/**` signButtonState : ButtonState -> Number `*/
const signButtonState = (state : ButtonState) : number =>
	state === 'Up'   || state === 'toUp'   ?  1 :
	state === 'Down' || state === 'toDown' ? -1 : 0

/********************************************************************************************************************************/
// Implementation of Constants and Functions of Multiple Algebraic Data Types //

/**` sequence_IO : List (IO a) -> IO (List a) `*/
const sequence_IO = <a>(ios : List <IO <a>>) : IO <List <a>> =>
	IO (() => ios .fmap (io => io.effect ()))

/**` execute_IO : List (IO a) -> IO () `*/
const execute_IO = <a>(ios : List <IO <a>>) : IO <null> =>
{
	const effects : Array <() => a> = []
	let ios_ = ios

	for (let i = 0; ios_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'execute_IO' traversed too many elements (${MAX}) when coverting the given List (IO a) to a primitive Array`)
			console.dir(`Signature : execute_IO (<IOS>)`)
			console.dir(`<IOS> =`, ios_)
			return halt
		}
		else
			effects.push(ios_.head.effect),
			ios_ = ios_.tail

	return IO (() => (effects.map(f => f ()), null))
}

/**` maybeHead : List a -> Maybe a `*/
const maybeHead = <a>(xs : List <a>) : Maybe <a> =>
	xs.variation === 'Nil'
		? Nothing
		: Just (xs.head)

/**` maybeLast : List a -> Maybe a `*/
const maybeLast = <a>(xs : List <a>) : Maybe <a> =>
{
	if (xs.variation === 'Nil')
		return Nothing
	else if (xs.$last === undefined)
	{
		let xs_ : List <a> = xs
		for (let i = 0; (xs_ as any).tail.variation === 'Cons'; ++i)
			if (i === MAX)
			{
				console.error(`'maybeLast' traversed too many elements (${MAX})`)
				console.dir(`Signature : maybeLast (<XS>)`)
				console.dir(`<XS> =`, xs)
				return halt
			}
			else
				xs_ = (xs_ as any).tail
		return Just (xs.$last = (xs_ as any).head)
	}
	else return Just (xs.$last)
}

/**` maybeTail : List a -> Maybe (List a) `*/
const maybeTail = <a>(xs : List <a>) : Maybe <List <a>> =>
	xs.variation === 'Nil'
		? Nothing
		: Just (xs.tail)

/**` maybeInit : List a -> Maybe (List a) `*/
const maybeInit = <a>(xs : List <a>) : Maybe <List <a>> =>
{
	if (xs.variation === 'Nil')
		return Nothing
	else if (xs.$init === undefined)
		return Just (xs.$init = xs.tail.variation === 'Nil' ? Nil : Cons (() => xs.head) (() => init (xs.tail)))
	else return Just (xs.$init)
}

/**` maybeSingleton : Maybe a -> List a `*/
const maybeSingleton = <a>(maybe : Maybe <a>) : List <a> =>
	maybe.variation === 'Nothing'
		? Nil
		: singleton (maybe.value)

/**` pairToV2 : Pair Number Number -> V2 `*/
const pairToV2 = (pair : Pair <number, number>) : V2 =>
	V2 (pair.fst, pair.snd)

/**` v2ToPair : V2 -> Pair Number Number `*/
const v2ToPair = (vector : V2) : Pair <number, number> =>
	Pair (vector.x, vector.y)

/**` pairOfMaybes : Pair (Maybe a) (Maybe b) -> Maybe (Pair a b) `*/
const pairOfMaybes = <a, b>(pair : Pair <Maybe <a>, Maybe <b>>) : Maybe <Pair <a, b>> =>
	pair.fst.variation === 'Just' && pair.snd.variation === 'Just'
		? Just (Pair (pair.fst.value, pair.snd.value))
		: Nothing

/**` find : (a -> Boolean) -> List a -> Maybe a `*/
const find = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Maybe <a> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'find' traversed too many elements (${MAX})`)
			console.dir(`Signature : find (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			return Just (xs_.head)
		else
			xs_ = xs_.tail
	return Nothing
}

/**` elemIndex : (Eq a) => a -> List a -> Maybe Number `*/
const elemIndex = <a extends Eq <a>>(value : a) => (xs : List <a>) : Maybe <number> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'elemIndex' traversed too many elements`)
			console.dir(`Signature : elemIndex (<VALUE>) (<XS>)`)
			console.dir(`<VALUE> =`, value)
			console.dir(`<XS>    =`, xs)
			return halt
		}
		else if (xs_.head .eq (value))
			return Just (i)
		else
			xs_ = xs_.tail
	return Nothing
}

/**` findIndex : (a -> Boolean) -> List a -> Maybe Number `*/
const findIndex = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Maybe <number> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'findIndex' traversed too many elements (${MAX})`)
			console.dir(`Signature : findIndex (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
			return Just (i)
		else
			xs_ = xs_.tail
	return Nothing
}

/**` justs : List (Maybe a) -> List a `*/
const justs = <a>(maybes : List <Maybe <a>>) : List <a> =>
{
	let maybes_ = maybes
	for (let i = 0; maybes_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'justs' traversed too many elements (${MAX})`)
			console.dir(`Signature : justs (<MAYBES>)`)
			console.dir(`<MAYBES> =`, maybes)
			return halt
		}
		else if (maybes_.head.variation === 'Just')
			return lprepend (maybes_.head.value) (() => justs ((maybes_ as any).tail))
		else
			maybes_ = maybes_.tail
	return Nil
}

/**` mapMaybe : (a -> Maybe b) -> List a -> List b `*/
const mapMaybe = <a, b>(mapping : (element : a) => Maybe <b>) => (xs : List <a>) : List <b> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'mapMaybe' traversed too many elements (${MAX})`)
			console.dir(`Signature : mapMaybe (<MAPPING>) (<XS>)`)
			console.dir(`<MAPPING> =`, mapping)
			console.dir(`<XS> =`, xs)
			return halt
		}
		else
		{
			const maybe = mapping (xs_.head)
			if (maybe.variation === 'Just')
				return lprepend (maybe.value) (() => mapMaybe (mapping) ((xs_ as any).tail))
			xs_ = xs_.tail
		}
	return Nil
}

/**` lefts : List (Either a b) -> List a `*/
const lefts = <a, b>(eithers : List <Either <a, b>>) : List <a> =>
{
	let eithers_ = eithers
	for (let i = 0; eithers_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'lefts' traversed too many elements (${MAX})`)
			console.dir(`Signature : lefts (<EITHERS>)`)
			console.dir(`<EITHERS> =`, eithers)
			return halt
		}
		else if (eithers_.head.variation === 'Left')
			return lprepend (eithers_.head.value) (() => lefts ((eithers_ as any).tail))
		else
			eithers_ = eithers_.tail
	return Nil
}

/**` rights : List (Either a b) -> List b `*/
const rights = <a, b>(eithers : List <Either <a, b>>) : List <b> =>
{
	let eithers_ = eithers
	for (let i = 0; eithers_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'rights' traversed too many elements (${MAX})`)
			console.dir(`Signature : rights (<EITHERS>)`)
			console.dir(`<EITHERS> =`, eithers)
			return halt
		}
		else if (eithers_.head.variation === 'Right')
			return lprepend (eithers_.head.value) (() => rights ((eithers_ as any).tail))
		else
			eithers_ = eithers_.tail
	return Nil
}

/**` maybeAt : Number -> List a -> Maybe a `*/
const maybeAt = (index : number) => <a>(xs : List <a>) : Maybe <a> =>
{
	let xs_ = xs
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'maybeAt' traversed too many elements (${MAX})`)
			console.dir(`Signature : maybeAt (<INDEX>) (<XS>)`)
			console.dir(`<INDEX> =`, index)
			console.dir(`<XS>    =`, xs)
			return halt
		}
		else if (i >= index)
			return Just (xs_.head)
		else
			xs_ = xs_.tail
	return Nothing
}

/**` processToList : Process s a -> s -> List a `*/
const processToList = <s, a>(process : Process <s, a>) => (state : s) : List <a> =>
{
	const p = process.computation (state)
	return lprepend (p.snd) (() => processToList (process) (p.fst))
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

/**` pickout : (a -> Boolean) -> List a -> Pair (Maybe a) (List a) `*/
const pickout = <a>(predicate : (element : a) => boolean) => (xs : List <a>) : Pair <Maybe <a>, List <a>> =>
{
	let xs_            = xs
	let rys : List <a> = Nil
	for (let i = 0; xs_.variation === 'Cons'; ++i)
		if (i === MAX)
		{
			console.error(`'pickout' traversed too many elements (${MAX})`)
			console.dir(`Signature : pickout (<PREDICATE>) (<XS>)`)
			console.dir(`<PREDICATE> =`, predicate)
			console.dir(`<XS>        =`, xs)
			return halt
		}
		else if (predicate (xs_.head))
		{
			let ys : List <a> = Nil
			while (rys.variation === 'Cons')
				ys  = prepend (rys.head) (ys),
				rys = rys.tail

			return Pair (Just (xs_.head), ys .link (xs_.tail))
		}
		else
			rys = prepend (xs_.head) (rys),
			xs_ = xs_.tail

	return Pair (Nothing, xs_ as List <a>)
}

/********************************************************************************************************************************/
// Minor IO Operations //

/**` forn : Number -> (Number -> IO a) -> IO () `*/
const forn = (amount : number) => <a>(operation : (index : number) => IO <a>) : IO <null> =>
{
	const effects : Array <() => a> = []
	for (let i = 0; i < amount; ++i) effects.push(operation (i).effect)
	return IO (() => (effects.forEach(f => f ()), null))
}

/**` forloop : Number -> Number -> Number -> (Number -> IO a) -> IO () `*/
const forloop =
	(start     : number                     ) =>
	(condition : (index : number) => boolean) =>
	(morphism  : (index : number) => number ) =>
	<a>(operation : (index : number) => IO <a>) : IO <null> =>
{
	const effects : Array <() => a> = []
	for (let i = start; condition (i); i = morphism (i)) effects.push(operation (i).effect)
	return IO (() => (effects.forEach(f => f ()), null))
}

/**` prompting : String -> IO (Maybe String) `*/
const prompting = (message : string) : IO <Maybe <string>> =>
	IO (() => {
		const input = prompt(message)
		return input === null
			? Nothing
			: Just (input)
	})

/**` inputting : String -> IO String `*/
const inputting = (message : string) : IO <string> =>
	IO (() => prompt(message) ?? '')

/**` alerting : String -> IO () `*/
const alerting = (message : string) : IO <null> =>
	IO (() => (alert(message), null))

/**` confirming : String -> IO Boolean `*/
const confirming = (message : string) : IO <boolean> =>
	IO (() => confirm(message))

/**` delay : Number -> IO a -> IO () `*/
const delay = (amount : number) => <a>(io : IO <a>) : IO <null> =>
	IO (() => (setTimeout (io.effect, amount), null))

/**` flush : IO () `*/
const flush : IO <null> =
	IO (() => (console.clear(), null))

/**` log : a -> IO () `*/
const log = <a>(message : a) : IO <null> =>
	IO (() => (console.log(message), null))

/**` warn : a -> IO () `*/
const warn = <a>(message : a) : IO <null> =>
	IO (() => (console.warn(message), null))

/**` debug : a -> IO () `*/
const debug = <a>(message : a) : IO <null> =>
	IO (() => (console.debug(message), null))

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

/**` time : IO Number `*/
const time : IO <number> = IO (Date.now)

/********************************************************************************************************************************/
// Major IO Operations //

const Ψ =
	{
		MUTABLE         : {} as any,
		ctxs            : [] as Array <CanvasRenderingContext2D>,
		ctx             : undefined as unknown as CanvasRenderingContext2D,
		resizeID        : undefined as unknown as number,
		isResized       : false,
		isPointerLocked : false,
		seed            : (Math.random() - 0.5) * Date.now() % 0xffffff,
		debugCounter    : 0,
		image           : Object.create(null) as { [x : string] : HTMLImageElement },
		audio           : Object.create(null) as { [x : string] : HTMLAudioElement },
		mouseSX         : 0, mouseSY : 0,
		mouseWX         : 0, mouseWY : 0,
		mouseCX         : 0, mouseCY : 0,
		mouseDX         : 0, mouseDY : 0,
		mouseScroll     : 'Zero' as Axis,
		mouseButtons    : Array(5).fill('Up') as [ButtonState, ButtonState, ButtonState, ButtonState, ButtonState],
		keyboard        : keyboardKeysArray.reduce(($, k) => ({ ...$, [k] : 'Up' }), Object.create(null)) as { [x in KeyboardKey] : ButtonState }
	}

/**` n_mouseScreenPositionX : IO Number `*/
const n_mouseScreenPositionX : IO <number> = IO (() => Ψ.mouseSX / screen.width)

/**` n_mouseScreenPositionY : IO Number `*/
const n_mouseScreenPositionY : IO <number> = IO (() => Ψ.mouseSY / screen.height)

/**` n_mouseScreenPosition : IO V2 `*/
const n_mouseScreenPosition : IO <V2> = IO (() => V2 (Ψ.mouseSX / screen.width, Ψ.mouseSY / screen.height))

/**` mouseScreenPositionX : IO Number `*/
const mouseScreenPositionX : IO <number> = IO (() => Ψ.mouseSX)

/**` mouseScreenPositionY : IO Number `*/
const mouseScreenPositionY : IO <number> = IO (() => Ψ.mouseSY)

/**` mouseScreenPosition : IO V2 `*/
const mouseScreenPosition : IO <V2> = IO (() => V2 (Ψ.mouseSX, Ψ.mouseSY))

/**` n_mouseWindowPositionX : IO Number `*/
const n_mouseWindowPositionX : IO <number> = IO (() => Ψ.mouseWX / innerWidth)

/**` n_mouseWindowPositionY : IO Number `*/
const n_mouseWindowPositionY : IO <number> = IO (() => Ψ.mouseWY / innerHeight)

/**` n_mouseWindowPosition : IO V2 `*/
const n_mouseWindowPosition : IO <V2> = IO (() => V2 (Ψ.mouseWX / innerWidth, Ψ.mouseWY / innerHeight))

/**` mouseWindowPositionX : IO Number `*/
const mouseWindowPositionX : IO <number> = IO (() => Ψ.mouseWX)

/**` mouseWindowPositionY : IO Number `*/
const mouseWindowPositionY : IO <number> = IO (() => Ψ.mouseWY)

/**` mouseWindowPosition : IO V2 `*/
const mouseWindowPosition : IO <V2> = IO (() => V2 (Ψ.mouseWX, Ψ.mouseWY))

/**` n_mouseCanvasPositionX : IO Number `*/
const n_mouseCanvasPositionX : IO <number> = IO (() => Ψ.mouseCX / Ψ.ctx.canvas.width)

/**` n_mouseCanvasPositionY : IO Number `*/
const n_mouseCanvasPositionY : IO <number> = IO (() => Ψ.mouseCY / Ψ.ctx.canvas.height)

/**` n_mouseCanvasPosition : IO V2 `*/
const n_mouseCanvasPosition : IO <V2> = IO (() => V2 (Ψ.mouseCX / Ψ.ctx.canvas.width, Ψ.mouseCY / Ψ.ctx.canvas.height))

/**` mouseCanvasPositionX : IO Number `*/
const mouseCanvasPositionX : IO <number> = IO (() => Ψ.mouseCX)

/**` mouseCanvasPositionY : IO Number `*/
const mouseCanvasPositionY : IO <number> = IO (() => Ψ.mouseCY)

/**` mouseCanvasPosition : IO V2 `*/
const mouseCanvasPosition : IO <V2> = IO (() => V2 (Ψ.mouseCX, Ψ.mouseCY))

/**` n_mouseDeltaX : IO Number `*/
const n_mouseDeltaX : IO <number> = IO (() => Ψ.mouseDX / Ψ.ctx.canvas.width)

/**` n_mouseDeltaY : IO Number `*/
const n_mouseDeltaY : IO <number> = IO (() => Ψ.mouseDY / Ψ.ctx.canvas.height)

/**` n_mouseDelta : IO V2 `*/
const n_mouseDelta : IO <V2> = IO (() => V2 (Ψ.mouseDX / Ψ.ctx.canvas.width, Ψ.mouseDY / Ψ.ctx.canvas.height))

/**` mouseDeltaX : IO Number `*/
const mouseDeltaX : IO <number> = IO (() => Ψ.mouseDX)

/**` mouseDeltaY : IO Number `*/
const mouseDeltaY : IO <number> = IO (() => Ψ.mouseDY)

/**` mouseDelta : IO V2 `*/
const mouseDelta : IO <V2> = IO (() => V2 (Ψ.mouseDX, Ψ.mouseDY))

/**` mouseScroll : IO Axis `*/
const mouseScroll : IO <Axis> = IO (() => Ψ.mouseScroll)

/**` mouseLeft : IO ButtonState `*/
const mouseLeft : IO <ButtonState> = IO (() => Ψ.mouseButtons [0])

/**` mouseMiddle : IO ButtonState `*/
const mouseMiddle : IO <ButtonState> = IO (() => Ψ.mouseButtons[1])

/**` mouseRight : IO ButtonState `*/
const mouseRight : IO <ButtonState> = IO (() => Ψ.mouseButtons[2])

/**` mouse4th : IO ButtonState `*/
const mouse4th : IO <ButtonState> = IO (() => Ψ.mouseButtons[3])

/**` mouse5th : IO ButtonState `*/
const mouse5th : IO <ButtonState> = IO (() => Ψ.mouseButtons[4])

/**` n_mouseLeftClickCoordinateX : IO (Maybe Number) `*/
const n_mouseLeftClickCoordinateX : IO <Maybe <number>> = __MACRO__.n_clickx(0)

/**` n_mouseLeftClickCoordinateY : IO (Maybe Number) `*/
const n_mouseLeftClickCoordinateY : IO <Maybe <number>> = __MACRO__.n_clicky(0)

/**` n_mouseLeftClickCoordinates : IO (Maybe V2) `*/
const n_mouseLeftClickCoordinates : IO <Maybe <V2>> = __MACRO__.n_clickv(0)

/**` mouseLeftClickCoordinateX : IO (Maybe Number) `*/
const mouseLeftClickCoordinateX : IO <Maybe <number>> = __MACRO__.clickx(0)

/**` mouseLeftClickCoordinateY : IO (Maybe Number) `*/
const mouseLeftClickCoordinateY : IO <Maybe <number>> = __MACRO__.clicky(0)

/**` mouseLeftClickCoordinates : IO (Maybe V2) `*/
const mouseLeftClickCoordinates : IO <Maybe <V2>> = __MACRO__.clickv(0)

/**` n_mouseMiddleClickCoordinateX : IO (Maybe Number) `*/
const n_mouseMiddleClickCoordinateX : IO <Maybe <number>> = __MACRO__.n_clickx(1)

/**` n_mouseMiddleClickCoordinateY : IO (Maybe Number) `*/
const n_mouseMiddleClickCoordinateY : IO <Maybe <number>> = __MACRO__.n_clicky(1)

/**` n_mouseMiddleClickCoordinates : IO (Maybe V2) `*/
const n_mouseMiddleClickCoordinates : IO <Maybe <V2>> = __MACRO__.n_clickv(1)

/**` mouseMiddleClickCoordinateX : IO (Maybe Number) `*/
const mouseMiddleClickCoordinateX : IO <Maybe <number>> = __MACRO__.clickx(1)

/**` mouseMiddleClickCoordinateY : IO (Maybe Number) `*/
const mouseMiddleClickCoordinateY : IO <Maybe <number>> = __MACRO__.clicky(1)

/**` mouseMiddleClickCoordinates : IO (Maybe V2) `*/
const mouseMiddleClickCoordinates : IO <Maybe <V2>> = __MACRO__.clickv(1)

/**` n_mouseRightClickCoordinateX : IO (Maybe Number) `*/
const n_mouseRightClickCoordinateX : IO <Maybe <number>> = __MACRO__.n_clickx(2)

/**` n_mouseRightClickCoordinateY : IO (Maybe Number) `*/
const n_mouseRightClickCoordinateY : IO <Maybe <number>> = __MACRO__.n_clicky(2)

/**` n_mouseRightClickCoordinates : IO (Maybe V2) `*/
const n_mouseRightClickCoordinates : IO <Maybe <V2>> = __MACRO__.n_clickv(2)

/**` mouseRighClickCoordinateX : IO (Maybe Number) `*/
const mouseRighClickCoordinateX : IO <Maybe <number>> = __MACRO__.clickx(2)

/**` mouseRighClickCoordinateY : IO (Maybe Number) `*/
const mouseRighClickCoordinateY : IO <Maybe <number>> = __MACRO__.clicky(2)

/**` mouseRighClickCoordinates : IO (Maybe V2) `*/
const mouseRighClickCoordinates : IO <Maybe <V2>> = __MACRO__.clickv(2)

/**` n_mouse4thClickCoordinateX : IO (Maybe Number) `*/
const n_mouse4thClickCoordinateX : IO <Maybe <number>> = __MACRO__.n_clickx(3)

/**` n_mouse4thClickCoordinateY : IO (Maybe Number) `*/
const n_mouse4thClickCoordinateY : IO <Maybe <number>> = __MACRO__.n_clicky(3)

/**` n_mouse4thClickCoordinates : IO (Maybe V2) `*/
const n_mouse4thClickCoordinates : IO <Maybe <V2>> = __MACRO__.n_clickv(3)

/**` mouse4thClickCoordinateX : IO (Maybe Number) `*/
const mouse4thClickCoordinateX : IO <Maybe <number>> = __MACRO__.clickx(3)

/**` mouse4thClickCoordinateY : IO (Maybe Number) `*/
const mouse4thClickCoordinateY : IO <Maybe <number>> = __MACRO__.clicky(3)

/**` mouse4thClickCoordinate : IO (Maybe V2) `*/
const mouse4thClickCoordinate : IO <Maybe <V2>> = __MACRO__.clickv(3)

/**` n_mouse5thClickCoordinateX : IO (Maybe Number) `*/
const n_mouse5thClickCoordinateX : IO <Maybe <number>> = __MACRO__.n_clickx(4)

/**` n_mouse5thClickCoordinateY : IO (Maybe Number) `*/
const n_mouse5thClickCoordinateY : IO <Maybe <number>> = __MACRO__.n_clicky(4)

/**` n_mouse5thClickCoordinates : IO (Maybe V2) `*/
const n_mouse5thClickCoordinates : IO <Maybe <V2>> = __MACRO__.n_clickv(4)

/**` mouse5thClickCoordinateX : IO (Maybe Number) `*/
const mouse5thClickCoordinateX : IO <Maybe <number>> = __MACRO__.clickx(4)

/**` mouse5thClickCoordinateY : IO (Maybe Number) `*/
const mouse5thClickCoordinateY : IO <Maybe <number>> = __MACRO__.clicky(4)

/**` mouse5thClickCoordinates : IO (Maybe V2) `*/
const mouse5thClickCoordinates : IO <Maybe <V2>> = __MACRO__.clickv(4)

/**` keyboardKey : KeyboardKey -> IO ButtonState `*/
const keyboardKey = (keyname : KeyboardKey) : IO <ButtonState> => IO (() => Ψ.keyboard[keyname])

/**` screenWidth : IO Number `*/
const screenWidth : IO <number> = IO (() => screen.width)

/**` screenHeight : IO Number `*/
const screenHeight : IO <number> = IO (() => screen.height)

/**` screenDimensions : IO V2 `*/
const screenDimensions : IO <V2> = IO (() => V2 (screen.width, screen.height))

/**` windowWidth : IO Number `*/
const windowWidth : IO <number> = IO (() => innerWidth)

/**` windowHeight : IO Number `*/
const windowHeight : IO <number> = IO (() => innerHeight)

/**` windowDimensions : IO V2 `*/
const windowDimensions : IO <V2> = IO (() => V2 (innerWidth, innerHeight))

/**` canvasWidth : IO Number `*/
const canvasWidth : IO <number> = IO (() => Ψ.ctx.canvas.width)

/**` canvasHeight : IO Number `*/
const canvasHeight : IO <number> = IO (() => Ψ.ctx.canvas.height)

/**` canvasDimensions : IO V2 `*/
const canvasDimensions : IO <V2> = IO (() => V2 (Ψ.ctx.canvas.width, Ψ.ctx.canvas.height))

/**` n_wasdKeys : IO V2 `*/
const n_wasdKeys : IO <V2> =
	IO (() => {
		const x = isButtonDown (Ψ.keyboard.KeyD) as unknown as number - (isButtonDown (Ψ.keyboard.KeyA) as unknown as number)
		const y = isButtonDown (Ψ.keyboard.KeyS) as unknown as number - (isButtonDown (Ψ.keyboard.KeyW) as unknown as number)
		const l = x ** 2 + y ** 2
		return l === 0
			? zeroV2
			: l === 1
				? V2 (x, y)
				: V2 (x * invsqrt2, y * invsqrt2)
	})

/**` wasdKeys : IO V2 `*/
const wasdKeys : IO <V2> =
	IO (() =>
		V2 (
			isButtonDown (Ψ.keyboard.KeyD) as unknown as number - (isButtonDown (Ψ.keyboard.KeyA) as unknown as number),
			isButtonDown (Ψ.keyboard.KeyS) as unknown as number - (isButtonDown (Ψ.keyboard.KeyW) as unknown as number)
		)
	)

/**` n_arrowKeys : IO V2 `*/
const n_arrowKeys : IO <V2> =
	IO (() => {
		const x = isButtonDown (Ψ.keyboard.ArrowRight) as unknown as number - (isButtonDown (Ψ.keyboard.ArrowLeft) as unknown as number)
		const y = isButtonDown (Ψ.keyboard.ArrowDown)  as unknown as number - (isButtonDown (Ψ.keyboard.ArrowUp)   as unknown as number)
		const l = x ** 2 + y ** 2
		return l === 0
			? zeroV2
			: l === 1
				? V2 (x, y)
				: V2 (x * invsqrt2, y * invsqrt2)
	})

/**` arrowKeys : IO V2 `*/
const arrowKeys : IO <V2> =
	IO (() =>
		V2 (
			isButtonDown (Ψ.keyboard.ArrowRight) as unknown as number - (isButtonDown (Ψ.keyboard.ArrowLeft) as unknown as number),
			isButtonDown (Ψ.keyboard.ArrowDown)  as unknown as number - (isButtonDown (Ψ.keyboard.ArrowUp)   as unknown as number)
		)
	)

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

/**` n_textDimensions : String -> IO V2 `*/
const n_textDimensions = (text : string) : IO <V2> =>
	IO (() => {
		const { actualBoundingBoxLeft, actualBoundingBoxRight, actualBoundingBoxAscent, actualBoundingBoxDescent } = Ψ.ctx.measureText(text)
		return V2 (
			(Math.abs (actualBoundingBoxLeft)   + Math.abs (actualBoundingBoxRight))   / Ψ.ctx.canvas.width,
			(Math.abs (actualBoundingBoxAscent) + Math.abs (actualBoundingBoxDescent)) / Ψ.ctx.canvas.height
		)
	})

/**` textDimensions : String -> IO V2 `*/
const textDimensions = (text : string) : IO <V2> =>
	IO (() => {
		const { actualBoundingBoxLeft, actualBoundingBoxRight, actualBoundingBoxAscent, actualBoundingBoxDescent }
			= Ψ.ctx.measureText(text)
		return V2 (
			Math.abs (actualBoundingBoxLeft)   + Math.abs (actualBoundingBoxRight),
			Math.abs (actualBoundingBoxAscent) + Math.abs (actualBoundingBoxDescent)
		)
	})

/**` n_imageWidth : String -> IO Number `*/
const n_imageWidth = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].width / Ψ.ctx.canvas.width
			: __MACRO__.err_nonexisting_image_path('n_iamgeWidth', path)
	)

/**` imageWidth : String -> IO Number `*/
const imageWidth = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].width
			: __MACRO__.err_nonexisting_image_path('iamgeWidth', path)
	)

/**` n_imageHeight : String -> IO Number `*/
const n_imageHeight = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].height / Ψ.ctx.canvas.height
			: __MACRO__.err_nonexisting_image_path('n_imageHeight', path)
	)

/**` imageHeight : String -> IO Number `*/
const imageHeight = (path : string) : IO <number> =>
	IO (() =>
		Ψ.image[path]
			? Ψ.image[path].height
			: __MACRO__.err_nonexisting_image_path('imageHeight', path)
	)

/**` n_imageDimensions : String -> IO V2 `*/
const n_imageDimensions = (path : string) : IO <V2> =>
	IO (() =>
		Ψ.image[path]
			? V2 (Ψ.image[path].width / Ψ.ctx.canvas.width, Ψ.image[path].height / Ψ.ctx.canvas.height)
			: __MACRO__.err_nonexisting_image_path('n_imageDimensions', path)
	)

/**` imageDimensions : String -> IO V2 `*/
const imageDimensions = (path : string) : IO <V2> =>
	IO (() =>
		Ψ.image[path]
			? V2 (Ψ.image[path].width, Ψ.image[path].height)
			: __MACRO__.err_nonexisting_image_path('imageDimensions', path)
	)

/**` n_audioTime : String -> IO Number `*/
const n_audioTime = (path : string) : IO <number> =>
	IO (() =>
		Ψ.audio[path]
			? Ψ.audio[path].currentTime / Ψ.audio[path].duration
			: __MACRO__.err_nonexisting_audio_path('n_audioTime', path)
	)

/**` audioTime : String -> IO Number `*/
const audioTime = (path : string) : IO <number> =>
	IO (() =>
		Ψ.audio[path]
			? Ψ.audio[path].currentTime
			: __MACRO__.err_nonexisting_audio_path('audioTime', path)
	)

/**` audioDuration : String -> IO Number `*/
const audioDuration = (path : string) : IO <number> =>
	IO (() =>
		Ψ.audio[path]
			? Ψ.audio[path].duration
			: __MACRO__.err_nonexisting_audio_path('audioDuration', path)
	)

/**` n_lineThickness : IO Number `*/
const n_lineThickness : IO <number> = IO (() => Ψ.ctx.lineWidth / Ψ.ctx.canvas.width)

/**` lineThickness : IO Number `*/
const lineThickness : IO <number> = IO (() => Ψ.ctx.lineWidth)

/**` n_lineDashPattern : IO (List Number) `*/
const n_lineDashPattern : IO <List <number>> = IO (() => List (...Ψ.ctx.getLineDash().map(x => x / Ψ.ctx.canvas.width)))

/**` lineDashPattern : IO (List Number) `*/
const lineDashPattern : IO <List <number>> = IO (() => List (...Ψ.ctx.getLineDash()))

/**` n_lineDashOffset : IO Number `*/
const n_lineDashOffset : IO <number> = IO (() => Ψ.ctx.lineDashOffset / Ψ.ctx.canvas.width)

/**` lineDashOffset : IO Number `*/
const lineDashOffset : IO <number> = IO (() => Ψ.ctx.lineDashOffset)

/**` miterLimit : IO Number `*/
const miterLimit : IO <number> = IO (() => Ψ.ctx.miterLimit)

/**` n_fontSize : IO Number `*/
const n_fontSize : IO <number> = IO (() => parseFloat (Ψ.ctx.font) / Ψ.ctx.canvas.width)

/**` fontStyle : IO String `*/
const fontStyle : IO <string> = IO (() => Ψ.ctx.font)

/**` fontSize : IO Number `*/
const fontSize : IO <number> = IO (() => parseFloat (Ψ.ctx.font))

/**` fontFamily : IO String `*/
const fontFamily : IO <string> = IO (() => Ψ.ctx.font.slice(Ψ.ctx.font.indexOf(' ') + 1))

/**` n_shadowOffsetX : IO Number `*/
const n_shadowOffsetX : IO <number> = IO (() => Ψ.ctx.shadowOffsetX / Ψ.ctx.canvas.height)

/**` shadowOffsetX : IO Number `*/
const shadowOffsetX : IO <number> = IO (() => Ψ.ctx.shadowOffsetX)

/**` n_shadowOffsetY : IO Number `*/
const n_shadowOffsetY : IO <number> = IO (() => Ψ.ctx.shadowOffsetY / Ψ.ctx.canvas.height)

/**` shadowOffsetY : IO Number `*/
const shadowOffsetY : IO <number> = IO (() => Ψ.ctx.shadowOffsetY)

/**` n_shadowOffset : IO V2 `*/
const n_shadowOffset : IO <V2> = IO (() => V2 (Ψ.ctx.shadowOffsetX / Ψ.ctx.canvas.width, Ψ.ctx.shadowOffsetY / Ψ.ctx.canvas.height))

/**` shadowOffset : IO V2 `*/
const shadowOffset : IO <V2> = IO (() => V2 (Ψ.ctx.shadowOffsetX, Ψ.ctx.shadowOffsetY))

/**` shadowBlurAmount : IO Number `*/
const shadowBlurAmount : IO <number> = IO (() => Ψ.ctx.shadowBlur)

/**` shadowColor : IO String `*/
const shadowColor : IO <string> = IO (() => Ψ.ctx.shadowColor)

/**` n_isInEvenOddPath : Number -> : Number -> IO Boolean `*/
const n_isInEvenOddPath = (x : number) => (y : number) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height, 'evenodd'))

/**` n_isInEvenOddPathV2 : V2 -> IO Boolean `*/
const n_isInEvenOddPathV2 = (xy : V2) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(xy.x / Ψ.ctx.canvas.width, xy.y / Ψ.ctx.canvas.height, 'evenodd'))

/**` isInEvenOddPath : Number -> : Number -> IO Boolean `*/
const isInEvenOddPath = (x : number) => (y : number) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(x, y, 'evenodd'))

/**` isInEvenOddPathV2 : V2 -> IO Boolean `*/
const isInEvenOddPathV2 = (xy : V2) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(xy.x, xy.y, 'evenodd'))

/**` n_isInNonZeroPath : Number -> : Number -> IO Boolean `*/
const n_isInNonZeroPath = (x : number) => (y : number) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height, 'nonzero'))

/**` n_isInNonZeroPathV2 : V2 -> IO Boolean `*/
const n_isInNonZeroPathV2 = (v : V2) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(v.x / Ψ.ctx.canvas.width, v.y / Ψ.ctx.canvas.height, 'nonzero'))

/**` isInNonZeroPath : Number -> : Number -> IO Boolean `*/
const isInNonZeroPath = (x : number) => (y : number) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(x, y, 'nonzero'))

/**` isInNonZeroPathV2 : V2 -> IO Boolean `*/
const isInNonZeroPathV2 = (v : V2) : IO <boolean> => IO (() => Ψ.ctx.isPointInPath(v.x, v.y, 'nonzero'))

/**` n_isInStroke : Number -> : Number -> IO Boolean `*/
const n_isInStroke = (x : number) => (y : number) : IO <boolean> => IO (() => Ψ.ctx.isPointInStroke(x / Ψ.ctx.canvas.width, y / Ψ.ctx.canvas.height))

/**` n_isInStrokeV2 : V2 -> IO Boolean `*/
const n_isInStrokeV2 = (v : V2) : IO <boolean> => IO (() => Ψ.ctx.isPointInStroke(v.x / Ψ.ctx.canvas.width, v.y / Ψ.ctx.canvas.height))

/**` isInStroke : Number -> : Number -> IO Boolean `*/
const isInStroke = (x : number) => (y : number) : IO <boolean> => IO (() => Ψ.ctx.isPointInStroke(x, y))

/**` isInStrokeV2 : V2 -> IO Boolean `*/
const isInStrokeV2 = (v : V2) : IO <boolean> => IO (() => Ψ.ctx.isPointInStroke(v.x, v.y))

/**` layerIndex : IO Number `*/
const layerIndex : IO <number> = IO (() => Ψ.ctxs.findIndex(ctx => ctx === Ψ.ctx))

/**` alpha : IO Number `*/
const alpha : IO <number> = IO (() => Ψ.ctx.globalAlpha)

/**` lineCap : IO LineCap `*/
const lineCap : IO <LineCap> = IO (() => Ψ.ctx.lineCap)

/**` lineJoin : IO LineJoin `*/
const lineJoin : IO <LineJoin> = IO (() => Ψ.ctx.lineJoin)

/**` textAlign : IO TextAlign `*/
const textAlign : IO <TextAlign> = IO (() => Ψ.ctx.textAlign)

/**` textBaseline : IO TextBaseline `*/
const textBaseline : IO <TextBaseline> = IO (() => Ψ.ctx.textBaseline)

/**` composition : IO Composition `*/
const composition : IO <Composition> = IO (() => Ψ.ctx.globalCompositeOperation as Composition)

/**` seed : IO Number `*/
const seed : IO <number> = IO (() => Ψ.seed)

/**` isWindowResized : IO Boolean `*/
const isWindowResized : IO <boolean> = IO (() => Ψ.isResized)

/**` isPointerLocked : IO Boolean `*/
const isPointerLocked : IO <boolean> = IO (() => Ψ.isPointerLocked)

/**` beginPath : IO () `*/
const beginPath : IO <null> =
	IO (() => (Ψ.ctx.beginPath(), null))

/**` closePath : IO () `*/
const closePath : IO <null> =
	IO (() => (Ψ.ctx.closePath(), null))

/**` n_relocateTo : Number -> Number -> IO () `*/
const n_relocateTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_relocateToV2 : V2 -> IO () `*/
const n_relocateToV2 = (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

/**` relocateTo : Number -> Number -> IO () `*/
const relocateTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x, y), null))

/**` relocateToV2 : V2 -> IO () `*/
const relocateToV2 = (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(v.x, v.y), null))

/**` n_lineTo : Number -> Number -> IO () `*/
const n_lineTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_lineToV2 : V2 -> IO () `*/
const n_lineToV2 = (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

/**` lineTo : Number -> Number -> IO () `*/
const lineTo = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(x, y), null))

/**` lineToV2 : V2 -> IO () `*/
const lineToV2 = (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.lineTo(v.x, v.y), null))

/**` n_bezierCurveTo : ...6 Number -> IO () `*/
const n_bezierCurveTo = (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.bezierCurveTo(
			cx0 * Ψ.ctx.canvas.width, cy0 * Ψ.ctx.canvas.height,
			cx1 * Ψ.ctx.canvas.width, cy1 * Ψ.ctx.canvas.height,
			x   * Ψ.ctx.canvas.width, y   * Ψ.ctx.canvas.height
		), null
	))

/**` bezierCurveTo : ...6 Number -> IO () `*/
const bezierCurveTo = (cx0 : number) => (cy0 : number) => (cx1 : number) => (cy1 : number) => (x   : number) => (y   : number) : IO <null> =>
	IO (() => (Ψ.ctx.bezierCurveTo(cx0, cy0, cx1, cy1, x, y), null))

/**` n_bezierCurveToV2 : V2 -> V2 -> V2 -> IO () `*/
const n_bezierCurveToV2 = (cxy0 : V2) => (cxy1 : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.bezierCurveTo(
			cxy0.x * Ψ.ctx.canvas.width, cxy0.y * Ψ.ctx.canvas.height,
			cxy1.x * Ψ.ctx.canvas.width, cxy1.y * Ψ.ctx.canvas.height,
			xy  .x * Ψ.ctx.canvas.width, xy  .y * Ψ.ctx.canvas.height
		), null
	))

/**` bezierCurveToV2 : V2 -> V2 -> V2 -> IO () `*/
const bezierCurveToV2 = (cxy0 : V2) => (cxy1 : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.bezierCurveTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, xy.x, xy.y), null))

/**` n_quadraticCurveTo : Number -> Number -> Number -> Number -> IO () `*/
const n_quadraticCurveTo = (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.quadraticCurveTo(
			cx * Ψ.ctx.canvas.width, cy * Ψ.ctx.canvas.height,
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height
		), null
	))

/**` n_quadraticCurveToV2 : V2 -> V2 -> IO () `*/
const n_quadraticCurveToV2 = (cxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.quadraticCurveTo(
			cxy.x * Ψ.ctx.canvas.width, cxy.y * Ψ.ctx.canvas.height,
			xy .x * Ψ.ctx.canvas.width, xy .y * Ψ.ctx.canvas.height
		), null
	))

/**` quadraticCurveTo : Number -> Number -> Number -> Number -> IO () `*/
const quadraticCurveTo = (cx : number) => (cy : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.quadraticCurveTo(cx, cy, x, y), null))

/**` quadraticCurveToV2 : V2 -> V2 -> IO () `*/
const quadraticCurveToV2 = (cxy : V2) => (xy : V2) : IO <null> =>
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

/**` n_arcToV2 : Number -> V2 -> V2 -> IO () `*/
const n_arcToV2 = (r : number) => (cxy0 : V2) => (cxy1 : V2) : IO <null> =>
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

/**` arcToV2 : Number -> V2 -> V2 -> IO () `*/
const arcToV2 = (r : number) => (cxy0 : V2) => (cxy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arcTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, r), null))

/**` n_rect : Number -> Number -> Number -> Number -> IO () `*/
const n_rect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_rectV2 : V2 -> V2 -> IO () `*/
const n_rectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` rect : Number -> Number -> Number -> Number -> IO () `*/
const rect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x, y, w, h), null))

/**` rectV2 : V2 -> V2 -> IO () `*/
const rectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_fillRect : Number -> Number -> Number -> Number -> IO () `*/
const n_fillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_fillRectV2 : V2 -> V2 -> IO () `*/
const n_fillRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` fillRect : Number -> Number -> Number -> Number -> IO () `*/
const fillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(x, y, w, h), null))

/**` fillRectV2 : V2 -> V2 -> IO () `*/
const fillRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_strokeRect : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_strokeRectV2 : V2 -> V2 -> IO () `*/
const n_strokeRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` strokeRect : Number -> Number -> Number -> Number -> IO () `*/
const strokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(x, y, w, h), null))

/**` strokeRectV2 : V2 -> V2 -> IO () `*/
const strokeRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_fillStrokeRect : Number -> Number -> Number -> Number -> IO () `*/
const n_fillStrokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` n_fillStrokeRectV2 : V2 -> V2 -> IO () `*/
const n_fillStrokeRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` fillStrokeRect : Number -> Number -> Number -> Number -> IO () `*/
const fillStrokeRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x, y, w, h), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeRectV2 : V2 -> V2 -> IO () `*/
const fillStrokeRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillRect : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeFillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` n_strokeFillRectV2 : V2 -> V2 -> IO () `*/
const n_strokeFillRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` strokeFillRect : Number -> Number -> Number -> Number -> IO () `*/
const strokeFillRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x, y, w, h), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillRectV2 : V2 -> V2 -> IO () `*/
const strokeFillRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy.x, xy.y, wh.x, wh.y), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_area : Number -> Number -> Number -> Number -> IO () `*/
const n_area = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_areaV2 : V2 -> V2 -> IO () `*/
const n_areaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.width,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.width
		), null
	))

/**` area : Number -> Number -> Number -> Number -> IO () `*/
const area = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), null))

/**` areaV2 : V2 -> V2 -> IO () `*/
const areaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

/**` n_fillArea : Number -> Number -> Number -> Number -> IO () `*/
const n_fillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_fillAreaV2 : V2 -> V2 -> IO () `*/
const n_fillAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.fillRect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), null
	))

/**` fillArea : Number -> Number -> Number -> Number -> IO () `*/
const fillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(x0, y0, x1 - x0, y1 - y0), null))

/**` fillAreaV2 : V2 -> V2 -> IO () `*/
const fillAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.fillRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

/**` n_strokeArea : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_strokeAreaV2 : V2 -> V2 -> IO () `*/
const n_strokeAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.strokeRect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), null
	))

/**` strokeArea : Number -> Number -> Number -> Number -> IO () `*/
const strokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(x0, y0, x1 - x0, y1 - y0), null))

/**` strokeAreaV2 : V2 -> V2 -> IO () `*/
const strokeAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))

/**` n_fillStrokeArea : Number -> Number -> Number -> Number -> IO () `*/
const n_fillStrokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` n_fillStrokeAreaV2 : V2 -> V2 -> IO () `*/
const n_fillStrokeAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` fillStrokeArea : Number -> Number -> Number -> Number -> IO () `*/
const fillStrokeArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeAreaV2 : V2 -> V2 -> IO () `*/
const fillStrokeAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillArea : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeFillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` n_strokeFillAreaV2 : V2 -> V2 -> IO () `*/
const n_strokeFillAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.rect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` strokeFillArea : Number -> Number -> Number -> Number -> IO () `*/
const strokeFillArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.rect(x0, y0, x1 - x0, y1 - y0), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillAreaV2 : V2 -> V2 -> IO () `*/
const strokeFillAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_arc : ...5 Number -> IO () `*/
const n_arc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau), null))

/**` n_arcV2 : Number -> Number -> Number -> V2 -> IO () `*/
const n_arcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau), null))

/**` arc : ...5 Number -> IO () `*/
const arc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a1), null))

/**` arcV2 : Number -> Number -> Number -> V2 -> IO () `*/
const arcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a1), null))

/**` n_strokeArc : ...5 Number -> IO () `*/
const n_strokeArc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau), Ψ.ctx.stroke(), null))

/**` n_strokeArcV2 : Number -> Number -> Number -> V2 -> IO () `*/
const n_strokeArcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, a1 * tau), Ψ.ctx.stroke(), null))

/**` strokeArc : ...5 Number -> IO () `*/
const strokeArc = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a1), Ψ.ctx.stroke(), null))

/**` strokeArcV2 : Number -> Number -> Number -> V2 -> IO () `*/
const strokeArcV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a1), Ψ.ctx.stroke(), null))

/**` n_arcSection : ...5 Number -> IO () `*/
const n_arcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau), null))

/**` n_arcSectionV2 : Number -> Number -> Number -> V2 -> IO () `*/
const n_arcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau), null))

/**` arcSection : ...5 Number -> IO () `*/
const arcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a0 + a1), null))

/**` arcSectionV2 : Number -> Number -> Number -> V2 -> IO () `*/
const arcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a0 + a1), null))

/**` n_strokeArcSection : ...5 Number -> IO () `*/
const n_strokeArcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau), Ψ.ctx.stroke(), null))

/**` n_strokeArcSectionV2 : Number -> Number -> Number -> V2 -> IO () `*/
const n_strokeArcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, a0 * tau, (a0 + a1) * tau), Ψ.ctx.stroke(), null))

/**` strokeArcSection : ...5 Number -> IO () `*/
const strokeArcSection = (r : number) => (a0 : number) => (a1 : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` strokeArcSectionV2 : Number -> Number -> Number -> V2 -> IO () `*/
const strokeArcSectionV2 = (r : number) => (a0 : number) => (a1 : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` n_circle : Number -> Number -> Number -> IO () `*/
const n_circle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), null))

/**` n_circleV2 : Number -> V2 -> IO () `*/
const n_circleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), null))

/**` circle : Number -> Number -> Number -> IO () `*/
const circle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), null))

/**` circleV2 : Number -> V2 -> IO () `*/
const circleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), null))

/**` n_fillCircle : Number -> Number -> Number -> IO () `*/
const n_fillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.fill(), null))

/**` n_fillCircleV2 : Number -> V2 -> IO () `*/
const n_fillCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.fill(), null))

/**` fillCircle : Number -> Number -> Number -> IO () `*/
const fillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.fill(), null))

/**` fillCircleV2 : Number -> V2 -> IO () `*/
const fillCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.fill(), null))

/**` n_strokeCircle : Number -> Number -> Number -> IO () `*/
const n_strokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.stroke(), null))

/**` n_strokeCircleV2 : Number -> V2 -> IO () `*/
const n_strokeCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.stroke(), null))

/**` strokeCircle : Number -> Number -> Number -> IO () `*/
const strokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.stroke(), null))

/**` strokeCircleV2 : Number -> V2 -> IO () `*/
const strokeCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.stroke(), null))

/**` n_fillStrokeCircle : Number -> Number -> Number -> IO () `*/
const n_fillStrokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_fillStrokeCircleV2 : Number -> V2 -> IO () `*/
const n_fillStrokeCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeCircle : Number -> Number -> Number -> IO () `*/
const fillStrokeCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeCircleV2 : Number -> V2 -> IO () `*/
const fillStrokeCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillCircle : Number -> Number -> Number -> IO () `*/
const n_strokeFillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_strokeFillCircleV2 : Number -> V2 -> IO () `*/
const n_strokeFillCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height, r * Ψ.ctx.canvas.width, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillCircle : Number -> Number -> Number -> IO () `*/
const strokeFillCircle = (r : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.arc(x, y, r, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillCircleV2 : Number -> V2 -> IO () `*/
const strokeFillCircleV2 = (r : number) => (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.arc(v.x, v.y, r, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_elliptic : ...7 Number -> IO () `*/
const n_elliptic = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height, kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height, a * tau, a0 * tau, a1 * tau), null))

/**` n_ellipticV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const n_ellipticV2 = (a : number ) => (a0 : number ) => (a1 : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.width, kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau), null))

/**` elliptic : ...7 Number -> IO () `*/
const elliptic = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a1), null))

/**` ellipticV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const ellipticV2 = (a : number ) => (a0 : number ) => (a1 : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a1), null))

/**` n_strokeElliptic : ...7 Number -> IO () `*/
const n_strokeElliptic = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.width,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau
		), Ψ.ctx.stroke(), null
	))

/**` n_strokeEllipticV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const n_strokeEllipticV2 = (a : number ) => (a0 : number ) => (a1 : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.width,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau
		), Ψ.ctx.stroke(), null
	))

/**` strokeElliptic : ...7 Number -> IO () `*/
const strokeElliptic = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a1), Ψ.ctx.stroke(), null))

/**` strokeEllipticV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const strokeEllipticV2 = (a : number ) => (a0 : number ) => (a1 : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a1), Ψ.ctx.stroke(), null))

/**` n_ellipticSection : ...7 Number -> IO () `*/
const n_ellipticSection = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.width, kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width, a * tau, a0 * tau, a1 * tau), null))

/**` n_ellipticSectionV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const n_ellipticSectionV2 = (a : number) => (a0 : number) => (a1 : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.width,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.width,
			a * tau, a0 * tau, (a0 + a1) * tau
		), null
	))

/**` ellipticSection : ...7 Number -> IO () `*/
const ellipticSection = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a0 + a1), null))

/**` ellipticSectionV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const ellipticSectionV2 = (a : number ) => (a0 : number ) => (a1 : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a0 + a1), null))

/**` n_strokeEllipticSection : ...7 Number -> IO () `*/
const n_strokeEllipticSection = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.width,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.width,
			a  * tau, a0 * tau, (a0 + a1) * tau
		), Ψ.ctx.stroke(), null
	))

/**` n_strokeEllipticSectionV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const n_strokeEllipticSectionV2 = (a : number ) => (a0 : number ) => (a1 : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, a0 * tau, (a0 + a1) * tau
		), Ψ.ctx.stroke(), null
	))

/**` strokeEllipticSection : ...7 Number -> IO () `*/
const strokeEllipticSection = (a : number) => (a0 : number) => (a1 : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` strokeEllipticSectionV2 : Number -> Number -> Number -> V2 -> V2 -> IO () `*/
const strokeEllipticSectionV2 =
	(a   : number ) => (a0 : number ) => (a1 : number) =>
	(kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, a0, a0 + a1), Ψ.ctx.stroke(), null))

/**` n_ellipse : ...5 Number -> IO () `*/
const n_ellipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height, kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height, a * tau, 0, tau), null))

/**` n_ellipseV2 : Number -> V2 -> V2 -> IO () `*/
const n_ellipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height, kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height, a * tau, 0, tau), null))

/**` ellipse : ...5 Number -> IO () `*/
const ellipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), null))

/**` ellipseV2 : Number -> V2 -> V2 -> IO () `*/
const ellipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), null))

/**` n_fillEllipse : ...5 Number -> IO () `*/
const n_fillEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height, kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height, a * tau, 0, tau), Ψ.ctx.fill(), null))

/**` n_fillEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const n_fillEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.fill(), null
	))

/**` fillEllipse : ...5 Number -> IO () `*/
const fillEllipse = (a  : number) => (kx : number) => (ky : number) => (x  : number) => (y  : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.fill(), null))

/**` fillEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const fillEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.fill(), null))

/**` n_strokeEllipse : ...5 Number -> IO () `*/
const n_strokeEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), null
	))

/**` n_strokeEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const n_strokeEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), null
	))

/**` strokeEllipse : ...5 Number -> IO () `*/
const strokeEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.stroke(), null))

/**` strokeEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const strokeEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.stroke(), null))

/**` n_fillStrokeEllipse : ...5 Number -> IO () `*/
const n_fillStrokeEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` n_fillStrokeEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const n_fillStrokeEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.fill(), Ψ.ctx.stroke(), null
	))

/**` fillStrokeEllipse : ...5 Number -> IO () `*/
const fillStrokeEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` fillStrokeEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const fillStrokeEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.fill(), Ψ.ctx.stroke(), null))

/**` n_strokeFillEllipse : ...5 Number -> IO () `*/
const n_strokeFillEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			x  * Ψ.ctx.canvas.width, y  * Ψ.ctx.canvas.height,
			kx * Ψ.ctx.canvas.width, ky * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` n_strokeFillEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const n_strokeFillEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.ellipse(
			xy.x  * Ψ.ctx.canvas.width, xy.y  * Ψ.ctx.canvas.height,
			kxy.x * Ψ.ctx.canvas.width, kxy.y * Ψ.ctx.canvas.height,
			a * tau, 0, tau
		), Ψ.ctx.stroke(), Ψ.ctx.fill(), null
	))

/**` strokeFillEllipse : ...5 Number -> IO () `*/
const strokeFillEllipse = (a : number) => (kx : number) => (ky : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(x, y, kx, ky, a, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` strokeFillEllipseV2 : Number -> V2 -> V2 -> IO () `*/
const strokeFillEllipseV2 = (a : number) => (kxy : V2) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.ellipse(xy.x, xy.y, kxy.x, kxy.y, a, 0, tau), Ψ.ctx.stroke(), Ψ.ctx.fill(), null))

/**` n_fillText : String -> Number -> Number -> IO () `*/
const n_fillText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_fillTextV2 : String -> V2 -> IO () `*/
const n_fillTextV2 = (text : string) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null))

/**` fillText : String -> Number -> Number -> IO () `*/
const fillText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, x, y), null))

/**` fillTextV2 : String -> V2 -> IO () `*/
const fillTextV2 = (text : string) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.fillText(text, xy.x, xy.y), null))

/**` n_strokeText : String -> Number -> Number -> IO () `*/
const n_strokeText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null))

/**` n_strokeTextV2 : String -> V2 -> IO () `*/
const n_strokeTextV2 = (text : string) => (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null))

/**` strokeText : String -> Number -> Number -> IO () `*/
const strokeText = (text : string) => (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeText(text, x, y), null))

/**` strokeTextV2 : String -> V2 -> IO () `*/
const strokeTextV2 = (text : string) => (xy : V2) : IO <null> =>
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

/**` n_fillStrokeText : String -> V2 -> IO () `*/
const n_fillStrokeTextV2 = (text : string) => (xy : V2) : IO <null> =>
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

/**` fillStrokeText : String -> V2 -> IO () `*/
const fillStrokeTextV2 = (text : string) => (xy : V2) : IO <null> =>
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

/**` n_strokeFillTextV2 : String -> V2 -> IO () `*/
const n_strokeFillTextV2 = (text : string) => (xy : V2) : IO <null> =>
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

/**` strokeFillText : String -> V2 -> IO () `*/
const strokeFillTextV2 = (text : string) => (xy : V2) : IO <null> =>
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

/**` n_fillMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const n_fillMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
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

/**` fillMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const fillMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
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

/**` n_strokeMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const n_strokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
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

/**` strokeMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const strokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
	IO (() => (text.split('\n').forEach((line, i) => Ψ.ctx.strokeText(line, xy.x, xy.y + spacing * i)), null))

/**` n_strokeFillMultilineText : String -> Number -> Number -> Number -> IO () `*/
const n_strokeFillMultilineText = (text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
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

/**` n_strokeFillMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const n_strokeFillMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
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
const strokeFillMultilineText = (text : string) => (spacing : number) => (x : number) => (y : number) : IO <null> =>
	IO (() => {
		let lineY = y
		text.split('\n').forEach(line => {
			Ψ.ctx.strokeText(line, x, lineY)
			Ψ.ctx.fillText  (line, x, lineY)
			lineY += spacing
		})
		return null
	})

/**` strokeFillMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const strokeFillMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
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

/**` n_fillStrokeMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const n_fillStrokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
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

/**` fillStrokeMultilineTextV2 : String -> Number -> V2 -> IO () `*/
const fillStrokeMultilineTextV2 = (text : string) => (spacing : number) => (xy : V2) : IO <null> =>
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

/**` n_lineV2 : V2 -> V2 -> IO () `*/
const n_lineV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo(xy1.x * Ψ.ctx.canvas.width, xy1.y * Ψ.ctx.canvas.height),
		null
	))

/**` line : Number -> Number -> Number -> Number -> IO () `*/
const line = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x0, y0), Ψ.ctx.lineTo(x1, y1), null))

/**` lineV2 : V2 -> V2 -> IO () `*/
const lineV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(xy0.x, xy0.y), Ψ.ctx.lineTo(xy1.x, xy1.y), null))

/**` n_strokeLine : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeLine = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(x0 * Ψ.ctx.canvas.width, y0 * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo(x1 * Ψ.ctx.canvas.width, y1 * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` n_strokeLineV2 : V2 -> V2 -> IO () `*/
const n_strokeLineV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy0.x * Ψ.ctx.canvas.width, xy0.y * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo(xy1.x * Ψ.ctx.canvas.width, xy1.y * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` strokeLine : Number -> Number -> Number -> Number -> IO () `*/
const strokeLine = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x0, y0), Ψ.ctx.lineTo(x1, y1), Ψ.ctx.stroke(), null))

/**` strokeLineV2 : V2 -> V2 -> IO () `*/
const strokeLineV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(xy0.x, xy0.y), Ψ.ctx.lineTo(xy1.x, xy1.y), Ψ.ctx.stroke(), null))

/**` n_vectorLine : Number -> Number -> Number -> Number -> IO () `*/
const n_vectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(x        * Ψ.ctx.canvas.width, y        * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((x + dx) * Ψ.ctx.canvas.width, (y + dy) * Ψ.ctx.canvas.height),
		null
	))

/**` n_vectorLineV2 : V2 -> V2 -> IO () `*/
const n_vectorLineV2 = (xy : V2) => (dxy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy.x           * Ψ.ctx.canvas.width, xy.y           * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((xy.x + dxy.x) * Ψ.ctx.canvas.width, (xy.y + dxy.y) * Ψ.ctx.canvas.height),
		null
	))

/**` vectorLine : Number -> Number -> Number -> Number -> IO () `*/
const vectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x, y), Ψ.ctx.lineTo(x + dx, y + dy), null))

/**` vectorLineV2 : V2 -> V2 -> IO () `*/
const vectorLineV2 = (xy : V2) => (dxy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(xy.x, xy.y), Ψ.ctx.lineTo(xy.x + dxy.x, xy.y + dxy.y), null))

/**` n_strokeVectorLine : Number -> Number -> Number -> Number -> IO () `*/
const n_strokeVectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(x        * Ψ.ctx.canvas.width, y        * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((x + dx) * Ψ.ctx.canvas.width, (y + dy) * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` n_strokeVectorLineV2 : V2 -> V2 -> IO () `*/
const n_strokeVectorLineV2 = (xy : V2) => (dxy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.moveTo(xy.x           * Ψ.ctx.canvas.width, xy.y           * Ψ.ctx.canvas.height),
		Ψ.ctx.lineTo((xy.x + dxy.x) * Ψ.ctx.canvas.width, (xy.y + dxy.y) * Ψ.ctx.canvas.height),
		Ψ.ctx.stroke(), null
	))

/**` strokeVectorLine : Number -> Number -> Number -> Number -> IO () `*/
const strokeVectorLine = (x : number) => (y : number) => (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.moveTo(x, y), Ψ.ctx.lineTo(x + dx, y + dy), Ψ.ctx.stroke(), null))

/**` strokeVectorLineV2 : V2 -> V2 -> IO () `*/
const strokeVectorLineV2 = (xy : V2) => (dxy : V2) : IO <null> =>
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
			: __MACRO__.err_nonexisting_image_path('n_image', path)
	)

/**` n_imageV2 : String -> ...4 V2 -> IO () `*/
const n_imageV2 =
	(path : string ) =>
	(cxy  : V2) => (cwh : V2) =>
	(xy   : V2) => (wh  : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				cxy.x * Ψ.image[path].width, cxy.y * Ψ.image[path].height,
				cwh.x * Ψ.image[path].width, cwh.y * Ψ.image[path].height,
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null)
		: __MACRO__.err_nonexisting_image_path('n_imageV2', path)
	)

/**` image : String -> ...8 Number -> IO () `*/
const image =
	(path : string) =>
	(cx   : number) => (cy : number) => (cw : number) => (ch : number) =>
	(x    : number) => (y  : number) => (w  : number) => (h  : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], cx, cy, cw, ch, x, y, w, h), null)
			: __MACRO__.err_nonexisting_image_path('image', path)
	)

/**` imageV2 : String -> ...4 V2 -> IO () `*/
const imageV2 =
	(path : string ) =>
	(cxy  : V2) => (cwh : V2) =>
	(xy   : V2) => (wh  : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], cxy.x, cxy.y, cwh.x, cwh.y, xy.x, xy.y, wh.x, wh.y), null)
			: __MACRO__.err_nonexisting_image_path('imageV2', path)
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
			: __MACRO__.err_nonexisting_image_path('n_uncroppedImage', path)
	)

/**` n_uncroppedImageV2 : String -> V2 -> V2 -> IO () `*/
const n_uncroppedImageV2 = (path : string) => (xy : V2) => (wh : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path] ?
			(Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
			), null)
		: __MACRO__.err_nonexisting_image_path('n_uncroppedImageV2', path)
	)

/**` uncroppedImage : String -> ...4 Number -> IO () `*/
const uncroppedImage =
	(path : string) =>
	(x    : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y, w, h), null)
			: __MACRO__.err_nonexisting_image_path('uncroppedImage', path)
	)

/**` uncroppedImageV2 : String -> V2 -> V2 -> IO () `*/
const uncroppedImageV2 = (path : string) => (xy : V2) => (wh : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, wh.x, wh.y), null)
			: __MACRO__.err_nonexisting_image_path('uncroppedImageV2', path)
	)

/**` n_fullImage : String -> Number -> Number -> IO () `*/
const n_fullImage = (path : string) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height), null)
			: __MACRO__.err_nonexisting_image_path('n_fullImage', path)
	)

/**` n_fullImageV2 : String -> V2 -> IO () `*/
const n_fullImageV2 = (path : string) => (xy : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height), null)
			: __MACRO__.err_nonexisting_image_path('n_fullImageV2', path)
	)

/**` fullImage : String -> Number -> Number -> IO () `*/
const fullImage = (path : string) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y), null)
			: __MACRO__.err_nonexisting_image_path('fullImage', path)
	)

/**` fullImageV2 : String -> V2 -> IO () `*/
const fullImageV2 = (path : string) => (xy : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y), null)
			: __MACRO__.err_nonexisting_image_path('fullImageV2', path)
	)

/**` n_fullScaledImage : String -> Number -> Number -> Number -> IO () `*/
const n_fullScaledImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(
				Ψ.image[path],
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				Ψ.image[path].width * k * Ψ.ctx.canvas.width, Ψ.image[path].height * k * Ψ.ctx.canvas.height
			), null)
			: __MACRO__.err_nonexisting_image_path('n_fullScaledImage', path)
	)

/**` n_fullScaledImageV2 : String -> Number -> V2 -> IO () `*/
const n_fullScaledImageV2 = (path : string) => (k : number) => (xy : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				Ψ.image[path].width * k * Ψ.ctx.canvas.width, Ψ.image[path].height * k * Ψ.ctx.canvas.height
			), null)
			: __MACRO__.err_nonexisting_image_path('n_fullScaledImageV2', path)
	)

/**` fullScaledImage : String -> Number -> Number -> Number -> IO () `*/
const fullScaledImage = (path : string) => (k : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y, Ψ.image[path].width * k, Ψ.image[path].height * k), null)
			: __MACRO__.err_nonexisting_image_path('fullScaledImage', path)
	)

/**` fullScaledImageV2 : String -> Number -> V2 -> IO () `*/
const fullScaledImageV2 = (path : string) => (k : number) => (xy : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, Ψ.image[path].width * k, Ψ.image[path].height * k), null)
			: __MACRO__.err_nonexisting_image_path('fullScaledImageV2', path)
	)

/**` n_squareImage : String -> Number -> Number -> Number -> IO () `*/
const n_squareImage = (path : string) => (w : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(
				Ψ.image[path],
				x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
				w * Ψ.ctx.canvas.width, w * Ψ.ctx.canvas.width
			), null)
		: __MACRO__.err_nonexisting_image_path('n_squareImage', path)
	)

/**` n_squareImageV2 : String -> Number -> V2 -> IO () `*/
const n_squareImageV2 = (path : string) => (w : number) => (xy : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(
				Ψ.image[path],
				xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
				w    * Ψ.ctx.canvas.width, w    * Ψ.ctx.canvas.width
			), null)
		: __MACRO__.err_nonexisting_image_path('n_squareImageV2', path)
	)

/**` squareImage : String -> Number -> Number -> Number -> IO () `*/
const squareImage = (path : string) => (w : number) => (x : number) => (y : number) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], x, y, w, w), null)
			: __MACRO__.err_nonexisting_image_path('squareImage', path)
	)

/**` squareImageV2 : String -> Number -> V2 -> IO () `*/
const squareImageV2 = (path : string) => (w : number) => (xy : V2) : IO <null> =>
	IO (() =>
		Ψ.image[path]
			? (Ψ.ctx.drawImage(Ψ.image[path], xy.x, xy.y, w, w), null)
			: __MACRO__.err_nonexisting_image_path('squareImageV2', path)
	)

/**` n_clearRect : Number -> Number -> Number -> Number -> IO () `*/
const n_clearRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			x * Ψ.ctx.canvas.width, y * Ψ.ctx.canvas.height,
			w * Ψ.ctx.canvas.width, h * Ψ.ctx.canvas.height
		), null
	))

/**` n_clearRectV2 : V2 -> V2 -> IO () `*/
const n_clearRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			xy.x * Ψ.ctx.canvas.width, xy.y * Ψ.ctx.canvas.height,
			wh.x * Ψ.ctx.canvas.width, wh.y * Ψ.ctx.canvas.height
		), null
	))

/**` clearRect : Number -> Number -> Number -> Number -> IO () `*/
const clearRect = (x : number) => (y : number) => (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(x, y, w, h), null))

/**` clearRectV2 : V2 -> V2 -> IO () `*/
const clearRectV2 = (xy : V2) => (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(xy.x, xy.y, wh.x, wh.y), null))

/**` n_clearArea : Number -> Number -> Number -> Number -> IO () `*/
const n_clearArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			x0        * Ψ.ctx.canvas.width, y0        * Ψ.ctx.canvas.height,
			(x1 - x0) * Ψ.ctx.canvas.width, (y1 - y0) * Ψ.ctx.canvas.height
		), null
	))

/**` n_clearAreaV2 : V2 -> V2 -> IO () `*/
const n_clearAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.clearRect(
			xy0.x           * Ψ.ctx.canvas.width, xy0.y           * Ψ.ctx.canvas.height,
			(xy1.x - xy0.x) * Ψ.ctx.canvas.width, (xy1.y - xy0.y) * Ψ.ctx.canvas.height
		), null
	))

/**` clearArea : Number -> Number -> Number -> Number -> IO () `*/
const clearArea = (x0 : number) => (y0 : number) => (x1 : number) => (y1 : number) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(x0, y0, x1 - x0, y1 - y0), null))

/**` clearAreaV2 : V2 -> V2 -> IO () `*/
const clearAreaV2 = (xy0 : V2) => (xy1 : V2) : IO <null> =>
	IO (() => (Ψ.ctx.clearRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))
/**` clearLayer : IO () `*/
const clearLayer : IO <null> =
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

/**` n_rotateLayer : Number -> IO () `*/
const n_rotateLayer = (angle : number) : IO <null> =>
	IO (() => (Ψ.ctx.rotate(angle * tau), null))

/**` rotateLayer : Number -> IO () `*/
const rotateLayer = (angle : number) : IO <null> =>
	IO (() => (Ψ.ctx.rotate(angle), null))

/**` n_translateLayerX : Number -> IO () `*/
const n_translateLayerX = (dx : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx * Ψ.ctx.canvas.width, 0), null))

/**` n_translateLayerY : Number -> IO () `*/
const n_translateLayerY = (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(0, dy * Ψ.ctx.canvas.height), null))

/**` n_translateLayer : Number -> Number -> IO () `*/
const n_translateLayer = (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx * Ψ.ctx.canvas.width, dy * Ψ.ctx.canvas.height), null))

/**` n_translateLayerV2 : V2 -> IO () `*/
const n_translateLayerV2 = (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.translate(v.x * Ψ.ctx.canvas.width, v.y * Ψ.ctx.canvas.height), null))

/**` translateLayerX : Number -> IO () `*/
const translateLayerX = (dx : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx, 0), null))

/**` translateLayerY : Number -> IO () `*/
const translateLayerY = (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(0, dy), null))

/**` translateLayer : Number -> Number -> IO () `*/
const translateLayer = (dx : number) => (dy : number) : IO <null> =>
	IO (() => (Ψ.ctx.translate(dx, dy), null))

/**` translateLayerV2 : V2 -> IO () `*/
const translateLayerV2 = (v : V2) : IO <null> =>
	IO (() => (Ψ.ctx.translate(v.x, v.y), null))

/**` scaleLayer : Number -> IO () `*/
const scaleLayer = (k : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(k, k), null))

/**` scaleLayerAxisX : Number -> IO () `*/
const scaleLayerAxisX = (kx : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(kx, 1), null))

/**` scaleLayerAxisY : Number -> IO () `*/
const scaleLayerAxisY = (ky : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(1, ky), null))

/**` scaleLayerAxis : Number -> Number -> IO () `*/
const scaleLayerAxis = (kx : number) => (ky : number) : IO <null> =>
	IO (() => (Ψ.ctx.scale(kx, ky), null))

/**` scaleLayerAxisV2 : V2 -> IO () `*/
const scaleLayerAxisV2 = (kxy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.scale(kxy.x, kxy.y), null))

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

/**` n_setCanvasDimensionsV2 : V2 -> IO () `*/
const n_setCanvasDimensionsV2 = (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => (ctx.canvas.width = wh.x * innerWidth, ctx.canvas.height = wh.y * innerHeight)), null))

/**` setCanvasDimensions : Number -> Number -> IO () `*/
const setCanvasDimensions = (w : number) => (h : number) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => (ctx.canvas.width = w, ctx.canvas.height = h)), null))

/**` setCanvasDimensionsV2 : V2 -> IO () `*/
const setCanvasDimensionsV2 = (wh : V2) : IO <null> =>
	IO (() => (Ψ.ctxs.forEach(ctx => (ctx.canvas.width = wh.x, ctx.canvas.height = wh.y)), null))

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

/**` setShadowRGBAV4 : V4 -> IO () `*/
const setShadowRGBAV4 = (v : V4) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

/**` setShadowRGBA : Number -> Number -> Number -> Number -> IO () `*/
const setShadowRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = `rgba(${r},${g},${b},${a})`, null))

/**` n_setShadowRGBAV4 : V4 -> IO () `*/
const n_setShadowRGBAV4 = (v : V4) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w * 255})`, null))

/**` n_setShadowOffsetX : Number -> IO () `*/
const n_setShadowOffsetX = (x : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x * Ψ.ctx.canvas.width, null))

/**` setShadowOffsetX : Number -> IO () `*/
const setShadowOffsetX = (x : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x, null))

/**` n_setShadowOffsetY : Number -> IO () `*/
const n_setShadowOffsetY = (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetY = y * Ψ.ctx.canvas.height, null))

/**` setShadowOffsetY : Number -> IO () `*/
const setShadowOffsetY = (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetY = y, null))

/**` n_setShadowOffset : Number -> Number -> IO () `*/
const n_setShadowOffset = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x * Ψ.ctx.canvas.width, Ψ.ctx.shadowOffsetY = y * Ψ.ctx.canvas.height, null))

/**` setShadowOffset : Number -> Number -> IO () `*/
const setShadowOffset = (x : number) => (y : number) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = x, Ψ.ctx.shadowOffsetY = y, null))

/**` n_setShadowOffsetV2 : IO () `*/
const n_setShadowOffsetV2 = (xy : V2) : IO <null> =>
	IO (() => (
		Ψ.ctx.shadowOffsetX = xy.x * Ψ.ctx.canvas.width,
		Ψ.ctx.shadowOffsetY = xy.y * Ψ.ctx.canvas.height,
		null
	))

/**` setShadowOffsetV2 : IO () `*/
const setShadowOffsetV2 = (xy : V2) : IO <null> =>
	IO (() => (Ψ.ctx.shadowOffsetX = xy.x, Ψ.ctx.shadowOffsetY = xy.y, null))

/**` setShadowColor : String -> IO () `*/
const setShadowColor = (color : string) : IO <null> =>
	IO (() => (Ψ.ctx.shadowColor = color, null))

/**` n_setFillRGBA : Number -> Number -> Number -> Number -> IO () `*/
const n_setFillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

/**` n_setFillRGBAV4 : V4 -> IO () `*/
const n_setFillRGBAV4 = (v : V4) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

/**` setFillRGBA : Number -> Number -> Number -> Number -> IO () `*/
const setFillRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${r},${g},${b},${a})`, null))

/**` setFillRGBAV4 : V4 -> IO () `*/
const setFillRGBAV4 = (v : V4) : IO <null> =>
	IO (() => (Ψ.ctx.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null))

/**` n_setStrokeRGBA : Number -> Number -> Number -> Number -> IO () `*/
const n_setStrokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null))

/**` n_setStrokeRGBAV4 : V4 -> IO () `*/
const n_setStrokeRGBAV4 = (v : V4) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null))

/**` setStrokeRGBA : Number -> Number -> Number -> Number -> IO () `*/
const setStrokeRGBA = (r : number) => (g : number) => (b : number) => (a : number) : IO <null> =>
	IO (() => (Ψ.ctx.strokeStyle = `rgba(${r},${g},${b},${a})`, null))

/**` setStrokeRGBAV4 : V4 -> IO () `*/
const setStrokeRGBAV4 = (v : V4) : IO <null> =>
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
	IO (() => (Ψ.ctx.lineCap = linecap, null))

/**` setLineJoin : LineJoin -> IO () `*/
const setLineJoin = (linejoin : LineJoin) : IO <null> =>
	IO (() => (Ψ.ctx.lineJoin = linejoin, null))

/**` setTextAlign : TextAlign -> IO () `*/
const setTextAlign = (alignment : TextAlign) : IO <null> =>
	IO (() => (Ψ.ctx.textAlign = alignment, null))

/**` setTextBaseline : TextBaseline -> IO () `*/
const setTextBaseline = (baseline : TextBaseline) : IO <null> =>
	IO (() => (Ψ.ctx.textBaseline = baseline, null))

/**` setComposition : Composition -> IO () `*/
const setComposition = (composition : Composition) : IO <null> =>
	IO (() => (Ψ.ctx.globalCompositeOperation = composition, null))

/**` setToCenteredTextStyle : IO () `*/
const setToCenteredTextStyle : IO <null> =
	IO (() => (Ψ.ctx.textAlign = 'center', Ψ.ctx.textBaseline = 'middle', null))

/**` setToDefaultTextStyle : IO () `*/
const setToDefaultTextStyle : IO <null> =
	IO (() => (Ψ.ctx.textAlign = 'start', Ψ.ctx.textBaseline = 'alphabetic', null))

/**` setActiveLayer : Number -> IO () `*/
const setActiveLayer = (index : number) : IO <null> =>
	IO (() =>
		index >= 0 && index < Ψ.ctxs.length && Number.isInteger (index)
			? (Ψ.ctx = Ψ.ctxs[index]!, null)
			: __MACRO__.err_layer_index('setActiveLayer', index)
	)

/**` loadFont : String -> IO () `*/
const loadFont = (path : string) : IO <null> =>
	IO (() => (
		new FontFace(path.slice(path.lastIndexOf('/') + 1, path.lastIndexOf('.')), `url(${path})`)
			.load()
			.then((font : any) => (document as any).fonts.add(font))
			.catch(() => {
				console.error(`'loadFont' could not load font`)
				console.dir(`Signature : loadFont (<PATH>)`)
				console.dir(`<PATH> =`, path)
				return halt
			}),
		null
	))

/**` loadImage : String -> IO () `*/
const loadImage = (path : string) : IO <null> =>
	IO (() => {
		Ψ.image[path]          = new Image
		Ψ.image[path]!.src     = path
		Ψ.image[path]!.onerror = () => {
			console.error(`'loadImage' could not load image`)
			console.dir(`Signature : loadImage (<PATH>)`)
			console.dir(`<PATH> =`, path)
			return halt
		}
		return null
	})

/**` loadAudio : String -> IO () `*/
const loadAudio = (path : string) : IO <null> =>
	IO (() => {
		Ψ.audio[path]          = new Audio(path)
		Ψ.audio[path]!.onerror = () => {
			console.error(`'loadAudio' could not load audio`)
			console.dir(`Signature : loadAudio (<PATH>)`)
			console.dir(`<PATH> =`, path)
			return halt
		}
		return null
	})

/**` setAudioTime : String -> Number -> IO () `*/
const setAudioTime = (path : string) => (time : number) : IO <null> =>
	IO (() => {
		if (Ψ.audio[path])
			if (time >= 0 && time <= Ψ.audio[path].duration)
			{
				Ψ.audio[path].currentTime = time
				return null
			}
			else
			{
				console.error(`'setAudioTime' only takes numbers in interval [0, ${Ψ.audio[path].duration}] for given audio file`)
				console.dir(`Signature : setAudioTime (<PATH>) (<TIME>`)
				console.dir(`<PATH> =`, path)
				console.dir(`<TIME> =`, time)
				return halt
			}
		else
		{
			console.error(`'setAudioTime' received an unloaded (possibly non-existing) audio path`)
			console.dir(`Signature : setAudioTime (<PATH>) (<TIME>`)
			console.dir(`<PATH> =`, path)
			console.dir(`<TIME> =`, time)
			return halt
		}
	})

/**` resetAudio : String -> IO () `*/
const resetAudio = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? (Ψ.audio[path].pause(), Ψ.audio[path].currentTime = 0, null)
			: __MACRO__.err_nonexisting_audio_path('resetAudio', path)
	)

/**` playAudio : String -> IO () `*/
const playAudio = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? (Ψ.audio[path].play(), null)
			: __MACRO__.err_nonexisting_audio_path('playAudio', path)
	)

/**` pauseAudio : String -> IO () `*/
const pauseAudio = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? (Ψ.audio[path].pause(), null)
			: __MACRO__.err_nonexisting_audio_path('pauseAudio', path)
	)

/**` playSFX : String -> IO () `*/
const playSFX = (path : string) : IO <null> =>
	IO (() =>
		Ψ.audio[path]
			? ((Ψ.audio[path].cloneNode() as any).play(), null)
			: __MACRO__.err_nonexisting_audio_path('playSFX', path)
	)

/**` saveCanvasState : IO () `*/
const saveCanvasState : IO <null> =
	IO (() => (Ψ.ctx.save(), null))

/**` restoreCanvasState : IO () `*/
const restoreCanvasState : IO <null> =
	IO (() => (Ψ.ctx.restore(), null))

/**` requestPointerLock : IO () `*/
const requestPointerLock : IO <null> =
	IO (() => (document.onmouseup = () => Ψ.isPointerLocked || Ψ.ctxs[0].canvas.requestPointerLock(), null))

/**` deactivatePointerLock : IO () `*/
const deactivatePointerLock : IO <null> =
	IO (() => (document.exitPointerLock(), document.onmouseup = null))

/**` queueIO : IO a -> IO () `*/
const queueIO = <a>(io : IO <a>) : IO <null> =>
	IO (() => {
		for (const k in Ψ.keyboard)     Ψ.keyboard[k as KeyboardKey] = iterateButtonState (Ψ.keyboard[k as KeyboardKey])
		for (const i in Ψ.mouseButtons) Ψ.mouseButtons[i]            = iterateButtonState (Ψ.mouseButtons[i]!)
		Ψ.mouseDX     = Ψ.mouseDY = 0
		Ψ.mouseScroll = 'Zero'
		Ψ.isResized   = false
		requestAnimationFrame(io.effect)
		return null
	})

/**` lqueueIO : (() -> IO a) -> IO () `*/
const lqueueIO = <a>(lio : () => IO <a>) : IO <null> =>
	IO (() => queueIO (lio ()).effect ())

/**` setCanvasBackgroundColor : String -> IO () `*/
const setCanvasBackgroundColor = (color : string) : IO <null> =>
	IO (() => (Ψ.ctxs[0].canvas.style.background = color, null))

/**` setCanvasBackgroundRGBA : Number -> Number -> Number -> Number -> -> IO () `*/
const setCanvasBackgroundRGBA = (r : string) => (g : string) => (b : string) => (a : string) : IO <null> =>
	IO (() => (Ψ.ctxs[0].canvas.style.background = `rgba(${r},${g},${b},${a})`, null))

/**` setCanvasBackgroundRGBAV4 : V4 -> -> IO () `*/
const setCanvasBackgroundRGBAV4 = (rgba : V4) : IO <null> =>
	IO (() => (Ψ.ctxs[0].canvas.style.background = `rgba(${rgba.x},${rgba.y},${rgba.z},${rgba.w})`, null))

/**` setWindowBackgroundColor : String -> IO () `*/
const setWindowBackgroundColor = (color : string) : IO <null> =>
	IO (() => (document.body.style.background = color, null))

/**` setWindowBackgroundRGBA : Number -> Number -> Number -> Number -> -> IO () `*/
const setWindowBackgroundRGBA = (r : string) => (g : string) => (b : string) => (a : string) : IO <null> =>
	IO (() => (document.body.style.background = `rgba(${r},${g},${b},${a})`, null))

/**` setWindowBackgroundRGBAV4 : V4 -> -> IO () `*/
const setWindowBackgroundRGBAV4 = (rgba : V4) : IO <null> =>
	IO (() => (document.body.style.background = `rgba(${rgba.x},${rgba.y},${rgba.z},${rgba.w})`, null))

/********************************************************************************************************************************/
// Underhead //

/**` Do_IO : IO $ `*/
const Do_IO : IO <{}> = send (SCOPE)

/**` Do_Process : Process s $ `*/
const Do_Process : Process <unknown, {}> = Process (s => Pair (s, SCOPE))

/**` Do_Maybe : Maybe $ `*/
const Do_Maybe : Maybe <{}> = Just (SCOPE)

/**` Do_List : List $ `*/
const Do_List : List <{}> = singleton (SCOPE)

onload = () =>
{
	Ψ.ctxs = Array.from(document.querySelectorAll('canvas')).map(x => x.getContext('2d')!)
	Ψ.ctx  = Ψ.ctxs[0]!

	onresize    = () => (clearTimeout(Ψ.resizeID), Ψ.resizeID = setTimeout(() => Ψ.isResized = true, 250))
	onmouseup   = ev => Ψ.mouseButtons[ev.button]                             = 'toUp'
	onmousedown = ev => Ψ.mouseButtons[ev.button]                             = 'toDown'
	onkeyup     = ev => Ψ.keyboard[ev.code as KeyboardKey]                    = 'toUp'
	onkeydown   = ev => ev.repeat ? null : Ψ.keyboard[ev.code as KeyboardKey] = 'toDown'
	onwheel     = ev =>
		Ψ.mouseScroll =
			ev.deltaY < 0 ? 'Positive' :
			ev.deltaY > 0 ? 'Negative' : 'Zero'
	onmousemove = ev =>
	{
		Ψ.mouseWX = ev.x,
		Ψ.mouseWY = ev.y,
		Ψ.mouseCX = ev.clientX - Ψ.ctxs[0].canvas.offsetLeft,
		Ψ.mouseCY = ev.clientY - Ψ.ctxs[0].canvas.offsetTop,
		Ψ.mouseSX = ev.screenX,
		Ψ.mouseSY = ev.screenY,
		Ψ.mouseDX = ev.movementX,
		Ψ.mouseDY = ev.movementY
	}

	document.onpointerlockchange = () => Ψ.isPointerLocked = document.pointerLockElement === Ψ.ctxs[0].canvas
	Ψ.ctxs[0].canvas.setAttribute("style", "background:white")

	if (typeof (window as any).main !== 'undefined')
		if ((window as any).main.variation === 'IO')
			(window as any).main.effect ()
		else
			console.log ((window as any).main)
}
