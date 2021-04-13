"use strict";
const RESIZING_THRESHOLD = 1;
const RESIZING_SPEED = 0.1;
const REFRESH_TIME = 15;
const THROW = (message) => { throw new Error(message); };
const THROWTYPE = (message) => { throw new TypeError(message); };
const THROWRANGE = (message) => { throw new RangeError(message); };
const never = undefined;
const E = 2.718281828459045;
const LN2 = 0.6931471805599453;
const LN10 = 2.302585092994046;
const LOG2E = 1.4426950408889634;
const LOG10E = 0.4342944819032518;
const PI = 3.141592653589793;
const TAU = 6.283185307179586;
const INVSQRT2 = 0.7071067811865476;
const SQRT2 = 1.4142135623730951;
const abs = Math.abs;
const acos = Math.acos;
const acosh = Math.acosh;
const add = (x) => (y) => x + y;
const AND = (x) => (y) => x & y;
const and = (x) => (y) => x && y;
const apply = (x) => (f) => f(x);
const applyWhen = (condition) => (f) => condition ? f : id;
const approx = (x) => (y) => (error) => Math.abs(x - y) < error;
const napprox = (x) => (y) => (error) => Math.abs(x - y) > error;
const asin = Math.asin;
const asinh = Math.asinh;
const atan = Math.atan;
const atan2 = (y) => (x) => Math.atan2(y, x);
const ratan2 = (x) => (y) => Math.atan2(y, x);
const atanh = Math.atanh;
const BIT = (x) => x ? 1 : 0;
const cbrt = Math.cbrt;
const ceil = Math.ceil;
const CLZ32 = Math.clz32;
const cos = Math.cos;
const cosh = Math.cosh;
const diff = (x) => (y) => Math.abs(x - y);
const div = (x) => (y) => x / y;
const rdiv = (y) => (x) => x / y;
const eq = (x) => (y) => x === y || x.eq(y);
const even = (x) => x % 2 === 0;
const exp = Math.exp;
const expm1 = Math.expm1;
const floor = Math.floor;
const fround = Math.fround;
const greater = (x) => (y) => y > x;
const greaterEqual = (x) => (y) => y >= x;
const gt = (x) => (y) => x > y;
const gte = (x) => (y) => x >= y;
const id = (x) => x;
const isInsideExclusive = (n) => (lower) => (upper) => lower < n && n < upper;
const isInsideInclusive = (n) => (lower) => (upper) => lower <= n && n <= upper;
const isOutsideExclusive = (n) => (lower) => (upper) => n < lower || upper < n;
const isOutsideInclusive = (n) => (lower) => (upper) => n <= lower && upper <= n;
const ln = Math.log;
const log10 = Math.log10;
const lnp1 = Math.log1p;
const log2 = Math.log2;
const LSHIFT = (x) => (y) => x << y;
const rLSHIFT = (y) => (x) => x << y;
const lerp = (t) => (x) => (y) => x + (y - x) * t;
const less = (x) => (y) => y < x;
const lessEqual = (x) => (y) => y <= x;
const lt = (x) => (y) => x < y;
const lte = (x) => (y) => x <= y;
const max = (x) => (y) => Math.max(x, y);
const min = (x) => (y) => Math.min(x, y);
const mod = (x) => (y) => x % y;
const rmod = (y) => (x) => x % y;
const mul = (x) => (y) => x * y;
const NAND = (x) => (y) => ~(x & y);
const nand = (x) => (y) => !(x && y);
const negate = (x) => -x;
const neq = (x) => (y) => x !== y;
const NOR = (x) => (y) => ~(x | y);
const NOT = (x) => ~x;
const not = (x) => !x;
const odd = (x) => Math.abs(x) % 2 === 1;
const OR = (x) => (y) => x | y;
const or = (x) => (y) => x || y;
const nor = (x) => (y) => !(x || y);
const pow = (x) => (y) => Math.pow(x, y);
const rpow = (y) => (x) => Math.pow(x, y);
const pythagoras = (x) => (y) => Math.sqrt(x * x + y * y);
const round = Math.round;
const RSHIFT = (x) => (y) => x >> y;
const rRSHIFT = (y) => (x) => x >> y;
const sign = Math.sign;
const sin = Math.sin;
const sinh = Math.sinh;
const sqrt = Math.sqrt;
const sub = (x) => (y) => x - y;
const rsub = (y) => (x) => x - y;
const tan = Math.tan;
const tanh = Math.tanh;
const toHexColor = (decimal) => decimal >= 0 && decimal <= 16777215 && Number.isInteger(decimal)
    ? `#${decimal.toString(16).padStart(6, '0')}`
    : THROWTYPE(`'toHexColor' requires a non-negative integer below '0xffffff' ('16777215'); received '${decimal}' instead`);
const trunc = Math.trunc;
const qtrunc = (x) => ~~x;
const URSHIFT = (x) => (y) => x >>> y;
const rURSHIFT = (y) => (x) => x >>> y;
const XOR = (x) => (y) => x ^ y;
const xor = (x) => (y) => x !== y;
Boolean.prototype.eq = function (x) { return this === x; };
Boolean.prototype.pipe = function (f) { return f(this); };
Number.prototype.eq = function (x) { return this === x; };
Number.prototype.pipe = function (f) { return f(this); };
String.prototype.eq = function (x) { return this === x; };
String.prototype.pipe = function (f) { return f(this); };
const Pair = (first, second) => ({
    CONS: 'Pair',
    eq: x => x.fst.eq(first) && x.snd.eq(second),
    get pipe() { return (f) => f(this); },
    fst: first,
    snd: second
});
const fst = (pair) => pair.fst;
const snd = (pair) => pair.snd;
const fpair = (f) => (firsts) => (seconds) => Pair(f(firsts.fst)(seconds.fst), f(firsts.snd)(seconds.snd));
const pick = (condition) => condition ? fst : snd;
const both = (f) => (pair) => Pair(f(pair.fst), f(pair.snd));
const uncurry = (f) => (pair) => f(pair.fst)(pair.snd);
const ffst = (f) => (pair) => Pair(f(pair.fst), pair.snd);
const fsnd = (f) => (pair) => Pair(pair.fst, f(pair.snd));
const IO = (sideeffect) => ({
    CONS: 'IO',
    INFO: sideeffect,
    get pipe() { return (f) => f(this); },
    then: x => IO(() => (sideeffect(), x.INFO())),
    side: x => IO(() => {
        const y = sideeffect();
        return x.INFO(), y;
    }),
    also: x => IO(() => {
        const y = sideeffect();
        return x(y).INFO(), y;
    }),
    bind: f => IO(() => f(sideeffect()).INFO()),
    bindto: x => f => IO(() => {
        const $ = sideeffect();
        return Object.assign(Object.assign({}, $), { [x]: f($).INFO() });
    }),
    fmap: f => IO(() => f(sideeffect())),
    fmapto: x => f => IO(() => {
        const $ = sideeffect();
        return Object.assign(Object.assign({}, $), { [x]: f($) });
    }),
    cast: x => IO(() => (sideeffect(), x))
});
const idle = IO(() => null);
const when = (condition) => (io) => condition ? io.fmap(_ => null) : idle;
const sequenceIOs = (ios) => IO(() => ios.fmap(io => io.INFO()));
const executeIOs = (ios) => IO(() => {
    for (let i = ios; i.CONS === 'Cons'; i = i.INFO.tail)
        i.INFO.head.INFO();
    return null;
});
const Nothing = {
    CONS: 'Nothing',
    eq: x => x === Nothing,
    pipe: f => f(Nothing),
    bind: _ => Nothing,
    bindto: _ => _ => Nothing,
    fmap: _ => Nothing,
    fmapto: _ => _ => Nothing,
    cast: _ => Nothing
};
const Just = (value) => ({
    CONS: 'Just',
    INFO: value,
    eq: x => x.CONS === 'Just' && x.INFO.eq(value),
    get pipe() { return (f) => f(this); },
    bind: f => {
        const x = f(value);
        return x.CONS === 'Nothing' ? Nothing : x;
    },
    bindto: x => f => {
        const y = f(value);
        return y.CONS === 'Nothing' ? Nothing : Just(Object.assign(Object.assign({}, value), { [x]: y.INFO }));
    },
    fmap: f => Just(f(value)),
    fmapto: x => f => Just(Object.assign(Object.assign({}, value), { [x]: f(value) })),
    cast: x => Just(x)
});
const State = (statefulComputation) => ({
    CONS: 'State',
    INFO: statefulComputation,
    get pipe() { return (f) => f(this); },
    then: s => State(x => s.INFO(statefulComputation(x).fst)),
    side: s => State(x => {
        const y = statefulComputation(x);
        return Pair(s.INFO(y.fst).fst, y.snd);
    }),
    also: f => State(x => {
        const y = statefulComputation(x);
        return Pair(f(y.snd).INFO(y.fst).fst, y.snd);
    }),
    bind: f => State(x => {
        const { fst: y, snd: z } = statefulComputation(x);
        return f(z).INFO(y);
    }),
    bindto: k => f => State(x => {
        const { fst: y, snd: $ } = statefulComputation(x);
        const { fst: z, snd: w } = f($).INFO(y);
        return Pair(z, Object.assign(Object.assign({}, $), { [k]: w }));
    }),
    fmap: f => State(x => {
        const { fst: y, snd: z } = statefulComputation(x);
        return Pair(y, f(z));
    }),
    fmapto: k => f => State(x => {
        const { fst: y, snd: $ } = statefulComputation(x);
        return Pair(y, Object.assign(Object.assign({}, $), { [k]: f($) }));
    }),
    cast: x => State(y => Pair(statefulComputation(y).fst, x))
});
const pseudoRandom = State(seed => Pair((-67 * seed * seed * seed + 23 * seed * seed - 91 * seed + 73) % 65536, Math.abs(97 * seed * seed * seed + 91 * seed * seed - 83 * seed + 79) % 65536 / 65536));
const Nil = (() => {
    const self = {
        CONS: 'Nil',
        INFO: {
            CACHE: {
                len: 0,
                reverse: undefined
            },
            len: 0,
            reverse: undefined,
            get head() { return THROWRANGE(`'head' cannot be used on 'Nil' (an empty 'List')`); },
            get tail() { return THROWRANGE(`'tail' cannot be used on 'Nil' (an empty 'List')`); },
            get last() { return THROWRANGE(`'last' cannot be used on 'Nil' (an empty 'List')`); },
            get init() { return THROWRANGE(`'init' cannot be used on 'Nil' (an empty 'List')`); }
        },
        eq: xs => xs === Nil,
        pipe: f => f(Nil),
        plus: id,
        bind: _ => Nil,
        bindto: _ => _ => Nil,
        fmap: _ => Nil,
        fmapto: _ => _ => Nil,
        cast: _ => Nil
    };
    return self.INFO.reverse = self.INFO.CACHE.reverse = self;
})();
const Cons = (lazyFirst) => (lazyRest) => {
    const self = {
        CONS: 'Cons',
        INFO: {
            CACHE: {},
            get head() { var _a; var _b; return (_a = (_b = this.CACHE).head) !== null && _a !== void 0 ? _a : (_b.head = lazyFirst()); },
            get tail() { var _a; var _b; return (_a = (_b = this.CACHE).tail) !== null && _a !== void 0 ? _a : (_b.tail = lazyRest()); },
            get last() {
                var _a;
                var _b;
                return (_a = (_b = this.CACHE).last) !== null && _a !== void 0 ? _a : (_b.last = (() => {
                    if (this.CACHE.reverse)
                        return this.CACHE.reverse.INFO.head;
                    let xs = self;
                    if (this.CACHE.len)
                        while (xs.INFO.tail.CONS === 'Cons')
                            xs = xs.INFO.tail;
                    else {
                        let i = 1;
                        while (xs.INFO.tail.CONS === 'Cons')
                            xs = xs.INFO.tail, ++i;
                        this.CACHE.len = i;
                    }
                    return xs.INFO.head;
                })());
            },
            get init() {
                var _a;
                var _b;
                return (_a = (_b = this.CACHE).init) !== null && _a !== void 0 ? _a : (_b.init = this.tail.CONS === 'Cons'
                    ? Cons(() => this.head)(() => this.tail.INFO.tail.CONS === 'Cons' ? this.tail.INFO.init : Nil)
                    : Nil);
            },
            get len() {
                var _a;
                var _b;
                return (_a = (_b = this.CACHE).len) !== null && _a !== void 0 ? _a : (_b.len = (() => {
                    let i = 1, xs = this.tail;
                    while (xs.CONS === 'Cons')
                        ++i, xs = xs.INFO.tail;
                    return i;
                })());
            },
            get reverse() {
                var _a;
                var _b;
                return (_a = (_b = this.CACHE).reverse) !== null && _a !== void 0 ? _a : (_b.reverse = (() => {
                    let xs = singleton(this.head), ys = this.tail;
                    if (this.CACHE.len)
                        while (ys.CONS === 'Cons')
                            xs = prepend(ys.INFO.head)(xs), ys = ys.INFO.tail;
                    else {
                        let i = 1;
                        while (ys.CONS === 'Cons')
                            xs = prepend(ys.INFO.head)(xs), ys = ys.INFO.tail, ++i;
                        this.CACHE.len = xs.INFO.CACHE.len = i;
                    }
                    xs.INFO.CACHE.last = this.head;
                    xs.INFO.CACHE.reverse = self;
                    return xs;
                })());
            }
        },
        eq: xs => {
            if (self === xs)
                return true;
            let ys = self, i = 0;
            while (xs.CONS === 'Cons' && ys.CONS === 'Cons') {
                if (i >= 256)
                    THROWRANGE(`(.eq) checked the max amount of elements ('256') in a possible 'List'`);
                if (!xs.INFO.head.eq(ys.INFO.head))
                    return false;
                xs = xs.INFO.tail, ys = ys.INFO.tail, ++i;
            }
            return xs.CONS === ys.CONS;
        },
        pipe: f => f(self),
        plus: xs => xs.CONS === 'Cons' ? Cons(() => self.INFO.head)(() => self.INFO.tail.plus(xs)) : self,
        bind: f => concat(self.fmap(f)),
        bindto: k => f => self.bind($ => f($).fmap(x => (Object.assign(Object.assign({}, $), { [k]: x })))),
        fmap: f => Cons(() => f(self.INFO.head))(() => self.INFO.tail.fmap(f)),
        fmapto: k => f => self.fmap($ => (Object.assign(Object.assign({}, $), { [k]: f($) }))),
        cast: x => {
            if (self.INFO.CACHE.len)
                return replicate(self.INFO.CACHE.len)(x);
            const xs = Cons(() => x)(() => self.INFO.tail.cast(x));
            xs.INFO.CACHE.head = xs.INFO.CACHE.last = x;
            return xs.INFO.CACHE.reverse = xs;
        }
    };
    return self;
};
const List = (...xs) => {
    let ys = Nil;
    for (let i = xs.length - 1; ~i; --i)
        ys = prepend(xs[i])(ys);
    ys.INFO.CACHE.len = xs.length;
    return ys;
};
const all = (predicate) => (xs) => !any((x) => !predicate(x))(xs);
const any = (predicate) => (xs) => {
    while (xs.CONS === 'Cons')
        if (predicate(xs.INFO.head))
            return true;
        else
            xs = xs.INFO.tail;
    return false;
};
const append = (element) => (xs) => xs.CONS === 'Nil' ? singleton(element) :
    Cons(() => xs.INFO.head)(() => append(element)(xs.INFO.tail));
const array = (xs) => {
    const ys = [];
    for (let i = 256; i && xs.CONS === 'Cons'; ys.push(xs.INFO.head), xs = xs.INFO.tail)
        if (!--i)
            console.warn(`'array' reached the max lengthed array of 'List' ('256') which could suggest infinity.`);
    return ys;
};
const at = (index) => (xs) => {
    if (index < 0)
        THROWRANGE(`'at' only accepts non-negatives as an index; received '${index}' as an input`);
    if (xs.INFO.CACHE.len <= index)
        THROWRANGE(`'at' cannot get element at index '${index}' in 'List' of length '${xs.INFO.len}'`);
    let i = index;
    while (xs.CONS === 'Cons')
        if (--i < 0)
            return xs.INFO.head;
        else
            xs = xs.INFO.tail;
    return THROWRANGE(`'at' cannot get element at index '${index}' in 'List' `);
};
const concat = (xxs) => xxs.CONS === 'Nil' ? Nil :
    xxs.INFO.head.CONS === 'Nil' ? concat(xxs.INFO.tail) :
        Cons(() => xxs.INFO.head.INFO.head)(() => xxs.INFO.head.INFO.tail.plus(concat(xxs.INFO.tail)));
const countBy = (delta) => (start) => Cons(() => start)(() => countBy(delta)(start + delta));
const countDownFrom = (start) => Cons(() => start)(() => countDownFrom(start - 1));
const countUpFrom = (start) => Cons(() => start)(() => countUpFrom(start + 1));
const cycle = (xs) => {
    const self = {
        CONS: 'Cons',
        INFO: {
            CACHE: {},
            head: xs.INFO.head,
            tail: undefined,
            init: undefined,
            get last() { return THROWRANGE(`'last' cannot be used on infinite 'List's from 'cycle'`); },
            get len() { return THROWRANGE(`'len' cannot be used on infinite 'List's from 'cycle'`); },
            get reverse() { return THROWRANGE(`'reverse' cannot be used on infinite 'List's from 'cycle'`); }
        },
        eq: ys => {
            if (self === ys)
                return true;
            let zs = self, i = 0;
            while (ys.CONS === 'Cons' && zs.CONS === 'Cons') {
                if (i >= 256)
                    THROWRANGE(`(.eq) checked the max amount of elements ('256') in a infinite 'List' from 'cycle'`);
                if (!ys.INFO.head.eq(zs.INFO.head))
                    return false;
                ys = ys.INFO.tail, zs = zs.INFO.tail, ++i;
            }
            return ys.CONS === zs.CONS;
        },
        pipe: f => f(self),
        plus: _ => self,
        bind: f => cycle(xs.bind(f)),
        bindto: _ => THROWTYPE(`'.bindto' should only be used on monads coming from 'Do', not 'repeat'`),
        fmap: f => cycle(xs.fmap(f)),
        fmapto: _ => THROWTYPE(`'.fmapto' should only be used on monads coming from 'Do', not 'repeat'`),
        cast: repeat
    };
    self.INFO.tail = xs.INFO.tail.plus(self);
    self.INFO.init = self;
    return self;
};
const drop = (amount) => (xs) => {
    if (xs.INFO.CACHE.len <= amount)
        return Nil;
    while (xs.CONS === 'Cons' && amount >= 1)
        xs = xs.INFO.tail, --amount;
    return xs;
};
const dropWhile = (predicate) => (xs) => {
    while (xs.CONS === 'Cons')
        if (predicate(xs.INFO.head))
            xs = xs.INFO.tail;
        else
            break;
    return xs;
};
const elem = (value) => (xs) => {
    while (xs.CONS === 'Cons')
        if (xs.INFO.head.eq(value))
            return true;
        else
            xs = xs.INFO.tail;
    return false;
};
const elemIndex = (value) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.INFO.tail)
        if (xs.INFO.head.eq(value))
            return Just(i);
    return Nothing;
};
const elemIndices = (value) => (xs) => xs.CONS === 'Nil' ? Nil :
    xs.INFO.head.eq(value)
        ? Cons(() => 0)(() => elemIndices(value)(xs.INFO.tail).fmap(x => x + 1))
        : elemIndices(value)(xs.INFO.tail).fmap(x => x + 1);
const filter = (predicate) => (xs) => xs.CONS === 'Nil' ? Nil :
    predicate(xs.INFO.head)
        ? Cons(() => xs.INFO.head)(() => filter(predicate)(xs.INFO.tail))
        : filter(predicate)(xs.INFO.tail);
const findIndex = (predicate) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.INFO.tail)
        if (predicate(xs.INFO.head))
            return Just(i);
    return Nothing;
};
const findIndices = (predicate) => (xs) => xs.CONS === 'Nil' ? Nil :
    predicate(xs.INFO.head)
        ? Cons(() => 0)(() => findIndices(predicate)(xs.INFO.tail).fmap(x => x + 1))
        : findIndices(predicate)(xs.INFO.tail).fmap(x => x + 1);
const foldl = (operation) => (initial) => (xs) => {
    if (xs.INFO.CACHE.len)
        while (xs.CONS === 'Cons')
            initial = operation(initial)(xs.INFO.head), xs = xs.INFO.tail;
    else {
        let i = 0, ys = xs;
        while (ys.CONS === 'Cons')
            initial = operation(initial)(ys.INFO.head), ys = ys.INFO.tail, ++i;
        xs.INFO.CACHE.len = i;
    }
    return initial;
};
const foldl1 = (operation) => (xs) => xs.CONS === 'Nil' ? THROWRANGE(`'foldl1' cannot be used on 'Nil' (an empty 'List')`) :
    foldl(operation)(xs.INFO.head)(xs.INFO.tail);
const foldr = (operation) => (initial) => (xs) => foldl(x => y => operation(y)(x))(initial)(xs.INFO.reverse);
const foldr1 = (operation) => (xs) => xs.CONS === 'Nil' ? THROWRANGE(`'foldr1' cannot be used on 'Nil' (an empty 'List')`) :
    foldl(x => y => operation(y)(x))(xs.INFO.reverse.INFO.head)(xs.INFO.reverse.INFO.tail);
const head = (xs) => xs.INFO.head;
const init = (xs) => xs.INFO.init;
const inits = (xs) => xs.CONS === 'Nil' ? singleton(Nil) :
    Cons(() => Nil)(() => inits(xs.INFO.tail).fmap(prepend(xs.INFO.head)));
const isEmpty = (xs) => xs.CONS === 'Nil';
const iterate = (endomorphism) => (initial) => Cons(() => initial)(() => iterate(endomorphism)(endomorphism(initial)));
const intersperse = (delimiter) => (xs) => xs.bind(x => List(delimiter, x)).INFO.tail;
const last = (xs) => xs.INFO.last;
const len = (xs) => xs.INFO.len;
const map = (morphism) => (xs) => xs.fmap(morphism);
const partition = (predicate) => (xs) => Pair(filter(predicate)(xs), filter((x) => !predicate(x))(xs));
const prepend = (element) => (xs) => xs.CONS === 'Nil' ? singleton(element) :
    (() => {
        const self = {
            CONS: 'Cons',
            INFO: {
                CACHE: {
                    head: element,
                    tail: xs
                },
                head: element,
                tail: xs,
                get len() { var _a; var _b; return (_a = (_b = this.CACHE).len) !== null && _a !== void 0 ? _a : (_b.len = xs.INFO.len + 1); },
                get last() { var _a; var _b; return (_a = (_b = this.CACHE).last) !== null && _a !== void 0 ? _a : (_b.last = xs.CONS === 'Cons' ? xs.INFO.last : element); },
                get init() { var _a; var _b; return (_a = (_b = this.CACHE).init) !== null && _a !== void 0 ? _a : (_b.init = xs.CONS === 'Cons' ? xs.INFO.init : Nil); },
                get reverse() {
                    var _a;
                    var _b;
                    return (_a = (_b = this.CACHE).reverse) !== null && _a !== void 0 ? _a : (_b.reverse = xs.CONS === 'Cons'
                        ? append(element)(xs.INFO.reverse)
                        : self);
                }
            },
            eq: ys => {
                if (self === ys)
                    return true;
                let zs = self;
                while (ys.CONS === 'Cons' && zs.CONS === 'Cons') {
                    if (!ys.INFO.head.eq(zs.INFO.head))
                        return false;
                    ys = ys.INFO.tail, zs = zs.INFO.tail;
                }
                return true;
            },
            pipe: f => f(self),
            plus: ys => ys.CONS === 'Cons' ? Cons(() => self.INFO.head)(() => self.INFO.tail.plus(ys)) : self,
            bind: f => concat(self.fmap(f)),
            bindto: k => f => self.bind($ => f($).fmap(x => (Object.assign(Object.assign({}, $), { [k]: x })))),
            fmap: f => Cons(() => f(self.INFO.head))(() => self.INFO.tail.fmap(f)),
            fmapto: k => f => self.fmap($ => (Object.assign(Object.assign({}, $), { [k]: f($) }))),
            cast: x => {
                if (self.INFO.CACHE.len)
                    return replicate(self.INFO.CACHE.len)(x);
                const ys = Cons(() => x)(() => xs.cast(x));
                ys.INFO.CACHE.head = ys.INFO.CACHE.last = x;
                return ys.INFO.CACHE.reverse = ys;
            }
        };
        return self;
    })();
const repeat = (value) => {
    const self = {
        CONS: 'Cons',
        INFO: {
            CACHE: {
                head: value
            },
            head: value,
            tail: undefined,
            init: undefined,
            get last() { return THROWRANGE(`'last' cannot be used on infinite 'List's from 'repeat'`); },
            get len() { return THROWRANGE(`'len' cannot be used on infinite 'List's from 'repeat'`); },
            get reverse() { return THROWRANGE(`'reverse' cannot be used on infinite 'List's from 'repeat'`); }
        },
        eq: xs => {
            if (self === xs)
                return true;
            let ys = self, i = 0;
            while (xs.CONS === 'Cons' && ys.CONS === 'Cons') {
                if (i >= 256)
                    THROWRANGE(`(.eq) checked the max amount of elements ('256') in a infinite 'List' from 'repeat'`);
                if (!xs.INFO.head.eq(ys.INFO.head))
                    return false;
                xs = xs.INFO.tail, ys = ys.INFO.tail, ++i;
            }
            return xs.CONS === ys.CONS;
        },
        pipe: f => f(self),
        plus: _ => self,
        bind: f => concat(repeat(f(value))),
        bindto: _ => THROWTYPE(`'.bindto' should only be used on monads coming from 'Do', not 'repeat'`),
        fmap: f => repeat(f(value)),
        fmapto: _ => THROWTYPE(`'.fmapto' should only be used on monads coming from 'Do', not 'repeat'`),
        cast: repeat
    };
    return self.INFO.tail = self.INFO.init = self;
};
const replicate = (amount) => (value) => amount < 1 ? Nil :
    Cons(() => value)(() => replicate(amount - 1)(value));
const reverse = (xs) => xs.INFO.reverse;
const scanl = (operation) => (initial) => (xs) => xs.CONS === 'Nil' ? singleton(initial) :
    Cons(() => initial)(() => scanl(operation)(operation(initial)(xs.INFO.head))(xs.INFO.tail));
const scanl1 = (operation) => (xs) => xs.CONS === 'Nil' ? Nil :
    scanl(operation)(xs.INFO.head)(xs.INFO.tail);
const scanr = (operation) => (initial) => (xs) => {
    xs = xs.INFO.reverse;
    let ys = singleton(initial);
    if (xs.INFO.CACHE.len)
        while (xs.CONS === 'Cons')
            ys = prepend(operation(xs.INFO.head)(ys.INFO.head))(ys), xs = xs.INFO.tail;
    else {
        let i = 1;
        while (xs.CONS === 'Cons')
            ys = prepend(operation(xs.INFO.head)(ys.INFO.head))(ys), xs = xs.INFO.tail, ++i;
        ys.INFO.CACHE.len = i;
    }
    return ys;
};
const scanr1 = (operation) => (xs) => {
    if (xs.CONS === 'Nil')
        return Nil;
    xs = xs.INFO.reverse;
    let ys = singleton(xs.INFO.head);
    xs = xs.INFO.tail;
    while (xs.CONS === 'Cons')
        ys = prepend(operation(xs.INFO.head)(ys.INFO.head))(ys), xs = xs.INFO.tail;
    return ys;
};
const singleton = (value) => {
    const self = {
        CONS: 'Cons',
        INFO: {
            CACHE: {
                head: value,
                tail: Nil,
                last: value,
                init: Nil,
                len: 1,
                reverse: undefined
            },
            head: value,
            tail: Nil,
            last: value,
            init: Nil,
            len: 1,
            reverse: undefined
        },
        eq: xs => {
            if (self === xs)
                return true;
            if (xs.CONS === 'Nil' || xs.INFO.tail.CONS === 'Cons')
                return false;
            return xs.INFO.head.eq(value);
        },
        pipe: f => f(self),
        plus: xs => xs.CONS === 'Cons' ? prepend(value)(xs) : self,
        bind: f => f(value),
        bindto: _ => THROWTYPE(`'.bindto' should be used in monads coming from 'Do', not 'singleton'`),
        fmap: f => singleton(f(value)),
        fmapto: _ => THROWTYPE(`'.fmapto' should be used in monads coming from 'Do', not 'singleton'`),
        cast: singleton
    };
    self.INFO.reverse = self.INFO.CACHE.reverse = self;
    return self;
};
const span = (predicate) => (xs) => Pair(takeWhile(predicate)(xs), dropWhile(predicate)(xs));
const splitAt = (index) => (xs) => Pair(take(index)(xs), drop(index)(xs));
const string = (str) => str
    ? (() => {
        const self = {
            CONS: 'Cons',
            INFO: {
                CACHE: {
                    head: str[0],
                    last: str.slice(-1),
                    len: str.length
                },
                head: str[0],
                last: str.slice(-1),
                len: str.length,
                get tail() { var _a; var _b; return (_a = (_b = this.CACHE).tail) !== null && _a !== void 0 ? _a : (_b.tail = string(str.slice(1))); },
                get init() { var _a; var _b; return (_a = (_b = this.CACHE).init) !== null && _a !== void 0 ? _a : (_b.init = string(str.slice(0, str.length - 1))); },
                get reverse() { var _a; var _b; return (_a = (_b = this.CACHE).reverse) !== null && _a !== void 0 ? _a : (_b.reverse = string(str.split("").reverse().join(""))); }
            },
            eq: xs => {
                if (self === xs)
                    return true;
                let ys = self, i = 0;
                while (xs.CONS === 'Cons' && ys.CONS === 'Cons') {
                    if (i >= 256)
                        THROWRANGE(`(.eq) checked the max amount of characters ('256') in a possible infinite 'List'`);
                    if (!xs.INFO.head.eq(ys.INFO.head))
                        return false;
                    xs = xs.INFO.tail, ys = ys.INFO.tail, ++i;
                }
                return xs.CONS === ys.CONS;
            },
            get pipe() { return (f) => f(this); },
            plus(xs) { return xs.CONS === 'Cons' ? Cons(() => this.INFO.head)(() => this.INFO.tail.plus(xs)) : Nil; },
            bind(f) { return concat(this.fmap(f)); },
            fmap(f) { return List(...str.split("").map(x => f(x))); },
            bindto: _ => THROWTYPE(`'.bindto' should be used in monads coming from 'Do', not 'string'`),
            fmapto: _ => THROWTYPE(`'.fmapto' should be used in monads coming from 'Do', not 'string'`),
            cast: x => replicate(str.length)(x)
        };
        return self;
    })()
    : Nil;
const tail = (xs) => xs.INFO.tail;
const tails = (xs) => xs.CONS === 'Nil' ? singleton(Nil) :
    Cons(() => xs)(() => tails(xs.INFO.tail));
const take = (amount) => (xs) => amount < 1 ? Nil :
    xs.INFO.CACHE.len <= amount ? xs :
        Cons(() => xs.INFO.head)(() => take(amount - 1)(xs.INFO.tail));
const takeWhile = (predicate) => (xs) => xs.CONS === 'Nil' ? xs :
    predicate(xs.INFO.head)
        ? Cons(() => xs.INFO.head)(() => takeWhile(predicate)(xs.INFO.tail))
        : Nil;
const unstring = (xs) => {
    let s = "";
    for (let i = 256; i && xs.CONS === 'Cons'; s += xs.INFO.head, xs = xs.INFO.tail)
        if (!--i)
            console.warn(`'unstring' reached the max lengthed string of 'List' ('256') which could suggest infinity.`);
    return s;
};
const unzip = (xs) => Pair(xs.fmap(fst), xs.fmap(snd));
const zip = (xs) => (ys) => xs.CONS === 'Nil' || ys.CONS === 'Nil' ? Nil :
    Cons(() => Pair(xs.INFO.head, ys.INFO.head))(() => zip(xs.INFO.tail)(ys.INFO.tail));
const zipWith = (zipper) => (xs) => (ys) => xs.CONS === 'Nil' || ys.CONS === 'Nil' ? Nil :
    Cons(() => zipper(xs.INFO.head)(ys.INFO.head))(() => zipWith(zipper)(xs.INFO.tail)(ys.INFO.tail));
const Vector2D = (x) => (y) => ({
    CONS: 'Vector2D',
    eq: v => v.x === x && v.y === y,
    get pipe() { return (f) => f(this); },
    x, y
});
const origin2D = Vector2D(0)(0);
const translate2D = (dx) => (dy) => (v) => Vector2D(v.x + dx)(v.y + dy);
const translateVector2D = (dv) => (v) => Vector2D(v.x + dv.x)(v.y + dv.y);
const abs2D = (v) => sqrt(v.x * v.x + v.y * v.y);
const scale2D = (k) => (v) => Vector2D(v.x * k)(v.y * k);
const invscale2D = (k) => (v) => Vector2D(v.x / k)(v.y / k);
const normalize2D = (v) => {
    if (v.x === 0 && v.y === 0)
        return v;
    const l = Math.sqrt(v.x * v.x + v.y * v.y);
    return Vector2D(v.x * l)(v.y * l);
};
const rescale2D = (k) => (v) => {
    if (v.x === 0 && v.y === 0)
        return v;
    const l = k * Math.sqrt(v.x * v.x + v.y * v.y);
    return Vector2D(v.x * l)(v.y * l);
};
const Vector3D = (x) => (y) => (z) => ({
    CONS: 'Vector3D',
    eq: v => v.x === x && v.y === y && v.z === z,
    get pipe() { return (f) => f(this); },
    x, y, z
});
const origin3D = Vector3D(0)(0)(0);
const translate3D = (dx) => (dy) => (dz) => (v) => Vector3D(v.x + dx)(v.y + dy)(v.z + dz);
const translateVector3D = (dv) => (v) => Vector3D(v.x + dv.x)(v.y + dv.y)(v.z + dv.z);
const abs3D = (v) => Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
const scale3D = (k) => (v) => Vector3D(v.x * k)(v.y * k)(v.z * k);
const invscale3D = (k) => (v) => Vector3D(v.x / k)(v.y / k)(v.z / k);
const normalize3D = (v) => {
    if (v.x === 0 && v.y === 0 && v.z === 0)
        return v;
    const l = Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
    return Vector3D(v.x * l)(v.y * l)(v.z * l);
};
const rescale3D = (k) => (v) => {
    if (v.x === 0 && v.y === 0 && v.z === 0)
        return v;
    const l = k * Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
    return Vector3D(v.x * l)(v.y * l)(v.z * l);
};
const Vector4D = (x) => (y) => (z) => (w) => ({
    CONS: 'Vector4D',
    eq: v => v.x === x && v.y === y && v.z === z && v.w === w,
    get pipe() { return (f) => f(this); },
    x, y, z, w
});
const origin4D = Vector4D(0)(0)(0)(0);
const translate4D = (dx) => (dy) => (dz) => (dw) => (v) => Vector4D(v.x + dx)(v.y + dy)(v.z + dz)(v.w + dw);
const translateVector4D = (dv) => (v) => Vector4D(v.x + dv.x)(v.y + dv.y)(v.z + dv.z)(v.w + dv.w);
const abs4D = (v) => sqrt(v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w);
const scale4D = (k) => (v) => Vector4D(v.x * k)(v.y * k)(v.z * k)(v.w * k);
const invscale4D = (k) => (v) => Vector4D(v.x / k)(v.y / k)(v.z / k)(v.w / k);
const normalize4D = (v) => {
    if (v.x === 0 && v.y === 0 && v.z === 0 && v.w === 0)
        return v;
    const l = Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w);
    return Vector4D(v.x * l)(v.y * l)(v.z * l)(v.w * l);
};
const rescale4D = (k) => (v) => {
    if (v.x === 0 && v.y === 0 && v.z === 0 && v.w === 0)
        return v;
    const l = k * Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w);
    return Vector4D(v.x * l)(v.y * l)(v.z * l)(v.w * l);
};
const Matrix2x2 = (ix) => (jx) => (iy) => (jy) => ({
    CONS: 'Matrix2x2',
    eq: m => m.ix === ix && m.jx === jx && m.iy === iy && m.jy === jy,
    get pipe() { return (f) => f(this); },
    ix, jx,
    iy, jy,
    i: Vector2D(ix)(iy), j: Vector2D(jx)(jy),
    x: Vector2D(ix)(jx), y: Vector2D(iy)(jy)
});
const Matrix2D = (i) => (j) => Matrix2x2(i.x)(j.x)(i.y)(j.y);
const multiply2x2 = (m) => (n) => Matrix2x2(m.ix * n.ix + m.jx * n.iy)(m.ix * n.jx + m.jx * n.jy)(m.iy * n.ix + m.jy * n.iy)(m.iy * n.jx + m.jy * n.jy);
const Matrix3x3 = (ix) => (jx) => (kx) => (iy) => (jy) => (ky) => (iz) => (jz) => (kz) => ({
    CONS: 'Matrix3x3',
    eq: m => m.ix === ix && m.jx === jx && m.kx === kx &&
        m.iy === iy && m.jy === jy && m.ky === ky &&
        m.iz === iz && m.jz === jz && m.kz === kz,
    get pipe() { return (f) => f(this); },
    ix, jx, kx,
    iy, jy, ky,
    iz, jz, kz,
    i: Vector3D(ix)(iy)(iz), j: Vector3D(jx)(jy)(jz), k: Vector3D(kx)(ky)(kz),
    x: Vector3D(ix)(jx)(kx), y: Vector3D(iy)(jy)(ky), z: Vector3D(iz)(jz)(kz)
});
const Matrix3D = (i) => (j) => (k) => Matrix3x3(i.x)(j.x)(k.x)(i.y)(j.y)(k.y)(i.z)(j.z)(k.z);
const multiply3x3 = (m) => (n) => Matrix3x3(m.ix * n.ix + m.jx * n.iy + m.kx * n.iz)(m.ix * n.jx + m.jx * n.jy + m.kx * n.jz)(m.ix * n.kx + m.jx * n.ky + m.kx * n.kz)(m.iy * n.ix + m.jy * n.iy + m.ky * n.iz)(m.iy * n.jx + m.jy * n.jy + m.ky * n.jz)(m.iy * n.kx + m.jy * n.ky + m.ky * n.kz)(m.iz * n.ix + m.jz * n.iy + m.kz * n.iz)(m.iz * n.jx + m.jz * n.jy + m.kz * n.jz)(m.iz * n.kx + m.jz * n.ky + m.kz * n.kz);
const Matrix4x4 = (ix) => (jx) => (kx) => (lx) => (iy) => (jy) => (ky) => (ly) => (iz) => (jz) => (kz) => (lz) => (iw) => (jw) => (kw) => (lw) => ({
    CONS: 'Matrix4x4',
    eq: m => m.ix === ix && m.jx === jx && m.kx === kx && m.lx === lx &&
        m.iy === iy && m.jy === jy && m.ky === ky && m.ly === ly &&
        m.iz === iz && m.jz === jz && m.kz === kz && m.lz === lz &&
        m.iw === iw && m.jw === jw && m.kw === kw && m.lw === lw,
    get pipe() { return (f) => f(this); },
    ix, jx, kx, lx,
    iy, jy, ky, ly,
    iz, jz, kz, lz,
    iw, jw, kw, lw,
    i: Vector4D(ix)(iy)(iz)(iw), j: Vector4D(jx)(jy)(jz)(jw), k: Vector4D(kx)(ky)(kz)(kw), l: Vector4D(lx)(ly)(lz)(lw),
    x: Vector4D(ix)(jx)(kx)(lx), y: Vector4D(iy)(jy)(ky)(ly), z: Vector4D(iz)(jz)(kz)(lz), w: Vector4D(iw)(jw)(kw)(lw)
});
const Matrix4D = (i) => (j) => (k) => (l) => Matrix4x4(i.x)(j.x)(k.x)(l.x)(i.y)(j.y)(k.y)(l.y)(i.z)(j.z)(k.z)(l.z)(i.w)(j.w)(k.w)(l.w);
const multiply4x4 = (m) => (n) => Matrix4x4(m.ix * n.ix + m.jx * n.iy + m.kx * n.iz + m.lx * n.iw)(m.ix * n.jx + m.jx * n.jy + m.kx * n.jz + m.lx * n.jw)(m.ix * n.kx + m.jx * n.ky + m.kx * n.kz + m.lx * n.kw)(m.ix * n.lx + m.jx * n.ly + m.kx * n.lz + m.lx * n.lw)(m.iy * n.ix + m.jy * n.iy + m.ky * n.iz + m.ly * n.iw)(m.iy * n.jx + m.jy * n.jy + m.ky * n.jz + m.ly * n.jw)(m.iy * n.kx + m.jy * n.ky + m.ky * n.kz + m.ly * n.kw)(m.iy * n.lx + m.jy * n.ly + m.ky * n.lz + m.ly * n.lw)(m.iz * n.ix + m.jz * n.iy + m.kz * n.iz + m.lz * n.iw)(m.iz * n.jx + m.jz * n.jy + m.kz * n.jz + m.lz * n.jw)(m.iz * n.kx + m.jz * n.ky + m.kz * n.kz + m.lz * n.kw)(m.iz * n.lx + m.jz * n.ly + m.kz * n.lz + m.lz * n.lw)(m.iw * n.ix + m.jw * n.iy + m.kw * n.iz + m.lw * n.iw)(m.iw * n.jx + m.jw * n.jy + m.kw * n.jz + m.lw * n.jw)(m.iw * n.kx + m.jw * n.ky + m.kw * n.kz + m.lw * n.kw)(m.iw * n.lx + m.jw * n.ly + m.kw * n.lz + m.lw * n.lw);
const TextMeasurement = (text) => (width) => (height) => ({
    CONS: 'TextMeasurement',
    text, width, height
});
const Mapping = (...mappings) => ({
    CONS: 'Mapping',
    domain: x => {
        const i = mappings.findIndex(p => p[0].eq(x));
        return ~i
            ? mappings[i][1]
            : THROWRANGE(`'Mapping' was non-exhaustive; could not find domain of '${x}'`);
    },
    codomain: x => {
        const i = mappings.findIndex(p => p[1].eq(x));
        return ~i
            ? mappings[i][0]
            : THROWRANGE(`'Mapping' was non-exhaustive; could not find codomain of '${x}'`);
    }
});
const Core = (record) => (Object.assign({ CONS: 'Core', get pipe() { return (f) => f(this); } }, record));
const updateCore = (core) => Do.IO
    .bindto('present')(_ => Import.timeSinceOpen)
    .bindto('maxCanvasScalar')(_ => fetchMaxCanvasScalar)
    .bindto('isResizing')($ => Import.isWindowResized
    .fmap(b => napprox(core.canvasScalar)($.maxCanvasScalar)(RESIZING_THRESHOLD) && (core.isResizing || b)))
    .fmapto('refreshTime')($ => (REFRESH_TIME < core.refreshTime ? 0 : core.refreshTime) + $.present - core.time)
    .fmap($ => Core({
    time: $.present,
    isResizing: $.isResizing,
    refreshTime: $.refreshTime,
    isRefresh: $.refreshTime > REFRESH_TIME,
    canvasScalar: $.isResizing && $.refreshTime > REFRESH_TIME
        ? lerp(RESIZING_SPEED)(core.canvasScalar)($.maxCanvasScalar)
        : core.canvasScalar
}));
const unit = {
    IO: (output) => IO(() => output),
    Maybe: Just,
    State: (output) => State(_ => Pair(null, output)),
    List: (element) => Cons(() => element)(() => Nil)
};
var Horizontal;
(function (Horizontal) {
    Horizontal["Leftward"] = "Leftward :: Horizontal";
    Horizontal["Left"] = "Left :: Horizontal";
    Horizontal["CenterX"] = "CenterX :: Horizontal";
    Horizontal["Right"] = "Right :: Horizontal";
    Horizontal["Rightward"] = "Rightward :: Horizontal";
})(Horizontal || (Horizontal = {}));
var Vertical;
(function (Vertical) {
    Vertical["Downward"] = "Downward :: Vertical";
    Vertical["Down"] = "Down :: Vertical";
    Vertical["CenterY"] = "CenterY :: Vertical";
    Vertical["Up"] = "Up :: Vertical";
    Vertical["Upward"] = "Upward :: Vertical";
})(Vertical || (Vertical = {}));
var Lateral;
(function (Lateral) {
    Lateral["Backward"] = "Backward :: Lateral";
    Lateral["Back"] = "Back :: Lateral";
    Lateral["CenterZ"] = "CenterZ :: Lateral";
    Lateral["Fore"] = "Fore :: Lateral";
    Lateral["Forward"] = "Forward :: Lateral";
})(Lateral || (Lateral = {}));
var LineCap;
(function (LineCap) {
    LineCap["Butt"] = "Butt :: LineCap";
    LineCap["Round"] = "Round :: LineCap";
    LineCap["Square"] = "Square :: LineCap";
})(LineCap || (LineCap = {}));
var LineJoin;
(function (LineJoin) {
    LineJoin["Round"] = "Round :: LineJoin";
    LineJoin["Bevel"] = "Bevel :: LineJoin";
    LineJoin["Miter"] = "Miter :: LineJoin";
})(LineJoin || (LineJoin = {}));
var TextAlign;
(function (TextAlign) {
    TextAlign["Start"] = "Start :: TextAlign";
    TextAlign["End"] = "End :: TextAlign";
    TextAlign["Left"] = "Left :: TextAlign";
    TextAlign["Right"] = "Right :: TextAlign";
    TextAlign["Center"] = "Center :: TextAlign";
})(TextAlign || (TextAlign = {}));
var TextBaseline;
(function (TextBaseline) {
    TextBaseline["Top"] = "Top :: TextBaseline";
    TextBaseline["Hanging"] = "Hanging :: TextBaseline";
    TextBaseline["Middle"] = "Middle :: TextBaseline";
    TextBaseline["Alphabetic"] = "Alphabetic :: TextBaseline";
    TextBaseline["Ideographic"] = "Ideographic :: TextBaseline";
    TextBaseline["Bottom"] = "Bottom :: TextBaseline";
})(TextBaseline || (TextBaseline = {}));
var CompositionOperation;
(function (CompositionOperation) {
    CompositionOperation["SourceOver"] = "SourceOver :: CompositionOperation";
    CompositionOperation["SourceAtop"] = "SourceAtop :: CompositionOperation";
    CompositionOperation["SourceIn"] = "SourceIn :: CompositionOperation";
    CompositionOperation["SourceOut"] = "SourceOut :: CompositionOperation";
    CompositionOperation["DestinationOver"] = "DestinationOver :: CompositionOperation";
    CompositionOperation["DestinationAtop"] = "DestinationAtop :: CompositionOperation";
    CompositionOperation["DestinationIn"] = "DestinationIn :: CompositionOperation";
    CompositionOperation["DestinationOut"] = "DestinationOut :: CompositionOperation";
    CompositionOperation["Lighter"] = "Lighter :: CompositionOperation";
    CompositionOperation["Xor"] = "Xor :: CompositionOperation";
    CompositionOperation["Copy"] = "Copy :: CompositionOperation";
    CompositionOperation["Multiply"] = "Multiply :: CompositionOperation";
    CompositionOperation["Screen"] = "Screen :: CompositionOperation";
    CompositionOperation["Overlay"] = "Overlay :: CompositionOperation";
    CompositionOperation["Darken"] = "Darken :: CompositionOperation";
    CompositionOperation["Lighten"] = "Lighten :: CompositionOperation";
    CompositionOperation["ColorDodge"] = "ColorDodge :: CompositionOperation";
    CompositionOperation["ColorBurn"] = "ColorBurn :: CompositionOperation";
    CompositionOperation["HardLight"] = "HardLight :: CompositionOperation";
    CompositionOperation["SoftLight"] = "SoftLight :: CompositionOperation";
    CompositionOperation["Difference"] = "Difference :: CompositionOperation";
    CompositionOperation["Exclusion"] = "Exclusion :: CompositionOperation";
    CompositionOperation["Hue"] = "Hue :: CompositionOperation";
    CompositionOperation["Saturation"] = "Saturation :: CompositionOperation";
    CompositionOperation["Color"] = "Color :: CompositionOperation";
    CompositionOperation["Luminosity"] = "Luminosity :: CompositionOperation";
})(CompositionOperation || (CompositionOperation = {}));
const relaxHorizontal = (direction) => direction === Horizontal.Leftward ? Horizontal.Left :
    direction === Horizontal.Rightward ? Horizontal.Right :
        direction;
const relaxVertical = (direction) => direction === Vertical.Downward ? Vertical.Down :
    direction === Vertical.Upward ? Vertical.Up :
        direction;
const relaxLateral = (direction) => direction === Lateral.Backward ? Lateral.Back :
    direction === Lateral.Forward ? Lateral.Fore :
        direction;
const isLeft = (direction) => direction === Horizontal.Left || direction === Horizontal.Leftward;
const isRight = (direction) => direction === Horizontal.Right || direction === Horizontal.Rightward;
const isDown = (direction) => direction === Vertical.Down || direction === Vertical.Downward;
const isUp = (direction) => direction === Vertical.Up || direction === Vertical.Upward;
const isBack = (direction) => direction === Lateral.Back || direction === Lateral.Backward;
const isFore = (direction) => direction === Lateral.Fore || direction === Lateral.Forward;
const mappingLineCap = Mapping([LineCap.Butt, 'butt'], [LineCap.Round, 'round'], [LineCap.Square, 'square']);
const mappingLineJoin = Mapping([LineJoin.Round, 'round'], [LineJoin.Bevel, 'bevel'], [LineJoin.Miter, 'miter']);
const mappingTextAlign = Mapping([TextAlign.Center, 'center'], [TextAlign.End, 'end'], [TextAlign.Left, 'left'], [TextAlign.Right, 'right'], [TextAlign.Start, 'start']);
const mappingTextBaseline = Mapping([TextBaseline.Alphabetic, 'alphabetic'], [TextBaseline.Bottom, 'bottom'], [TextBaseline.Hanging, 'hanging'], [TextBaseline.Ideographic, 'ideographic'], [TextBaseline.Middle, 'middle'], [TextBaseline.Top, 'top']);
const mappingCompositionOperation = Mapping([CompositionOperation.SourceOver, 'source-over'], [CompositionOperation.SourceIn, 'source-in'], [CompositionOperation.SourceOut, 'source-out'], [CompositionOperation.SourceAtop, 'source-atop'], [CompositionOperation.DestinationOver, 'destination-over'], [CompositionOperation.DestinationIn, 'destination-in'], [CompositionOperation.DestinationOut, 'destination-out'], [CompositionOperation.DestinationAtop, 'destination-atop'], [CompositionOperation.Lighter, 'lighter'], [CompositionOperation.Copy, 'copy'], [CompositionOperation.Xor, 'xor'], [CompositionOperation.Multiply, 'multiply'], [CompositionOperation.Screen, 'screen'], [CompositionOperation.Overlay, 'overlay'], [CompositionOperation.Darken, 'darken'], [CompositionOperation.Lighten, 'lighten'], [CompositionOperation.ColorDodge, 'color-dodge'], [CompositionOperation.ColorBurn, 'color-burn'], [CompositionOperation.HardLight, 'hard-light'], [CompositionOperation.SoftLight, 'soft-light'], [CompositionOperation.Difference, 'difference'], [CompositionOperation.Exclusion, 'exclusion'], [CompositionOperation.Hue, 'hue'], [CompositionOperation.Saturation, 'saturation'], [CompositionOperation.Color, 'color'], [CompositionOperation.Luminosity, 'luminosity']);
const Do = {
    IO: IO(() => Object.create(null)),
    Maybe: Just(Object.create(null)),
    State: State((s) => Pair(s, Object.create(null))),
    List: Cons(() => Object.create(null))(() => Nil)
};
const __KEYBOARD_KEYS_ARRAY__ = [
    'AltLeft', 'AltRight', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'ArrowUp', 'Backquote',
    'Backslash', 'Backspace', 'BracketLeft', 'BracketRight', 'CapsLock', 'Comma', 'ControlLeft',
    'ControlRight', 'Delete', 'NumpadAdd', 'NumpadDecimal', 'NumpadDivide', 'NumpadEnter', 'NumpadMultiply',
    'NumpadSubtract', 'PageUp', 'Pagedown', 'Period', 'Quote', 'Semicolon', 'ShiftLeft',
    'ShiftRight', 'Slash', 'End', 'Enter', 'Equal', 'Home', 'Insert',
    'Minus', 'Space', 'Tab', 'Digit0', 'Digit1', 'Digit2', 'Digit3',
    'Digit4', 'Digit5', 'Digit6', 'Digit7', 'Digit8', 'Digit9', 'Numpad0',
    'Numpad1', 'Numpad2', 'Numpad3', 'Numpad4', 'Numpad5', 'Numpad6', 'Numpad7',
    'Numpad8', 'Numpad9', 'KeyA', 'KeyB', 'KeyC', 'KeyD', 'KeyE',
    'KeyF', 'KeyG', 'KeyH', 'KeyI', 'KeyJ', 'KeyK', 'KeyL',
    'KeyM', 'KeyN', 'KeyO', 'KeyP', 'KeyQ', 'KeyR', 'KeyS',
    'KeyT', 'KeyU', 'KeyV', 'KeyW', 'KeyX', 'KeyY', 'KeyZ'
];
const __EXTERNAL__ = {
    context: undefined,
    resizeID: undefined,
    isResized: false,
    isPointerLocked: false,
    seed: (Math.random() - 0.5) * Date.now(),
    image: {},
    audio: {},
    mouse: {
        screenX: 0, screenY: 0,
        windowX: 0, windowY: 0,
        canvasX: 0, canvasY: 0,
        deltaX: 0, deltaY: 0,
        scroll: Vertical.CenterY,
        buttons: Array(5).fill(Vertical.Up)
    },
    keyboard: __KEYBOARD_KEYS_ARRAY__.reduce(($, k) => (Object.assign(Object.assign({}, $), { [k]: Vertical.Up })), {})
};
var Import;
(function (Import) {
    let Norm;
    (function (Norm) {
        Norm.mouseCanvasPosition = IO(() => Pair(__EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width, __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height));
        Norm.mouseCanvasPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width)(__EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height));
        Norm.mouseCanvasPositionX = IO(() => __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width);
        Norm.mouseCanvasPositionY = IO(() => __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height);
        Norm.mouseVelocity = IO(() => Pair(__EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width, __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height));
        Norm.mouseVelocityVector = IO(() => Vector2D(__EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width)(__EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height));
        Norm.mouseVelocityX = IO(() => __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width);
        Norm.mouseVelocityY = IO(() => __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height);
        Norm.textMeasurement = (text) => IO(() => {
            const { width, height } = __EXTERNAL__.context.canvas;
            const { actualBoundingBoxLeft, actualBoundingBoxRight, actualBoundingBoxAscent, actualBoundingBoxDescent } = __EXTERNAL__.context.measureText(text);
            return TextMeasurement(text)((Math.abs(actualBoundingBoxLeft) + Math.abs(actualBoundingBoxRight)) / width)((Math.abs(actualBoundingBoxAscent) + Math.abs(actualBoundingBoxDescent)) / height);
        });
        Norm.lineWidth = IO(() => __EXTERNAL__.context.lineWidth / __EXTERNAL__.context.canvas.width);
        Norm.lineDashPattern = IO(() => List(...__EXTERNAL__.context.getLineDash().map(n => n / __EXTERNAL__.context.canvas.width)));
        Norm.lineDashOffset = IO(() => __EXTERNAL__.context.lineDashOffset / __EXTERNAL__.context.canvas.width);
        Norm.fontSize = IO(() => parseFloat(__EXTERNAL__.context.font) / __EXTERNAL__.context.canvas.width);
        Norm.shadowOffset = IO(() => Pair(__EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width, __EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height));
        Norm.shadowOffsetVector = IO(() => Vector2D(__EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width)(__EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height));
        Norm.shadowOffsetX = IO(() => __EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width);
        Norm.shadowOffsetY = IO(() => __EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height);
        Norm.isPointInEvenOddPath = (x) => (y) => IO(() => __EXTERNAL__.context.isPointInPath(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, 'evenodd'));
        Norm.isPointInNonZeroPath = (x) => (y) => IO(() => __EXTERNAL__.context.isPointInPath(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, 'nonzero'));
        Norm.isVectorInEvenOddPath = (v) => IO(() => __EXTERNAL__.context.isPointInPath(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height, 'evenodd'));
        Norm.isVectorInNonZeroPath = (v) => IO(() => __EXTERNAL__.context.isPointInPath(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height, 'nonzero'));
        Norm.isPointInStroke = (x) => (y) => IO(() => __EXTERNAL__.context.isPointInStroke(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height));
        Norm.isVectorInStroke = (v) => IO(() => __EXTERNAL__.context.isPointInStroke(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height));
        Norm.transformationMatrix = IO(() => {
            const m = __EXTERNAL__.context.getTransform();
            return Matrix3x3(m.a)(m.c)(m.e / __EXTERNAL__.context.canvas.width)(m.b)(m.d)(m.f / __EXTERNAL__.context.canvas.height)(0)(0)(1);
        });
    })(Norm = Import.Norm || (Import.Norm = {}));
    Import.timeSinceOpen = IO(() => performance.now());
    Import.timeSince1970 = IO(() => Date.now());
    Import.universalSeed = IO(() => __EXTERNAL__.seed);
    Import.isWindowResized = IO(() => __EXTERNAL__.isResized);
    Import.screenDimensions = IO(() => Pair(screen.width, screen.height));
    Import.screenDimensionsVector = IO(() => Vector2D(screen.width)(screen.height));
    Import.screenDimensionW = IO(() => screen.width);
    Import.screenDimensionH = IO(() => screen.height);
    Import.windowDimensions = IO(() => Pair(innerWidth, innerHeight));
    Import.windowDimensionsVector = IO(() => Vector2D(innerWidth)(innerHeight));
    Import.windowDimensionW = IO(() => innerWidth);
    Import.windowDimensionH = IO(() => innerHeight);
    Import.canvasDimensions = IO(() => Pair(__EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height));
    Import.canvasDimensionsVector = IO(() => Vector2D(__EXTERNAL__.context.canvas.width)(__EXTERNAL__.context.canvas.height));
    Import.canvasDimensionW = IO(() => __EXTERNAL__.context.canvas.width);
    Import.canvasDimensionH = IO(() => __EXTERNAL__.context.canvas.height);
    Import.mouseScreenPosition = IO(() => Pair(__EXTERNAL__.mouse.screenX, __EXTERNAL__.mouse.screenY));
    Import.mouseScreenPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.screenX)(__EXTERNAL__.mouse.screenY));
    Import.mouseScreenPositionX = IO(() => __EXTERNAL__.mouse.screenX);
    Import.mouseScreenPositionY = IO(() => __EXTERNAL__.mouse.screenY);
    Import.mouseWindowPosition = IO(() => Pair(__EXTERNAL__.mouse.windowX, __EXTERNAL__.mouse.windowY));
    Import.mouseWindowPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.windowX)(__EXTERNAL__.mouse.windowY));
    Import.mouseWindowPositionX = IO(() => __EXTERNAL__.mouse.windowX);
    Import.mouseWindowPositionY = IO(() => __EXTERNAL__.mouse.windowY);
    Import.mouseCanvasPosition = IO(() => Pair(__EXTERNAL__.mouse.canvasX, __EXTERNAL__.mouse.canvasY));
    Import.mouseCanvasPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.canvasX)(__EXTERNAL__.mouse.canvasY));
    Import.mouseCanvasPositionX = IO(() => __EXTERNAL__.mouse.canvasX);
    Import.mouseCanvasPositionY = IO(() => __EXTERNAL__.mouse.canvasY);
    Import.mouseVelocity = IO(() => Pair(__EXTERNAL__.mouse.deltaX, __EXTERNAL__.mouse.deltaY));
    Import.mouseVelocityVector = IO(() => Vector2D(__EXTERNAL__.mouse.deltaX)(__EXTERNAL__.mouse.deltaY));
    Import.mouseVelocityX = IO(() => __EXTERNAL__.mouse.deltaX);
    Import.mouseVelocityY = IO(() => __EXTERNAL__.mouse.deltaY);
    Import.mouseButtonLeft = IO(() => __EXTERNAL__.mouse.buttons[0]);
    Import.mouseButtonMiddle = IO(() => __EXTERNAL__.mouse.buttons[1]);
    Import.mouseButtonRight = IO(() => __EXTERNAL__.mouse.buttons[2]);
    Import.mouseButtonA = IO(() => __EXTERNAL__.mouse.buttons[3]);
    Import.mouseButtonB = IO(() => __EXTERNAL__.mouse.buttons[4]);
    Import.keyboardKey = (key) => IO(() => __EXTERNAL__.keyboard[key]);
    Import.textMeasurement = (text) => IO(() => {
        const metrics = __EXTERNAL__.context.measureText(text);
        return TextMeasurement(text)(Math.abs(metrics.actualBoundingBoxLeft) + Math.abs(metrics.actualBoundingBoxRight))(Math.abs(metrics.actualBoundingBoxAscent) + Math.abs(metrics.actualBoundingBoxDescent));
    });
    Import.lineWidth = IO(() => __EXTERNAL__.context.lineWidth);
    Import.lineCap = IO(() => mappingLineCap.codomain(__EXTERNAL__.context.lineCap));
    Import.lineJoin = IO(() => mappingLineJoin.codomain(__EXTERNAL__.context.lineJoin));
    Import.lineDashPattern = IO(() => List(...__EXTERNAL__.context.getLineDash()));
    Import.lineDashOffset = IO(() => __EXTERNAL__.context.lineDashOffset);
    Import.miterLimit = IO(() => __EXTERNAL__.context.miterLimit);
    Import.font = IO(() => __EXTERNAL__.context.font);
    Import.fontSize = IO(() => parseFloat(__EXTERNAL__.context.font));
    Import.fontFamily = IO(() => __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1));
    Import.textAlign = IO(() => mappingTextAlign.codomain(__EXTERNAL__.context.textAlign));
    Import.textBaseline = IO(() => mappingTextBaseline.codomain(__EXTERNAL__.context.textBaseline));
    Import.shadowBlurAmount = IO(() => __EXTERNAL__.context.shadowBlur);
    Import.shadowColor = IO(() => __EXTERNAL__.context.shadowColor);
    Import.shadowOffset = IO(() => Pair(__EXTERNAL__.context.shadowOffsetX, __EXTERNAL__.context.shadowOffsetY));
    Import.shadowOffsetVector = IO(() => Vector2D(__EXTERNAL__.context.shadowOffsetX)(__EXTERNAL__.context.shadowOffsetY));
    Import.shadowOffsetX = IO(() => __EXTERNAL__.context.shadowOffsetX);
    Import.shadowOffsetY = IO(() => __EXTERNAL__.context.shadowOffsetY);
    Import.isPointInEvenOddPath = (x) => (y) => IO(() => __EXTERNAL__.context.isPointInPath(x, y, 'evenodd'));
    Import.isPointInNonZeroPath = (x) => (y) => IO(() => __EXTERNAL__.context.isPointInPath(x, y, 'nonzero'));
    Import.isVectorInEvenOddPath = (v) => IO(() => __EXTERNAL__.context.isPointInPath(v.x, v.y, 'evenodd'));
    Import.isVectorInNonZeroPath = (v) => IO(() => __EXTERNAL__.context.isPointInPath(v.x, v.y, 'nonzero'));
    Import.isPointInStroke = (x) => (y) => IO(() => __EXTERNAL__.context.isPointInStroke(x, y));
    Import.isVectorInStroke = (v) => IO(() => __EXTERNAL__.context.isPointInStroke(v.x, v.y));
    Import.transformationMatrix = IO(() => {
        const m = __EXTERNAL__.context.getTransform();
        return Matrix3x3(m.a)(m.c)(m.e)(m.b)(m.d)(m.f)(0)(0)(1);
    });
    Import.alpha = IO(() => __EXTERNAL__.context.globalAlpha);
    Import.compositionOperation = IO(() => mappingCompositionOperation.codomain(__EXTERNAL__.context.globalCompositeOperation));
})(Import || (Import = {}));
var Mutate;
(function (Mutate) {
    let Norm;
    (function (Norm) {
        Norm.lineWidth = (w) => IO(() => (__EXTERNAL__.context.lineWidth = w * __EXTERNAL__.context.canvas.width, null));
        Norm.lineDashPattern = (pattern) => IO(() => {
            __EXTERNAL__.context.setLineDash(array(pattern.fmap(n => n * __EXTERNAL__.context.canvas.width)));
            return null;
        });
        Norm.lineDashOffset = (offset) => IO(() => (__EXTERNAL__.context.lineDashOffset = offset * __EXTERNAL__.context.canvas.width, null));
        Norm.fontSize = (size) => IO(() => {
            __EXTERNAL__.context.font =
                `${size * __EXTERNAL__.context.canvas.width}px` +
                    `${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" "))}`;
            return null;
        });
        Norm.fillRGBA = (r) => (g) => (b) => (a) => IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null));
        Norm.fillVector = (v) => IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null));
        Norm.strokeRGBA = (r) => (g) => (b) => (a) => IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a * 255})`, null));
        Norm.strokeVector = (v) => IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null));
        Norm.shadowRGBA = (r) => (g) => (b) => (a) => IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null));
        Norm.shadowVector = (v) => IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null));
        Norm.shadowOffset = (x) => (y) => IO(() => {
            __EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width;
            __EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height;
            return null;
        });
        Norm.shadowOffsetVector = (v) => IO(() => {
            __EXTERNAL__.context.shadowOffsetX = v.x * __EXTERNAL__.context.canvas.width;
            __EXTERNAL__.context.shadowOffsetY = v.y * __EXTERNAL__.context.canvas.height;
            return null;
        });
        Norm.shadowOffsetX = (x) => IO(() => (__EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width, null));
        Norm.shadowOffsetY = (y) => IO(() => (__EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height, null));
        Norm.transformationMatrix = (m) => IO(() => {
            __EXTERNAL__.context.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx * __EXTERNAL__.context.canvas.width, m.ky * __EXTERNAL__.context.canvas.height);
            return null;
        });
    })(Norm = Mutate.Norm || (Mutate.Norm = {}));
    Mutate.canvasDimensions = (w) => (h) => IO(() => {
        __EXTERNAL__.context.canvas.width = w;
        __EXTERNAL__.context.canvas.height = h;
        return null;
    });
    Mutate.canvasDimensionVector = (v) => IO(() => {
        __EXTERNAL__.context.canvas.width = v.x;
        __EXTERNAL__.context.canvas.height = v.y;
        return null;
    });
    Mutate.canvasDimensionW = (w) => IO(() => (__EXTERNAL__.context.canvas.width = w, null));
    Mutate.canvasDimensionH = (h) => IO(() => (__EXTERNAL__.context.canvas.height = h, null));
    Mutate.lineWidth = (w) => IO(() => (__EXTERNAL__.context.lineWidth = w, null));
    Mutate.lineCap = (cap) => IO(() => (__EXTERNAL__.context.lineCap = mappingLineCap.domain(cap), null));
    Mutate.lineJoin = (joining) => IO(() => (__EXTERNAL__.context.lineJoin = mappingLineJoin.domain(joining), null));
    Mutate.lineDashPattern = (pattern) => IO(() => (__EXTERNAL__.context.setLineDash(array(pattern)), null));
    Mutate.lineDashOffset = (offset) => IO(() => (__EXTERNAL__.context.lineDashOffset = offset, null));
    Mutate.miterLimit = (limit) => IO(() => (__EXTERNAL__.context.miterLimit = limit, null));
    Mutate.font = (fontDescription) => IO(() => (__EXTERNAL__.context.font = fontDescription, null));
    Mutate.fontSize = (size) => IO(() => {
        __EXTERNAL__.context.font =
            `${size}px ${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1)}`;
        return null;
    });
    Mutate.fontFamily = (family) => IO(() => {
        __EXTERNAL__.context.font = `${parseFloat(__EXTERNAL__.context.font)}px ${family}`;
        return null;
    });
    Mutate.textAlign = (align) => IO(() => (__EXTERNAL__.context.textAlign = mappingTextAlign.domain(align), null));
    Mutate.textBaseline = (baseline) => IO(() => (__EXTERNAL__.context.textBaseline = mappingTextBaseline.domain(baseline), null));
    Mutate.fillColor = (color) => IO(() => (__EXTERNAL__.context.fillStyle = color, null));
    Mutate.fillRGBA = (r) => (g) => (b) => (a) => IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${r},${g},${b},${a})`, null));
    Mutate.fillVector = (v) => IO(() => (__EXTERNAL__.context.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null));
    Mutate.strokeColor = (color) => IO(() => (__EXTERNAL__.context.strokeStyle = color, null));
    Mutate.strokeRGBA = (r) => (g) => (b) => (a) => IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${r},${g},${b},${a})`, null));
    Mutate.strokeVector = (v) => IO(() => (__EXTERNAL__.context.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null));
    Mutate.shadowBlurAmount = (amount) => IO(() => (__EXTERNAL__.context.shadowBlur = amount, null));
    Mutate.shadowColor = (color) => IO(() => (__EXTERNAL__.context.shadowColor = color, null));
    Mutate.shadowRGBA = (r) => (g) => (b) => (a) => IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${r},${g},${b},${a})`, null));
    Mutate.shadowVector = (v) => IO(() => (__EXTERNAL__.context.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`, null));
    Mutate.shadowOffset = (x) => (y) => IO(() => {
        __EXTERNAL__.context.shadowOffsetX = x;
        __EXTERNAL__.context.shadowOffsetY = y;
        return null;
    });
    Mutate.shadowOffsetVector = (v) => IO(() => {
        __EXTERNAL__.context.shadowOffsetX = v.x;
        __EXTERNAL__.context.shadowOffsetY = v.y;
        return null;
    });
    Mutate.shadowOffsetX = (x) => IO(() => (__EXTERNAL__.context.shadowOffsetX = x, null));
    Mutate.shadowOffsetY = (y) => IO(() => (__EXTERNAL__.context.shadowOffsetY = y, null));
    Mutate.transformationMatrix = (m) => IO(() => (__EXTERNAL__.context.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null));
    Mutate.alpha = (opacity) => IO(() => (__EXTERNAL__.context.globalAlpha = opacity, null));
    Mutate.compositionOperation = (composition) => IO(() => {
        __EXTERNAL__.context.globalCompositeOperation = mappingCompositionOperation.domain(composition);
        return null;
    });
})(Mutate || (Mutate = {}));
var Effect;
(function (Effect) {
    let Norm;
    (function (Norm) {
        Norm.drawImage = (path) => (cropX) => (cropY) => (cropW) => (cropH) => (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropX * __EXTERNAL__.image[path].width, cropY * __EXTERNAL__.image[path].height, cropW * __EXTERNAL__.image[path].width, cropH * __EXTERNAL__.image[path].height, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.drawImageVector = (path) => (cropCoordinates) => (cropDimensions) => (position) => (dimensions) => IO(() => {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropCoordinates.x * __EXTERNAL__.image[path].width, cropCoordinates.y * __EXTERNAL__.image[path].height, cropDimensions.x * __EXTERNAL__.image[path].width, cropDimensions.y * __EXTERNAL__.image[path].height, position.x * __EXTERNAL__.context.canvas.width, position.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.drawFullImage = (path) => (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.drawFullImageVector = (path) => (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.drawFixedImage = (path) => (x) => (y) => (k) => IO(() => {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, k * __EXTERNAL__.context.canvas.width, k * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.drawFixedImageVector = (path) => (coordinates) => (k) => IO(() => {
            const l = k * __EXTERNAL__.context.canvas.width;
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, l, l);
            return null;
        });
        Norm.drawScaledImage = (path) => (x) => (y) => (k) => IO(() => {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, k * __EXTERNAL__.context.canvas.width * __EXTERNAL__.image[path].width, k * __EXTERNAL__.context.canvas.height * __EXTERNAL__.image[path].height);
            return null;
        });
        Norm.drawScaledImageVector = (path) => (coordinates) => (k) => IO(() => {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, k * __EXTERNAL__.context.canvas.width * __EXTERNAL__.image[path].width, k * __EXTERNAL__.context.canvas.height * __EXTERNAL__.image[path].height);
            return null;
        });
        Norm.clearRectangle = (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.clearRect(x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5, w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.clearRectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.clearRect(coordinates.x * __EXTERNAL__.context.canvas.width - 0.5, coordinates.y * __EXTERNAL__.context.canvas.height - 0.5, dimensions.x * __EXTERNAL__.context.canvas.width + 1, dimensions.y * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.rotate = (angle) => IO(() => {
            __EXTERNAL__.context.rotate(angle * TAU);
            return null;
        });
        Norm.translate = (dx) => (dy) => IO(() => {
            __EXTERNAL__.context.translate(dx * __EXTERNAL__.context.canvas.width, dy * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.translateVector = (v) => IO(() => {
            __EXTERNAL__.context.translate(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.transformation = (m) => IO(() => {
            __EXTERNAL__.context.transform(m.ix, m.iy, m.jx, m.jy, m.kx * __EXTERNAL__.context.canvas.width, m.ky * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.moveTo = (x) => (y) => IO(() => {
            __EXTERNAL__.context.moveTo(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.moveToVector = (v) => IO(() => {
            __EXTERNAL__.context.moveTo(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.lineTo = (x) => (y) => IO(() => {
            __EXTERNAL__.context.lineTo(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.lineToVector = (v) => IO(() => {
            __EXTERNAL__.context.lineTo(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.bezierCurveTo = (ix) => (iy) => (jx) => (jy) => (x) => (y) => IO(() => {
            __EXTERNAL__.context.bezierCurveTo(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, jx * __EXTERNAL__.context.canvas.width, jy * __EXTERNAL__.context.canvas.height, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.bezierCurveToVector = (i) => (j) => (v) => IO(() => {
            __EXTERNAL__.context.bezierCurveTo(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, j.x * __EXTERNAL__.context.canvas.width, j.y * __EXTERNAL__.context.canvas.height, v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.quadraticCurveTo = (ix) => (iy) => (x) => (y) => IO(() => {
            __EXTERNAL__.context.quadraticCurveTo(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.quadraticCurveToVector = (i) => (v) => IO(() => {
            __EXTERNAL__.context.quadraticCurveTo(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.arcTo = (ix) => (iy) => (jx) => (jy) => (r) => IO(() => {
            __EXTERNAL__.context.arcTo(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, jx * __EXTERNAL__.context.canvas.width, jy * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width);
            return null;
        });
        Norm.arcToVector = (i) => (j) => (r) => IO(() => {
            __EXTERNAL__.context.arcTo(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, j.x * __EXTERNAL__.context.canvas.width, j.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width);
            return null;
        });
        Norm.rectangle = (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.rect(x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5, w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.rectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.rect(coordinates.x * __EXTERNAL__.context.canvas.width - 0.5, coordinates.y * __EXTERNAL__.context.canvas.height - 0.5, dimensions.x * __EXTERNAL__.context.canvas.width + 1, dimensions.y * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.fillRectangle = (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.fillRect(x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5, w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.fillRectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.fillRect(coordinates.x * __EXTERNAL__.context.canvas.width - 0.5, coordinates.y * __EXTERNAL__.context.canvas.height - 0.5, dimensions.x * __EXTERNAL__.context.canvas.width + 1, dimensions.y * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.strokeRectangle = (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.strokeRect(x * __EXTERNAL__.context.canvas.width - 0.5, y * __EXTERNAL__.context.canvas.height - 0.5, w * __EXTERNAL__.context.canvas.width + 1, h * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.strokeRectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.strokeRect(coordinates.x * __EXTERNAL__.context.canvas.width - 0.5, coordinates.y * __EXTERNAL__.context.canvas.height - 0.5, dimensions.x * __EXTERNAL__.context.canvas.width + 1, dimensions.y * __EXTERNAL__.context.canvas.height + 1);
            return null;
        });
        Norm.arc = (r) => (x) => (y) => (a) => (b) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, a * TAU, b * TAU);
            return null;
        });
        Norm.arcVector = (r) => (v) => (a) => (b) => IO(() => {
            __EXTERNAL__.context.arc(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, a * TAU, b * TAU);
            return null;
        });
        Norm.circle = (r) => (x) => (y) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            return null;
        });
        Norm.circleVector = (r) => (coordinates) => IO(() => {
            __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            return null;
        });
        Norm.strokeCircle = (r) => (x) => (y) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.stroke();
            return null;
        });
        Norm.strokeCircleVector = (r) => (coordinates) => IO(() => {
            __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.stroke();
            return null;
        });
        Norm.fillCircle = (r) => (x) => (y) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.fill();
            return null;
        });
        Norm.fillCircleVector = (r) => (coordinates) => IO(() => {
            __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.fill();
            return null;
        });
        Norm.elliptic = (x) => (y) => (kx) => (ky) => (a) => (b) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * TAU, a * TAU, b * TAU);
            return null;
        });
        Norm.ellipticVector = (coordinates) => (dimensions) => (a) => (b) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * TAU, a * TAU, b * TAU);
            return null;
        });
        Norm.ellipse = (x) => (y) => (kx) => (ky) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            return null;
        });
        Norm.ellipseVector = (coordinates) => (dimensions) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            return null;
        });
        Norm.strokeEllipse = (x) => (y) => (kx) => (ky) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.stroke();
            return null;
        });
        Norm.strokeEllipseVector = (coordinates) => (dimensions) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.stroke();
            return null;
        });
        Norm.fillEllipse = (x) => (y) => (kx) => (ky) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.fill();
            return null;
        });
        Norm.fillEllipseVector = (coordinates) => (dimensions) => (r) => IO(() => {
            __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.fill();
            return null;
        });
        Norm.strokeText = (text) => (x) => (y) => IO(() => {
            __EXTERNAL__.context.strokeText(text, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.strokeTextVector = (text) => (coordinates) => IO(() => {
            __EXTERNAL__.context.strokeText(text, coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.width);
            return null;
        });
        Norm.fillText = (text) => (x) => (y) => IO(() => {
            __EXTERNAL__.context.fillText(text, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.fillTextVector = (text) => (coordinates) => IO(() => {
            __EXTERNAL__.context.fillText(text, coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.width);
            return null;
        });
        Norm.area = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.rect(ix * __EXTERNAL__.context.canvas.width - 0.5, iy * __EXTERNAL__.context.canvas.height - 0.5, (jx - ix) * __EXTERNAL__.context.canvas.width + 1, (jy - iy) * __EXTERNAL__.context.canvas.height + 1), null));
        Norm.areaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.rect(i.x * __EXTERNAL__.context.canvas.width - 0.5, i.y * __EXTERNAL__.context.canvas.height - 0.5, (j.x - i.x) * __EXTERNAL__.context.canvas.width + 1, (j.y - i.y) * __EXTERNAL__.context.canvas.height + 1), null));
        Norm.strokeArea = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.strokeRect(ix * __EXTERNAL__.context.canvas.width - 0.5, iy * __EXTERNAL__.context.canvas.height - 0.5, (jx - ix) * __EXTERNAL__.context.canvas.width + 1, (jy - iy) * __EXTERNAL__.context.canvas.height + 1), null));
        Norm.strokeAreaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.strokeRect(i.x * __EXTERNAL__.context.canvas.width - 0.5, i.y * __EXTERNAL__.context.canvas.height - 0.5, (j.x - i.x) * __EXTERNAL__.context.canvas.width + 1, (j.y - i.y) * __EXTERNAL__.context.canvas.height + 1), null));
        Norm.fillArea = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.fillRect(ix * __EXTERNAL__.context.canvas.width - 0.5, iy * __EXTERNAL__.context.canvas.height - 0.5, (jx - ix) * __EXTERNAL__.context.canvas.width + 1, (jy - iy) * __EXTERNAL__.context.canvas.height + 1), null));
        Norm.fillAreaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.fillRect(i.x * __EXTERNAL__.context.canvas.width - 0.5, i.y * __EXTERNAL__.context.canvas.height - 0.5, (j.x - i.x) * __EXTERNAL__.context.canvas.width + 1, (j.y - i.y) * __EXTERNAL__.context.canvas.height + 1), null));
    })(Norm = Effect.Norm || (Effect.Norm = {}));
    Effect.log = (message) => IO(() => (console.log(message), null));
    Effect.flush = IO(() => (console.clear(), null));
    Effect.queue = (io) => IO(() => (requestAnimationFrame(() => io.INFO()), null));
    Effect.tick = IO(() => {
        for (const k in __EXTERNAL__.keyboard)
            __EXTERNAL__.keyboard[k] = relaxVertical(__EXTERNAL__.keyboard[k]);
        for (const i in __EXTERNAL__.mouse.buttons)
            __EXTERNAL__.mouse.buttons[i] = relaxVertical(__EXTERNAL__.mouse.buttons[i]);
        __EXTERNAL__.mouse.scroll = Vertical.CenterY;
        __EXTERNAL__.mouse.deltaX = 0;
        __EXTERNAL__.mouse.deltaY = 0;
        __EXTERNAL__.isResized = false;
        return null;
    });
    Effect.activatePointerLock = IO(() => {
        __EXTERNAL__.context.canvas.onmouseup = () => {
            if (!__EXTERNAL__.isPointerLocked)
                __EXTERNAL__.context.canvas.requestPointerLock();
        };
        return null;
    });
    Effect.deactivatePointerLock = IO(() => {
        __EXTERNAL__.context.canvas.onmousedown = null;
        return null;
    });
    Effect.loadImage = (path) => IO(() => {
        __EXTERNAL__.image[path] = new Image;
        __EXTERNAL__.image[path].src = path;
        __EXTERNAL__.image[path].onerror = () => THROW(`Could not load image: '${path}'`);
        return null;
    });
    Effect.drawImage = (path) => (cropX) => (cropY) => (cropW) => (cropH) => (x) => (y) => (w) => (h) => IO(() => {
        __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropX, cropY, cropW, cropH, x, y, w, h);
        return null;
    });
    Effect.drawImageVector = (path) => (cropCoordinates) => (cropDimensions) => (position) => (dimensions) => IO(() => {
        __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropCoordinates.x, cropCoordinates.y, cropDimensions.x, cropDimensions.y, position.x, position.y, dimensions.x, dimensions.y);
        return null;
    });
    Effect.drawFullImage = (path) => (x) => (y) => (w) => (h) => IO(() => (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x, y, w, h), null));
    Effect.drawFullImageVector = (path) => (coordinates) => (dimensions) => IO(() => {
        __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x, coordinates.y, dimensions.x, dimensions.y);
        return null;
    });
    Effect.drawFixedImage = (path) => (x) => (y) => (k) => IO(() => (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x, y, k, k), null));
    Effect.drawFixedImageVector = (path) => (coordinates) => (k) => IO(() => (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x, coordinates.y, k, k), null));
    Effect.drawScaledImage = (path) => (x) => (y) => (k) => IO(() => {
        __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x, y, k * __EXTERNAL__.image[path].width, k * __EXTERNAL__.image[path].height);
        return null;
    });
    Effect.drawScaledImageVector = (path) => (coordinates) => (k) => IO(() => {
        __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x, coordinates.y, k * __EXTERNAL__.image[path].width, k * __EXTERNAL__.image[path].height);
        return null;
    });
    Effect.loadAudio = (path) => IO(() => {
        __EXTERNAL__.audio[path] = new Audio(path);
        __EXTERNAL__.audio[path].onerror = () => THROW(`Could not load audio: '${path}'`);
        return null;
    });
    Effect.playAudio = (path) => IO(() => ((__EXTERNAL__.audio[path] || THROW(`Audio not preloaded: '${path}'`)).play(), null));
    Effect.playSFX = (path) => IO(() => ((__EXTERNAL__.audio[path] || THROW(`Audio not preloaded: '${path}'`)).cloneNode().play(), null));
    Effect.loadFont = (path) => IO(() => {
        document.styleSheets[0].insertRule(`@font-face{font-family:"${path.slice(path.lastIndexOf("/") + 1, path.lastIndexOf("."))}";src:url("${path}")}`);
        return null;
    });
    Effect.clearRectangle = (x) => (y) => (w) => (h) => IO(() => (__EXTERNAL__.context.clearRect(x, y, w, h), null));
    Effect.clearRectangleVector = (coordinates) => (dimensions) => IO(() => (__EXTERNAL__.context.clearRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null));
    Effect.clearCanvas = IO(() => {
        __EXTERNAL__.context.clearRect(0, 0, __EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height);
        return null;
    });
    Effect.fill = IO(() => (__EXTERNAL__.context.fill(), null));
    Effect.stroke = IO(() => (__EXTERNAL__.context.stroke(), null));
    Effect.save = IO(() => (__EXTERNAL__.context.save(), null));
    Effect.restore = IO(() => (__EXTERNAL__.context.restore(), null));
    Effect.clipEvenOdd = IO(() => (__EXTERNAL__.context.clip('evenodd'), null));
    Effect.clipNonZero = IO(() => (__EXTERNAL__.context.clip('nonzero'), null));
    Effect.rotate = (angle) => IO(() => (__EXTERNAL__.context.rotate(angle), null));
    Effect.scale = (k) => IO(() => (__EXTERNAL__.context.scale(k, k), null));
    Effect.scaleAxis = (kx) => (ky) => IO(() => (__EXTERNAL__.context.scale(kx, ky), null));
    Effect.scaleAxisVector = (v) => IO(() => (__EXTERNAL__.context.scale(v.x, v.y), null));
    Effect.translate = (dx) => (dy) => IO(() => (__EXTERNAL__.context.translate(dx, dy), null));
    Effect.translateVector = (v) => IO(() => {
        __EXTERNAL__.context.translate(v.x, v.y);
        return null;
    });
    Effect.transformation = (m) => IO(() => {
        __EXTERNAL__.context.transform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky);
        return null;
    });
    Effect.beginPath = IO(() => (__EXTERNAL__.context.beginPath(), null));
    Effect.closePath = IO(() => (__EXTERNAL__.context.closePath(), null));
    Effect.moveTo = (x) => (y) => IO(() => (__EXTERNAL__.context.moveTo(x, y), null));
    Effect.moveToVector = (v) => IO(() => (__EXTERNAL__.context.moveTo(v.x, v.y), null));
    Effect.lineTo = (x) => (y) => IO(() => (__EXTERNAL__.context.lineTo(x, y), null));
    Effect.lineToVector = (v) => IO(() => (__EXTERNAL__.context.lineTo(v.x, v.y), null));
    Effect.bezierCurveTo = (ix) => (iy) => (jx) => (jy) => (x) => (y) => IO(() => (__EXTERNAL__.context.bezierCurveTo(ix, iy, jx, jy, x, y), null));
    Effect.bezierCurveToVector = (i) => (j) => (v) => IO(() => (__EXTERNAL__.context.bezierCurveTo(i.x, i.y, j.x, j.y, v.x, v.y), null));
    Effect.quadraticCurveTo = (ix) => (iy) => (x) => (y) => IO(() => (__EXTERNAL__.context.quadraticCurveTo(ix, iy, x, y), null));
    Effect.quadraticCurveToVector = (i) => (v) => IO(() => (__EXTERNAL__.context.quadraticCurveTo(i.x, i.y, v.x, v.y), null));
    Effect.arcTo = (ix) => (iy) => (jx) => (jy) => (r) => IO(() => (__EXTERNAL__.context.arcTo(ix, iy, jx, jy, r), null));
    Effect.arcToVector = (i) => (j) => (r) => IO(() => (__EXTERNAL__.context.arcTo(i.x, i.y, j.x, j.y, r), null));
    Effect.rectangle = (x) => (y) => (w) => (h) => IO(() => (__EXTERNAL__.context.rect(x, y, w, h), null));
    Effect.rectangleVector = (coordinates) => (dimensions) => IO(() => (__EXTERNAL__.context.rect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null));
    Effect.fillRectangle = (x) => (y) => (w) => (h) => IO(() => (__EXTERNAL__.context.fillRect(x, y, w, h), null));
    Effect.fillRectangleVector = (coordinates) => (dimensions) => IO(() => (__EXTERNAL__.context.fillRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null));
    Effect.strokeRectangle = (x) => (y) => (w) => (h) => IO(() => (__EXTERNAL__.context.strokeRect(x, y, w, h), null));
    Effect.strokeRectangleVector = (coordinates) => (dimensions) => IO(() => (__EXTERNAL__.context.strokeRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null));
    Effect.arc = (r) => (x) => (y) => (a) => (b) => IO(() => (__EXTERNAL__.context.arc(x, y, r, a, b), null));
    Effect.arcVector = (r) => (v) => (a) => (b) => IO(() => (__EXTERNAL__.context.arc(v.x, v.y, r, a, b), null));
    Effect.circle = (r) => (x) => (y) => IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), null));
    Effect.circleVector = (r) => (coordinates) => IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), null));
    Effect.strokeCircle = (r) => (x) => (y) => IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.stroke(), null));
    Effect.strokeCircleVector = (r) => (coordinates) => IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.stroke(), null));
    Effect.fillCircle = (r) => (x) => (y) => IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.fill(), null));
    Effect.fillCircleVector = (r) => (coordinates) => IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.fill(), null));
    Effect.elliptic = (x) => (y) => (kx) => (ky) => (a) => (b) => (r) => IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, a, b), null));
    Effect.ellipticVector = (coordinates) => (dimensions) => (a) => (b) => (r) => IO(() => (__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, a, b), null));
    Effect.ellipse = (x) => (y) => (kx) => (ky) => (r) => IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), null));
    Effect.ellipseVector = (coordinates) => (dimensions) => (r) => IO(() => (__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU), null));
    Effect.strokeEllipse = (x) => (y) => (kx) => (ky) => (r) => IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), __EXTERNAL__.context.stroke(), null));
    Effect.strokeEllipseVector = (coordinates) => (dimensions) => (r) => IO(() => {
        __EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU);
        __EXTERNAL__.context.stroke();
        return null;
    });
    Effect.fillEllipse = (x) => (y) => (kx) => (ky) => (r) => IO(() => (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), __EXTERNAL__.context.fill(), null));
    Effect.fillEllipseVector = (coordinates) => (dimensions) => (r) => IO(() => {
        __EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU);
        __EXTERNAL__.context.fill();
        return null;
    });
    Effect.strokeText = (text) => (x) => (y) => IO(() => (__EXTERNAL__.context.strokeText(text, x, y), null));
    Effect.strokeTextVector = (text) => (coordinates) => IO(() => (__EXTERNAL__.context.strokeText(text, coordinates.x, coordinates.y), null));
    Effect.fillText = (text) => (x) => (y) => IO(() => (__EXTERNAL__.context.fillText(text, x, y), null));
    Effect.fillTextVector = (text) => (coordinates) => IO(() => (__EXTERNAL__.context.fillText(text, coordinates.x, coordinates.y), null));
    Effect.area = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.rect(ix, iy, jx - ix, jy - iy), null));
    Effect.areaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.rect(i.x, i.y, j.x - i.x, j.y - i.y), null));
    Effect.strokeArea = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.strokeRect(ix, iy, jx - ix, jy - iy), null));
    Effect.strokeAreaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.strokeRect(i.x, i.y, j.x - i.x, j.y - i.y), null));
    Effect.fillArea = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.fillRect(ix, iy, jx - ix, jy - iy), null));
    Effect.fillAreaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.fillRect(i.x, i.y, j.x - i.x, j.y - i.y), null));
})(Effect || (Effect = {}));
onload = () => {
    __EXTERNAL__.context = document.querySelector('canvas').getContext('2d');
    onkeydown = event => {
        if (!event.repeat)
            __EXTERNAL__.keyboard[event.code] = Vertical.Downward;
    };
    onkeyup = event => {
        __EXTERNAL__.keyboard[event.code] = Vertical.Upward;
    };
    onmousemove = event => {
        __EXTERNAL__.mouse.windowX = event.x;
        __EXTERNAL__.mouse.windowY = event.y;
        __EXTERNAL__.mouse.canvasX = event.clientX - __EXTERNAL__.context.canvas.offsetLeft;
        __EXTERNAL__.mouse.canvasY = event.clientY - __EXTERNAL__.context.canvas.offsetTop;
        __EXTERNAL__.mouse.screenX = event.screenX;
        __EXTERNAL__.mouse.screenY = event.screenY;
        __EXTERNAL__.mouse.deltaX = event.movementX;
        __EXTERNAL__.mouse.deltaY = event.movementY;
    };
    onmousedown = event => {
        __EXTERNAL__.mouse.buttons[event.button] = Vertical.Downward;
    };
    onmouseup = event => {
        __EXTERNAL__.mouse.buttons[event.button] = Vertical.Upward;
    };
    onwheel = event => {
        if (event.deltaY < 0)
            __EXTERNAL__.mouse.scroll = Vertical.Up;
        else
            (event.deltaY > 0);
        __EXTERNAL__.mouse.scroll = Vertical.Down;
    };
    onresize = () => {
        clearTimeout(__EXTERNAL__.resizeID);
        __EXTERNAL__.resizeID =
            setTimeout(() => { __EXTERNAL__.isResized = true; }, 250);
    };
    document.onpointerlockchange = () => {
        __EXTERNAL__.isPointerLocked = document.pointerLockElement === __EXTERNAL__.context.canvas;
    };
};
