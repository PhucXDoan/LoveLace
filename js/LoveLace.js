"use strict";
const THROW = (message) => { throw new Error(message); };
const MAXARRAY = 1024;
const MAXSTRING = 1024;
const MAX_LIST_OPS = 1024;
const ERROR = {
    MAX_LIST_OPS: (op, org) => THROW(`'${op}' reached the max amount of traversal (${MAX_LIST_OPS}) allowed in a list ${org ? `| origin : '${org}'` : ''}`),
    BINDING_NILS: (org, op) => THROW(`'(${op})' was used on a infinite list with an operation that always return a nil | origin : '${org}'`),
    ONLY_INTEGER: (org, n) => THROW(`'${org}' only accepts integers as an amount; instead received '${n}'`),
    ONLY_NATURAL: (org, n) => THROW(`'${org}' only accepts natural numbers (0 inclusive); instead received '${n}'`),
    ONLY_CONS: (org) => THROW(`'${org}' only accepts non-empty lists`)
};
var X;
(function (X) {
    X["L"] = "X.L :: X";
    X["LL"] = "X.LL :: X";
    X["Rest"] = "X.Rest :: X";
    X["R"] = "X.R :: X";
    X["RR"] = "X.RR :: X";
})(X || (X = {}));
var Y;
(function (Y) {
    Y["D"] = "Y.D :: Y";
    Y["DD"] = "Y.DD :: Y";
    Y["Rest"] = "Y.Rest :: Y";
    Y["U"] = "Y.U :: Y";
    Y["UU"] = "Y.UU :: Y";
})(Y || (Y = {}));
var Z;
(function (Z) {
    Z["B"] = "Z.B :: Z";
    Z["BB"] = "Z.BB :: Z";
    Z["Rest"] = "Z.Rest :: Z";
    Z["F"] = "Z.F :: Z";
    Z["FF"] = "Z.FF :: Z";
})(Z || (Z = {}));
var LineCap;
(function (LineCap) {
    LineCap["Butt"] = "LineCap.Butt :: LineCap";
    LineCap["Round"] = "LineCap.Round :: LineCap";
    LineCap["Square"] = "LineCap.Square :: LineCap";
})(LineCap || (LineCap = {}));
var LineJoin;
(function (LineJoin) {
    LineJoin["Round"] = "LineJoin.Round :: LineJoin";
    LineJoin["Bevel"] = "LineJoin.Bevel :: LineJoin";
    LineJoin["Miter"] = "LineJoin.Miter :: LineJoin";
})(LineJoin || (LineJoin = {}));
var TextAlign;
(function (TextAlign) {
    TextAlign["Start"] = "TextAlign.Start :: TextAlign";
    TextAlign["End"] = "TextAlign.End :: TextAlign";
    TextAlign["Leftside"] = "TextAlign.Leftside :: TextAlign";
    TextAlign["Rightside"] = "TextAlign.Rightside :: TextAlign";
    TextAlign["Center"] = "TextAlign.Center :: TextAlign";
})(TextAlign || (TextAlign = {}));
var TextBaseline;
(function (TextBaseline) {
    TextBaseline["Top"] = "TextBaseline.Top :: TextBaseline";
    TextBaseline["Hanging"] = "TextBaseline.Hanging :: TextBaseline";
    TextBaseline["Middle"] = "TextBaseline.Middle :: TextBaseline";
    TextBaseline["Alphabetic"] = "TextBaseline.Alphabetic :: TextBaseline";
    TextBaseline["Ideographic"] = "TextBaseline.Ideographic :: TextBaseline";
    TextBaseline["Bottom"] = "TextBaseline.Bottom :: TextBaseline";
})(TextBaseline || (TextBaseline = {}));
var Composition;
(function (Composition) {
    Composition["SourceOver"] = "Composition.SourceOver :: Composition";
    Composition["SourceAtop"] = "Composition.SourceAtop :: Composition";
    Composition["SourceIn"] = "Composition.SourceIn :: Composition";
    Composition["SourceOut"] = "Composition.SourceOut :: Composition";
    Composition["DestinationOver"] = "Composition.DestinationOver :: Composition";
    Composition["DestinationAtop"] = "Composition.DestinationAtop :: Composition";
    Composition["DestinationIn"] = "Composition.DestinationIn :: Composition";
    Composition["DestinationOut"] = "Composition.DestinationOut :: Composition";
    Composition["Lighter"] = "Composition.Lighter :: Composition";
    Composition["Xor"] = "Composition.Xor :: Composition";
    Composition["Copy"] = "Composition.Copy :: Composition";
    Composition["Multiply"] = "Composition.Multiply :: Composition";
    Composition["Screen"] = "Composition.Screen :: Composition";
    Composition["Overlay"] = "Composition.Overlay :: Composition";
    Composition["Darken"] = "Composition.Darken :: Composition";
    Composition["Lighten"] = "Composition.Lighten :: Composition";
    Composition["ColorDodge"] = "Composition.ColorDodge :: Composition";
    Composition["ColorBurn"] = "Composition.ColorBurn :: Composition";
    Composition["HardLight"] = "Composition.HardLight :: Composition";
    Composition["SoftLight"] = "Composition.SoftLight :: Composition";
    Composition["Difference"] = "Composition.Difference :: Composition";
    Composition["Exclusion"] = "Composition.Exclusion :: Composition";
    Composition["Hue"] = "Composition.Hue :: Composition";
    Composition["Saturation"] = "Composition.Saturation :: Composition";
    Composition["Color"] = "Composition.Color :: Composition";
    Composition["Luminosity"] = "Composition.Luminosity :: Composition";
})(Composition || (Composition = {}));
const pipe = (value) => (morphism) => value.pipe(morphism);
const eq = (x) => (y) => x.eq(y);
const neq = (x) => (y) => !x.eq(y);
const fst = (pair) => pair.fst;
const snd = (pair) => pair.snd;
const link = (firsts) => (seconds) => firsts.link(seconds);
const head = (xs) => xs.head;
const tail = (xs) => xs.tail;
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
const clz32 = Math.clz32;
const cos = Math.cos;
const cosh = Math.cosh;
const diff = (x) => (y) => Math.abs(x - y);
const div = (x) => (y) => x / y;
const rdiv = (y) => (x) => x / y;
const even = (x) => x % 2 === 0;
const exp = Math.exp;
const expm1 = Math.expm1;
const flip = (f) => (y) => (x) => f(x)(y);
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
const isOutsideInclusive = (n) => (lower) => (upper) => n <= lower || upper <= n;
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
const NOR = (x) => (y) => ~(x | y);
const nor = (x) => (y) => !(x || y);
const NOT = (x) => ~x;
const not = (x) => !x;
const odd = (x) => Math.abs(x) % 2 === 1;
const OR = (x) => (y) => x | y;
const or = (x) => (y) => x || y;
const pow = (x) => (y) => Math.pow(x, y);
const rpow = (y) => (x) => Math.pow(x, y);
const pythagoras = (x) => (y) => Math.sqrt(x * x + y * y);
const reciprocate = (x) => 1 / x;
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
const toHexColor = (decimal) => `#${((~~Math.abs(decimal)) % 16777216).toString(16).padStart(6, '0')}`;
const trunc = (x) => ~~x;
const URSHIFT = (x) => (y) => x >>> y;
const rURSHIFT = (y) => (x) => x >>> y;
const XOR = (x) => (y) => x ^ y;
const xor = (x) => (y) => x !== y;
Boolean.prototype.pipe = Number.prototype.pipe = (String.prototype.pipe = function (f) { return f(this); });
Boolean.prototype.eq = Number.prototype.eq = (String.prototype.eq = function (x) { return this === x; });
const Pair = (first, second) => ({
    CONS: 'Pair',
    pipe(f) { return f(this); },
    eq: x => x.fst.eq(first) && x.snd.eq(second),
    fst: first,
    snd: second
});
const IO = (sideeffect) => ({
    CONS: 'IO',
    INFO: sideeffect,
    pipe(f) { return f(this); },
    bind: f => IO(() => f(sideeffect()).INFO()),
    fmap: f => IO(() => f(sideeffect())),
    bindto: (k, f) => IO(() => {
        const $ = sideeffect();
        return Object.assign(Object.assign({}, $), { [k]: f($).INFO() });
    }),
    fmapto: (k, f) => IO(() => {
        const $ = sideeffect();
        return Object.assign(Object.assign({}, $), { [k]: f($) });
    }),
    then: x => IO(() => (sideeffect(), x.INFO())),
    side: x => IO(() => {
        const y = sideeffect();
        x.INFO();
        return y;
    }),
    also: f => IO(() => {
        const y = sideeffect();
        f(y).INFO();
        return y;
    }),
    cast: x => IO(() => (sideeffect(), x))
});
const Nothing = {
    CONS: 'Nothing',
    pipe: f => f(Nothing),
    eq: x => x === Nothing,
    bind: _ => Nothing,
    fmap: _ => Nothing,
    bindto: _ => Nothing,
    fmapto: _ => Nothing,
    cast: _ => Nothing
};
const Just = (value) => ({
    CONS: 'Just',
    INFO: value,
    pipe(f) { return f(this); },
    eq: x => x.CONS === 'Just' && x.INFO.eq(value),
    bind: f => f(value),
    fmap: f => Just(f(value)),
    bindto: (k, f) => f(value).fmap(x => (Object.assign(Object.assign({}, value), { [k]: x }))),
    fmapto: (k, f) => Just(Object.assign(Object.assign({}, value), { [k]: f(value) })),
    cast: Just
});
const Process = (computation) => ({
    CONS: 'Process',
    INFO: computation,
    pipe(f) { return f(this); },
    bind: f => Process(s => {
        const x = computation(s);
        return f(x.snd).INFO(x.fst);
    }),
    fmap: f => Process(s => {
        const x = computation(s);
        return Pair(x.fst, f(x.snd));
    }),
    bindto: (k, f) => Process(s => {
        const x = computation(s), y = f(x.snd).INFO(x.fst);
        return Pair(y.fst, Object.assign(Object.assign({}, x.snd), { [k]: y.snd }));
    }),
    fmapto: (k, f) => Process(s => {
        const x = computation(s);
        return Pair(x.fst, Object.assign(Object.assign({}, x.snd), { [k]: f(x.snd) }));
    }),
    then: x => Process(s => x.INFO(computation(s).fst)),
    side: x => Process(s => {
        const y = computation(s);
        return Pair(x.INFO(y.fst).fst, y.snd);
    }),
    also: f => Process(s => {
        const y = computation(s);
        return Pair(f(y.snd).INFO(y.fst).fst, y.snd);
    }),
    cast: x => Process(s => Pair(computation(s).fst, x))
});
const Nil = {
    CONS: 'Nil',
    pipe: f => f(Nil),
    eq: xs => xs === Nil,
    bind: _ => Nil,
    fmap: _ => Nil,
    bindto: _ => Nil,
    fmapto: _ => Nil,
    cast: _ => Nil,
    link: xs => xs,
    get head() { return THROW(`'(.head)' cannot be used on an empty list`); },
    get tail() { return THROW(`'(.tail)' cannot be used on an empty list`); }
};
const Cons = (lfirst) => (lrest) => ({
    CONS: 'Cons',
    pipe(f) { return f(this); },
    eq(xs) {
        let ys = this;
        for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
            if (i === MAX_LIST_OPS)
                ERROR.MAX_LIST_OPS('(.eq)', 'Cons');
            else if (!xs.head.eq(ys.head))
                return false;
        return xs.CONS === ys.CONS;
    },
    bind(f) {
        const xs = f(this.head);
        return xs.CONS === 'Nil'
            ? this.tail.bind(f)
            : Cons(() => xs.head)(() => xs.tail.link(this.tail.bind(f)));
    },
    fmap(f) {
        return Cons(() => f(this.head))(() => this.tail.fmap(f));
    },
    bindto(k, f) {
        return this.bind($ => f($).fmap(x => (Object.assign(Object.assign({}, $), { [k]: x }))));
    },
    fmapto(k, f) {
        return this.fmap($ => (Object.assign(Object.assign({}, $), { [k]: f($) })));
    },
    cast(x) {
        return this.fmap(_ => x);
    },
    link(xs) {
        return xs.CONS === 'Nil'
            ? this
            : Cons(() => this.head)(() => this.tail.link(xs));
    },
    get head() { var _a; return (_a = this.$head) !== null && _a !== void 0 ? _a : (this.$head = lfirst()); },
    get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = lrest()); }
});
const List = (...elements) => {
    let xs = Nil;
    for (let i = elements.length - 1; ~i; --i)
        xs = prepend(elements[i])(xs);
    return xs;
};
const Left = (lefty) => ({
    CONS: 'Left',
    INFO: lefty,
    pipe(f) { return f(this); },
    eq: x => x.CONS === 'Left' && x.INFO.eq(lefty)
});
const Right = (righty) => ({
    CONS: 'Right',
    INFO: righty,
    pipe(f) { return f(this); },
    eq: x => x.CONS === 'Right' && x.INFO.eq(righty)
});
const Vector2 = (x, y) => ({
    CONS: 'Vector2',
    eq: v => v.x === x && v.y === y,
    pipe(f) { return f(this); },
    x, y
});
const Vector3 = (x, y, z) => ({
    CONS: 'Vector3',
    eq: v => v.x === x && v.y === y && v.z === z,
    pipe(f) { return f(this); },
    x, y, z
});
const Vector4 = (x, y, z, w) => ({
    CONS: 'Vector4',
    eq: v => v.x === x && v.y === y && v.z === z && v.w === w,
    pipe(f) { return f(this); },
    x, y, z, w
});
const Matrix2 = (ix, jx, iy, jy) => ({
    CONS: 'Matrix2',
    pipe(f) { return f(this); },
    eq: m => m.ix === ix && m.jx === jx &&
        m.iy === iy && m.jy === jy,
    ix, jx, iy, jy
});
const Matrix3 = (ix, jx, kx, iy, jy, ky, iz, jz, kz) => ({
    CONS: 'Matrix3',
    pipe(f) { return f(this); },
    eq: m => m.ix === ix && m.jx === jx && m.kx === kx &&
        m.iy === iy && m.jy === jy && m.ky === ky &&
        m.iz === iz && m.jz === jz && m.kz === kz,
    ix, jx, kx, iy, jy, ky, iz, jz, kz
});
const Matrix4 = (ix, jx, kx, lx, iy, jy, ky, ly, iz, jz, kz, lz, iw, jw, kw, lw) => ({
    CONS: 'Matrix4',
    pipe(f) { return f(this); },
    eq: m => m.ix === ix && m.jx === jx && m.kx === kx && m.lx === lx &&
        m.iy === iy && m.jy === jy && m.ky === ky && m.ly === ly &&
        m.iz === iz && m.jz === jz && m.kz === kz && m.lz === lz &&
        m.iw === iw && m.jw === jw && m.kw === kw && m.lw === lw,
    ix, jx, kx, lx, iy, jy, ky, ly, iz, jz, kz, lz, iw, jw, kw, lw
});
const TextMeasurement = (text) => (width) => (height) => ({
    CONS: 'TextMeasurement',
    pipe(f) { return f(this); },
    eq: m => m.text === text && m.width === width && m.height === height,
    text, width, height
});
const Mapping = (...pairs) => ({
    CONS: 'Mapping',
    codomain: x => {
        var _a;
        return ((_a = pairs.find(p => p[0].eq(x))) !== null && _a !== void 0 ? _a : THROW(`'(.codomain)' was non-exhaustive in 'Mapping'; no corresponding codomain for value '${x}'`))[1];
    },
    domain: x => {
        var _a;
        return ((_a = pairs.find(p => p[1].eq(x))) !== null && _a !== void 0 ? _a : THROW(`'(.domain)' was non-exhaustive in 'Mapping'; no corresponding domain for value '${x}'`))[0];
    }
});
const curry = (f) => (first) => (second) => f(Pair(first, second));
const uncurry = (f) => (parameters) => f(parameters.fst)(parameters.snd);
const swap = (pair) => Pair(pair.snd, pair.fst);
const ffst = (morphism) => (pair) => Pair(morphism(pair.fst), pair.snd);
const fsnd = (morphism) => (pair) => Pair(pair.fst, morphism(pair.snd));
const fboth = (morphism) => (pair) => Pair(morphism(pair.fst), morphism(pair.snd));
const pick = (bool) => bool ? fst : snd;
const idle = IO(() => null);
const when = (condition) => (io) => condition ? io.cast(null) : idle;
const isNothing = (maybe) => maybe.CONS === 'Nothing';
const isJust = (maybe) => maybe.CONS === 'Just';
const ffromMaybe = (fallback) => (morphism) => (maybe) => maybe.CONS === 'Nothing'
    ? fallback
    : morphism(maybe.INFO);
const fromJust = (maybe) => maybe.CONS === 'Nothing'
    ? THROW(`'fromJust' cannot be used on 'Nothing'`)
    : maybe.INFO;
const fromMaybe = (fallback) => (maybe) => maybe.CONS === 'Nothing'
    ? fallback
    : maybe.INFO;
const put = (replacement) => (process) => Process(s => Pair(replacement, process.INFO(s).snd));
const get = (process) => Process(s => {
    const x = process.INFO(s).fst;
    return Pair(x, x);
});
const runProcess = (process) => (state) => process.INFO(state);
const execProcess = (process) => (state) => process.INFO(state).fst;
const evalProcess = (process) => (state) => process.INFO(state).snd;
const mapProcess = (morphism) => (process) => Process(s => morphism(process.INFO(s)));
const endomapState = (endomorphism) => (process) => Process(s => {
    const x = process.INFO(s);
    return Pair(endomorphism(x.fst), x.snd);
});
const isNil = (xs) => xs.CONS === 'Nil';
const isCons = (xs) => xs.CONS === 'Cons';
const array = (xs) => {
    const ys = [];
    for (let i = 0; xs.CONS === 'Cons'; ++i, ys.push(xs.head), xs = xs.tail)
        if (i === MAXARRAY) {
            console.warn(`'array' has reached the maximum array representation possible (${MAXARRAY}) for the given list`);
            break;
        }
    return ys;
};
const string = (xs) => {
    let str = "";
    for (let i = 0; xs.CONS === 'Cons'; ++i, str += xs.head)
        if (i === MAXSTRING) {
            console.warn(`'string' has reached the maximum string representation possible (${MAXSTRING}) for the given list`);
            break;
        }
    return str;
};
const chars = (str) => str
    ?
        ({
            CONS: 'Cons',
            pipe(f) { return f(this); },
            eq(xs) {
                for (let i = 0; i < str.length && xs.CONS === 'Cons'; ++i, xs = xs.tail)
                    if (xs.head !== str[i])
                        return false;
                return xs.CONS === 'Nil';
            },
            bind(f) {
                const xs = f(str[0]);
                return xs.CONS === 'Nil'
                    ? this.tail.bind(f)
                    : Cons(() => xs.head)(() => xs.tail.link(this.tail.bind(f)));
            },
            fmap(f) {
                return Cons(() => f(str[0]))(() => this.tail.fmap(f));
            },
            bindto: _ => THROW(`'(.bindto)' was used on a list of characters; likely done on accident | origin : 'chars'`),
            fmapto: _ => THROW(`'(.fmapto)' was used on a list of characters; likely done on accident | origin : 'chars'`),
            cast(x) {
                return lprepend(x)(() => this.tail.cast(x));
            },
            link(xs) {
                return lprepend(str[0])(() => this.tail.link(xs));
            },
            head: str[0],
            get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = chars(str.slice(1))); },
            $head: str[0],
            $last: str[str.length - 1],
            $len: str.length
        })
    : Nil;
const singleton = (value) => ({
    CONS: 'Cons',
    pipe(f) { return f(this); },
    eq: xs => xs.CONS === 'Cons' && xs.tail.CONS === 'Nil' && xs.head.eq(value),
    bind: f => f(value),
    fmap: f => singleton(f(value)),
    bindto: (k, f) => f(value).fmap(x => (Object.assign(Object.assign({}, value), { [k]: x }))),
    fmapto: (k, f) => singleton(Object.assign(Object.assign({}, value), { [k]: f(value) })),
    cast: singleton,
    link: prepend(value),
    head: value,
    tail: Nil,
    $head: value,
    $tail: Nil,
    $last: value,
    $init: Nil,
    get $reverse() { return this; },
    $len: 1
});
const prepend = (first) => (rest) => ({
    CONS: 'Cons',
    pipe(f) { return f(this); },
    eq: xs => {
        if (xs.CONS === 'Nil' || !xs.head.eq(first))
            return false;
        let ys = rest;
        xs = xs.tail;
        for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
            if (i === MAX_LIST_OPS)
                ERROR.MAX_LIST_OPS('(.eq)', 'prepend');
            else if (!xs.head.eq(ys.head))
                return false;
        return xs.CONS === ys.CONS;
    },
    bind: f => {
        const xs = f(first);
        return xs.CONS === 'Nil'
            ? rest.bind(f)
            : Cons(() => xs.head)(() => xs.tail.link(rest.bind(f)));
    },
    fmap: f => Cons(() => f(first))(() => rest.fmap(f)),
    bindto: (k, f) => {
        const xs = f(first).fmap(x => (Object.assign(Object.assign({}, first), { [k]: x })));
        return xs.CONS === 'Nil'
            ? rest.bindto(k, f)
            : Cons(() => xs.head)(() => xs.tail.link(rest.bindto(k, f)));
    },
    fmapto: (k, f) => Cons(() => (Object.assign(Object.assign({}, first), { [k]: f(first) })))(() => rest.fmapto(k, f)),
    cast: x => lprepend(x)(() => rest.cast(x)),
    link: xs => lprepend(first)(() => rest.link(xs)),
    head: first,
    tail: rest,
    $head: first,
    $tail: rest,
    $last: rest.CONS === 'Nil' ? first : rest.$last,
    $len: rest.$len === undefined ? undefined : 1 + rest.$len
});
const lprepend = (first) => (lrest) => ({
    CONS: 'Cons',
    pipe(f) { return f(this); },
    eq(xs) {
        if (xs.CONS === 'Nil' || !xs.head.eq(first))
            return false;
        let ys = this.tail;
        xs = xs.tail;
        for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
            if (i === MAX_LIST_OPS)
                ERROR.MAX_LIST_OPS('(.eq)', 'lprepend');
            else if (!xs.head.eq(ys.head))
                return false;
        return xs.CONS === ys.CONS;
    },
    bind(f) {
        const xs = f(first);
        return xs.CONS === 'Nil'
            ? this.tail.bind(f)
            : Cons(() => xs.head)(() => xs.tail.link(this.tail.bind(f)));
    },
    fmap(f) {
        return Cons(() => f(first))(() => this.tail.fmap(f));
    },
    bindto(k, f) {
        const xs = f(first).fmap(x => (Object.assign(Object.assign({}, first), { [k]: x })));
        return xs.CONS === 'Nil'
            ? this.tail.bindto(k, f)
            : Cons(() => xs.head)(() => xs.tail.link(this.tail.bindto(k, f)));
    },
    fmapto(k, f) {
        return Cons(() => (Object.assign(Object.assign({}, first), { [k]: f(first) })))(() => this.tail.fmapto(k, f));
    },
    cast(x) {
        return lprepend(x)(() => this.tail.cast(x));
    },
    link(xs) {
        return lprepend(first)(() => this.tail.link(xs));
    },
    head: first,
    get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = lrest()); },
    $head: first
});
const llprepend = (lfirst) => (rest) => ({
    CONS: 'Cons',
    pipe(f) { return f(this); },
    eq(xs) {
        if (xs.CONS === 'Nil' || !xs.head.eq(this.head))
            return false;
        let ys = rest;
        xs = xs.tail;
        for (let i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
            if (i === MAX_LIST_OPS)
                ERROR.MAX_LIST_OPS('(.eq)', 'llprepend');
            else if (!xs.head.eq(ys.head))
                return false;
        return xs.CONS === ys.CONS;
    },
    bind(f) {
        const xs = f(this.head);
        return xs.CONS === 'Nil'
            ? rest.bind(f)
            : Cons(() => xs.head)(() => xs.tail.link(rest.bind(f)));
    },
    fmap(f) {
        return Cons(() => f(this.head))(() => rest.fmap(f));
    },
    bindto(k, f) {
        const xs = f(this.head).fmap(x => (Object.assign(Object.assign({}, this.head), { [k]: x })));
        return xs.CONS === 'Nil'
            ? rest.bindto(k, f)
            : Cons(() => xs.head)(() => xs.tail.link(rest.bindto(k, f)));
    },
    fmapto(k, f) {
        return Cons(() => (Object.assign(Object.assign({}, this.head), { [k]: f(this.head) })))(() => rest.fmapto(k, f));
    },
    cast(x) {
        return lprepend(x)(() => rest.cast(x));
    },
    link(xs) {
        return Cons(() => this.head)(() => rest.link(xs));
    },
    get head() { var _a; return (_a = this.$head) !== null && _a !== void 0 ? _a : (this.$head = lfirst()); },
    tail: rest,
    $tail: rest,
    $last: rest.$last,
    $len: rest.$len === undefined ? undefined : 1 + rest.$len
});
const repeat = (value) => ({
    CONS: 'Cons',
    pipe(f) { return f(this); },
    eq: xs => {
        for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
            if (i === MAX_LIST_OPS)
                ERROR.MAX_LIST_OPS('(.eq)', 'repeat');
            else if (!xs.head.eq(value))
                return false;
        return false;
    },
    bind: f => {
        const xs = f(value);
        return xs.CONS === 'Nil'
            ? ERROR.BINDING_NILS('repeat', 'bind')
            : cycle(xs);
    },
    fmap: f => repeat(f(value)),
    bindto: (k, f) => {
        const xs = f(value).fmap(x => (Object.assign(Object.assign({}, value), { [k]: x })));
        return xs.CONS === 'Nil'
            ? ERROR.BINDING_NILS('repeat', 'bindto')
            : cycle(xs);
    },
    fmapto: (k, f) => repeat(Object.assign(Object.assign({}, value), { [k]: f(value) })),
    link(_) { return this; },
    cast: repeat,
    head: value,
    get tail() { return this; },
    $head: value,
    get $tail() { return this; },
    get $init() { return this; }
});
const cycle = (pattern) => pattern.CONS === 'Nil'
    ? ERROR.ONLY_CONS('cycle')
    :
        ({
            CONS: 'Cons',
            pipe(f) { return f(this); },
            eq(xs) {
                let ys = this;
                for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
                    if (i === MAX_LIST_OPS)
                        ERROR.MAX_LIST_OPS('(.eq)', 'cycle');
                    else if (!xs.head.eq(ys.head))
                        return false;
                return false;
            },
            bind(f) {
                const xs = pattern.bind(f);
                return xs.CONS === 'Nil'
                    ? ERROR.BINDING_NILS('cycle', 'bind')
                    : cycle(xs);
            },
            fmap: f => cycle(pattern.fmap(f)),
            bindto(k, f) {
                const xs = pattern.bind($ => f($).fmap(x => (Object.assign(Object.assign({}, $), { [k]: x }))));
                return xs.CONS === 'Nil'
                    ? ERROR.BINDING_NILS('cycle', 'bindto')
                    : cycle(xs);
            },
            fmapto: (k, f) => cycle(pattern.fmap($ => (Object.assign(Object.assign({}, $), { [k]: f($) })))),
            link(_) { return this; },
            cast: repeat,
            get head() { var _a; return (_a = this.$head) !== null && _a !== void 0 ? _a : (this.$head = pattern.head); },
            get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = pattern.tail.link(this)); },
            get $init() { return this; }
        });
const iterate = (endomorphism) => (initial) => ({
    CONS: 'Cons',
    pipe(f) { return f(this); },
    eq(xs) {
        let ys = this;
        for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
            if (i === MAX_LIST_OPS)
                ERROR.MAX_LIST_OPS('(.eq)', 'iterate');
            else if (!xs.head.eq(ys.head))
                return false;
        return false;
    },
    bind(f) {
        const xs = f(initial);
        return xs.CONS === 'Nil'
            ? this.tail.bind(f)
            : Cons(() => xs.head)(() => xs.tail.link(this.tail.bind(f)));
    },
    fmap(f) {
        return Cons(() => f(initial))(() => this.tail.fmap(f));
    },
    bindto(k, f) {
        const xs = f(initial).fmap(x => (Object.assign(Object.assign({}, initial), { [k]: x })));
        return xs.CONS === 'Nil'
            ? this.tail.bindto(k, f)
            : Cons(() => xs.head)(() => xs.tail.link(this.tail.bindto(k, f)));
    },
    fmapto(k, f) {
        return Cons(() => (Object.assign(Object.assign({}, initial), { [k]: f(initial) })))(() => this.fmapto(k, f));
    },
    link(_) { return this; },
    cast: repeat,
    head: initial,
    get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = iterate(endomorphism)(endomorphism(initial))); },
    $head: initial,
    get $init() { return this; }
});
const replicate = (amount) => (value) => Number.isInteger(value)
    ? amount > 0
        ?
            ({
                CONS: 'Cons',
                pipe(f) { return f(this); },
                eq: xs => {
                    for (let i = 0; i < amount; ++i, xs = xs.tail)
                        if (xs.CONS === 'Nil' || !xs.head.eq(value))
                            return false;
                    return xs.CONS === 'Nil';
                },
                bind: f => concat(replicate(amount)(f(value))),
                fmap: f => replicate(amount)(f(value)),
                bindto: (k, f) => concat(replicate(amount)(Object.assign(Object.assign({}, value), { [k]: f(value) }))),
                fmapto: (k, f) => replicate(amount)(Object.assign(Object.assign({}, value), { [k]: f(value) })),
                cast: replicate(amount),
                link(xs) {
                    return xs.CONS === 'Nil'
                        ? this
                        : lprepend(value)(() => this.tail.link(xs));
                },
                head: value,
                get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = replicate(amount - 1)(value)); },
                $head: value,
                $last: value,
                get $init() { return this.tail; },
                get $reverse() { return this; },
                $len: amount
            })
        : Nil
    : ERROR.ONLY_INTEGER('replicate', amount);
const all = (predicate) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons'; ++i)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('all');
        else if (predicate(xs.head))
            xs = xs.tail;
        else
            return false;
    return true;
};
const any = (predicate) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('any');
        else if (predicate(xs.head))
            return true;
    return false;
};
const at = (index) => (xs) => {
    if (index < 0 || !Number.isInteger(index))
        ERROR.ONLY_NATURAL('at', index);
    for (let i = 0; i < index; ++i)
        if (xs.CONS === 'Nil')
            THROW(`'at' received an index beyond the list; stopped at index '${i}' with goal of '${index}'`);
        else
            xs = xs.tail;
    if (xs.CONS === 'Nil')
        THROW(`'at' received an off-by-one error; cannot get index '${index}' in list of length ${index}`);
    return xs.head;
};
const concat = (xss) => xss.CONS === 'Nil'
    ? Nil
    : xss.head.CONS === 'Nil'
        ? concat(xss.tail)
        : Cons(() => xss.head.head)(() => xss.head.tail.link(concat(xss.tail)));
const drop = (amount) => (xs) => {
    if (!Number.isInteger(amount))
        ERROR.ONLY_INTEGER('drop', amount);
    for (let i = 0; i < amount && xs.CONS === 'Cons'; ++i)
        xs = xs.tail;
    return xs;
};
const dropWhile = (predicate) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons' && predicate(xs.head); ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('dropWhile');
    return xs;
};
const elem = (value) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('elem');
        else if (xs.head.eq(value))
            return true;
    return false;
};
const elemIndices = (value) => (xs) => xs.CONS === 'Nil'
    ? Nil
    : xs.head.eq(value)
        ? lprepend(0)(() => elemIndices(value)(xs.tail).fmap(x => x + 1))
        : elemIndices(value)(xs.tail).fmap(x => x + 1);
const filter = (predicate) => (xs) => xs.CONS === 'Nil'
    ? Nil
    : predicate(xs.head)
        ? lprepend(xs.head)(() => filter(predicate)(xs.tail))
        : filter(predicate)(xs.tail);
const findIndices = (predicate) => (xs) => xs.CONS === 'Nil'
    ? Nil
    : predicate(xs.head)
        ? lprepend(0)(() => findIndices(predicate)(xs.tail).fmap(x => x + 1))
        : findIndices(predicate)(xs.tail).fmap(x => x + 1);
const foldl = (reducer) => (initial) => (xs) => {
    let x = initial;
    for (let i = 0; xs.CONS === 'Cons'; ++i, x = reducer(x)(xs.head), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldl');
    return x;
};
const foldl1 = (reducer) => (xs) => {
    if (xs.CONS === 'Nil')
        ERROR.ONLY_CONS('foldl1');
    let x = xs.head;
    for (let i = 0; (xs = xs.tail).CONS === 'Cons'; ++i, x = reducer(x)(xs.head))
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldl1');
    return x;
};
const foldr = (reducer) => (initial) => (xs) => {
    xs = reverse(xs);
    let x = initial;
    for (let i = 0; xs.CONS === 'Cons'; ++i, x = reducer(xs.head)(x), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldr');
    return x;
};
const foldr1 = (reducer) => (xs) => {
    if (xs.CONS === 'Nil')
        ERROR.ONLY_CONS('foldr1');
    xs = reverse(xs);
    let x = xs.head;
    for (let i = 0; (xs = xs.tail).CONS === 'Cons'; ++i, x = reducer(xs.head)(x))
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldr1');
    return x;
};
const init = (xs) => {
    var _a;
    return ((_a = xs.$init) !== null && _a !== void 0 ? _a : xs.CONS === 'Nil')
        ? ERROR.ONLY_CONS('init')
        : xs.tail.CONS === 'Nil'
            ? Nil
            : Cons(() => xs.head)(() => init(xs.tail));
};
const inits = (xs) => xs.CONS === 'Nil'
    ? singleton(Nil)
    : lprepend(Nil)(() => inits(xs.tail).fmap(llprepend(() => xs.head)));
const intersperese = (delimiter) => (xs) => xs.CONS === 'Nil'
    ? Nil
    : xs.tail.CONS === 'Nil'
        ? xs
        : llprepend(() => xs.head)(lprepend(delimiter)(() => intersperese(delimiter)(xs.tail)));
const last = (xs) => {
    if (xs.$last !== undefined)
        return xs.$last;
    if (xs.CONS === 'Nil')
        ERROR.ONLY_CONS('last');
    if (xs.tail.CONS === 'Nil')
        return xs.head;
    for (let i = 0; (xs = xs.tail).tail.CONS === 'Cons'; ++i)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('last');
    return xs.head;
};
const len = (xs) => {
    if (xs.$len !== undefined)
        return xs.$len;
    let i = 0;
    while (xs.CONS === 'Cons')
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('len');
        else
            ++i, xs = xs.tail;
    return i;
};
const map = (morphism) => (xs) => xs.fmap(morphism);
const nelem = (value) => (xs) => !elem(value)(xs);
const partition = (predicate) => (xs) => {
    let ys = Nil;
    let zs = Nil;
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('partition');
        else if (predicate(xs.head))
            ys = prepend(xs.head)(ys);
        else
            zs = prepend(xs.head)(zs);
    return Pair(reverse(ys), reverse(zs));
};
const reverse = (xs) => {
    if (xs.$reverse !== undefined)
        return xs.$reverse;
    let ys = Nil;
    for (let i = 0; xs.CONS === 'Cons'; ++i, ys = prepend(xs.head)(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('reverse');
    return ys;
};
const scanl = (reducer) => (initial) => (xs) => xs.CONS === 'Nil'
    ? singleton(initial)
    : lprepend(initial)(() => scanl(reducer)(reducer(initial)(xs.head))(xs.tail));
const scanl1 = (reducer) => (xs) => xs.CONS === 'Nil'
    ? Nil
    : scanl(reducer)(xs.head)(xs.tail);
const scanr = (reducer) => (initial) => (xs) => {
    xs = reverse(xs);
    let ys = singleton(initial);
    for (let i = 0; xs.CONS === 'Cons'; ++i, ys = prepend(reducer(xs.head)(ys.head))(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('scanr');
    return ys;
};
const scanr1 = (reducer) => (xs) => {
    xs = reverse(xs);
    let ys = singleton(xs.head);
    for (let i = 0; (xs = xs.tail).CONS === 'Cons'; ++i, ys = prepend(reducer(xs.head)(ys.head))(ys))
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('scanr1');
    return ys;
};
const span = (predicate) => (xs) => {
    let ys = Nil;
    for (let i = 0; xs.CONS === 'Cons' && predicate(xs.head); ++i, ys = prepend(xs.head)(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('span');
    return Pair(reverse(ys), xs);
};
const splitAt = (amount) => (xs) => {
    let ys = Nil;
    for (let i = 0; i < amount && xs.CONS === 'Cons'; ++i, ys = prepend(xs.head)(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('splitAt');
    return Pair(reverse(ys), xs);
};
const tails = (xs) => xs.CONS === 'Nil'
    ? singleton(Nil)
    : lprepend(xs)(() => tails(xs.tail));
const take = (amount) => (xs) => Number.isInteger(amount)
    ? amount > 0
        ? Cons(() => xs.head)(() => take(amount - 1)(xs.tail))
        : Nil
    : ERROR.ONLY_INTEGER('take', amount);
const takeWhile = (predicate) => (xs) => xs.CONS === 'Cons' && predicate(xs.head)
    ? lprepend(xs.head)(() => takeWhile(predicate)(xs.tail))
    : Nil;
const unzip = (xs) => Pair(xs.fmap(fst), xs.fmap(snd));
const unzipWith = (f) => (xs) => unzip(xs.fmap(f));
const zip = (xs) => (ys) => xs.CONS === 'Nil' || ys.CONS === 'Nil'
    ? Nil
    : Cons(() => Pair(xs.head, ys.head))(() => zip(xs.tail)(ys.tail));
const zipWith = (zipper) => (xs) => (ys) => xs.CONS === 'Nil' || ys.CONS === 'Nil'
    ? Nil
    : Cons(() => zipper(xs.head)(ys.head))(() => zipWith(zipper)(xs.tail)(ys.tail));
const isLeft = (either) => either.CONS === 'Left';
const isRight = (either) => either.CONS === 'Right';
const eitherway = (lf) => (rf) => (either) => either.CONS === 'Left'
    ? lf(either.INFO)
    : rf(either.INFO);
const onLeft = (lf) => (either) => either.CONS === 'Left'
    ? Left(lf(either.INFO))
    : either;
const onRight = (rf) => (either) => either.CONS === 'Right'
    ? Right(rf(either.INFO))
    : either;
const haveLeft = (fallback) => (either) => either.CONS === 'Left'
    ? either.INFO
    : fallback;
const haveRight = (fallback) => (either) => either.CONS === 'Right'
    ? either.INFO
    : fallback;
const fromLeft = (either) => either.CONS === 'Left'
    ? either.INFO
    : THROW(`'fromLeft' was used on a right-value`);
const fromRight = (either) => either.CONS === 'Right'
    ? either.INFO
    : THROW(`'fromRight' was used on a left-value`);
const V2 = {
    origin: Vector2(0, 0),
    translate: (dx) => (dy) => (v) => Vector2(v.x + dx, v.y + dy),
    untranslate: (dx) => (dy) => (v) => Vector2(v.x - dx, v.y - dy),
    add: (v) => (w) => Vector2(v.x + w.x, v.y + w.y),
    sub: (v) => (w) => Vector2(v.x - w.x, v.y - w.y),
    scale: (k) => (v) => Vector2(v.x * k, v.y * k),
    unscale: (k) => (v) => Vector2(v.x / k, v.y / k),
    norm: (v) => Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2)),
    normalize: (v) => v.eq(V2.origin)
        ? V2.origin
        : V2.unscale(Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2)))(v),
    dot: (v) => (w) => v.x * w.x + v.y * w.y,
    transform: (m) => (v) => Vector2(m.ix * v.x + m.jx * v.y, m.iy * v.x + m.jy * v.y)
};
const V3 = {
    origin: Vector3(0, 0, 0),
    translate: (dx) => (dy) => (dz) => (v) => Vector3(v.x + dx, v.y + dy, v.z + dz),
    untranslate: (dx) => (dy) => (dz) => (v) => Vector3(v.x - dx, v.y - dy, v.z - dz),
    add: (v) => (w) => Vector3(v.x + w.x, v.y + w.y, v.z + w.z),
    sub: (v) => (w) => Vector3(v.x - w.x, v.y - w.y, v.z - w.z),
    scale: (k) => (v) => Vector3(v.x * k, v.y * k, v.z * k),
    unscale: (k) => (v) => Vector3(v.x / k, v.y / k, v.z / k),
    norm: (v) => Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2)),
    normalize: (v) => v.eq(V3.origin)
        ? V3.origin
        : V3.unscale(Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2)))(v),
    dot: (v) => (w) => v.x * w.x + v.y * w.y + v.z * w.z,
    cross: (v) => (w) => Vector3(v.y * w.z - v.z * w.y, v.z * w.x - v.x * w.z, v.x * w.y - v.y * w.x),
    transform: (m) => (v) => Vector3(m.ix * v.x + m.jx * v.y + m.kx * v.z, m.iy * v.x + m.jy * v.y + m.ky * v.z, m.iz * v.x + m.jz * v.y + m.kz * v.z)
};
const V4 = {
    origin: Vector4(0, 0, 0, 0),
    translate: (dx) => (dy) => (dz) => (dw) => (v) => Vector4(v.x + dx, v.y + dy, v.z + dz, v.w + dw),
    untranslate: (dx) => (dy) => (dz) => (dw) => (v) => Vector4(v.x - dx, v.y - dy, v.z - dz, v.w - dw),
    add: (v) => (w) => Vector4(v.x + w.x, v.y + w.y, v.z + w.z, v.w + w.w),
    sub: (v) => (w) => Vector4(v.x - w.x, v.y - w.y, v.z - w.z, v.w - w.w),
    scale: (k) => (v) => Vector4(v.x * k, v.y * k, v.z * k, v.w * k),
    unscale: (k) => (v) => Vector4(v.x / k, v.y / k, v.z / k, v.w / k),
    norm: (v) => Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2) + Math.pow(v.w, 2)),
    normalize: (v) => v.eq(V4.origin)
        ? V4.origin
        : V4.unscale(Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2) + Math.pow(v.w, 2)))(v),
    dot: (v) => (w) => v.x * w.x + v.y * w.y + v.z * w.z + v.w * w.w,
    transform: (m) => (v) => Vector4(m.ix * v.x + m.jx * v.y + m.kx * v.z + m.lx * v.w, m.iy * v.x + m.jy * v.y + m.ky * v.z + m.ly * v.w, m.iz * v.x + m.jz * v.y + m.kz * v.z + m.lz * v.w, m.iw * v.x + m.jw * v.y + m.kw * v.z + m.lw * v.w)
};
const M2 = {
    id: Matrix2(1, 0, 0, 1),
    fromBasis: (i, j) => Matrix2(i.x, j.x, i.y, j.y),
    mul: (m) => (n) => Matrix2(m.ix * n.ix + m.jx * n.iy, m.ix * n.jx + m.jx * n.jy, m.iy * n.ix + m.jy * n.iy, m.iy * n.jx + m.jy * n.jy)
};
const M3 = {
    id: Matrix3(1, 0, 0, 0, 1, 0, 0, 0, 1),
    fromBasis: (i, j, k) => Matrix3(i.x, j.x, k.x, i.y, j.y, k.y, i.z, j.z, k.z),
    mul: (m) => (n) => Matrix3(m.ix * n.ix + m.jx * n.iy + m.kx * n.iz, m.ix * n.jx + m.jx * n.jy + m.kx * n.jz, m.ix * n.kx + m.jx * n.ky + m.kx * n.kz, m.iy * n.ix + m.jy * n.iy + m.ky * n.iz, m.iy * n.jx + m.jy * n.jy + m.ky * n.jz, m.iy * n.kx + m.jy * n.ky + m.ky * n.kz, m.iz * n.ix + m.jz * n.iy + m.kz * n.iz, m.iz * n.jx + m.jz * n.jy + m.kz * n.jz, m.iz * n.kx + m.jz * n.ky + m.kz * n.kz)
};
const M4 = {
    id: Matrix4(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1),
    fromBasis: (i, j, k, l) => Matrix4(i.x, j.x, k.x, l.x, i.y, j.y, k.y, l.y, i.z, j.z, k.z, l.z, i.w, j.w, k.w, l.w),
    mul: (m) => (n) => Matrix4(m.ix * n.ix + m.jx * n.iy + m.kx * n.iz + m.lx * n.iw, m.ix * n.jx + m.jx * n.jy + m.kx * n.jz + m.lx * n.jw, m.ix * n.kx + m.jx * n.ky + m.kx * n.kz + m.lx * n.kw, m.ix * n.lx + m.jx * n.ly + m.kx * n.lz + m.lx * n.lw, m.iy * n.ix + m.jy * n.iy + m.ky * n.iz + m.ly * n.iw, m.iy * n.jx + m.jy * n.jy + m.ky * n.jz + m.ly * n.jw, m.iy * n.kx + m.jy * n.ky + m.ky * n.kz + m.ly * n.kw, m.iy * n.lx + m.jy * n.ly + m.ky * n.lz + m.ly * n.lw, m.iz * n.ix + m.jz * n.iy + m.kz * n.iz + m.lz * n.iw, m.iz * n.jx + m.jz * n.jy + m.kz * n.jz + m.lz * n.jw, m.iz * n.kx + m.jz * n.ky + m.kz * n.kz + m.lz * n.kw, m.iz * n.lx + m.jz * n.ly + m.kz * n.lz + m.lz * n.lw, m.iw * n.ix + m.jw * n.iy + m.kw * n.iz + m.lw * n.iw, m.iw * n.jx + m.jw * n.jy + m.kw * n.jz + m.lw * n.jw, m.iw * n.kx + m.jw * n.ky + m.kw * n.kz + m.lw * n.kw, m.iw * n.lx + m.jw * n.ly + m.kw * n.lz + m.lw * n.lw)
};
const sequenceIOs = (ios) => IO(() => ios.fmap(io => io.INFO()));
const executeIOs = (ios) => IO(() => {
    for (let i = ios; i.CONS === 'Cons'; i = i.tail)
        i.head.INFO();
    return null;
});
const maybeHead = (xs) => xs.CONS === 'Nil'
    ? Nothing
    : Just(xs.head);
const maybeLast = (xs) => xs.CONS === 'Nil'
    ? Nothing
    : Just(last(xs));
const maybeTail = (xs) => xs.CONS === 'Nil'
    ? Nothing
    : Just(xs.tail);
const maybeInit = (xs) => xs.CONS === 'Nil'
    ? Nothing
    : Just(init(xs));
const fromMaybes = (maybes) => maybes.CONS === 'Nil'
    ? Nil
    : maybes.head.CONS === 'Nothing'
        ? fromMaybes(maybes.tail)
        : lprepend(maybes.head.INFO)(() => fromMaybes(maybes.tail));
const mapMaybe = (reaction) => (xs) => fromMaybes(xs.fmap(reaction));
const maybeSingleton = (maybe) => maybe.CONS === 'Nothing'
    ? Nil
    : singleton(maybe.INFO);
const maybeAt = (index) => (xs) => {
    if (index < 0 || !Number.isInteger)
        return Nothing;
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === index)
            return Just(xs.head);
    return Nothing;
};
const find = (predicate) => (xs) => {
    while (xs.CONS === 'Cons')
        if (predicate(xs.head))
            return Just(xs.head);
        else
            xs = xs.tail;
    return Nothing;
};
const elemIndex = (value) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (xs.head.eq(value))
            return Just(i);
    return Nothing;
};
const findIndex = (predicate) => (xs) => {
    for (let i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (predicate(xs.head))
            return Just(i);
    return Nothing;
};
const lefts = (eithers) => eithers.CONS === 'Nil'
    ? Nil
    : eithers.head.CONS === 'Left'
        ? lprepend(eithers.head.INFO)(() => lefts(eithers.tail))
        : lefts(eithers.tail);
const rights = (eithers) => eithers.CONS === 'Nil'
    ? Nil
    : eithers.head.CONS === 'Right'
        ? lprepend(eithers.head.INFO)(() => rights(eithers.tail))
        : rights(eithers.tail);
const relaxX = (direction) => direction === X.LL ? X.L :
    direction === X.RR ? X.R :
        direction;
const relaxY = (direction) => direction === Y.DD ? Y.D :
    direction === Y.UU ? Y.U :
        direction;
const relaxZ = (direction) => direction === Z.BB ? Z.B :
    direction === Z.FF ? Z.F :
        direction;
const isGoingLeft = (direction) => direction === X.L || direction === X.LL;
const isGoingRight = (direction) => direction === X.R || direction === X.RR;
const isGoingDown = (direction) => direction === Y.D || direction === Y.DD;
const isGoingUp = (direction) => direction === Y.U || direction === Y.UU;
const isGoingBack = (direction) => direction === Z.B || direction === Z.BB;
const isGoingFor = (direction) => direction === Z.F || direction === Z.FF;
const mappingLineCap = Mapping([LineCap.Butt, 'butt'], [LineCap.Round, 'round'], [LineCap.Square, 'square']);
const mappingLineJoin = Mapping([LineJoin.Round, 'round'], [LineJoin.Bevel, 'bevel'], [LineJoin.Miter, 'miter']);
const mappingTextAlign = Mapping([TextAlign.Center, 'center'], [TextAlign.End, 'end'], [TextAlign.Leftside, 'left'], [TextAlign.Rightside, 'right'], [TextAlign.Start, 'start']);
const mappingTextBaseline = Mapping([TextBaseline.Alphabetic, 'alphabetic'], [TextBaseline.Bottom, 'bottom'], [TextBaseline.Hanging, 'hanging'], [TextBaseline.Ideographic, 'ideographic'], [TextBaseline.Middle, 'middle'], [TextBaseline.Top, 'top']);
const mappingComposition = Mapping([Composition.SourceOver, 'source-over'], [Composition.SourceIn, 'source-in'], [Composition.SourceOut, 'source-out'], [Composition.SourceAtop, 'source-atop'], [Composition.DestinationOver, 'destination-over'], [Composition.DestinationIn, 'destination-in'], [Composition.DestinationOut, 'destination-out'], [Composition.DestinationAtop, 'destination-atop'], [Composition.Lighter, 'lighter'], [Composition.Copy, 'copy'], [Composition.Xor, 'xor'], [Composition.Multiply, 'multiply'], [Composition.Screen, 'screen'], [Composition.Overlay, 'overlay'], [Composition.Darken, 'darken'], [Composition.Lighten, 'lighten'], [Composition.ColorDodge, 'color-dodge'], [Composition.ColorBurn, 'color-burn'], [Composition.HardLight, 'hard-light'], [Composition.SoftLight, 'soft-light'], [Composition.Difference, 'difference'], [Composition.Exclusion, 'exclusion'], [Composition.Hue, 'hue'], [Composition.Saturation, 'saturation'], [Composition.Color, 'color'], [Composition.Luminosity, 'luminosity']);
const unit = {
    IO: (outcome) => IO(() => outcome),
    Maybe: Just,
    Process: (output) => Process(s => Pair(s, output)),
    List: singleton
};
const Do = {
    IO: unit.IO(Object.create(null)),
    Maybe: unit.Maybe(Object.create(null)),
    Process: unit.Process(Object.create(null)),
    List: unit.List(Object.create(null))
};
const KEYBOARD = [
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
const λ = {
    ctx: undefined,
    resizeID: undefined,
    isResized: false,
    isPointerLocked: false,
    seed: (Math.random() - 0.5) * Date.now(),
    image: Object.create(null),
    audio: Object.create(null),
    mouseScreenX: 0, mouseScreenY: 0,
    mouseWindowX: 0, mouseWindowY: 0,
    mouseCanvasX: 0, mouseCanvasY: 0,
    mouseDeltaX: 0, mouseDeltaY: 0,
    mouseScroll: Y.Rest,
    mouseButtons: Array(5).fill(Y.U),
    keyboard: KEYBOARD.reduce(($, k) => (Object.assign(Object.assign({}, $), { [k]: Y.U })), Object.create(null))
};
const Input = {
    Norm: {
        mouseCanvasX: IO(() => λ.mouseCanvasX / λ.ctx.canvas.width),
        mouseCanvasY: IO(() => λ.mouseCanvasY / λ.ctx.canvas.height),
        mouseCanvasP: IO(() => Pair(λ.mouseCanvasX / λ.ctx.canvas.width, λ.mouseCanvasY / λ.ctx.canvas.height)),
        mouseCanvasV: IO(() => Vector2(λ.mouseCanvasX / λ.ctx.canvas.width, λ.mouseCanvasY / λ.ctx.canvas.height)),
        mouseDeltaX: IO(() => λ.mouseDeltaX / λ.ctx.canvas.width),
        mouseDeltaY: IO(() => λ.mouseDeltaY / λ.ctx.canvas.height),
        mouseDeltaP: IO(() => Pair(λ.mouseDeltaX / λ.ctx.canvas.width, λ.mouseDeltaY / λ.ctx.canvas.height)),
        mouseDeltaV: IO(() => Vector2(λ.mouseDeltaX / λ.ctx.canvas.width, λ.mouseDeltaY / λ.ctx.canvas.height)),
        lineThickness: IO(() => λ.ctx.lineWidth / λ.ctx.canvas.width),
        lineDashPattern: IO(() => List(...λ.ctx.getLineDash().map(x => x / λ.ctx.canvas.width))),
        lineDashOffset: IO(() => λ.ctx.lineDashOffset / λ.ctx.canvas.width),
        fontSize: IO(() => parseFloat(λ.ctx.font) / λ.ctx.canvas.width),
        shadowOffsetX: IO(() => λ.ctx.shadowOffsetX / λ.ctx.canvas.width),
        shadowOffsetY: IO(() => λ.ctx.shadowOffsetY / λ.ctx.canvas.height),
        shadowOffsetP: IO(() => Pair(λ.ctx.shadowOffsetX / λ.ctx.canvas.width, λ.ctx.shadowOffsetY / λ.ctx.canvas.height)),
        shadowOffsetV: IO(() => Vector2(λ.ctx.shadowOffsetX / λ.ctx.canvas.width, λ.ctx.shadowOffsetY / λ.ctx.canvas.height)),
        transformationMatrix: IO(() => {
            const m = λ.ctx.getTransform();
            return Matrix3(m.a, m.c, m.e / λ.ctx.canvas.width, m.b, m.d, m.f / λ.ctx.canvas.height, 0, 0, 1);
        })
    },
    isWindowResized: IO(() => λ.isResized),
    isPointerLocked: IO(() => λ.isPointerLocked),
    seed: unit.IO(λ.seed),
    screenW: IO(() => screen.width),
    screenH: IO(() => screen.height),
    screenP: IO(() => Pair(screen.width, screen.height)),
    screenV: IO(() => Vector2(screen.width, screen.height)),
    windowW: IO(() => innerWidth),
    windowH: IO(() => innerHeight),
    windowP: IO(() => Pair(innerWidth, innerHeight)),
    windowV: IO(() => Vector2(innerWidth, innerHeight)),
    canvasW: IO(() => λ.ctx.canvas.width),
    canvasH: IO(() => λ.ctx.canvas.height),
    canvasP: IO(() => Pair(λ.ctx.canvas.width, λ.ctx.canvas.height)),
    canvasV: IO(() => Vector2(λ.ctx.canvas.width, λ.ctx.canvas.height)),
    mouseScreenX: IO(() => λ.mouseScreenX),
    mouseScreenY: IO(() => λ.mouseScreenY),
    mouseScreenP: IO(() => Pair(λ.mouseScreenX, λ.mouseScreenY)),
    mouseScreenV: IO(() => Vector2(λ.mouseScreenX, λ.mouseScreenY)),
    mouseWindowX: IO(() => λ.mouseWindowX),
    mouseWindowY: IO(() => λ.mouseWindowY),
    mouseWindowP: IO(() => Pair(λ.mouseWindowX, λ.mouseWindowY)),
    mouseWindowV: IO(() => Vector2(λ.mouseWindowX, λ.mouseWindowY)),
    mouseCanvasX: IO(() => λ.mouseCanvasX),
    mouseCanvasY: IO(() => λ.mouseCanvasY),
    mouseCanvasP: IO(() => Pair(λ.mouseCanvasX, λ.mouseCanvasY)),
    mouseCanvasV: IO(() => Vector2(λ.mouseCanvasX, λ.mouseCanvasY)),
    mouseDeltaX: IO(() => λ.mouseDeltaX),
    mouseDeltaY: IO(() => λ.mouseDeltaY),
    mouseDeltaP: IO(() => Pair(λ.mouseDeltaX, λ.mouseDeltaY)),
    mouseDeltaV: IO(() => Vector2(λ.mouseDeltaX, λ.mouseDeltaY)),
    mouseScroll: IO(() => λ.mouseScroll),
    mouseButtonLeft: IO(() => λ.mouseButtons[0]),
    mouseButtonMiddle: IO(() => λ.mouseButtons[1]),
    mouseButtonRight: IO(() => λ.mouseButtons[2]),
    mouseButtonEsotericX: IO(() => λ.mouseButtons[3]),
    mouseButtonEsotericY: IO(() => λ.mouseButtons[4]),
    keyboard: (key) => IO(() => λ.keyboard[key]),
    time: IO(Date.now),
    textMeasurement: (text) => IO(() => {
        const metrics = λ.ctx.measureText(text);
        return TextMeasurement(text)(Math.abs(metrics.actualBoundingBoxLeft) + Math.abs(metrics.actualBoundingBoxRight))(Math.abs(metrics.actualBoundingBoxAscent) + Math.abs(metrics.actualBoundingBoxDescent));
    }),
    lineThickness: IO(() => λ.ctx.lineWidth),
    lineCap: IO(() => mappingLineCap.domain(λ.ctx.lineCap)),
    lineJoin: IO(() => mappingLineJoin.domain(λ.ctx.lineJoin)),
    lineDashPattern: IO(() => List(...λ.ctx.getLineDash())),
    lineDashOffset: IO(() => λ.ctx.lineDashOffset),
    miterLimit: IO(() => λ.ctx.miterLimit),
    font: IO(() => λ.ctx.font),
    fontSize: IO(() => parseFloat(λ.ctx.font)),
    fontFamily: IO(() => λ.ctx.font.slice(λ.ctx.font.indexOf(" ") + 1)),
    textAlign: IO(() => mappingTextAlign.domain(λ.ctx.textAlign)),
    textBaseline: IO(() => mappingTextBaseline.domain(λ.ctx.textBaseline)),
    shadowBlurAmount: IO(() => λ.ctx.shadowBlur),
    shadowColor: IO(() => λ.ctx.shadowColor),
    shadowOffsetX: IO(() => λ.ctx.shadowOffsetX),
    shadowOffsetY: IO(() => λ.ctx.shadowOffsetY),
    shadowOffsetP: IO(() => Pair(λ.ctx.shadowOffsetX, λ.ctx.shadowOffsetY)),
    shadowOffsetV: IO(() => Vector2(λ.ctx.shadowOffsetX, λ.ctx.shadowOffsetY)),
    isInEvenOddPathP: (x) => (y) => IO(() => λ.ctx.isPointInPath(x, y, 'evenodd')),
    isInEvenOddPathV: (v) => IO(() => λ.ctx.isPointInPath(v.x, v.y, 'evenodd')),
    isInNonZeroPathP: (x) => (y) => IO(() => λ.ctx.isPointInPath(x, y, 'nonzero')),
    isInNonZeroPathV: (v) => IO(() => λ.ctx.isPointInPath(v.x, v.y, 'nonzero')),
    isInStrokeP: (x) => (y) => IO(() => λ.ctx.isPointInStroke(x, y)),
    isInStrokeV: (v) => IO(() => λ.ctx.isPointInStroke(v.x, v.y)),
    transformationMatrix: IO(() => {
        const m = λ.ctx.getTransform();
        return Matrix3(m.a, m.c, m.e, m.b, m.d, m.f, 0, 0, 1);
    }),
    alpha: IO(() => λ.ctx.globalAlpha),
    composition: IO(() => mappingComposition.domain(λ.ctx.globalCompositeOperation))
};
const Reput = {
    Norm: {
        canvasW: (w) => IO(() => (λ.ctx.canvas.width = w * innerWidth, null)),
        canvasH: (h) => IO(() => (λ.ctx.canvas.height = h * innerHeight, null)),
        canvasWH: (w) => (h) => IO(() => (λ.ctx.canvas.width = w * innerWidth, λ.ctx.canvas.height = h * innerHeight, null)),
        canvasP: (p) => IO(() => (λ.ctx.canvas.width = p.fst * innerWidth, λ.ctx.canvas.height = p.snd * innerHeight, null)),
        canvasV: (v) => IO(() => (λ.ctx.canvas.width = v.x * innerWidth, λ.ctx.canvas.height = v.y * innerHeight, null)),
        lineThickness: (t) => IO(() => (λ.ctx.lineWidth = t * λ.ctx.canvas.width, null)),
        lineDashPattern: (pattern) => IO(() => (λ.ctx.setLineDash(array(pattern).map(x => x * λ.ctx.canvas.width)), null)),
        lineDashOffset: (offset) => IO(() => (λ.ctx.lineDashOffset = offset * λ.ctx.canvas.width, null)),
        fontSize: (size) => IO(() => (λ.ctx.font = `${size * λ.ctx.canvas.width}px${λ.ctx.font.slice(λ.ctx.font.indexOf(" "))}`, null)),
        fillRGBA: (r) => (g) => (b) => (a) => IO(() => (λ.ctx.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null)),
        fillRGBAV: (v) => IO(() => (λ.ctx.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null)),
        strokeRGBA: (r) => (g) => (b) => (a) => IO(() => (λ.ctx.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null)),
        strokeRGBAV: (v) => IO(() => (λ.ctx.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null)),
        shadowRGBA: (r) => (g) => (b) => (a) => IO(() => (λ.ctx.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`, null)),
        shadowRGBAV: (v) => IO(() => (λ.ctx.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`, null)),
        shadowOffsetX: (x) => IO(() => (λ.ctx.shadowOffsetX = x * λ.ctx.canvas.width, null)),
        shadowOffsetY: (y) => IO(() => (λ.ctx.shadowOffsetY = y * λ.ctx.canvas.height, null)),
        shadowOffsetXY: (x) => (y) => IO(() => (λ.ctx.shadowOffsetX = x * λ.ctx.canvas.width,
            λ.ctx.shadowOffsetY = y * λ.ctx.canvas.height,
            null)),
        shadowOffsetP: (p) => IO(() => (λ.ctx.shadowOffsetX = p.fst * λ.ctx.canvas.width,
            λ.ctx.shadowOffsetY = p.snd * λ.ctx.canvas.height,
            null)),
        shadowOffsetV: (v) => IO(() => (λ.ctx.shadowOffsetX = v.x * λ.ctx.canvas.width,
            λ.ctx.shadowOffsetY = v.y * λ.ctx.canvas.height,
            null)),
        transformationMatrix: (m) => IO(() => (λ.ctx.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx * λ.ctx.canvas.width, m.ky * λ.ctx.canvas.height),
            null))
    },
    canvasW: (w) => IO(() => (λ.ctx.canvas.width = w, null)),
    canvasH: (h) => IO(() => (λ.ctx.canvas.height = h, null)),
    canvasC: (w) => (h) => IO(() => (λ.ctx.canvas.width = w, λ.ctx.canvas.height = h, null)),
    canvasP: (p) => IO(() => (λ.ctx.canvas.width = p.fst, λ.ctx.canvas.height = p.snd, null)),
    canvasV: (v) => IO(() => (λ.ctx.canvas.width = v.x, λ.ctx.canvas.height = v.y, null)),
    lineThickness: (t) => IO(() => (λ.ctx.lineWidth = t, null)),
    lineCap: (cap) => IO(() => (λ.ctx.lineCap = mappingLineCap.codomain(cap), null)),
    lineJoin: (joining) => IO(() => (λ.ctx.lineJoin = mappingLineJoin.codomain(joining), null)),
    lineDashPattern: (pattern) => IO(() => (λ.ctx.setLineDash(array(pattern)), null)),
    lineDashOffset: (offset) => IO(() => (λ.ctx.lineDashOffset = offset, null)),
    miterLimit: (limit) => IO(() => (λ.ctx.miterLimit = limit, null)),
    font: (fontInfo) => IO(() => (λ.ctx.font = fontInfo, null)),
    fontSize: (size) => IO(() => (λ.ctx.font = `${size}px${λ.ctx.font.slice(λ.ctx.font.indexOf(" "))}`, null)),
    fontFamily: (family) => IO(() => (λ.ctx.font = `${parseFloat(λ.ctx.font)}px ${family}`, null)),
    textAlign: (align) => IO(() => (λ.ctx.textAlign = mappingTextAlign.codomain(align), null)),
    textBaseline: (baseline) => IO(() => (λ.ctx.textBaseline = mappingTextBaseline.codomain(baseline), null)),
    fillColor: (color) => IO(() => (λ.ctx.fillStyle = color, null)),
    fillRGBAC: (r) => (g) => (b) => (a) => IO(() => (λ.ctx.fillStyle = `rgba(${r},${g},${b},${a})`, null)),
    fillRGBAV: (v) => IO(() => (λ.ctx.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null)),
    strokeColor: (color) => IO(() => (λ.ctx.strokeStyle = color, null)),
    strokeRGBAC: (r) => (g) => (b) => (a) => IO(() => (λ.ctx.strokeStyle = `rgba(${r},${g},${b},${a})`, null)),
    strokeRGBAV: (v) => IO(() => (λ.ctx.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`, null)),
    shadowBlurAmount: (amount) => IO(() => (λ.ctx.shadowBlur = amount, null)),
    shadowColor: (color) => IO(() => (λ.ctx.shadowColor = color, null)),
    shadowRGBAC: (r) => (g) => (b) => (a) => IO(() => (λ.ctx.shadowColor = `rgba(${r},${g},${b},${a})`, null)),
    shadowRGBAV: (v) => IO(() => (λ.ctx.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`, null)),
    shadowOffsetX: (x) => IO(() => (λ.ctx.shadowOffsetX = x, null)),
    shadowOffsetY: (y) => IO(() => (λ.ctx.shadowOffsetY = y, null)),
    shadowOffsetC: (x) => (y) => IO(() => (λ.ctx.shadowOffsetX = x, λ.ctx.shadowOffsetY = y, null)),
    shadowOffsetV: (v) => IO(() => (λ.ctx.shadowOffsetX = v.x, λ.ctx.shadowOffsetY = v.y, null)),
    transformationMatrix: (m) => IO(() => (λ.ctx.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null)),
    alpha: (opacity) => IO(() => (λ.ctx.globalAlpha = opacity, null)),
    compositionOperation: (composition) => IO(() => (λ.ctx.globalCompositeOperation = mappingComposition.codomain(composition), null))
};
const Output = {
    Norm: {
        drawImage: (path) => (x) => (y) => IO(() => (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),
        drawImageP: (path) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        drawImageV: (path) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),
        drawCroppedImage: (path) => (cx) => (cy) => (cw) => (ch) => (x) => (y) => (w) => (h) => IO(() => (λ.ctx.drawImage(λ.image[path], cx * λ.image[path].width, cy * λ.image[path].height, cw * λ.image[path].width, ch * λ.image[path].height, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null)),
        drawCroppedImageP: (path) => (cxy) => (cwh) => (xy) => (wh) => IO(() => {
            λ.ctx.drawImage(λ.image[path], cxy.fst * λ.image[path].width, cxy.snd * λ.image[path].width, cwh.fst * λ.image[path].width, cwh.snd * λ.image[path].width, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height);
            return null;
        }),
        drawCroppedImageV: (path) => (cxy) => (cwh) => (xy) => (wh) => IO(() => (λ.ctx.drawImage(λ.image[path], cxy.x * λ.image[path].width, cxy.y * λ.image[path].width, cwh.x * λ.image[path].height, cwh.y * λ.image[path].height, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null)),
        drawFullImage: (path) => (x) => (y) => (w) => (h) => IO(() => (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null)),
        drawFullImageP: (path) => (xy) => (wh) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height), null)),
        drawFullImageV: (path) => (xy) => (wh) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null)),
        drawSquareImage: (path) => (k) => (x) => (y) => IO(() => (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, k, k), null)),
        drawSquareImageP: (path) => (k) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, k, k), null)),
        drawSquareImageV: (path) => (k) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, k, k), null)),
        drawScaledImage: (path) => (k) => (x) => (y) => IO(() => (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, λ.image[path].width * k, λ.image[path].height * k), null)),
        drawScaledImageP: (path) => (k) => (xy) => IO(() => {
            λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, λ.image[path].width * k, λ.image[path].height * k);
            return null;
        }),
        drawScaledImageV: (path) => (k) => (xy) => IO(() => {
            λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, λ.image[path].width * k, λ.image[path].height * k);
            return null;
        }),
        clearRectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.clearRect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.width, w * λ.ctx.canvas.width, h * λ.ctx.canvas.width), null)),
        clearRectangleP: (xy) => (wh) => IO(() => (λ.ctx.clearRect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        clearRectangleV: (xy) => (wh) => IO(() => (λ.ctx.clearRect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null)),
        rotate: (angle) => IO(() => (λ.ctx.rotate(angle * TAU), null)),
        scaleAxisP: (kxy) => IO(() => (λ.ctx.scale(kxy.fst * λ.ctx.canvas.width, kxy.snd * λ.ctx.canvas.height), null)),
        scaleAxisV: (kxy) => IO(() => (λ.ctx.scale(kxy.x * λ.ctx.canvas.width, kxy.y * λ.ctx.canvas.height), null)),
        translateX: (dx) => IO(() => (λ.ctx.translate(dx * λ.ctx.canvas.width, 0), null)),
        translateY: (dy) => IO(() => (λ.ctx.translate(0, dy * λ.ctx.canvas.height), null)),
        translate: (dx) => (dy) => IO(() => (λ.ctx.translate(dx * λ.ctx.canvas.width, dy * λ.ctx.canvas.height), null)),
        translateP: (dxy) => IO(() => (λ.ctx.translate(dxy.fst * λ.ctx.canvas.width, dxy.snd * λ.ctx.canvas.height), null)),
        translateV: (dxy) => IO(() => (λ.ctx.translate(dxy.x * λ.ctx.canvas.width, dxy.y * λ.ctx.canvas.height), null)),
        transformation: (m) => IO(() => (λ.ctx.transform(m.ix, m.iy, m.jx, m.jy, m.kx * λ.ctx.canvas.width, m.ky * λ.ctx.canvas.height),
            null)),
        moveTo: (x) => (y) => IO(() => (λ.ctx.moveTo(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),
        moveToP: (xy) => IO(() => (λ.ctx.moveTo(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        moveToV: (xy) => IO(() => (λ.ctx.moveTo(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),
        lineTo: (x) => (y) => IO(() => (λ.ctx.lineTo(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),
        lineToP: (xy) => IO(() => (λ.ctx.lineTo(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        lineToV: (xy) => IO(() => (λ.ctx.lineTo(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),
        bezierCurveTo: (cx0) => (cy0) => (cx1) => (cy1) => (x) => (y) => IO(() => (λ.ctx.bezierCurveTo(cx0 * λ.ctx.canvas.width, cy0 * λ.ctx.canvas.height, cx1 * λ.ctx.canvas.width, cy1 * λ.ctx.canvas.height, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height),
            null)),
        bezierCurveToP: (cxy0) => (cxy1) => (xy) => IO(() => (λ.ctx.bezierCurveTo(cxy0.fst * λ.ctx.canvas.width, cxy0.snd * λ.ctx.canvas.height, cxy1.fst * λ.ctx.canvas.width, cxy1.snd * λ.ctx.canvas.height, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        bezierCurveToV: (cxy0) => (cxy1) => (xy) => IO(() => (λ.ctx.bezierCurveTo(cxy0.x * λ.ctx.canvas.width, cxy0.y * λ.ctx.canvas.height, cxy1.x * λ.ctx.canvas.width, cxy1.y * λ.ctx.canvas.height, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),
        quadraticCurveTo: (cx) => (cy) => (x) => (y) => IO(() => (λ.ctx.quadraticCurveTo(cx * λ.ctx.canvas.width, cy * λ.ctx.canvas.width, x * λ.ctx.canvas.width, y * λ.ctx.canvas.width), null)),
        quadraticCurveToP: (cxy) => (xy) => IO(() => (λ.ctx.quadraticCurveTo(cxy.fst * λ.ctx.canvas.width, cxy.snd * λ.ctx.canvas.height, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        quadraticCurveToV: (cxy) => (xy) => IO(() => (λ.ctx.quadraticCurveTo(cxy.x * λ.ctx.canvas.width, cxy.y * λ.ctx.canvas.height, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),
        arcTo: (r) => (cx0) => (cy0) => (cx1) => (cy1) => IO(() => (λ.ctx.arcTo(cx0 * λ.ctx.canvas.width, cy0 * λ.ctx.canvas.height, cx1 * λ.ctx.canvas.width, cy1 * λ.ctx.canvas.height, r * λ.ctx.canvas.width), null)),
        arcToP: (r) => (cxy0) => (cxy1) => IO(() => (λ.ctx.arcTo(cxy0.fst * λ.ctx.canvas.width, cxy0.snd * λ.ctx.canvas.height, cxy1.fst * λ.ctx.canvas.width, cxy1.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width), null)),
        arcToV: (r) => (cxy0) => (cxy1) => IO(() => (λ.ctx.arcTo(cxy0.x * λ.ctx.canvas.width, cxy0.y * λ.ctx.canvas.height, cxy1.x * λ.ctx.canvas.width, cxy1.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width), null)),
        rectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.rect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null)),
        rectangleP: (xy) => (wh) => IO(() => (λ.ctx.rect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.width, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.width), null)),
        rectangleV: (xy) => (wh) => IO(() => (λ.ctx.rect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null)),
        fillRectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.fillRect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null)),
        fillRectangleP: (xy) => (wh) => IO(() => (λ.ctx.fillRect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height), null)),
        fillRectangleV: (xy) => (wh) => IO(() => (λ.ctx.fillRect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null)),
        strokeRectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.strokeRect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null)),
        strokeRectangleP: (xy) => (wh) => IO(() => (λ.ctx.strokeRect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height), null)),
        strokeRectangleV: (xy) => (wh) => IO(() => (λ.ctx.strokeRect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null)),
        arc: (r) => (a) => (b) => (x) => (y) => IO(() => (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null)),
        arcP: (r) => (a) => (b) => (xy) => IO(() => (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null)),
        arcV: (r) => (a) => (b) => (xy) => IO(() => (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * 360), null)),
        circle: (r) => (x) => (y) => IO(() => (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null)),
        circleP: (r) => (xy) => IO(() => (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null)),
        circleV: (r) => (xy) => IO(() => (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null)),
        strokeCircle: (r) => (x) => (y) => IO(() => (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null)),
        strokeCircleP: (r) => (xy) => IO(() => (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null)),
        strokeCircleV: (r) => (xy) => IO(() => (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null)),
        fillCircle: (r) => (x) => (y) => IO(() => (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.fill(), null)),
        fillCircleP: (r) => (xy) => IO(() => (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.height, 0, TAU), λ.ctx.fill(), null)),
        fillCircleV: (r) => (xy) => IO(() => (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.fill(), null)),
        elliptic: (r) => (a) => (b) => (x) => (y) => (kx) => (ky) => IO(() => (λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null)),
        ellipticP: (r) => (a) => (b) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null)),
        ellipticV: (r) => (a) => (b) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null)),
        ellipse: (r) => (x) => (y) => (kx) => (ky) => IO(() => (λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null)),
        ellipseP: (r) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null)),
        ellipseV: (r) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null)),
        strokeEllipse: (r) => (x) => (y) => (kx) => (ky) => IO(() => (λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null)),
        strokeEllipseP: (r) => (xy) => (wh) => IO(() => {
            λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
            λ.ctx.stroke();
            return null;
        }),
        strokeEllipseV: (r) => (xy) => (wh) => IO(() => {
            λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
            λ.ctx.stroke();
            return null;
        }),
        fillEllipse: (r) => (x) => (y) => (kx) => (ky) => IO(() => {
            λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
            λ.ctx.fill();
            return null;
        }),
        fillEllipseP: (r) => (xy) => (wh) => IO(() => {
            λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
            λ.ctx.fill();
            return null;
        }),
        fillEllipseV: (r) => (xy) => (wh) => IO(() => {
            λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
            λ.ctx.fill();
            return null;
        }),
        strokeText: (text) => (x) => (y) => IO(() => (λ.ctx.strokeText(text, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),
        strokeTextP: (text) => (xy) => IO(() => (λ.ctx.strokeText(text, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        strokeTextV: (text) => (xy) => IO(() => (λ.ctx.strokeText(text, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),
        fillText: (text) => (x) => (y) => IO(() => (λ.ctx.fillText(text, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null)),
        fillTextP: (text) => (xy) => IO(() => (λ.ctx.fillText(text, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null)),
        fillTextV: (text) => (xy) => IO(() => (λ.ctx.fillText(text, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null)),
        area: (x0) => (y0) => (x1) => (y1) => IO(() => {
            λ.ctx.rect(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height, (x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.height);
            return null;
        }),
        areaP: (xy0) => (xy1) => IO(() => {
            λ.ctx.rect(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height, (xy1.fst - xy0.fst) * λ.ctx.canvas.width, (xy1.snd - xy0.snd) * λ.ctx.canvas.height);
            return null;
        }),
        areaV: (xy0) => (xy1) => IO(() => {
            λ.ctx.rect(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height, (xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height);
            return null;
        }),
        strokeArea: (x0) => (y0) => (x1) => (y1) => IO(() => {
            λ.ctx.strokeRect(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height, (x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.width);
            return null;
        }),
        strokeAreaP: (xy0) => (xy1) => IO(() => {
            λ.ctx.strokeRect(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height, (xy1.fst - xy0.fst) * λ.ctx.canvas.width, (xy1.snd - xy0.snd) * λ.ctx.canvas.height);
            return null;
        }),
        strokeAreaV: (xy0) => (xy1) => IO(() => {
            λ.ctx.strokeRect(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height, (xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height);
            return null;
        }),
        fillArea: (x0) => (y0) => (x1) => (y1) => IO(() => {
            λ.ctx.fillRect(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height, (x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.width);
            return null;
        }),
        fillAreaP: (xy0) => (xy1) => IO(() => {
            λ.ctx.fillRect(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height, (xy1.fst - xy0.fst) * λ.ctx.canvas.width, (xy1.snd - xy0.snd) * λ.ctx.canvas.height);
            return null;
        }),
        fillAreaV: (xy0) => (xy1) => IO(() => {
            λ.ctx.fillRect(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height, (xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height);
            return null;
        })
    },
    log: (message) => IO(() => (console.log(message), null)),
    warn: (message) => IO(() => (console.warn(message), null)),
    flush: IO(() => (console.clear(), null)),
    queue: (io) => IO(() => (requestAnimationFrame(io.INFO), null)),
    tick: IO(() => {
        for (const k in λ.keyboard)
            λ.keyboard[k] = relaxY(λ.keyboard[k]);
        for (const i in λ.mouseButtons)
            λ.mouseButtons[i] = relaxY(λ.mouseButtons[i]);
        λ.mouseScroll = Y.Rest;
        λ.mouseDeltaX = λ.mouseDeltaY = 0;
        λ.isResized = false;
        return null;
    }),
    activatePointerLock: IO(() => (λ.ctx.canvas.onmouseup = () => λ.isPointerLocked && λ.ctx.canvas.requestPointerLock(), null)),
    deactivatePointerLock: IO(() => λ.ctx.canvas.onmousedown = null),
    loadImage: (path) => IO(() => {
        λ.image[path] = new Image;
        λ.image[path].src = path;
        λ.image[path].onerror = () => THROW(`'Output.loadImage' failed; could not load image: '${path}'`);
        return null;
    }),
    drawImage: (path) => (x) => (y) => IO(() => (λ.ctx.drawImage(λ.image[path], x, y), null)),
    drawImageP: (path) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd), null)),
    drawImageV: (path) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.x, xy.y), null)),
    drawCroppedImage: (path) => (cx) => (cy) => (cw) => (ch) => (x) => (y) => (w) => (h) => IO(() => (λ.ctx.drawImage(λ.image[path], cx, cy, cw, ch, x, y, w, h), null)),
    drawCroppedImageP: (path) => (cxy) => (cwh) => (xy) => (wh) => IO(() => {
        λ.ctx.drawImage(λ.image[path], cxy.fst, cxy.snd, cwh.fst, cwh.snd, xy.fst, xy.snd, wh.fst, wh.snd);
        return null;
    }),
    drawCroppedImageV: (path) => (cxy) => (cwh) => (xy) => (wh) => IO(() => (λ.ctx.drawImage(λ.image[path], cxy.x, cxy.y, cwh.x, cwh.y, xy.x, xy.y, wh.x, wh.y), null)),
    drawFullImage: (path) => (x) => (y) => (w) => (h) => IO(() => (λ.ctx.drawImage(λ.image[path], x, y, w, h), null)),
    drawFullImageP: (path) => (xy) => (wh) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd, wh.fst, wh.snd), null)),
    drawFullImageV: (path) => (xy) => (wh) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.x, xy.y, wh.x, wh.y), null)),
    drawSquareImage: (path) => (k) => (x) => (y) => IO(() => (λ.ctx.drawImage(λ.image[path], x, y, k, k), null)),
    drawSquareImageP: (path) => (k) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd, k, k), null)),
    drawSquareImageV: (path) => (k) => (xy) => IO(() => (λ.ctx.drawImage(λ.image[path], xy.x, xy.y, k, k), null)),
    drawScaledImage: (path) => (k) => (x) => (y) => IO(() => (λ.ctx.drawImage(λ.image[path], x, y, λ.image[path].width * k, λ.image[path].height * k), null)),
    drawScaledImageP: (path) => (k) => (xy) => IO(() => {
        λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd, λ.image[path].width * k, λ.image[path].height * k);
        return null;
    }),
    drawScaledImageV: (path) => (k) => (xy) => IO(() => {
        λ.ctx.drawImage(λ.image[path], xy.x, xy.y, λ.image[path].width * k, λ.image[path].height * k);
        return null;
    }),
    loadAudio: (path) => IO(() => {
        λ.audio[path] = new Audio(path);
        λ.audio[path].onerror = () => THROW(`'Output.loadAudio' failed; could not load audio: '${path}'`);
        return null;
    }),
    playAudio: (path) => IO(() => { var _a; return (((_a = λ.audio[path]) !== null && _a !== void 0 ? _a : THROW(`'Output.playAudio' failed; audio not preloaded: '${path}'`)).play(), null); }),
    playSFX: (path) => IO(() => {
        var _a;
        ((_a = λ.audio[path]) !== null && _a !== void 0 ? _a : THROW(`'Output.playSFX' failed; audio not preloaded: '${path}'`)).cloneNode().play();
        return null;
    }),
    loadFont: (path) => IO(() => {
        document.styleSheets[0].insertRule(`@font-face{font-family:"${path.slice(path.lastIndexOf("/") + 1, path.lastIndexOf("."))}";src:url("${path}")}`);
        return null;
    }),
    clearRectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.clearRect(x, y, w, h), null)),
    clearRectangleP: (xy) => (wh) => IO(() => (λ.ctx.clearRect(xy.fst, xy.snd, wh.fst, xy.snd), null)),
    clearRectangleV: (xy) => (wh) => IO(() => (λ.ctx.clearRect(xy.x, xy.y, wh.x, wh.y), null)),
    clearCanvas: IO(() => (λ.ctx.clearRect(0, 0, λ.ctx.canvas.width, λ.ctx.canvas.height), null)),
    fill: IO(() => (λ.ctx.fill(), null)),
    stroke: IO(() => (λ.ctx.stroke(), null)),
    save: IO(() => (λ.ctx.save(), null)),
    restore: IO(() => (λ.ctx.restore(), null)),
    clipEvenOdd: IO(() => (λ.ctx.clip('evenodd'), null)),
    clipNonZero: IO(() => (λ.ctx.clip('nonzero'), null)),
    rotate: (angle) => IO(() => (λ.ctx.rotate(angle), null)),
    scale: (k) => IO(() => (λ.ctx.scale(k, k), null)),
    scaleAxisX: (kx) => IO(() => (λ.ctx.scale(kx, 1), null)),
    scaleAxisY: (ky) => IO(() => (λ.ctx.scale(1, ky), null)),
    scaleAxis: (kx) => (ky) => IO(() => (λ.ctx.scale(kx, ky), null)),
    scaleAxisP: (kxy) => IO(() => (λ.ctx.scale(kxy.fst, kxy.snd), null)),
    scaleAxisV: (kxy) => IO(() => (λ.ctx.scale(kxy.x, kxy.y), null)),
    translateX: (dx) => IO(() => (λ.ctx.translate(dx, 0), null)),
    translateY: (dy) => IO(() => (λ.ctx.translate(0, dy), null)),
    translate: (dx) => (dy) => IO(() => (λ.ctx.translate(dx, dy), null)),
    translateP: (dxy) => IO(() => (λ.ctx.translate(dxy.fst, dxy.snd), null)),
    translateV: (dxy) => IO(() => (λ.ctx.translate(dxy.x, dxy.y), null)),
    transformation: (m) => IO(() => (λ.ctx.transform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null)),
    beginPath: IO(() => (λ.ctx.beginPath(), null)),
    closePath: IO(() => (λ.ctx.closePath(), null)),
    moveTo: (x) => (y) => IO(() => (λ.ctx.moveTo(x, y), null)),
    moveToP: (xy) => IO(() => (λ.ctx.moveTo(xy.fst, xy.snd), null)),
    moveToV: (xy) => IO(() => (λ.ctx.moveTo(xy.x, xy.y), null)),
    lineTo: (x) => (y) => IO(() => (λ.ctx.lineTo(x, y), null)),
    lineToP: (xy) => IO(() => (λ.ctx.lineTo(xy.fst, xy.snd), null)),
    lineToV: (xy) => IO(() => (λ.ctx.lineTo(xy.x, xy.y), null)),
    bezierCurveTo: (cx0) => (cy0) => (cx1) => (cy1) => (x) => (y) => IO(() => (λ.ctx.bezierCurveTo(cx0, cy0, cx1, cy1, x, y), null)),
    bezierCurveToP: (cxy0) => (cxy1) => (xy) => IO(() => (λ.ctx.bezierCurveTo(cxy0.fst, cxy0.snd, cxy1.fst, cxy1.snd, xy.fst, xy.snd), null)),
    bezierCurveToV: (cxy0) => (cxy1) => (xy) => IO(() => (λ.ctx.bezierCurveTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, xy.x, xy.y), null)),
    quadraticCurveTo: (cx) => (cy) => (x) => (y) => IO(() => (λ.ctx.quadraticCurveTo(cx, cy, x, y), null)),
    quadraticCurveToP: (cxy) => (xy) => IO(() => (λ.ctx.quadraticCurveTo(cxy.fst, cxy.snd, xy.fst, xy.snd), null)),
    quadraticCurveToV: (cxy) => (xy) => IO(() => (λ.ctx.quadraticCurveTo(cxy.x, cxy.y, xy.x, xy.y), null)),
    arcTo: (r) => (cx0) => (cy0) => (cx1) => (cy1) => IO(() => (λ.ctx.arcTo(cx0, cy0, cx1, cy1, r), null)),
    arcToP: (r) => (cxy0) => (cxy1) => IO(() => (λ.ctx.arcTo(cxy0.fst, cxy0.snd, cxy1.fst, cxy1.snd, r), null)),
    arcToV: (r) => (cxy0) => (cxy1) => IO(() => (λ.ctx.arcTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, r), null)),
    rectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.rect(x, y, w, h), null)),
    rectangleP: (xy) => (wh) => IO(() => (λ.ctx.rect(xy.fst, xy.snd, wh.fst, wh.snd), null)),
    rectangleV: (xy) => (wh) => IO(() => (λ.ctx.rect(xy.x, xy.y, wh.x, wh.y), null)),
    fillRectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.fillRect(x, y, w, h), null)),
    fillRectangleP: (xy) => (wh) => IO(() => (λ.ctx.fillRect(xy.fst, xy.snd, wh.fst, wh.snd), null)),
    fillRectangleV: (xy) => (wh) => IO(() => (λ.ctx.fillRect(xy.x, xy.y, wh.x, wh.y), null)),
    strokeRectangle: (x) => (y) => (w) => (h) => IO(() => (λ.ctx.strokeRect(x, y, w, h), null)),
    strokeRectangleP: (xy) => (wh) => IO(() => (λ.ctx.strokeRect(xy.fst, xy.snd, wh.fst, wh.snd), null)),
    strokeRectangleV: (xy) => (wh) => IO(() => (λ.ctx.strokeRect(xy.x, xy.y, wh.x, wh.y), null)),
    arc: (r) => (a) => (b) => (x) => (y) => IO(() => (λ.ctx.arc(x, y, r, a, b), null)),
    arcP: (r) => (a) => (b) => (xy) => IO(() => (λ.ctx.arc(xy.fst, xy.snd, r, a, b), null)),
    arcV: (r) => (a) => (b) => (xy) => IO(() => (λ.ctx.arc(xy.x, xy.y, r, a, b), null)),
    circle: (r) => (x) => (y) => IO(() => (λ.ctx.arc(x, y, r, 0, TAU), null)),
    circleP: (r) => (xy) => IO(() => (λ.ctx.arc(xy.fst, xy.snd, r, 0, TAU), null)),
    circleV: (r) => (xy) => IO(() => (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), null)),
    strokeCircle: (r) => (x) => (y) => IO(() => (λ.ctx.arc(x, y, r, 0, TAU), λ.ctx.stroke(), null)),
    strokeCircleP: (r) => (xy) => IO(() => (λ.ctx.arc(xy.fst, xy.snd, r, 0, TAU), λ.ctx.stroke(), null)),
    strokeCircleV: (r) => (xy) => IO(() => (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), λ.ctx.stroke(), null)),
    fillCircle: (r) => (x) => (y) => IO(() => (λ.ctx.arc(x, y, r, 0, TAU), λ.ctx.fill(), null)),
    fillCircleP: (r) => (xy) => IO(() => (λ.ctx.arc(xy.fst, xy.snd, r, 0, TAU), λ.ctx.fill(), null)),
    fillCircleV: (r) => (xy) => IO(() => (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), λ.ctx.fill(), null)),
    elliptic: (r) => (a) => (b) => (x) => (y) => (kx) => (ky) => IO(() => (λ.ctx.ellipse(x, y, kx, ky, r, a, b), null)),
    ellipticP: (r) => (a) => (b) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, a, b), null)),
    ellipticV: (r) => (a) => (b) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, a, b), null)),
    ellipse: (r) => (x) => (y) => (kx) => (ky) => IO(() => (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), null)),
    ellipseP: (r) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, 0, TAU), null)),
    ellipseV: (r) => (xy) => (wh) => IO(() => (λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU), null)),
    strokeEllipse: (r) => (x) => (y) => (kx) => (ky) => IO(() => (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), λ.ctx.stroke(), null)),
    strokeEllipseP: (r) => (xy) => (wh) => IO(() => {
        λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, 0, TAU);
        λ.ctx.stroke();
        return null;
    }),
    strokeEllipseV: (r) => (xy) => (wh) => IO(() => {
        λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU);
        λ.ctx.stroke();
        return null;
    }),
    fillEllipse: (r) => (x) => (y) => (kx) => (ky) => IO(() => (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), λ.ctx.fill(), null)),
    fillEllipseP: (r) => (xy) => (wh) => IO(() => {
        λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, 0, TAU);
        λ.ctx.fill();
        return null;
    }),
    fillEllipseV: (r) => (xy) => (wh) => IO(() => {
        λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU);
        λ.ctx.fill();
        return null;
    }),
    strokeText: (text) => (x) => (y) => IO(() => (λ.ctx.strokeText(text, x, y), null)),
    strokeTextP: (text) => (xy) => IO(() => (λ.ctx.strokeText(text, xy.fst, xy.snd), null)),
    strokeTextV: (text) => (xy) => IO(() => (λ.ctx.strokeText(text, xy.x, xy.y), null)),
    fillText: (text) => (x) => (y) => IO(() => (λ.ctx.fillText(text, x, y), null)),
    fillTextP: (text) => (xy) => IO(() => (λ.ctx.fillText(text, xy.fst, xy.snd), null)),
    fillTextV: (text) => (xy) => IO(() => (λ.ctx.fillText(text, xy.x, xy.y), null)),
    area: (x0) => (y0) => (x1) => (y1) => IO(() => (λ.ctx.rect(x0, y0, x1 - x0, y1 - y0), null)),
    areaP: (xy0) => (xy1) => IO(() => (λ.ctx.rect(xy0.fst, xy0.snd, xy1.fst - xy0.fst, xy1.snd - xy0.snd), null)),
    areaV: (xy0) => (xy1) => IO(() => (λ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null)),
    strokeArea: (x0) => (y0) => (x1) => (y1) => IO(() => (λ.ctx.strokeRect(x0, y0, x1 - x0, y1 - y0), null)),
    strokeAreaP: (xy0) => (xy1) => IO(() => (λ.ctx.strokeRect(xy0.fst, xy0.snd, xy1.fst - xy0.fst, xy1.snd - xy0.snd), null)),
    strokeAreaV: (xy0) => (xy1) => IO(() => (λ.ctx.strokeRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null)),
    fillArea: (x0) => (y0) => (x1) => (y1) => IO(() => (λ.ctx.fillRect(x0, y0, x1 - x0, y1 - y0), null)),
    fillAreaP: (xy0) => (xy1) => IO(() => (λ.ctx.fillRect(xy0.fst, xy0.snd, xy1.fst - xy0.fst, xy1.snd - xy0.snd), null)),
    fillAreaV: (xy0) => (xy1) => IO(() => (λ.ctx.fillRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null))
};
onload = () => {
    λ.ctx = document.querySelector('canvas').getContext('2d');
    onmousemove = ev => {
        λ.mouseWindowX = ev.x,
            λ.mouseWindowY = ev.y,
            λ.mouseCanvasX = ev.clientX - λ.ctx.canvas.offsetLeft,
            λ.mouseCanvasY = ev.clientY - λ.ctx.canvas.offsetTop,
            λ.mouseScreenX = ev.screenX,
            λ.mouseScreenY = ev.screenY,
            λ.mouseDeltaX = ev.movementX,
            λ.mouseDeltaY = ev.movementY;
    };
    onmousedown = ev => λ.mouseButtons[ev.button] = Y.DD;
    onmouseup = ev => λ.mouseButtons[ev.button] = Y.UU;
    onkeyup = ev => λ.keyboard[ev.code] = Y.UU;
    onkeydown = ev => λ.keyboard[ev.code] =
        ev.repeat
            ? λ.keyboard[ev.code]
            : Y.DD;
    onwheel = ev => λ.mouseScroll =
        ev.deltaY < 0 ? Y.U :
            ev.deltaY > 0 ? Y.D : Y.Rest;
    onresize = () => clearTimeout(λ.resizeID),
        λ.resizeID = setTimeout(() => λ.isResized = true, 250);
    document.onpointerlockchange = () => λ.isPointerLocked = document.pointerLockElement === λ.ctx.canvas;
};
