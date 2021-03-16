"use strict";
const THROW = (message) => { throw new Error(message); };
const THROWTYPE = (message) => { throw new TypeError(message); };
const THROWRANGE = (message) => { throw new RangeError(message); };
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
const asin = Math.asin;
const asinh = Math.asinh;
const atan = Math.atan;
const atan2 = (y) => (x) => Math.atan2(y, x);
const ratan2 = (x) => (y) => Math.atan2(y, x);
const atanh = Math.atanh;
const cbrt = Math.cbrt;
const ceil = Math.ceil;
const CLZ32 = Math.clz32;
const cos = Math.cos;
const cosh = Math.cosh;
const div = (x) => (y) => x / y;
const rdiv = (y) => (x) => x / y;
const eq = (x) => (y) => x === y;
const exp = Math.exp;
const expm1 = Math.expm1;
const floor = Math.floor;
const fround = Math.fround;
const gt = (x) => (y) => x > y;
const gte = (x) => (y) => x >= y;
const ln = Math.log;
const log10 = Math.log10;
const lnp1 = Math.log1p;
const log2 = Math.log2;
const LSHIFT = (x) => (y) => x << y;
const rLSHIFT = (y) => (x) => x << y;
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
const trunc = Math.trunc;
const URSHIFT = (x) => (y) => x >>> y;
const rURSHIFT = (y) => (x) => x >>> y;
const XOR = (x) => (y) => x ^ y;
const xor = (x) => (y) => x !== y;
const IO = (sideeffect) => ({
    CONS: 'IO',
    INFO: sideeffect,
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
    then: x => IO(() => (sideeffect(), x.INFO()))
});
{
    IO.bind = (io) => (reaction) => io.bind(reaction);
    IO.bindto =
        (io) => (name) => (reaction) => io.bindto(name)(reaction);
    IO.fmap = (io) => (computation) => io.fmap(computation);
    IO.fmapto =
        (io) => (name) => (computation) => io.fmapto(name)(computation);
    IO.then = (io) => (successor) => io.then(successor);
}
const Nothing = {
    CONS: 'Nothing',
    bind: _ => Nothing,
    bindto: _ => _ => Nothing,
    fmap: _ => Nothing,
    fmapto: _ => _ => Nothing
};
const Just = (value) => ({
    CONS: 'Just',
    INFO: value,
    bind: f => {
        const x = f(value);
        return x.CONS === 'Nothing' ? Nothing : x;
    },
    bindto: x => f => {
        const y = f(value);
        return y.CONS === 'Nothing' ? Nothing : Just(Object.assign(Object.assign({}, value), { [x]: y.INFO }));
    },
    fmap: f => Just(f(value)),
    fmapto: x => f => Just(Object.assign(Object.assign({}, value), { [x]: f(value) }))
});
const Maybe = {
    bind: (maybe) => (reaction) => maybe.bind(reaction),
    bindto: (maybe) => (name) => (reaction) => maybe.bindto(name)(reaction),
    fmap: (maybe) => (computation) => maybe.fmap(computation),
    fmapto: (maybe) => (name) => (computation) => maybe.fmapto(name)(computation)
};
const State = (statefulComputation) => ({
    CONS: 'State',
    INFO: statefulComputation,
    bind: f => State(x => {
        const [y, z] = statefulComputation(x);
        return f(z).INFO(y);
    }),
    fmap: f => State(x => {
        const [y, z] = statefulComputation(x);
        return [y, f(z)];
    }),
    bindto: k => f => State(x => {
        const [y, $] = statefulComputation(x);
        const [z, w] = f($).INFO(y);
        return [z, Object.assign(Object.assign({}, $), { [k]: w })];
    }),
    fmapto: k => f => State(x => {
        const [y, $] = statefulComputation(x);
        return [y, Object.assign(Object.assign({}, $), { [k]: f($) })];
    }),
    then: s => State(x => s.INFO(statefulComputation(x)[0])),
    runState: s => statefulComputation(s),
    evalState: s => statefulComputation(s)[0],
    execState: s => statefulComputation(s)[1]
});
{
    State.bind = (state) => (reaction) => state.bind(reaction);
    State.bindto =
        (state) => (name) => (reaction) => state.bindto(name)(reaction);
    State.fmap = (state) => (computation) => state.fmap(computation);
    State.fmapto =
        (state) => (name) => (computation) => state.fmapto(name)(computation);
    State.then = (state) => (successor) => state.then(successor);
}
const List = (...elements) => ({
    CONS: 'List',
    INFO: elements,
    all: f => elements.every(x => f(x)),
    any: f => elements.some(x => f(x)),
    append: x => List(...elements, x),
    at: i => i < elements.length && i >= 0
        ? elements[i]
        : THROWRANGE(`Cannot retrive element at index '${i}' in 'List' of length '${elements.length}'`),
    bind: f => List(...elements.flatMap(x => f(x).INFO)),
    bindto: k => f => List(...elements.flatMap($ => f($).INFO.map(x => (Object.assign(Object.assign({}, $), { [k]: x }))))),
    break: f => {
        const i = elements.findIndex(x => f(x));
        return ~i
            ? [List(...elements.slice(0, i)), List(...elements.slice(i))]
            : [List(...elements), List()];
    },
    concat: xs => List(...elements, ...xs.INFO),
    drop: x => List(...elements.slice(Math.max(0, x))),
    dropWhile: f => {
        const i = elements.findIndex(x => !f(x));
        return List(...(~i ? elements.slice(i) : []));
    },
    elem: x => elements.includes(x),
    elemIndex: x => {
        const i = elements.indexOf(x);
        return ~i ? Just(i) : Nothing;
    },
    elemIndices: x => {
        const is = [];
        elements.forEach((y, i) => {
            if (x === y)
                is.push(i);
        });
        return List(...is);
    },
    filter: f => List(...elements.filter(x => f(x))),
    find: f => {
        const x = elements.find(y => f(y));
        return x === undefined ? Nothing : Just(x);
    },
    findIndex: f => {
        const i = elements.findIndex(y => f(y));
        return ~i ? Just(i) : Nothing;
    },
    findIndices: f => {
        const is = [];
        elements.forEach((y, i) => {
            if (f(y))
                is.push(i);
        });
        return List(...is);
    },
    fmap: f => List(...elements.map(x => f(x))),
    fmapto: k => f => List(...elements.map($ => (Object.assign(Object.assign({}, $), { [k]: f($) })))),
    foldl: f => x => elements.reduce((y, z) => f(y)(z), x),
    foldr: f => x => elements.reduceRight((y, z) => f(z)(y), x),
    foldl1: f => elements.length
        ? elements.reduce((y, z) => f(y)(z))
        : THROWRANGE(`Cannot 'foldl1' on an empty 'List'`),
    foldr1: f => elements.length
        ? elements.reduceRight((y, z) => f(z)(y))
        : THROWRANGE(`Cannot 'foldr1' on an empty 'List'`),
    get head() {
        return elements.length
            ? elements[0]
            : THROWRANGE(`Cannot get 'head' of an empty 'List'`);
    },
    get init() {
        return List(...elements.slice(0, -1));
    },
    get inits() {
        return List(...Array(elements.length + 1).fill(null).map((_, i) => List(...elements.slice(0, i))));
    },
    intersperse: x => List(...Array(Math.max(0, elements.length * 2 - 1)).fill(null).map((_, i) => i % 2 ? x : elements[i / 2])),
    get last() {
        return elements.length
            ? elements[elements.length - 1]
            : THROWRANGE(`Cannot get 'last' of an empty 'List'`);
    },
    notElem: x => !elements.includes(x),
    prepend: x => List(x, ...elements),
    get reverse() {
        return List(...elements.slice().reverse());
    },
    scanl: f => x => List(...elements.reduce((y, z) => y.concat(f(y[y.length - 1])(z)), [x])),
    scanr: f => x => List(...elements.reduceRight((y, z) => [f(z)(y[0])].concat(y), [x])),
    scanl1: f => List(...elements.slice(1).reduce((x, y) => x.concat(f(x[x.length - 1])(y)), [elements[0]])),
    scanr1: f => List(...elements.slice(0, -1).reduceRight((x, y) => [f(y)(x[0])].concat(x), [elements[elements.length - 1]])),
    span: f => {
        const i = elements.findIndex(x => !f(x));
        return ~i
            ? [List(...elements.slice(0, i)), List(...elements.slice(i))]
            : [List(), List(...elements)];
    },
    splitAt: i => [List(...elements.slice(0, Math.max(0, i))), List(...elements.slice(Math.max(0, i)))],
    get tail() {
        return elements.length
            ? List(...elements.slice(1))
            : THROWRANGE(`Cannot get 'tail' of an empty 'List'`);
    },
    get tails() {
        return List(...Array(elements.length + 1).fill(null).map((_, i) => List(...elements.slice(i))));
    },
    take: x => List(...elements.slice(0, Math.max(0, x))),
    takeWhile: f => {
        const i = elements.findIndex(x => !f(x));
        return ~i
            ? List(...elements.slice(0, i))
            : List(...elements);
    },
    zip: xs => List(...Array(Math.min(elements.length, xs.INFO.length)).fill(null).map((_, i) => [elements[i], xs.INFO[i]])),
    zipWith: xs => f => List(...Array(Math.min(elements.length, xs.INFO.length)).fill(null).map((_, i) => f(elements[i])(xs.INFO[i])))
});
{
    List.all = (list) => (predicate) => list.all(predicate);
    List.any = (list) => (predicate) => list.any(predicate);
    List.append = (list) => (element) => list.append(element);
    List.at = (list) => (index) => list.at(index);
    List.bind = (list) => (reaction) => list.bind(reaction);
    List.bindto =
        (list) => (name) => (reaction) => list.bindto(name)(reaction);
    List.break = (list) => (predicate) => list.break(predicate);
    List.concat = (list) => (succeeding) => list.concat(succeeding);
    List.drop = (list) => (count) => list.drop(count);
    List.dropWhile = (list) => (predicate) => list.dropWhile(predicate);
    List.elem = (list) => (match) => list.elem(match);
    List.elemIndex = (list) => (match) => list.elemIndex(match);
    List.elemIndices = (list) => (match) => list.elemIndices(match);
    List.filter = (list) => (predicate) => list.filter(predicate);
    List.find = (list) => (predicate) => list.find(predicate);
    List.findIndex = (list) => (predicate) => list.findIndex(predicate);
    List.findIndices = (list) => (predicate) => list.findIndices(predicate);
    List.fmap = (list) => (computation) => list.fmap(computation);
    List.fmapto =
        (list) => (name) => (computation) => list.fmapto(name)(computation);
    List.foldl = (list) => (builder) => (initialValue) => list.foldl(builder)(initialValue);
    List.foldr = (list) => (builder) => (initialValue) => list.foldr(builder)(initialValue);
    List.foldl1 = (list) => (builder) => list.foldl1(builder);
    List.foldr1 = (list) => (builder) => list.foldr1(builder);
    List.head = (list) => list.head;
    List.init = (list) => list.init;
    List.inits = (list) => list.inits;
    List.intersperse = (list) => (delimiter) => list.intersperse(delimiter);
    List.last = (list) => list.last;
    List.notElem = (list) => (delimiter) => list.notElem(delimiter);
    List.prepend = (list) => (element) => list.prepend(element);
    List.reverse = (list) => list.reverse;
    List.scanl = (list) => (builder) => (initialValue) => list.scanl(builder)(initialValue);
    List.scanr = (list) => (builder) => (initialValue) => list.scanr(builder)(initialValue);
    List.scanl1 = (list) => (builder) => list.scanl1(builder);
    List.scanr1 = (list) => (builder) => list.scanr1(builder);
    List.span = (list) => (predicate) => list.span(predicate);
    List.splitAt = (list) => (index) => list.splitAt(index);
    List.tail = (list) => list.tail;
    List.tails = (list) => list.tails;
    List.take = (list) => (count) => list.take(count);
    List.takeWhile = (list) => (predicate) => list.takeWhile(predicate);
    List.zip = (list) => (postfixes) => list.zip(postfixes);
    List.zipWith =
        (list) => (postfixes) => (builder) => list.zipWith(postfixes)(builder);
}
const Vector2D = (x) => (y) => (({
    CONS: 'Vector2D',
    x, y
}));
{
    Vector2D.x = (v) => v.x;
    Vector2D.y = (v) => v.y;
}
const Vector3D = (x) => (y) => (z) => (({
    CONS: 'Vector3D',
    x, y, z
}));
{
    Vector3D.x = (v) => v.x;
    Vector3D.y = (v) => v.y;
    Vector3D.z = (v) => v.z;
}
const Vector4D = (x) => (y) => (z) => (w) => (({
    CONS: 'Vector4D',
    x, y, z, w
}));
{
    Vector4D.x = (v) => v.x;
    Vector4D.y = (v) => v.y;
    Vector4D.z = (v) => v.z;
    Vector4D.w = (v) => v.w;
}
const Matrix2x2 = (ix) => (jx) => (iy) => (jy) => ({
    CONS: 'Matrix2x2',
    ix, jx,
    iy, jy,
    i: Vector2D(ix)(iy), j: Vector2D(jx)(jy),
    x: Vector2D(ix)(jx), y: Vector2D(iy)(jy)
});
{
    Matrix2x2.ix = (matrix) => matrix.ix;
    Matrix2x2.jx = (matrix) => matrix.jx;
    Matrix2x2.iy = (matrix) => matrix.iy;
    Matrix2x2.jy = (matrix) => matrix.jy;
    Matrix2x2.i = (matrix) => matrix.i;
    Matrix2x2.j = (matrix) => matrix.j;
    Matrix2x2.x = (matrix) => matrix.x;
    Matrix2x2.y = (matrix) => matrix.y;
}
const Matrix3x3 = (ix) => (jx) => (kx) => (iy) => (jy) => (ky) => (iz) => (jz) => (kz) => ({
    CONS: 'Matrix3x3',
    ix, jx, kx,
    iy, jy, ky,
    iz, jz, kz,
    i: Vector3D(ix)(iy)(iz), j: Vector3D(jx)(jy)(jz), k: Vector3D(kx)(ky)(kz),
    x: Vector3D(ix)(jx)(kx), y: Vector3D(iy)(jy)(ky), z: Vector3D(iz)(jz)(kz)
});
{
    Matrix3x3.ix = (matrix) => matrix.ix;
    Matrix3x3.jx = (matrix) => matrix.jx;
    Matrix3x3.kx = (matrix) => matrix.kx;
    Matrix3x3.iy = (matrix) => matrix.iy;
    Matrix3x3.jy = (matrix) => matrix.jy;
    Matrix3x3.ky = (matrix) => matrix.ky;
    Matrix3x3.iz = (matrix) => matrix.iz;
    Matrix3x3.jz = (matrix) => matrix.jz;
    Matrix3x3.kz = (matrix) => matrix.kz;
    Matrix3x3.i = (matrix) => matrix.i;
    Matrix3x3.j = (matrix) => matrix.j;
    Matrix3x3.k = (matrix) => matrix.k;
    Matrix3x3.x = (matrix) => matrix.x;
    Matrix3x3.y = (matrix) => matrix.y;
    Matrix3x3.z = (matrix) => matrix.z;
}
const Matrix4x4 = (ix) => (jx) => (kx) => (lx) => (iy) => (jy) => (ky) => (ly) => (iz) => (jz) => (kz) => (lz) => (iw) => (jw) => (kw) => (lw) => ({
    CONS: 'Matrix4x4',
    ix, jx, kx, lx,
    iy, jy, ky, ly,
    iz, jz, kz, lz,
    iw, jw, kw, lw,
    i: Vector4D(ix)(iy)(iz)(iw), j: Vector4D(jx)(jy)(jz)(jw), k: Vector4D(kx)(ky)(kz)(kw), l: Vector4D(lx)(ly)(lz)(lw),
    x: Vector4D(ix)(jx)(kx)(lx), y: Vector4D(iy)(jy)(ky)(ly), z: Vector4D(iz)(jz)(kz)(lz), w: Vector4D(iw)(jw)(kw)(lw)
});
{
    Matrix4x4.ix = (matrix) => matrix.ix;
    Matrix4x4.jx = (matrix) => matrix.jx;
    Matrix4x4.kx = (matrix) => matrix.kx;
    Matrix4x4.lx = (matrix) => matrix.lx;
    Matrix4x4.iy = (matrix) => matrix.iy;
    Matrix4x4.jy = (matrix) => matrix.jy;
    Matrix4x4.ky = (matrix) => matrix.ky;
    Matrix4x4.ly = (matrix) => matrix.ly;
    Matrix4x4.iz = (matrix) => matrix.iz;
    Matrix4x4.jz = (matrix) => matrix.jz;
    Matrix4x4.kz = (matrix) => matrix.kz;
    Matrix4x4.lz = (matrix) => matrix.lz;
    Matrix4x4.iw = (matrix) => matrix.iw;
    Matrix4x4.jw = (matrix) => matrix.jw;
    Matrix4x4.kw = (matrix) => matrix.kw;
    Matrix4x4.lw = (matrix) => matrix.lw;
    Matrix4x4.i = (matrix) => matrix.i;
    Matrix4x4.j = (matrix) => matrix.j;
    Matrix4x4.k = (matrix) => matrix.k;
    Matrix4x4.l = (matrix) => matrix.l;
    Matrix4x4.x = (matrix) => matrix.x;
    Matrix4x4.y = (matrix) => matrix.y;
    Matrix4x4.z = (matrix) => matrix.z;
    Matrix4x4.w = (matrix) => matrix.w;
}
const TextMeasurement = (text) => (width) => (height) => (({
    CONS: 'TextMeasurement',
    text, width, height
}));
{
    TextMeasurement.text = (measurement) => measurement.text;
    TextMeasurement.width = (measurement) => measurement.width;
    TextMeasurement.height = (measurement) => measurement.height;
}
const Switch = (f) => ({
    CONS: 'Switch',
    case: x => y => Switch(z => {
        const w = f(z);
        return w === undefined && z === x ? y() : w;
    }),
    else: x => Switch(y => {
        const z = f(y);
        return z === undefined ? x() : z;
    }),
    with: x => {
        const y = f(x);
        return y === undefined
            ? THROWRANGE(`'Switch' did not cover all cases; missing case on value: '${x}'`)
            : y;
    }
});
Switch.case = (domain) => (codomain) => Switch(x => x === domain ? codomain() : undefined);
const Bijection = (pairs) => ({
    CONS: 'Bijection',
    INFO: pairs,
    of: x => y => Bijection([...pairs, [x, y]]),
    domain: x => {
        const y = pairs.find(([z, _]) => z === x);
        return y === undefined
            ? THROWRANGE(`'Bijection' did not have a well-defined enough domain for value: '${x}'`)
            : y[1];
    },
    codomain: x => {
        const y = pairs.find(([_, z]) => z === x);
        return y === undefined
            ? THROWRANGE(`'Bijection' did not have a well-defined enough codomain for value: '${x}'`)
            : y[0];
    }
});
Bijection.of = (domainValue) => (codomainValue) => Bijection([[domainValue, codomainValue]]);
{
    Bijection.domain = (bijection) => (domain) => bijection.domain(domain);
    Bijection.codomain = (bijection) => (codomain) => bijection.codomain(codomain);
}
const Clock = (time) => (delta) => (counter) => (({ CONS: 'Clock', time, delta, counter }));
{
    Clock.time = (clock) => clock.time;
    Clock.delta = (clock) => clock.delta;
    Clock.counter = (clock) => clock.counter;
}
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
const relaxHorizontal = (direction) => Switch
    .case(Horizontal.Leftward)(() => Horizontal.Left)
    .case(Horizontal.Rightward)(() => Horizontal.Right)
    .else(() => direction)
    .with(direction);
const relaxVertical = (direction) => Switch
    .case(Vertical.Downward)(() => Vertical.Down)
    .case(Vertical.Upward)(() => Vertical.Up)
    .else(() => direction)
    .with(direction);
const relaxLateral = (direction) => Switch
    .case(Lateral.Backward)(() => Lateral.Back)
    .case(Lateral.Forward)(() => Lateral.Fore)
    .else(() => direction)
    .with(direction);
const bijectionLineCap = Bijection
    .of(LineCap.Butt)('butt')
    .of(LineCap.Round)('round')
    .of(LineCap.Square)('square');
const bijectionLineJoin = Bijection
    .of(LineJoin.Round)('round')
    .of(LineJoin.Bevel)('bevel')
    .of(LineJoin.Miter)('miter');
const bijectionTextAlign = Bijection
    .of(TextAlign.Center)('center')
    .of(TextAlign.End)('end')
    .of(TextAlign.Left)('left')
    .of(TextAlign.Right)('right')
    .of(TextAlign.Start)('start');
const bijectionTextBaseline = Bijection
    .of(TextBaseline.Alphabetic)('alphabetic')
    .of(TextBaseline.Bottom)('bottom')
    .of(TextBaseline.Hanging)('hanging')
    .of(TextBaseline.Ideographic)('ideographic')
    .of(TextBaseline.Middle)('middle')
    .of(TextBaseline.Top)('top');
const bijectionCompositionOperation = Bijection
    .of(CompositionOperation.SourceOver)('source-over')
    .of(CompositionOperation.SourceIn)('source-in')
    .of(CompositionOperation.SourceOut)('source-out')
    .of(CompositionOperation.SourceAtop)('source-atop')
    .of(CompositionOperation.DestinationOver)('destination-over')
    .of(CompositionOperation.DestinationIn)('destination-in')
    .of(CompositionOperation.DestinationOut)('destination-out')
    .of(CompositionOperation.DestinationAtop)('destination-atop')
    .of(CompositionOperation.Lighter)('lighter')
    .of(CompositionOperation.Copy)('copy')
    .of(CompositionOperation.Xor)('xor')
    .of(CompositionOperation.Multiply)('multiply')
    .of(CompositionOperation.Screen)('screen')
    .of(CompositionOperation.Overlay)('overlay')
    .of(CompositionOperation.Darken)('darken')
    .of(CompositionOperation.Lighten)('lighten')
    .of(CompositionOperation.ColorDodge)('color-dodge')
    .of(CompositionOperation.ColorBurn)('color-burn')
    .of(CompositionOperation.HardLight)('hard-light')
    .of(CompositionOperation.SoftLight)('soft-light')
    .of(CompositionOperation.Difference)('difference')
    .of(CompositionOperation.Exclusion)('exclusion')
    .of(CompositionOperation.Hue)('hue')
    .of(CompositionOperation.Saturation)('saturation')
    .of(CompositionOperation.Color)('color')
    .of(CompositionOperation.Luminosity)('luminosity');
const Matrix2D = (i) => (j) => Matrix2x2(i.x)(j.x)(i.y)(j.y);
const Matrix3D = (i) => (j) => (k) => Matrix3x3(i.x)(j.x)(k.x)(i.y)(j.y)(k.y)(i.z)(j.z)(k.z);
const Matrix4D = (i) => (j) => (k) => (l) => Matrix4x4(i.x)(j.x)(k.x)(l.x)(i.y)(j.y)(k.y)(l.y)(i.z)(j.z)(k.z)(l.z)(i.w)(j.w)(k.w)(l.w);
const pseudoRandom = State(seed => [
    (-67 * seed * seed * seed + 23 * seed * seed - 91 * seed + 73) % 65536,
    Math.abs(97 * seed * seed * seed + 91 * seed * seed - 83 * seed + 79) % 65536 / 65536
]);
const Do = {
    IO: IO(() => Object.create(null)),
    Maybe: Just(Object.create(null)),
    State: State((s) => [s, Object.create(null)]),
    List: List(Object.create(null))
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
        buttons: new Array(5).fill(Vertical.Up)
    },
    keyboard: __KEYBOARD_KEYS_ARRAY__.reduce(($, k) => (Object.assign(Object.assign({}, $), { [k]: Vertical.Up })), {})
};
var Import;
(function (Import) {
    let Norm;
    (function (Norm) {
        Norm.mouseCanvasPosition = IO(() => [
            __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height
        ]);
        Norm.mouseCanvasPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width)(__EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height));
        Norm.mouseCanvasPositionX = IO(() => __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width);
        Norm.mouseCanvasPositionY = IO(() => __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height);
        Norm.mouseVelocity = IO(() => [
            __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height
        ]);
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
        Norm.shadowOffset = IO(() => [
            __EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height
        ]);
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
    Import.screenDimensions = IO(() => [screen.width, screen.height]);
    Import.screenDimensionsVector = IO(() => Vector2D(screen.width)(screen.height));
    Import.screenDimensionW = IO(() => screen.width);
    Import.screenDimensionH = IO(() => screen.height);
    Import.windowDimensions = IO(() => [innerWidth, innerHeight]);
    Import.windowDimensionsVector = IO(() => Vector2D(innerWidth)(innerHeight));
    Import.windowDimensionW = IO(() => innerWidth);
    Import.windowDimensionH = IO(() => innerHeight);
    Import.canvasDimensions = IO(() => [__EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height]);
    Import.canvasDimensionsVector = IO(() => Vector2D(__EXTERNAL__.context.canvas.width)(__EXTERNAL__.context.canvas.height));
    Import.canvasDimensionW = IO(() => __EXTERNAL__.context.canvas.width);
    Import.canvasDimensionH = IO(() => __EXTERNAL__.context.canvas.height);
    Import.mouseScreenPosition = IO(() => [__EXTERNAL__.mouse.screenX, __EXTERNAL__.mouse.screenY]);
    Import.mouseScreenPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.screenX)(__EXTERNAL__.mouse.screenY));
    Import.mouseScreenPositionX = IO(() => __EXTERNAL__.mouse.screenX);
    Import.mouseScreenPositionY = IO(() => __EXTERNAL__.mouse.screenY);
    Import.mouseWindowPosition = IO(() => [__EXTERNAL__.mouse.windowX, __EXTERNAL__.mouse.windowY]);
    Import.mouseWindowPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.windowX)(__EXTERNAL__.mouse.windowY));
    Import.mouseWindowPositionX = IO(() => __EXTERNAL__.mouse.windowX);
    Import.mouseWindowPositionY = IO(() => __EXTERNAL__.mouse.windowY);
    Import.mouseCanvasPosition = IO(() => [__EXTERNAL__.mouse.canvasX, __EXTERNAL__.mouse.canvasY]);
    Import.mouseCanvasPositionVector = IO(() => Vector2D(__EXTERNAL__.mouse.canvasX)(__EXTERNAL__.mouse.canvasY));
    Import.mouseCanvasPositionX = IO(() => __EXTERNAL__.mouse.canvasX);
    Import.mouseCanvasPositionY = IO(() => __EXTERNAL__.mouse.canvasY);
    Import.mouseVelocity = IO(() => [__EXTERNAL__.mouse.deltaX, __EXTERNAL__.mouse.deltaY]);
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
    Import.lineCap = IO(() => bijectionLineCap.codomain(__EXTERNAL__.context.lineCap));
    Import.lineJoin = IO(() => bijectionLineJoin.codomain(__EXTERNAL__.context.lineJoin));
    Import.lineDashPattern = IO(() => List(...__EXTERNAL__.context.getLineDash()));
    Import.lineDashOffset = IO(() => __EXTERNAL__.context.lineDashOffset);
    Import.miterLimit = IO(() => __EXTERNAL__.context.miterLimit);
    Import.font = IO(() => __EXTERNAL__.context.font);
    Import.fontSize = IO(() => parseFloat(__EXTERNAL__.context.font));
    Import.fontFamily = IO(() => __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1));
    Import.textAlign = IO(() => bijectionTextAlign.codomain(__EXTERNAL__.context.textAlign));
    Import.textBaseline = IO(() => bijectionTextBaseline.codomain(__EXTERNAL__.context.textBaseline));
    Import.shadowBlurAmount = IO(() => __EXTERNAL__.context.shadowBlur);
    Import.shadowColor = IO(() => __EXTERNAL__.context.shadowColor);
    Import.shadowOffset = IO(() => [__EXTERNAL__.context.shadowOffsetX, __EXTERNAL__.context.shadowOffsetY]);
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
    Import.compositionOperation = IO(() => bijectionCompositionOperation.codomain(__EXTERNAL__.context.globalCompositeOperation));
})(Import || (Import = {}));
var Mutate;
(function (Mutate) {
    let Norm;
    (function (Norm) {
        Norm.lineWidth = (w) => IO(() => {
            __EXTERNAL__.context.lineWidth = w * __EXTERNAL__.context.canvas.width;
            return null;
        });
        Norm.lineDashPattern = (pattern) => IO(() => {
            __EXTERNAL__.context.setLineDash(pattern.INFO.map(n => n * __EXTERNAL__.context.canvas.width));
            return null;
        });
        Norm.lineDashOffset = (offset) => IO(() => {
            __EXTERNAL__.context.lineDashOffset = offset * __EXTERNAL__.context.canvas.width;
            return null;
        });
        Norm.fontSize = (size) => IO(() => {
            __EXTERNAL__.context.font =
                `${size * __EXTERNAL__.context.canvas.width}px ` +
                    `${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1)}`;
            return null;
        });
        Norm.fillRGBA = (r) => (g) => (b) => (a) => IO(() => {
            __EXTERNAL__.context.fillStyle = `rgba(${r * 255},${g * 255},${b * 255},${a})`;
            return null;
        });
        Norm.fillVector = (v) => IO(() => {
            __EXTERNAL__.context.fillStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`;
            return null;
        });
        Norm.strokeRGBA = (r) => (g) => (b) => (a) => IO(() => {
            __EXTERNAL__.context.strokeStyle = `rgba(${r * 255},${g * 255},${b * 255},${a * 255})`;
            return null;
        });
        Norm.strokeVector = (v) => IO(() => {
            __EXTERNAL__.context.strokeStyle = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`;
            return null;
        });
        Norm.shadowRGBA = (r) => (g) => (b) => (a) => IO(() => {
            __EXTERNAL__.context.shadowColor = `rgba(${r * 255},${g * 255},${b * 255},${a})`;
            return null;
        });
        Norm.shadowVector = (v) => IO(() => {
            __EXTERNAL__.context.shadowColor = `rgba(${v.x * 255},${v.y * 255},${v.z * 255},${v.w})`;
            return null;
        });
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
        Norm.shadowOffsetX = (x) => IO(() => {
            __EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width;
            return null;
        });
        Norm.shadowOffsetY = (y) => IO(() => {
            __EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height;
            return null;
        });
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
    Mutate.canvasDimensionW = (w) => IO(() => {
        __EXTERNAL__.context.canvas.width = w;
        return null;
    });
    Mutate.canvasDimensionH = (h) => IO(() => {
        __EXTERNAL__.context.canvas.height = h;
        return null;
    });
    Mutate.lineWidth = (w) => IO(() => {
        __EXTERNAL__.context.lineWidth = w;
        return null;
    });
    Mutate.lineCap = (cap) => IO(() => {
        __EXTERNAL__.context.lineCap = bijectionLineCap.domain(cap);
        return null;
    });
    Mutate.lineJoin = (joining) => IO(() => {
        __EXTERNAL__.context.lineJoin = bijectionLineJoin.domain(joining);
        return null;
    });
    Mutate.lineDashPattern = (pattern) => IO(() => {
        __EXTERNAL__.context.setLineDash(pattern.INFO.slice());
        return null;
    });
    Mutate.lineDashOffset = (offset) => IO(() => {
        __EXTERNAL__.context.lineDashOffset = offset;
        return null;
    });
    Mutate.miterLimit = (limit) => IO(() => {
        __EXTERNAL__.context.miterLimit = limit;
        return null;
    });
    Mutate.font = (fontDescription) => IO(() => {
        __EXTERNAL__.context.font = fontDescription;
        return null;
    });
    Mutate.fontSize = (size) => IO(() => {
        __EXTERNAL__.context.font =
            `${size}px ${__EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1)}`;
        return null;
    });
    Mutate.fontFamily = (family) => IO(() => {
        __EXTERNAL__.context.font = `${parseFloat(__EXTERNAL__.context.font)}px ${family}`;
        return null;
    });
    Mutate.textAlign = (align) => IO(() => {
        __EXTERNAL__.context.textAlign = bijectionTextAlign.domain(align);
        return null;
    });
    Mutate.textBaseline = (baseline) => IO(() => {
        __EXTERNAL__.context.textBaseline = bijectionTextBaseline.domain(baseline);
        return null;
    });
    Mutate.fillColor = (color) => IO(() => {
        __EXTERNAL__.context.fillStyle = color;
        return null;
    });
    Mutate.fillRGBA = (r) => (g) => (b) => (a) => IO(() => {
        __EXTERNAL__.context.fillStyle = `rgba(${r},${g},${b},${a})`;
        return null;
    });
    Mutate.fillVector = (v) => IO(() => {
        __EXTERNAL__.context.fillStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`;
        return null;
    });
    Mutate.strokeColor = (color) => IO(() => {
        __EXTERNAL__.context.strokeStyle = color;
        return null;
    });
    Mutate.strokeRGBA = (r) => (g) => (b) => (a) => IO(() => {
        __EXTERNAL__.context.strokeStyle = `rgba(${r},${g},${b},${a})`;
        return null;
    });
    Mutate.strokeVector = (v) => IO(() => {
        __EXTERNAL__.context.strokeStyle = `rgba(${v.x},${v.y},${v.z},${v.w})`;
        return null;
    });
    Mutate.shadowBlurAmount = (amount) => IO(() => {
        __EXTERNAL__.context.shadowBlur = amount;
        return null;
    });
    Mutate.shadowColor = (color) => IO(() => {
        __EXTERNAL__.context.shadowColor = color;
        return null;
    });
    Mutate.shadowRGBA = (r) => (g) => (b) => (a) => IO(() => {
        __EXTERNAL__.context.shadowColor = `rgba(${r},${g},${b},${a})`;
        return null;
    });
    Mutate.shadowVector = (v) => IO(() => {
        __EXTERNAL__.context.shadowColor = `rgba(${v.x},${v.y},${v.z},${v.w})`;
        return null;
    });
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
    Mutate.shadowOffsetX = (x) => IO(() => {
        __EXTERNAL__.context.shadowOffsetX = x;
        return null;
    });
    Mutate.shadowOffsetY = (y) => IO(() => {
        __EXTERNAL__.context.shadowOffsetY = y;
        return null;
    });
    Mutate.transformationMatrix = (m) => IO(() => {
        __EXTERNAL__.context.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky);
        return null;
    });
    Mutate.alpha = (opacity) => IO(() => {
        __EXTERNAL__.context.globalAlpha = opacity;
        return null;
    });
    Mutate.compositionOperation = (composition) => IO(() => {
        __EXTERNAL__.context.globalCompositeOperation = bijectionCompositionOperation.domain(composition);
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
            __EXTERNAL__.context.clearRect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.clearRectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.clearRect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
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
            __EXTERNAL__.context.rect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.rectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.rect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.fillRectangle = (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.fillRect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.fillRectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.fillRect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.strokeRectangle = (x) => (y) => (w) => (h) => IO(() => {
            __EXTERNAL__.context.strokeRect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.strokeRectangleVector = (coordinates) => (dimensions) => IO(() => {
            __EXTERNAL__.context.strokeRect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
            return null;
        });
        Norm.arc = (x) => (y) => (r) => (a) => (b) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, a * TAU, b * TAU);
            return null;
        });
        Norm.arcVector = (v) => (r) => (a) => (b) => IO(() => {
            __EXTERNAL__.context.arc(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, a * TAU, b * TAU);
            return null;
        });
        Norm.circle = (x) => (y) => (r) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            return null;
        });
        Norm.circleVector = (coordinates) => (r) => IO(() => {
            __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            return null;
        });
        Norm.strokeCircle = (x) => (y) => (r) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.stroke();
            return null;
        });
        Norm.strokeCircleVector = (coordinates) => (r) => IO(() => {
            __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.stroke();
            return null;
        });
        Norm.fillCircle = (x) => (y) => (r) => IO(() => {
            __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
            __EXTERNAL__.context.fill();
            return null;
        });
        Norm.fillCircleVector = (coordinates) => (r) => IO(() => {
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
        Norm.area = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.rect(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, (jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height), null));
        Norm.areaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.rect(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, (j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height), null));
        Norm.strokeArea = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.strokeRect(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, (jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height), null));
        Norm.strokeAreaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.strokeRect(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, (j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height), null));
        Norm.fillArea = (ix) => (iy) => (jx) => (jy) => IO(() => (__EXTERNAL__.context.fillRect(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, (jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height), null));
        Norm.fillAreaVector = (i) => (j) => IO(() => (__EXTERNAL__.context.fillRect(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, (j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height), null));
    })(Norm = Effect.Norm || (Effect.Norm = {}));
    Effect.log = (message) => IO(() => (console.log(message), null));
    Effect.flush = IO(() => (console.clear(), null));
    Effect.queue = (io) => IO(() => (io.INFO(), null));
    Effect.tick = IO(() => {
        for (const k in __EXTERNAL__.keyboard)
            __EXTERNAL__.keyboard[k] = relaxVertical(__EXTERNAL__.keyboard[k]);
        for (const i in __EXTERNAL__.mouse.buttons)
            __EXTERNAL__.mouse.buttons[i] = relaxVertical(__EXTERNAL__.mouse.buttons[i]);
        __EXTERNAL__.mouse.scroll = Vertical.CenterY;
        __EXTERNAL__.mouse.deltaX = 0;
        __EXTERNAL__.mouse.deltaY = 0;
        return null;
    });
    Effect.activatePointerLock = IO(() => {
        __EXTERNAL__.context.canvas.onmousedown = () => {
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
    Effect.arc = (x) => (y) => (r) => (a) => (b) => IO(() => (__EXTERNAL__.context.arc(x, y, r, a, b), null));
    Effect.arcVector = (v) => (r) => (a) => (b) => IO(() => (__EXTERNAL__.context.arc(v.x, v.y, r, a, b), null));
    Effect.circle = (x) => (y) => (r) => IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), null));
    Effect.circleVector = (coordinates) => (r) => IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), null));
    Effect.strokeCircle = (x) => (y) => (r) => IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.stroke(), null));
    Effect.strokeCircleVector = (coordinates) => (r) => IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.stroke(), null));
    Effect.fillCircle = (x) => (y) => (r) => IO(() => (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.fill(), null));
    Effect.fillCircleVector = (coordinates) => (r) => IO(() => (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.fill(), null));
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
