"use strict";
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var THROW = function (message) { throw new Error(message); };
var MAXARRAY = 1024;
var MAXSTRING = 1024;
var MAX_LIST_OPS = 1024;
var ERROR = {
    MAX_LIST_OPS: function (op, org) {
        return THROW("'" + op + "' reached the max amount of traversal (" + MAX_LIST_OPS + ") allowed in a list " + (org ? "| origin : '" + org + "'" : ''));
    },
    BINDING_NILS: function (org, op) {
        return THROW("'(" + op + ")' was used on a infinite list with an operation that always return a nil | origin : '" + org + "'");
    },
    ONLY_INTEGER: function (org, n) {
        return THROW("'" + org + "' only accepts integers as an amount; instead received '" + n + "'");
    },
    ONLY_NATURAL: function (org, n) {
        return THROW("'" + org + "' only accepts natural numbers (0 inclusive); instead received '" + n + "'");
    },
    ONLY_CONS: function (org) {
        return THROW("'" + org + "' only accepts non-empty lists");
    }
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
var pipe = function (value) { return function (morphism) { return value.pipe(morphism); }; };
var eq = function (x) { return function (y) { return x.eq(y); }; };
var neq = function (x) { return function (y) { return !x.eq(y); }; };
var fst = function (pair) { return pair.fst; };
var snd = function (pair) { return pair.snd; };
var link = function (firsts) { return function (seconds) { return firsts.link(seconds); }; };
var head = function (xs) { return xs.head; };
var tail = function (xs) { return xs.tail; };
var E = 2.718281828459045;
var LN2 = 0.6931471805599453;
var LN10 = 2.302585092994046;
var LOG2E = 1.4426950408889634;
var LOG10E = 0.4342944819032518;
var PI = 3.141592653589793;
var PIDIV180 = 0.017453292519943295;
var PIDIV180INV = 57.29577951308232;
var TAU = 6.283185307179586;
var INVSQRT2 = 0.7071067811865476;
var SQRT2 = 1.4142135623730951;
var abs = Math.abs;
var acos = Math.acos;
var acosh = Math.acosh;
var add = function (x) { return function (y) { return x + y; }; };
var AND = function (x) { return function (y) { return x & y; }; };
var and = function (x) { return function (y) { return x && y; }; };
var applyWhen = function (condition) { return function (f) { return condition ? f : id; }; };
var approx = function (x) { return function (y) { return function (error) { return Math.abs(x - y) < error; }; }; };
var napprox = function (x) { return function (y) { return function (error) { return Math.abs(x - y) > error; }; }; };
var asin = Math.asin;
var asinh = Math.asinh;
var atan = Math.atan;
var atan2 = function (y) { return function (x) { return Math.atan2(y, x); }; };
var ratan2 = function (x) { return function (y) { return Math.atan2(y, x); }; };
var atanh = Math.atanh;
var BIT = function (x) { return x ? 1 : 0; };
var cbrt = Math.cbrt;
var ceil = Math.ceil;
var clz32 = Math.clz32;
var cos = Math.cos;
var cosh = Math.cosh;
var diff = function (x) { return function (y) { return Math.abs(x - y); }; };
var div = function (x) { return function (y) { return x / y; }; };
var rdiv = function (y) { return function (x) { return x / y; }; };
var even = function (x) { return x % 2 === 0; };
var exp = Math.exp;
var expm1 = Math.expm1;
var flip = function (f) { return function (y) { return function (x) { return f(x)(y); }; }; };
var floor = Math.floor;
var fround = Math.fround;
var greater = function (x) { return function (y) { return y > x; }; };
var greaterEqual = function (x) { return function (y) { return y >= x; }; };
var gt = function (x) { return function (y) { return x > y; }; };
var gte = function (x) { return function (y) { return x >= y; }; };
var id = function (x) { return x; };
var isInsideExclusive = function (n) { return function (lower) { return function (upper) { return lower < n && n < upper; }; }; };
var isInsideInclusive = function (n) { return function (lower) { return function (upper) { return lower <= n && n <= upper; }; }; };
var isOutsideExclusive = function (n) { return function (lower) { return function (upper) { return n < lower || upper < n; }; }; };
var isOutsideInclusive = function (n) { return function (lower) { return function (upper) { return n <= lower || upper <= n; }; }; };
var ln = Math.log;
var log10 = Math.log10;
var lnp1 = Math.log1p;
var log2 = Math.log2;
var LSHIFT = function (x) { return function (y) { return x << y; }; };
var rLSHIFT = function (y) { return function (x) { return x << y; }; };
var lerp = function (t) { return function (x) { return function (y) { return x + (y - x) * t; }; }; };
var less = function (x) { return function (y) { return y < x; }; };
var lessEqual = function (x) { return function (y) { return y <= x; }; };
var lt = function (x) { return function (y) { return x < y; }; };
var lte = function (x) { return function (y) { return x <= y; }; };
var max = function (x) { return function (y) { return Math.max(x, y); }; };
var min = function (x) { return function (y) { return Math.min(x, y); }; };
var mod = function (x) { return function (y) { return x % y; }; };
var rmod = function (y) { return function (x) { return x % y; }; };
var mul = function (x) { return function (y) { return x * y; }; };
var NAND = function (x) { return function (y) { return ~(x & y); }; };
var nand = function (x) { return function (y) { return !(x && y); }; };
var negate = function (x) { return -x; };
var NOR = function (x) { return function (y) { return ~(x | y); }; };
var nor = function (x) { return function (y) { return !(x || y); }; };
var NOT = function (x) { return ~x; };
var not = function (x) { return !x; };
var odd = function (x) { return Math.abs(x) % 2 === 1; };
var OR = function (x) { return function (y) { return x | y; }; };
var or = function (x) { return function (y) { return x || y; }; };
var pow = function (x) { return function (y) { return Math.pow(x, y); }; };
var rpow = function (y) { return function (x) { return Math.pow(x, y); }; };
var pythagoras = function (x) { return function (y) { return Math.sqrt(x * x + y * y); }; };
var reciprocate = function (x) { return 1 / x; };
var round = Math.round;
var RSHIFT = function (x) { return function (y) { return x >> y; }; };
var rRSHIFT = function (y) { return function (x) { return x >> y; }; };
var sign = Math.sign;
var sin = Math.sin;
var sinh = Math.sinh;
var sqrt = Math.sqrt;
var sub = function (x) { return function (y) { return x - y; }; };
var rsub = function (y) { return function (x) { return x - y; }; };
var tan = Math.tan;
var tanh = Math.tanh;
var toDegrees = function (degrees) { return degrees * PIDIV180INV; };
var toHexColor = function (decimal) { return "#" + ((~~Math.abs(decimal)) % 16777216).toString(16).padStart(6, '0'); };
var toRadians = function (degrees) { return degrees * PIDIV180; };
var trunc = function (x) { return ~~x; };
var URSHIFT = function (x) { return function (y) { return x >>> y; }; };
var rURSHIFT = function (y) { return function (x) { return x >>> y; }; };
var XOR = function (x) { return function (y) { return x ^ y; }; };
var xor = function (x) { return function (y) { return x !== y; }; };
Boolean.prototype.pipe = Number.prototype.pipe = (String.prototype.pipe = function (f) { return f(this); });
Boolean.prototype.eq = Number.prototype.eq = (String.prototype.eq = function (x) { return this === x; });
var Pair = function (first, second) {
    return ({
        CONS: 'Pair',
        pipe: function (f) { return f(this); },
        eq: function (x) { return x.fst.eq(first) && x.snd.eq(second); },
        fst: first,
        snd: second
    });
};
var IO = function (sideeffect) {
    return ({
        CONS: 'IO',
        INFO: sideeffect,
        pipe: function (f) { return f(this); },
        bind: function (f) { return IO(function () { return f(sideeffect()).INFO(); }); },
        fmap: function (f) { return IO(function () { return f(sideeffect()); }); },
        bindto: function (k, f) {
            return IO(function () {
                var _a;
                var $ = sideeffect();
                return __assign(__assign({}, $), (_a = {}, _a[k] = f($).INFO(), _a));
            });
        },
        fmapto: function (k, f) {
            return IO(function () {
                var _a;
                var $ = sideeffect();
                return __assign(__assign({}, $), (_a = {}, _a[k] = f($), _a));
            });
        },
        then: function (x) { return IO(function () { return (sideeffect(), x.INFO()); }); },
        side: function (x) {
            return IO(function () {
                var y = sideeffect();
                x.INFO();
                return y;
            });
        },
        also: function (f) {
            return IO(function () {
                var y = sideeffect();
                f(y).INFO();
                return y;
            });
        },
        cast: function (x) { return IO(function () { return (sideeffect(), x); }); }
    });
};
var Nothing = {
    CONS: 'Nothing',
    pipe: function (f) { return f(Nothing); },
    eq: function (x) { return x === Nothing; },
    bind: function (_) { return Nothing; },
    fmap: function (_) { return Nothing; },
    bindto: function (_) { return Nothing; },
    fmapto: function (_) { return Nothing; },
    cast: function (_) { return Nothing; }
};
var Just = function (value) {
    return ({
        CONS: 'Just',
        INFO: value,
        pipe: function (f) { return f(this); },
        eq: function (x) { return x.CONS === 'Just' && x.INFO.eq(value); },
        bind: function (f) { return f(value); },
        fmap: function (f) { return Just(f(value)); },
        bindto: function (k, f) { return f(value).fmap(function (x) {
            var _a;
            return (__assign(__assign({}, value), (_a = {}, _a[k] = x, _a)));
        }); },
        fmapto: function (k, f) {
            var _a;
            return Just(__assign(__assign({}, value), (_a = {}, _a[k] = f(value), _a)));
        },
        cast: Just
    });
};
var Process = function (computation) {
    return ({
        CONS: 'Process',
        INFO: computation,
        pipe: function (f) { return f(this); },
        bind: function (f) {
            return Process(function (s) {
                var x = computation(s);
                return f(x.snd).INFO(x.fst);
            });
        },
        fmap: function (f) {
            return Process(function (s) {
                var x = computation(s);
                return Pair(x.fst, f(x.snd));
            });
        },
        bindto: function (k, f) {
            return Process(function (s) {
                var _a;
                var x = computation(s), y = f(x.snd).INFO(x.fst);
                return Pair(y.fst, __assign(__assign({}, x.snd), (_a = {}, _a[k] = y.snd, _a)));
            });
        },
        fmapto: function (k, f) {
            return Process(function (s) {
                var _a;
                var x = computation(s);
                return Pair(x.fst, __assign(__assign({}, x.snd), (_a = {}, _a[k] = f(x.snd), _a)));
            });
        },
        then: function (x) { return Process(function (s) { return x.INFO(computation(s).fst); }); },
        side: function (x) {
            return Process(function (s) {
                var y = computation(s);
                return Pair(x.INFO(y.fst).fst, y.snd);
            });
        },
        also: function (f) {
            return Process(function (s) {
                var y = computation(s);
                return Pair(f(y.snd).INFO(y.fst).fst, y.snd);
            });
        },
        cast: function (x) { return Process(function (s) { return Pair(computation(s).fst, x); }); }
    });
};
var Nil = {
    CONS: 'Nil',
    pipe: function (f) { return f(Nil); },
    eq: function (xs) { return xs === Nil; },
    bind: function (_) { return Nil; },
    fmap: function (_) { return Nil; },
    bindto: function (_) { return Nil; },
    fmapto: function (_) { return Nil; },
    cast: function (_) { return Nil; },
    link: function (xs) { return xs; },
    get head() { return THROW("'(.head)' cannot be used on an empty list"); },
    get tail() { return THROW("'(.tail)' cannot be used on an empty list"); }
};
var Cons = function (lfirst) { return function (lrest) {
    return ({
        CONS: 'Cons',
        pipe: function (f) { return f(this); },
        eq: function (xs) {
            var ys = this;
            for (var i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
                if (i === MAX_LIST_OPS)
                    ERROR.MAX_LIST_OPS('(.eq)', 'Cons');
                else if (!xs.head.eq(ys.head))
                    return false;
            return xs.CONS === ys.CONS;
        },
        bind: function (f) {
            var _this = this;
            var xs = f(this.head);
            return xs.CONS === 'Nil'
                ? this.tail.bind(f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(_this.tail.bind(f)); });
        },
        fmap: function (f) {
            var _this = this;
            return Cons(function () { return f(_this.head); })(function () { return _this.tail.fmap(f); });
        },
        bindto: function (k, f) {
            return this.bind(function ($) { return f($).fmap(function (x) {
                var _a;
                return (__assign(__assign({}, $), (_a = {}, _a[k] = x, _a)));
            }); });
        },
        fmapto: function (k, f) {
            return this.fmap(function ($) {
                var _a;
                return (__assign(__assign({}, $), (_a = {}, _a[k] = f($), _a)));
            });
        },
        cast: function (x) {
            return this.fmap(function (_) { return x; });
        },
        link: function (xs) {
            var _this = this;
            return xs.CONS === 'Nil'
                ? this
                : Cons(function () { return _this.head; })(function () { return _this.tail.link(xs); });
        },
        get head() { var _a; return (_a = this.$head) !== null && _a !== void 0 ? _a : (this.$head = lfirst()); },
        get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = lrest()); }
    });
}; };
var List = function () {
    var elements = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        elements[_i] = arguments[_i];
    }
    var xs = Nil;
    for (var i = elements.length - 1; ~i; --i)
        xs = prepend(elements[i])(xs);
    return xs;
};
var Left = function (lefty) {
    return ({
        CONS: 'Left',
        INFO: lefty,
        pipe: function (f) { return f(this); },
        eq: function (x) { return x.CONS === 'Left' && x.INFO.eq(lefty); }
    });
};
var Right = function (righty) {
    return ({
        CONS: 'Right',
        INFO: righty,
        pipe: function (f) { return f(this); },
        eq: function (x) { return x.CONS === 'Right' && x.INFO.eq(righty); }
    });
};
var Vector2 = function (x, y) {
    return ({
        CONS: 'Vector2',
        eq: function (v) { return v.x === x && v.y === y; },
        pipe: function (f) { return f(this); },
        x: x, y: y
    });
};
var Vector3 = function (x, y, z) {
    return ({
        CONS: 'Vector3',
        eq: function (v) { return v.x === x && v.y === y && v.z === z; },
        pipe: function (f) { return f(this); },
        x: x, y: y, z: z
    });
};
var Vector4 = function (x, y, z, w) {
    return ({
        CONS: 'Vector4',
        eq: function (v) { return v.x === x && v.y === y && v.z === z && v.w === w; },
        pipe: function (f) { return f(this); },
        x: x, y: y, z: z, w: w
    });
};
var Matrix2 = function (ix, jx, iy, jy) {
    return ({
        CONS: 'Matrix2',
        pipe: function (f) { return f(this); },
        eq: function (m) {
            return m.ix === ix && m.jx === jx &&
                m.iy === iy && m.jy === jy;
        },
        ix: ix, jx: jx, iy: iy, jy: jy
    });
};
var Matrix3 = function (ix, jx, kx, iy, jy, ky, iz, jz, kz) {
    return ({
        CONS: 'Matrix3',
        pipe: function (f) { return f(this); },
        eq: function (m) {
            return m.ix === ix && m.jx === jx && m.kx === kx &&
                m.iy === iy && m.jy === jy && m.ky === ky &&
                m.iz === iz && m.jz === jz && m.kz === kz;
        },
        ix: ix, jx: jx, kx: kx, iy: iy, jy: jy, ky: ky, iz: iz, jz: jz, kz: kz
    });
};
var Matrix4 = function (ix, jx, kx, lx, iy, jy, ky, ly, iz, jz, kz, lz, iw, jw, kw, lw) {
    return ({
        CONS: 'Matrix4',
        pipe: function (f) { return f(this); },
        eq: function (m) {
            return m.ix === ix && m.jx === jx && m.kx === kx && m.lx === lx &&
                m.iy === iy && m.jy === jy && m.ky === ky && m.ly === ly &&
                m.iz === iz && m.jz === jz && m.kz === kz && m.lz === lz &&
                m.iw === iw && m.jw === jw && m.kw === kw && m.lw === lw;
        },
        ix: ix, jx: jx, kx: kx, lx: lx, iy: iy, jy: jy, ky: ky, ly: ly, iz: iz, jz: jz, kz: kz, lz: lz, iw: iw, jw: jw, kw: kw, lw: lw
    });
};
var TextMeasurement = function (text) { return function (width) { return function (height) {
    return ({
        CONS: 'TextMeasurement',
        pipe: function (f) { return f(this); },
        eq: function (m) { return m.text === text && m.width === width && m.height === height; },
        text: text, width: width, height: height
    });
}; }; };
var Mapping = function () {
    var pairs = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        pairs[_i] = arguments[_i];
    }
    return ({
        CONS: 'Mapping',
        codomain: function (x) {
            var _a;
            return ((_a = pairs.find(function (p) { return p[0].eq(x); })) !== null && _a !== void 0 ? _a : THROW("'(.codomain)' was non-exhaustive in 'Mapping'; no corresponding codomain for value '" + x + "'"))[1];
        },
        domain: function (x) {
            var _a;
            return ((_a = pairs.find(function (p) { return p[1].eq(x); })) !== null && _a !== void 0 ? _a : THROW("'(.domain)' was non-exhaustive in 'Mapping'; no corresponding domain for value '" + x + "'"))[0];
        }
    });
};
var curry = function (f) { return function (first) { return function (second) {
    return f(Pair(first, second));
}; }; };
var uncurry = function (f) { return function (parameters) {
    return f(parameters.fst)(parameters.snd);
}; };
var swap = function (pair) {
    return Pair(pair.snd, pair.fst);
};
var ffst = function (morphism) { return function (pair) {
    return Pair(morphism(pair.fst), pair.snd);
}; };
var fsnd = function (morphism) { return function (pair) {
    return Pair(pair.fst, morphism(pair.snd));
}; };
var fboth = function (morphism) { return function (pair) {
    return Pair(morphism(pair.fst), morphism(pair.snd));
}; };
var pick = function (bool) {
    return bool ? fst : snd;
};
var idle = IO(function () { return null; });
var isNothing = function (maybe) { return maybe.CONS === 'Nothing'; };
var isJust = function (maybe) { return maybe.CONS === 'Just'; };
var ffromMaybe = function (fallback) { return function (morphism) { return function (maybe) {
    return maybe.CONS === 'Nothing'
        ? fallback
        : morphism(maybe.INFO);
}; }; };
var fromJust = function (maybe) {
    return maybe.CONS === 'Nothing'
        ? THROW("'fromJust' cannot be used on 'Nothing'")
        : maybe.INFO;
};
var fromMaybe = function (fallback) { return function (maybe) {
    return maybe.CONS === 'Nothing'
        ? fallback
        : maybe.INFO;
}; };
var ensure = function (predicate) { return function (value) {
    return predicate(value)
        ? Just(value)
        : Nothing;
}; };
var put = function (replacement) { return function (process) {
    return Process(function (s) { return Pair(replacement, process.INFO(s).snd); });
}; };
var get = function (process) {
    return Process(function (s) {
        var x = process.INFO(s).fst;
        return Pair(x, x);
    });
};
var runProcess = function (process) { return function (state) {
    return process.INFO(state);
}; };
var execProcess = function (process) { return function (state) {
    return process.INFO(state).fst;
}; };
var evalProcess = function (process) { return function (state) {
    return process.INFO(state).snd;
}; };
var mapProcess = function (morphism) { return function (process) {
    return Process(function (s) { return morphism(process.INFO(s)); });
}; };
var endomapState = function (endomorphism) { return function (process) {
    return Process(function (s) {
        var x = process.INFO(s);
        return Pair(endomorphism(x.fst), x.snd);
    });
}; };
var isNil = function (xs) { return xs.CONS === 'Nil'; };
var isCons = function (xs) { return xs.CONS === 'Cons'; };
var array = function (xs) {
    var ys = [];
    for (var i = 0; xs.CONS === 'Cons'; ++i, ys.push(xs.head), xs = xs.tail)
        if (i === MAXARRAY) {
            console.warn("'array' has reached the maximum array representation possible (" + MAXARRAY + ") for the given list");
            break;
        }
    return ys;
};
var string = function (xs) {
    var str = "";
    for (var i = 0; xs.CONS === 'Cons'; ++i, str += xs.head)
        if (i === MAXSTRING) {
            console.warn("'string' has reached the maximum string representation possible (" + MAXSTRING + ") for the given list");
            break;
        }
    return str;
};
var chars = function (str) {
    return str
        ?
            ({
                CONS: 'Cons',
                pipe: function (f) { return f(this); },
                eq: function (xs) {
                    for (var i = 0; i < str.length && xs.CONS === 'Cons'; ++i, xs = xs.tail)
                        if (xs.head !== str[i])
                            return false;
                    return xs.CONS === 'Nil';
                },
                bind: function (f) {
                    var _this = this;
                    var xs = f(str[0]);
                    return xs.CONS === 'Nil'
                        ? this.tail.bind(f)
                        : Cons(function () { return xs.head; })(function () { return xs.tail.link(_this.tail.bind(f)); });
                },
                fmap: function (f) {
                    var _this = this;
                    return Cons(function () { return f(str[0]); })(function () { return _this.tail.fmap(f); });
                },
                bindto: function (_) { return THROW("'(.bindto)' was used on a list of characters; likely done on accident | origin : 'chars'"); },
                fmapto: function (_) { return THROW("'(.fmapto)' was used on a list of characters; likely done on accident | origin : 'chars'"); },
                cast: function (x) {
                    var _this = this;
                    return lprepend(x)(function () { return _this.tail.cast(x); });
                },
                link: function (xs) {
                    var _this = this;
                    return lprepend(str[0])(function () { return _this.tail.link(xs); });
                },
                head: str[0],
                get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = chars(str.slice(1))); },
                $head: str[0],
                $last: str[str.length - 1],
                $len: str.length
            })
        : Nil;
};
var singleton = function (value) {
    return ({
        CONS: 'Cons',
        pipe: function (f) { return f(this); },
        eq: function (xs) { return xs.CONS === 'Cons' && xs.tail.CONS === 'Nil' && xs.head.eq(value); },
        bind: function (f) { return f(value); },
        fmap: function (f) { return singleton(f(value)); },
        bindto: function (k, f) { return f(value).fmap(function (x) {
            var _a;
            return (__assign(__assign({}, value), (_a = {}, _a[k] = x, _a)));
        }); },
        fmapto: function (k, f) {
            var _a;
            return singleton(__assign(__assign({}, value), (_a = {}, _a[k] = f(value), _a)));
        },
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
};
var prepend = function (first) { return function (rest) {
    return ({
        CONS: 'Cons',
        pipe: function (f) { return f(this); },
        eq: function (xs) {
            if (xs.CONS === 'Nil' || !xs.head.eq(first))
                return false;
            var ys = rest;
            xs = xs.tail;
            for (var i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
                if (i === MAX_LIST_OPS)
                    ERROR.MAX_LIST_OPS('(.eq)', 'prepend');
                else if (!xs.head.eq(ys.head))
                    return false;
            return xs.CONS === ys.CONS;
        },
        bind: function (f) {
            var xs = f(first);
            return xs.CONS === 'Nil'
                ? rest.bind(f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(rest.bind(f)); });
        },
        fmap: function (f) { return Cons(function () { return f(first); })(function () { return rest.fmap(f); }); },
        bindto: function (k, f) {
            var xs = f(first).fmap(function (x) {
                var _a;
                return (__assign(__assign({}, first), (_a = {}, _a[k] = x, _a)));
            });
            return xs.CONS === 'Nil'
                ? rest.bindto(k, f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(rest.bindto(k, f)); });
        },
        fmapto: function (k, f) { return Cons(function () {
            var _a;
            return (__assign(__assign({}, first), (_a = {}, _a[k] = f(first), _a)));
        })(function () { return rest.fmapto(k, f); }); },
        cast: function (x) { return lprepend(x)(function () { return rest.cast(x); }); },
        link: function (xs) { return lprepend(first)(function () { return rest.link(xs); }); },
        head: first,
        tail: rest,
        $head: first,
        $tail: rest,
        $last: rest.CONS === 'Nil' ? first : rest.$last,
        $len: rest.$len === undefined ? undefined : 1 + rest.$len
    });
}; };
var lprepend = function (first) { return function (lrest) {
    return ({
        CONS: 'Cons',
        pipe: function (f) { return f(this); },
        eq: function (xs) {
            if (xs.CONS === 'Nil' || !xs.head.eq(first))
                return false;
            var ys = this.tail;
            xs = xs.tail;
            for (var i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
                if (i === MAX_LIST_OPS)
                    ERROR.MAX_LIST_OPS('(.eq)', 'lprepend');
                else if (!xs.head.eq(ys.head))
                    return false;
            return xs.CONS === ys.CONS;
        },
        bind: function (f) {
            var _this = this;
            var xs = f(first);
            return xs.CONS === 'Nil'
                ? this.tail.bind(f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(_this.tail.bind(f)); });
        },
        fmap: function (f) {
            var _this = this;
            return Cons(function () { return f(first); })(function () { return _this.tail.fmap(f); });
        },
        bindto: function (k, f) {
            var _this = this;
            var xs = f(first).fmap(function (x) {
                var _a;
                return (__assign(__assign({}, first), (_a = {}, _a[k] = x, _a)));
            });
            return xs.CONS === 'Nil'
                ? this.tail.bindto(k, f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(_this.tail.bindto(k, f)); });
        },
        fmapto: function (k, f) {
            var _this = this;
            return Cons(function () {
                var _a;
                return (__assign(__assign({}, first), (_a = {}, _a[k] = f(first), _a)));
            })(function () { return _this.tail.fmapto(k, f); });
        },
        cast: function (x) {
            var _this = this;
            return lprepend(x)(function () { return _this.tail.cast(x); });
        },
        link: function (xs) {
            var _this = this;
            return lprepend(first)(function () { return _this.tail.link(xs); });
        },
        head: first,
        get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = lrest()); },
        $head: first
    });
}; };
var llprepend = function (lfirst) { return function (rest) {
    return ({
        CONS: 'Cons',
        pipe: function (f) { return f(this); },
        eq: function (xs) {
            if (xs.CONS === 'Nil' || !xs.head.eq(this.head))
                return false;
            var ys = rest;
            xs = xs.tail;
            for (var i = 0; xs.CONS === 'Cons' && ys.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
                if (i === MAX_LIST_OPS)
                    ERROR.MAX_LIST_OPS('(.eq)', 'llprepend');
                else if (!xs.head.eq(ys.head))
                    return false;
            return xs.CONS === ys.CONS;
        },
        bind: function (f) {
            var xs = f(this.head);
            return xs.CONS === 'Nil'
                ? rest.bind(f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(rest.bind(f)); });
        },
        fmap: function (f) {
            var _this = this;
            return Cons(function () { return f(_this.head); })(function () { return rest.fmap(f); });
        },
        bindto: function (k, f) {
            var _this = this;
            var xs = f(this.head).fmap(function (x) {
                var _a;
                return (__assign(__assign({}, _this.head), (_a = {}, _a[k] = x, _a)));
            });
            return xs.CONS === 'Nil'
                ? rest.bindto(k, f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(rest.bindto(k, f)); });
        },
        fmapto: function (k, f) {
            var _this = this;
            return Cons(function () {
                var _a;
                return (__assign(__assign({}, _this.head), (_a = {}, _a[k] = f(_this.head), _a)));
            })(function () { return rest.fmapto(k, f); });
        },
        cast: function (x) {
            return lprepend(x)(function () { return rest.cast(x); });
        },
        link: function (xs) {
            var _this = this;
            return Cons(function () { return _this.head; })(function () { return rest.link(xs); });
        },
        get head() { var _a; return (_a = this.$head) !== null && _a !== void 0 ? _a : (this.$head = lfirst()); },
        tail: rest,
        $tail: rest,
        $last: rest.$last,
        $len: rest.$len === undefined ? undefined : 1 + rest.$len
    });
}; };
var repeat = function (value) {
    return ({
        CONS: 'Cons',
        pipe: function (f) { return f(this); },
        eq: function (xs) {
            for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
                if (i === MAX_LIST_OPS)
                    ERROR.MAX_LIST_OPS('(.eq)', 'repeat');
                else if (!xs.head.eq(value))
                    return false;
            return false;
        },
        bind: function (f) {
            var xs = f(value);
            return xs.CONS === 'Nil'
                ? ERROR.BINDING_NILS('repeat', 'bind')
                : cycle(xs);
        },
        fmap: function (f) { return repeat(f(value)); },
        bindto: function (k, f) {
            var xs = f(value).fmap(function (x) {
                var _a;
                return (__assign(__assign({}, value), (_a = {}, _a[k] = x, _a)));
            });
            return xs.CONS === 'Nil'
                ? ERROR.BINDING_NILS('repeat', 'bindto')
                : cycle(xs);
        },
        fmapto: function (k, f) {
            var _a;
            return repeat(__assign(__assign({}, value), (_a = {}, _a[k] = f(value), _a)));
        },
        link: function (_) { return this; },
        cast: repeat,
        head: value,
        get tail() { return this; },
        $head: value,
        get $tail() { return this; },
        get $init() { return this; }
    });
};
var cycle = function (pattern) {
    return pattern.CONS === 'Nil'
        ? ERROR.ONLY_CONS('cycle')
        :
            ({
                CONS: 'Cons',
                pipe: function (f) { return f(this); },
                eq: function (xs) {
                    var ys = this;
                    for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
                        if (i === MAX_LIST_OPS)
                            ERROR.MAX_LIST_OPS('(.eq)', 'cycle');
                        else if (!xs.head.eq(ys.head))
                            return false;
                    return false;
                },
                bind: function (f) {
                    var xs = pattern.bind(f);
                    return xs.CONS === 'Nil'
                        ? ERROR.BINDING_NILS('cycle', 'bind')
                        : cycle(xs);
                },
                fmap: function (f) { return cycle(pattern.fmap(f)); },
                bindto: function (k, f) {
                    var xs = pattern.bind(function ($) { return f($).fmap(function (x) {
                        var _a;
                        return (__assign(__assign({}, $), (_a = {}, _a[k] = x, _a)));
                    }); });
                    return xs.CONS === 'Nil'
                        ? ERROR.BINDING_NILS('cycle', 'bindto')
                        : cycle(xs);
                },
                fmapto: function (k, f) { return cycle(pattern.fmap(function ($) {
                    var _a;
                    return (__assign(__assign({}, $), (_a = {}, _a[k] = f($), _a)));
                })); },
                link: function (_) { return this; },
                cast: repeat,
                get head() { var _a; return (_a = this.$head) !== null && _a !== void 0 ? _a : (this.$head = pattern.head); },
                get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = pattern.tail.link(this)); },
                get $init() { return this; }
            });
};
var iterate = function (endomorphism) { return function (initial) {
    return ({
        CONS: 'Cons',
        pipe: function (f) { return f(this); },
        eq: function (xs) {
            var ys = this;
            for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail, ys = ys.tail)
                if (i === MAX_LIST_OPS)
                    ERROR.MAX_LIST_OPS('(.eq)', 'iterate');
                else if (!xs.head.eq(ys.head))
                    return false;
            return false;
        },
        bind: function (f) {
            var _this = this;
            var xs = f(initial);
            return xs.CONS === 'Nil'
                ? this.tail.bind(f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(_this.tail.bind(f)); });
        },
        fmap: function (f) {
            var _this = this;
            return Cons(function () { return f(initial); })(function () { return _this.tail.fmap(f); });
        },
        bindto: function (k, f) {
            var _this = this;
            var xs = f(initial).fmap(function (x) {
                var _a;
                return (__assign(__assign({}, initial), (_a = {}, _a[k] = x, _a)));
            });
            return xs.CONS === 'Nil'
                ? this.tail.bindto(k, f)
                : Cons(function () { return xs.head; })(function () { return xs.tail.link(_this.tail.bindto(k, f)); });
        },
        fmapto: function (k, f) {
            var _this = this;
            return Cons(function () {
                var _a;
                return (__assign(__assign({}, initial), (_a = {}, _a[k] = f(initial), _a)));
            })(function () { return _this.fmapto(k, f); });
        },
        link: function (_) { return this; },
        cast: repeat,
        head: initial,
        get tail() { var _a; return (_a = this.$tail) !== null && _a !== void 0 ? _a : (this.$tail = iterate(endomorphism)(endomorphism(initial))); },
        $head: initial,
        get $init() { return this; }
    });
}; };
var replicate = function (amount) { return function (value) {
    return Number.isInteger(value)
        ? amount > 0
            ?
                ({
                    CONS: 'Cons',
                    pipe: function (f) { return f(this); },
                    eq: function (xs) {
                        for (var i = 0; i < amount; ++i, xs = xs.tail)
                            if (xs.CONS === 'Nil' || !xs.head.eq(value))
                                return false;
                        return xs.CONS === 'Nil';
                    },
                    bind: function (f) { return concat(replicate(amount)(f(value))); },
                    fmap: function (f) { return replicate(amount)(f(value)); },
                    bindto: function (k, f) {
                        var _a;
                        return concat(replicate(amount)(__assign(__assign({}, value), (_a = {}, _a[k] = f(value), _a))));
                    },
                    fmapto: function (k, f) {
                        var _a;
                        return replicate(amount)(__assign(__assign({}, value), (_a = {}, _a[k] = f(value), _a)));
                    },
                    cast: replicate(amount),
                    link: function (xs) {
                        var _this = this;
                        return xs.CONS === 'Nil'
                            ? this
                            : lprepend(value)(function () { return _this.tail.link(xs); });
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
}; };
var all = function (predicate) { return function (xs) {
    for (var i = 0; xs.CONS === 'Cons'; ++i)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('all');
        else if (predicate(xs.head))
            xs = xs.tail;
        else
            return false;
    return true;
}; };
var any = function (predicate) { return function (xs) {
    for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('any');
        else if (predicate(xs.head))
            return true;
    return false;
}; };
var at = function (index) { return function (xs) {
    if (index < 0 || !Number.isInteger(index))
        ERROR.ONLY_NATURAL('at', index);
    for (var i = 0; i < index; ++i)
        if (xs.CONS === 'Nil')
            THROW("'at' received an index beyond the list; stopped at index '" + i + "' with goal of '" + index + "'");
        else
            xs = xs.tail;
    if (xs.CONS === 'Nil')
        THROW("'at' received an off-by-one error; cannot get index '" + index + "' in list of length " + index);
    return xs.head;
}; };
var concat = function (xss) {
    return xss.CONS === 'Nil'
        ? Nil
        : xss.head.CONS === 'Nil'
            ? concat(xss.tail)
            : Cons(function () { return xss.head.head; })(function () { return xss.head.tail.link(concat(xss.tail)); });
};
var drop = function (amount) { return function (xs) {
    if (!Number.isInteger(amount))
        ERROR.ONLY_INTEGER('drop', amount);
    for (var i = 0; i < amount && xs.CONS === 'Cons'; ++i)
        xs = xs.tail;
    return xs;
}; };
var dropWhile = function (predicate) { return function (xs) {
    for (var i = 0; xs.CONS === 'Cons' && predicate(xs.head); ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('dropWhile');
    return xs;
}; };
var elem = function (value) { return function (xs) {
    for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('elem');
        else if (xs.head.eq(value))
            return true;
    return false;
}; };
var elemIndices = function (value) { return function (xs) {
    return xs.CONS === 'Nil'
        ? Nil
        : xs.head.eq(value)
            ? lprepend(0)(function () { return elemIndices(value)(xs.tail).fmap(function (x) { return x + 1; }); })
            : elemIndices(value)(xs.tail).fmap(function (x) { return x + 1; });
}; };
var filter = function (predicate) { return function (xs) {
    return xs.CONS === 'Nil'
        ? Nil
        : predicate(xs.head)
            ? lprepend(xs.head)(function () { return filter(predicate)(xs.tail); })
            : filter(predicate)(xs.tail);
}; };
var findIndices = function (predicate) { return function (xs) {
    return xs.CONS === 'Nil'
        ? Nil
        : predicate(xs.head)
            ? lprepend(0)(function () { return findIndices(predicate)(xs.tail).fmap(function (x) { return x + 1; }); })
            : findIndices(predicate)(xs.tail).fmap(function (x) { return x + 1; });
}; };
var from = function (xs) { return function (index) {
    if (index < 0 || !Number.isInteger(index))
        ERROR.ONLY_NATURAL('from', index);
    var ys = xs;
    for (var i = 0; i < index; ++i)
        if (ys.CONS === 'Nil')
            THROW("'from' received an index beyond the list; stopped at index '" + i + "' with goal of '" + index + "'");
        else
            ys = ys.tail;
    if (ys.CONS === 'Nil')
        THROW("'from' received an off-by-one error; cannot get index '" + index + "' in list of length " + index);
    return ys.head;
}; };
var foldl = function (reducer) { return function (initial) { return function (xs) {
    var x = initial;
    for (var i = 0; xs.CONS === 'Cons'; ++i, x = reducer(x)(xs.head), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldl');
    return x;
}; }; };
var foldl1 = function (reducer) { return function (xs) {
    if (xs.CONS === 'Nil')
        ERROR.ONLY_CONS('foldl1');
    var x = xs.head;
    for (var i = 0; (xs = xs.tail).CONS === 'Cons'; ++i, x = reducer(x)(xs.head))
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldl1');
    return x;
}; };
var foldr = function (reducer) { return function (initial) { return function (xs) {
    xs = reverse(xs);
    var x = initial;
    for (var i = 0; xs.CONS === 'Cons'; ++i, x = reducer(xs.head)(x), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldr');
    return x;
}; }; };
var foldr1 = function (reducer) { return function (xs) {
    if (xs.CONS === 'Nil')
        ERROR.ONLY_CONS('foldr1');
    xs = reverse(xs);
    var x = xs.head;
    for (var i = 0; (xs = xs.tail).CONS === 'Cons'; ++i, x = reducer(xs.head)(x))
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('foldr1');
    return x;
}; };
var init = function (xs) {
    var _a;
    return ((_a = xs.$init) !== null && _a !== void 0 ? _a : xs.CONS === 'Nil')
        ? ERROR.ONLY_CONS('init')
        : xs.tail.CONS === 'Nil'
            ? Nil
            : Cons(function () { return xs.head; })(function () { return init(xs.tail); });
};
var inits = function (xs) {
    return xs.CONS === 'Nil'
        ? singleton(Nil)
        : lprepend(Nil)(function () { return inits(xs.tail).fmap(llprepend(function () { return xs.head; })); });
};
var intersperese = function (delimiter) { return function (xs) {
    return xs.CONS === 'Nil'
        ? Nil
        : xs.tail.CONS === 'Nil'
            ? xs
            : llprepend(function () { return xs.head; })(lprepend(delimiter)(function () { return intersperese(delimiter)(xs.tail); }));
}; };
var last = function (xs) {
    if (xs.$last !== undefined)
        return xs.$last;
    if (xs.CONS === 'Nil')
        ERROR.ONLY_CONS('last');
    if (xs.tail.CONS === 'Nil')
        return xs.head;
    for (var i = 0; (xs = xs.tail).tail.CONS === 'Cons'; ++i)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('last');
    return xs.head;
};
var len = function (xs) {
    if (xs.$len !== undefined)
        return xs.$len;
    var i = 0;
    while (xs.CONS === 'Cons')
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('len');
        else
            ++i, xs = xs.tail;
    return i;
};
var map = function (morphism) { return function (xs) {
    return xs.fmap(morphism);
}; };
var nelem = function (value) { return function (xs) {
    return !elem(value)(xs);
}; };
var partition = function (predicate) { return function (xs) {
    var ys = Nil;
    var zs = Nil;
    for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('partition');
        else if (predicate(xs.head))
            ys = prepend(xs.head)(ys);
        else
            zs = prepend(xs.head)(zs);
    return Pair(reverse(ys), reverse(zs));
}; };
var reverse = function (xs) {
    if (xs.$reverse !== undefined)
        return xs.$reverse;
    var ys = Nil;
    for (var i = 0; xs.CONS === 'Cons'; ++i, ys = prepend(xs.head)(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('reverse');
    return ys;
};
var scanl = function (reducer) { return function (initial) { return function (xs) {
    return xs.CONS === 'Nil'
        ? singleton(initial)
        : lprepend(initial)(function () { return scanl(reducer)(reducer(initial)(xs.head))(xs.tail); });
}; }; };
var scanl1 = function (reducer) { return function (xs) {
    return xs.CONS === 'Nil'
        ? Nil
        : scanl(reducer)(xs.head)(xs.tail);
}; };
var scanr = function (reducer) { return function (initial) { return function (xs) {
    xs = reverse(xs);
    var ys = singleton(initial);
    for (var i = 0; xs.CONS === 'Cons'; ++i, ys = prepend(reducer(xs.head)(ys.head))(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('scanr');
    return ys;
}; }; };
var scanr1 = function (reducer) { return function (xs) {
    xs = reverse(xs);
    var ys = singleton(xs.head);
    for (var i = 0; (xs = xs.tail).CONS === 'Cons'; ++i, ys = prepend(reducer(xs.head)(ys.head))(ys))
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('scanr1');
    return ys;
}; };
var span = function (predicate) { return function (xs) {
    var ys = Nil;
    for (var i = 0; xs.CONS === 'Cons' && predicate(xs.head); ++i, ys = prepend(xs.head)(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('span');
    return Pair(reverse(ys), xs);
}; };
var splitAt = function (amount) { return function (xs) {
    var ys = Nil;
    for (var i = 0; i < amount && xs.CONS === 'Cons'; ++i, ys = prepend(xs.head)(ys), xs = xs.tail)
        if (i === MAX_LIST_OPS)
            ERROR.MAX_LIST_OPS('splitAt');
    return Pair(reverse(ys), xs);
}; };
var tails = function (xs) {
    return xs.CONS === 'Nil'
        ? singleton(Nil)
        : lprepend(xs)(function () { return tails(xs.tail); });
};
var take = function (amount) { return function (xs) {
    return Number.isInteger(amount)
        ? amount > 0
            ? Cons(function () { return xs.head; })(function () { return take(amount - 1)(xs.tail); })
            : Nil
        : ERROR.ONLY_INTEGER('take', amount);
}; };
var takeWhile = function (predicate) { return function (xs) {
    return xs.CONS === 'Cons' && predicate(xs.head)
        ? lprepend(xs.head)(function () { return takeWhile(predicate)(xs.tail); })
        : Nil;
}; };
var unzip = function (xs) {
    return Pair(xs.fmap(fst), xs.fmap(snd));
};
var unzipWith = function (f) { return function (xs) {
    return unzip(xs.fmap(f));
}; };
var zip = function (xs) { return function (ys) {
    return xs.CONS === 'Nil' || ys.CONS === 'Nil'
        ? Nil
        : Cons(function () { return Pair(xs.head, ys.head); })(function () { return zip(xs.tail)(ys.tail); });
}; };
var zipWith = function (zipper) { return function (xs) { return function (ys) {
    return xs.CONS === 'Nil' || ys.CONS === 'Nil'
        ? Nil
        : Cons(function () { return zipper(xs.head)(ys.head); })(function () { return zipWith(zipper)(xs.tail)(ys.tail); });
}; }; };
var isLeft = function (either) { return either.CONS === 'Left'; };
var isRight = function (either) { return either.CONS === 'Right'; };
var eitherway = function (lf) { return function (rf) { return function (either) {
    return either.CONS === 'Left'
        ? lf(either.INFO)
        : rf(either.INFO);
}; }; };
var onLeft = function (lf) { return function (either) {
    return either.CONS === 'Left'
        ? Left(lf(either.INFO))
        : either;
}; };
var onRight = function (rf) { return function (either) {
    return either.CONS === 'Right'
        ? Right(rf(either.INFO))
        : either;
}; };
var haveLeft = function (fallback) { return function (either) {
    return either.CONS === 'Left'
        ? either.INFO
        : fallback;
}; };
var haveRight = function (fallback) { return function (either) {
    return either.CONS === 'Right'
        ? either.INFO
        : fallback;
}; };
var fromLeft = function (either) {
    return either.CONS === 'Left'
        ? either.INFO
        : THROW("'fromLeft' was used on a right-value");
};
var fromRight = function (either) {
    return either.CONS === 'Right'
        ? either.INFO
        : THROW("'fromRight' was used on a left-value");
};
var V2 = {
    origin: Vector2(0, 0),
    half: Vector2(0.5, 0.5),
    demoteV3: function (v) {
        return Vector2(v.x, v.y);
    },
    demoteV4: function (v) {
        return Vector2(v.x, v.y);
    },
    displace: function (delta) { return function (v) {
        return Vector2(v.x + delta, v.y + delta);
    }; },
    undisplace: function (delta) { return function (v) {
        return Vector2(v.x - delta, v.y - delta);
    }; },
    conjugate: function (v) {
        return Vector2(v.x, -v.y);
    },
    rotate: function (angle) { return function (v) {
        var c = Math.cos(angle), s = Math.sin(angle);
        return Vector2(v.x * c - v.y * s, v.y * c + v.x * s);
    }; },
    translate: function (dx) { return function (dy) { return function (v) {
        return Vector2(v.x + dx, v.y + dy);
    }; }; },
    untranslate: function (dx) { return function (dy) { return function (v) {
        return Vector2(v.x - dx, v.y - dy);
    }; }; },
    add: function (v) { return function (w) {
        return Vector2(v.x + w.x, v.y + w.y);
    }; },
    sub: function (v) { return function (w) {
        return Vector2(v.x - w.x, v.y - w.y);
    }; },
    scale: function (k) { return function (v) {
        return Vector2(v.x * k, v.y * k);
    }; },
    unscale: function (k) { return function (v) {
        return Vector2(v.x / k, v.y / k);
    }; },
    norm: function (v) {
        return Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2));
    },
    normalize: function (v) {
        return v.eq(V2.origin)
            ? V2.origin
            : V2.unscale(Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2)))(v);
    },
    dot: function (v) { return function (w) {
        return v.x * w.x + v.y * w.y;
    }; },
    transform: function (m) { return function (v) {
        return Vector2(m.ix * v.x + m.jx * v.y, m.iy * v.x + m.jy * v.y);
    }; },
    translateX: function (delta) { return function (v) {
        return Vector2(v.x + delta, v.y);
    }; },
    translateY: function (delta) { return function (v) {
        return Vector2(v.x, v.y + delta);
    }; },
    untranslateX: function (delta) { return function (v) {
        return Vector2(v.x - delta, v.y);
    }; },
    untranslateY: function (delta) { return function (v) {
        return Vector2(v.x, v.y - delta);
    }; },
    scaleX: function (scalar) { return function (v) {
        return Vector2(v.x * scalar, v.y);
    }; },
    scaleY: function (scalar) { return function (v) {
        return Vector2(v.x, v.y * scalar);
    }; },
    unscaleX: function (scalar) { return function (v) {
        return Vector2(v.x / scalar, v.y);
    }; },
    unscaleY: function (scalar) { return function (v) {
        return Vector2(v.x, v.y / scalar);
    }; }
};
var V3 = {
    origin: Vector3(0, 0, 0),
    half: Vector3(0.5, 0.5, 0.5),
    promoteV2: function (v) {
        return Vector3(v.x, v.y, 0);
    },
    demoteV4: function (v) {
        return Vector3(v.x, v.y, v.z);
    },
    displace: function (delta) { return function (v) {
        return Vector3(v.x + delta, v.y + delta, v.z + delta);
    }; },
    undisplace: function (delta) { return function (v) {
        return Vector3(v.x - delta, v.y - delta, v.z - delta);
    }; },
    rotateX: function (angle) { return function (v) {
        var c = Math.cos(angle), s = Math.sin(angle);
        return Vector3(v.x, v.y * c - v.z * s, v.z * c + v.y * s);
    }; },
    rotateY: function (angle) { return function (v) {
        var c = Math.cos(angle), s = Math.sin(angle);
        return Vector3(v.x * c - v.z * s, v.y, v.z * c + v.x * s);
    }; },
    rotateZ: function (angle) { return function (v) {
        var c = Math.cos(angle), s = Math.sin(angle);
        return Vector3(v.x * c - v.y * s, v.y * c + v.x * s, v.z);
    }; },
    rotateYXZ: function (yaw) { return function (pitch) { return function (roll) { return function (v) {
        return v.pipe(V3.rotateY(yaw)).pipe(V3.rotateX(pitch)).pipe(V3.rotateZ(roll));
    }; }; }; },
    rotateYXZv: function (angles) { return function (v) {
        return v.pipe(V3.rotateY(angles.y)).pipe(V3.rotateX(angles.x)).pipe(V3.rotateZ(angles.z));
    }; },
    unrotateYXZ: function (yaw) { return function (pitch) { return function (roll) { return function (v) {
        return v.pipe(V3.rotateZ(-roll)).pipe(V3.rotateX(-pitch)).pipe(V3.rotateY(-yaw));
    }; }; }; },
    unrotateYXZv: function (angles) { return function (v) {
        return v.pipe(V3.rotateZ(-angles.z)).pipe(V3.rotateX(-angles.x)).pipe(V3.rotateY(-angles.y));
    }; },
    translate: function (dx) { return function (dy) { return function (dz) { return function (v) {
        return Vector3(v.x + dx, v.y + dy, v.z + dz);
    }; }; }; },
    untranslate: function (dx) { return function (dy) { return function (dz) { return function (v) {
        return Vector3(v.x - dx, v.y - dy, v.z - dz);
    }; }; }; },
    add: function (v) { return function (w) {
        return Vector3(v.x + w.x, v.y + w.y, v.z + w.z);
    }; },
    sub: function (v) { return function (w) {
        return Vector3(v.x - w.x, v.y - w.y, v.z - w.z);
    }; },
    scale: function (k) { return function (v) {
        return Vector3(v.x * k, v.y * k, v.z * k);
    }; },
    unscale: function (k) { return function (v) {
        return Vector3(v.x / k, v.y / k, v.z / k);
    }; },
    norm: function (v) {
        return Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2));
    },
    normalize: function (v) {
        return v.eq(V3.origin)
            ? V3.origin
            : V3.unscale(Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2)))(v);
    },
    dot: function (v) { return function (w) {
        return v.x * w.x + v.y * w.y + v.z * w.z;
    }; },
    cross: function (v) { return function (w) {
        return Vector3(v.y * w.z - v.z * w.y, v.z * w.x - v.x * w.z, v.x * w.y - v.y * w.x);
    }; },
    transform: function (m) { return function (v) {
        return Vector3(m.ix * v.x + m.jx * v.y + m.kx * v.z, m.iy * v.x + m.jy * v.y + m.ky * v.z, m.iz * v.x + m.jz * v.y + m.kz * v.z);
    }; },
    translateX: function (delta) { return function (v) {
        return Vector3(v.x + delta, v.y, v.z);
    }; },
    translateY: function (delta) { return function (v) {
        return Vector3(v.x, v.y + delta, v.z);
    }; },
    translateZ: function (delta) { return function (v) {
        return Vector3(v.x, v.y, v.z + delta);
    }; },
    untranslateX: function (delta) { return function (v) {
        return Vector3(v.x - delta, v.y, v.z);
    }; },
    untranslateY: function (delta) { return function (v) {
        return Vector3(v.x, v.y - delta, v.z);
    }; },
    untranslateZ: function (delta) { return function (v) {
        return Vector3(v.x, v.y, v.z - delta);
    }; },
    scaleX: function (scalar) { return function (v) {
        return Vector3(v.x * scalar, v.y, v.z);
    }; },
    scaleY: function (scalar) { return function (v) {
        return Vector3(v.x, v.y * scalar, v.z);
    }; },
    scaleZ: function (scalar) { return function (v) {
        return Vector3(v.x, v.y, v.z * scalar);
    }; },
    unscaleX: function (scalar) { return function (v) {
        return Vector3(v.x / scalar, v.y, v.z);
    }; },
    unscaleY: function (scalar) { return function (v) {
        return Vector3(v.x, v.y / scalar, v.z);
    }; },
    unscaleZ: function (scalar) { return function (v) {
        return Vector3(v.x, v.y, v.z / scalar);
    }; }
};
var V4 = {
    origin: Vector4(0, 0, 0, 0),
    half: Vector4(0.5, 0.5, 0.5, 0.5),
    promoteV2: function (v) {
        return Vector4(v.x, v.y, 0, 0);
    },
    promoteV3: function (v) {
        return Vector4(v.x, v.y, v.y, 0);
    },
    displace: function (delta) { return function (v) {
        return Vector4(v.x + delta, v.y + delta, v.z + delta, v.w + delta);
    }; },
    undisplace: function (delta) { return function (v) {
        return Vector4(v.x - delta, v.y - delta, v.z - delta, v.w - delta);
    }; },
    translate: function (dx) { return function (dy) { return function (dz) { return function (dw) { return function (v) {
        return Vector4(v.x + dx, v.y + dy, v.z + dz, v.w + dw);
    }; }; }; }; },
    untranslate: function (dx) { return function (dy) { return function (dz) { return function (dw) { return function (v) {
        return Vector4(v.x - dx, v.y - dy, v.z - dz, v.w - dw);
    }; }; }; }; },
    add: function (v) { return function (w) {
        return Vector4(v.x + w.x, v.y + w.y, v.z + w.z, v.w + w.w);
    }; },
    sub: function (v) { return function (w) {
        return Vector4(v.x - w.x, v.y - w.y, v.z - w.z, v.w - w.w);
    }; },
    scale: function (k) { return function (v) {
        return Vector4(v.x * k, v.y * k, v.z * k, v.w * k);
    }; },
    unscale: function (k) { return function (v) {
        return Vector4(v.x / k, v.y / k, v.z / k, v.w / k);
    }; },
    norm: function (v) {
        return Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2) + Math.pow(v.w, 2));
    },
    normalize: function (v) {
        return v.eq(V4.origin)
            ? V4.origin
            : V4.unscale(Math.sqrt(Math.pow(v.x, 2) + Math.pow(v.y, 2) + Math.pow(v.z, 2) + Math.pow(v.w, 2)))(v);
    },
    dot: function (v) { return function (w) {
        return v.x * w.x + v.y * w.y + v.z * w.z + v.w * w.w;
    }; },
    transform: function (m) { return function (v) {
        return Vector4(m.ix * v.x + m.jx * v.y + m.kx * v.z + m.lx * v.w, m.iy * v.x + m.jy * v.y + m.ky * v.z + m.ly * v.w, m.iz * v.x + m.jz * v.y + m.kz * v.z + m.lz * v.w, m.iw * v.x + m.jw * v.y + m.kw * v.z + m.lw * v.w);
    }; },
    translateX: function (delta) { return function (v) {
        return Vector4(v.x + delta, v.y, v.z, v.w);
    }; },
    translateY: function (delta) { return function (v) {
        return Vector4(v.x, v.y + delta, v.z, v.w);
    }; },
    translateZ: function (delta) { return function (v) {
        return Vector4(v.x, v.y, v.z + delta, v.w);
    }; },
    translateW: function (delta) { return function (v) {
        return Vector4(v.x, v.y, v.z, v.w + delta);
    }; },
    untranslateX: function (delta) { return function (v) {
        return Vector4(v.x - delta, v.y, v.z, v.w);
    }; },
    untranslateY: function (delta) { return function (v) {
        return Vector4(v.x, v.y - delta, v.z, v.w);
    }; },
    untranslateZ: function (delta) { return function (v) {
        return Vector4(v.x, v.y, v.z - delta, v.w);
    }; },
    untranslateW: function (delta) { return function (v) {
        return Vector4(v.x, v.y, v.z, v.w - delta);
    }; },
    scaleX: function (scalar) { return function (v) {
        return Vector4(v.x * scalar, v.y, v.z, v.w);
    }; },
    scaleY: function (scalar) { return function (v) {
        return Vector4(v.x, v.y * scalar, v.z, v.w);
    }; },
    scaleZ: function (scalar) { return function (v) {
        return Vector4(v.x, v.y, v.z * scalar, v.w);
    }; },
    scaleW: function (scalar) { return function (v) {
        return Vector4(v.x, v.y, v.z, v.w * scalar);
    }; },
    unscaleX: function (scalar) { return function (v) {
        return Vector4(v.x / scalar, v.y, v.z, v.w);
    }; },
    unscaleY: function (scalar) { return function (v) {
        return Vector4(v.x, v.y / scalar, v.z, v.w);
    }; },
    unscaleZ: function (scalar) { return function (v) {
        return Vector4(v.x, v.y, v.z / scalar, v.w);
    }; },
    unscaleW: function (scalar) { return function (v) {
        return Vector4(v.x, v.y, v.z, v.w / scalar);
    }; }
};
var M2 = {
    id: Matrix2(1, 0, 0, 1),
    fromBasis: function (i, j) {
        return Matrix2(i.x, j.x, i.y, j.y);
    },
    mul: function (m) { return function (n) {
        return Matrix2(m.ix * n.ix + m.jx * n.iy, m.ix * n.jx + m.jx * n.jy, m.iy * n.ix + m.jy * n.iy, m.iy * n.jx + m.jy * n.jy);
    }; }
};
var M3 = {
    id: Matrix3(1, 0, 0, 0, 1, 0, 0, 0, 1),
    fromBasis: function (i, j, k) {
        return Matrix3(i.x, j.x, k.x, i.y, j.y, k.y, i.z, j.z, k.z);
    },
    mul: function (m) { return function (n) {
        return Matrix3(m.ix * n.ix + m.jx * n.iy + m.kx * n.iz, m.ix * n.jx + m.jx * n.jy + m.kx * n.jz, m.ix * n.kx + m.jx * n.ky + m.kx * n.kz, m.iy * n.ix + m.jy * n.iy + m.ky * n.iz, m.iy * n.jx + m.jy * n.jy + m.ky * n.jz, m.iy * n.kx + m.jy * n.ky + m.ky * n.kz, m.iz * n.ix + m.jz * n.iy + m.kz * n.iz, m.iz * n.jx + m.jz * n.jy + m.kz * n.jz, m.iz * n.kx + m.jz * n.ky + m.kz * n.kz);
    }; }
};
var M4 = {
    id: Matrix4(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1),
    fromBasis: function (i, j, k, l) {
        return Matrix4(i.x, j.x, k.x, l.x, i.y, j.y, k.y, l.y, i.z, j.z, k.z, l.z, i.w, j.w, k.w, l.w);
    },
    mul: function (m) { return function (n) {
        return Matrix4(m.ix * n.ix + m.jx * n.iy + m.kx * n.iz + m.lx * n.iw, m.ix * n.jx + m.jx * n.jy + m.kx * n.jz + m.lx * n.jw, m.ix * n.kx + m.jx * n.ky + m.kx * n.kz + m.lx * n.kw, m.ix * n.lx + m.jx * n.ly + m.kx * n.lz + m.lx * n.lw, m.iy * n.ix + m.jy * n.iy + m.ky * n.iz + m.ly * n.iw, m.iy * n.jx + m.jy * n.jy + m.ky * n.jz + m.ly * n.jw, m.iy * n.kx + m.jy * n.ky + m.ky * n.kz + m.ly * n.kw, m.iy * n.lx + m.jy * n.ly + m.ky * n.lz + m.ly * n.lw, m.iz * n.ix + m.jz * n.iy + m.kz * n.iz + m.lz * n.iw, m.iz * n.jx + m.jz * n.jy + m.kz * n.jz + m.lz * n.jw, m.iz * n.kx + m.jz * n.ky + m.kz * n.kz + m.lz * n.kw, m.iz * n.lx + m.jz * n.ly + m.kz * n.lz + m.lz * n.lw, m.iw * n.ix + m.jw * n.iy + m.kw * n.iz + m.lw * n.iw, m.iw * n.jx + m.jw * n.jy + m.kw * n.jz + m.lw * n.jw, m.iw * n.kx + m.jw * n.ky + m.kw * n.kz + m.lw * n.kw, m.iw * n.lx + m.jw * n.ly + m.kw * n.lz + m.lw * n.lw);
    }; }
};
var sequenceIOs = function (ios) {
    return IO(function () { return ios.fmap(function (io) { return io.INFO(); }); });
};
var executeIOs = function (ios) {
    return IO(function () {
        for (var i = ios; i.CONS === 'Cons'; i = i.tail)
            i.head.INFO();
        return null;
    });
};
var pairMaybes = function (pmaybes) {
    return pmaybes.fst.CONS === 'Just' && pmaybes.snd.CONS === 'Just'
        ? Just(Pair(pmaybes.fst.INFO, pmaybes.snd.INFO))
        : Nothing;
};
var maybeHead = function (xs) {
    return xs.CONS === 'Nil'
        ? Nothing
        : Just(xs.head);
};
var maybeLast = function (xs) {
    return xs.CONS === 'Nil'
        ? Nothing
        : Just(last(xs));
};
var maybeTail = function (xs) {
    return xs.CONS === 'Nil'
        ? Nothing
        : Just(xs.tail);
};
var maybeInit = function (xs) {
    return xs.CONS === 'Nil'
        ? Nothing
        : Just(init(xs));
};
var fromMaybes = function (maybes) {
    return maybes.CONS === 'Nil'
        ? Nil
        : maybes.head.CONS === 'Nothing'
            ? fromMaybes(maybes.tail)
            : lprepend(maybes.head.INFO)(function () { return fromMaybes(maybes.tail); });
};
var mapMaybe = function (reaction) { return function (xs) {
    return fromMaybes(xs.fmap(reaction));
}; };
var maybeSingleton = function (maybe) {
    return maybe.CONS === 'Nothing'
        ? Nil
        : singleton(maybe.INFO);
};
var maybeAt = function (index) { return function (xs) {
    if (index < 0 || !Number.isInteger)
        return Nothing;
    for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (i === index)
            return Just(xs.head);
    return Nothing;
}; };
var find = function (predicate) { return function (xs) {
    while (xs.CONS === 'Cons')
        if (predicate(xs.head))
            return Just(xs.head);
        else
            xs = xs.tail;
    return Nothing;
}; };
var elemIndex = function (value) { return function (xs) {
    for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (xs.head.eq(value))
            return Just(i);
    return Nothing;
}; };
var findIndex = function (predicate) { return function (xs) {
    for (var i = 0; xs.CONS === 'Cons'; ++i, xs = xs.tail)
        if (predicate(xs.head))
            return Just(i);
    return Nothing;
}; };
var lefts = function (eithers) {
    return eithers.CONS === 'Nil'
        ? Nil
        : eithers.head.CONS === 'Left'
            ? lprepend(eithers.head.INFO)(function () { return lefts(eithers.tail); })
            : lefts(eithers.tail);
};
var rights = function (eithers) {
    return eithers.CONS === 'Nil'
        ? Nil
        : eithers.head.CONS === 'Right'
            ? lprepend(eithers.head.INFO)(function () { return rights(eithers.tail); })
            : rights(eithers.tail);
};
var relaxX = function (direction) {
    return direction === X.LL ? X.L :
        direction === X.RR ? X.R :
            direction;
};
var relaxY = function (direction) {
    return direction === Y.DD ? Y.D :
        direction === Y.UU ? Y.U :
            direction;
};
var relaxZ = function (direction) {
    return direction === Z.BB ? Z.B :
        direction === Z.FF ? Z.F :
            direction;
};
var isL = function (direction) { return direction === X.L || direction === X.LL; };
var isR = function (direction) { return direction === X.R || direction === X.RR; };
var isD = function (direction) { return direction === Y.D || direction === Y.DD; };
var isU = function (direction) { return direction === Y.U || direction === Y.UU; };
var isB = function (direction) { return direction === Z.B || direction === Z.BB; };
var isF = function (direction) { return direction === Z.F || direction === Z.FF; };
var mappingLineCap = Mapping([LineCap.Butt, 'butt'], [LineCap.Round, 'round'], [LineCap.Square, 'square']);
var mappingLineJoin = Mapping([LineJoin.Round, 'round'], [LineJoin.Bevel, 'bevel'], [LineJoin.Miter, 'miter']);
var mappingTextAlign = Mapping([TextAlign.Center, 'center'], [TextAlign.End, 'end'], [TextAlign.Leftside, 'left'], [TextAlign.Rightside, 'right'], [TextAlign.Start, 'start']);
var mappingTextBaseline = Mapping([TextBaseline.Alphabetic, 'alphabetic'], [TextBaseline.Bottom, 'bottom'], [TextBaseline.Hanging, 'hanging'], [TextBaseline.Ideographic, 'ideographic'], [TextBaseline.Middle, 'middle'], [TextBaseline.Top, 'top']);
var mappingComposition = Mapping([Composition.SourceOver, 'source-over'], [Composition.SourceIn, 'source-in'], [Composition.SourceOut, 'source-out'], [Composition.SourceAtop, 'source-atop'], [Composition.DestinationOver, 'destination-over'], [Composition.DestinationIn, 'destination-in'], [Composition.DestinationOut, 'destination-out'], [Composition.DestinationAtop, 'destination-atop'], [Composition.Lighter, 'lighter'], [Composition.Copy, 'copy'], [Composition.Xor, 'xor'], [Composition.Multiply, 'multiply'], [Composition.Screen, 'screen'], [Composition.Overlay, 'overlay'], [Composition.Darken, 'darken'], [Composition.Lighten, 'lighten'], [Composition.ColorDodge, 'color-dodge'], [Composition.ColorBurn, 'color-burn'], [Composition.HardLight, 'hard-light'], [Composition.SoftLight, 'soft-light'], [Composition.Difference, 'difference'], [Composition.Exclusion, 'exclusion'], [Composition.Hue, 'hue'], [Composition.Saturation, 'saturation'], [Composition.Color, 'color'], [Composition.Luminosity, 'luminosity']);
var unit = {
    IO: function (outcome) { return IO(function () { return outcome; }); },
    Maybe: Just,
    Process: function (output) { return Process(function (s) { return Pair(s, output); }); },
    List: singleton
};
var Do = {
    IO: unit.IO(Object.create(null)),
    Maybe: unit.Maybe(Object.create(null)),
    Process: unit.Process(Object.create(null)),
    List: unit.List(Object.create(null))
};
var KEYBOARD = [
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
var λ = {
    MUTABLE: {},
    ctxs: [],
    ctx: undefined,
    resizeID: undefined,
    isResized: false,
    isPointerLocked: false,
    seed: (Math.random() - 0.5) * Date.now(),
    debugCounter: 0,
    image: Object.create(null),
    audio: Object.create(null),
    mouseScreenX: 0, mouseScreenY: 0,
    mouseWindowX: 0, mouseWindowY: 0,
    mouseCanvasX: 0, mouseCanvasY: 0,
    mouseDeltaX: 0, mouseDeltaY: 0,
    mouseScroll: Y.Rest,
    mouseButtons: Array(5).fill(Y.U),
    keyboard: KEYBOARD.reduce(function ($, k) {
        var _a;
        return (__assign(__assign({}, $), (_a = {}, _a[k] = Y.U, _a)));
    }, Object.create(null))
};
var Input = {
    Norm: {
        mouseX: IO(function () { return λ.mouseCanvasX / λ.ctx.canvas.width; }),
        mouseY: IO(function () { return λ.mouseCanvasY / λ.ctx.canvas.height; }),
        mouseP: IO(function () { return Pair(λ.mouseCanvasX / λ.ctx.canvas.width, λ.mouseCanvasY / λ.ctx.canvas.height); }),
        mouseV: IO(function () { return Vector2(λ.mouseCanvasX / λ.ctx.canvas.width, λ.mouseCanvasY / λ.ctx.canvas.height); }),
        mouseDX: IO(function () { return λ.mouseDeltaX / λ.ctx.canvas.width; }),
        mouseDY: IO(function () { return λ.mouseDeltaY / λ.ctx.canvas.height; }),
        mouseDP: IO(function () { return Pair(λ.mouseDeltaX / λ.ctx.canvas.width, λ.mouseDeltaY / λ.ctx.canvas.height); }),
        mouseDV: IO(function () { return Vector2(λ.mouseDeltaX / λ.ctx.canvas.width, λ.mouseDeltaY / λ.ctx.canvas.height); }),
        lineThickness: IO(function () { return λ.ctx.lineWidth / λ.ctx.canvas.width; }),
        lineDashPattern: IO(function () { return List.apply(void 0, λ.ctx.getLineDash().map(function (x) { return x / λ.ctx.canvas.width; })); }),
        lineDashOffset: IO(function () { return λ.ctx.lineDashOffset / λ.ctx.canvas.width; }),
        fontSize: IO(function () { return parseFloat(λ.ctx.font) / λ.ctx.canvas.width; }),
        shadowDX: IO(function () { return λ.ctx.shadowOffsetX / λ.ctx.canvas.width; }),
        shadowDY: IO(function () { return λ.ctx.shadowOffsetY / λ.ctx.canvas.height; }),
        shadowDP: IO(function () { return Pair(λ.ctx.shadowOffsetX / λ.ctx.canvas.width, λ.ctx.shadowOffsetY / λ.ctx.canvas.height); }),
        shadowDV: IO(function () { return Vector2(λ.ctx.shadowOffsetX / λ.ctx.canvas.width, λ.ctx.shadowOffsetY / λ.ctx.canvas.height); }),
        transformationMatrix: IO(function () {
            var m = λ.ctx.getTransform();
            return Matrix3(m.a, m.c, m.e / λ.ctx.canvas.width, m.b, m.d, m.f / λ.ctx.canvas.height, 0, 0, 1);
        })
    },
    layer: IO(function () { return λ.ctxs.findIndex(function (ctx) { return ctx === λ.ctx; }); }),
    isWindowResized: IO(function () { return λ.isResized; }),
    isPointerLocked: IO(function () { return λ.isPointerLocked; }),
    seed: unit.IO(λ.seed),
    screenW: IO(function () { return screen.width; }),
    screenH: IO(function () { return screen.height; }),
    screenP: IO(function () { return Pair(screen.width, screen.height); }),
    screenV: IO(function () { return Vector2(screen.width, screen.height); }),
    windowW: IO(function () { return innerWidth; }),
    windowH: IO(function () { return innerHeight; }),
    windowP: IO(function () { return Pair(innerWidth, innerHeight); }),
    windowV: IO(function () { return Vector2(innerWidth, innerHeight); }),
    canvasW: IO(function () { return λ.ctx.canvas.width; }),
    canvasH: IO(function () { return λ.ctx.canvas.height; }),
    canvasP: IO(function () { return Pair(λ.ctx.canvas.width, λ.ctx.canvas.height); }),
    canvasV: IO(function () { return Vector2(λ.ctx.canvas.width, λ.ctx.canvas.height); }),
    mouseScreenX: IO(function () { return λ.mouseScreenX; }),
    mouseScreenY: IO(function () { return λ.mouseScreenY; }),
    mouseScreenP: IO(function () { return Pair(λ.mouseScreenX, λ.mouseScreenY); }),
    mouseScreenV: IO(function () { return Vector2(λ.mouseScreenX, λ.mouseScreenY); }),
    mouseWindowX: IO(function () { return λ.mouseWindowX; }),
    mouseWindowY: IO(function () { return λ.mouseWindowY; }),
    mouseWindowP: IO(function () { return Pair(λ.mouseWindowX, λ.mouseWindowY); }),
    mouseWindowV: IO(function () { return Vector2(λ.mouseWindowX, λ.mouseWindowY); }),
    mouseX: IO(function () { return λ.mouseCanvasX; }),
    mouseY: IO(function () { return λ.mouseCanvasY; }),
    mouseP: IO(function () { return Pair(λ.mouseCanvasX, λ.mouseCanvasY); }),
    mouseV: IO(function () { return Vector2(λ.mouseCanvasX, λ.mouseCanvasY); }),
    mouseDX: IO(function () { return λ.mouseDeltaX; }),
    mouseDY: IO(function () { return λ.mouseDeltaY; }),
    mouseDP: IO(function () { return Pair(λ.mouseDeltaX, λ.mouseDeltaY); }),
    mouseDV: IO(function () { return Vector2(λ.mouseDeltaX, λ.mouseDeltaY); }),
    mouseScroll: IO(function () { return λ.mouseScroll; }),
    mouseLeft: IO(function () { return λ.mouseButtons[0]; }),
    mouseMiddle: IO(function () { return λ.mouseButtons[1]; }),
    mouseRight: IO(function () { return λ.mouseButtons[2]; }),
    mouseA: IO(function () { return λ.mouseButtons[3]; }),
    mouseB: IO(function () { return λ.mouseButtons[4]; }),
    keyboard: function (key) { return IO(function () { return λ.keyboard[key]; }); },
    time: IO(Date.now),
    textMeasurement: function (text) {
        return IO(function () {
            var metrics = λ.ctx.measureText(text);
            return TextMeasurement(text)(Math.abs(metrics.actualBoundingBoxLeft) + Math.abs(metrics.actualBoundingBoxRight))(Math.abs(metrics.actualBoundingBoxAscent) + Math.abs(metrics.actualBoundingBoxDescent));
        });
    },
    lineThickness: IO(function () { return λ.ctx.lineWidth; }),
    lineCap: IO(function () { return mappingLineCap.domain(λ.ctx.lineCap); }),
    lineJoin: IO(function () { return mappingLineJoin.domain(λ.ctx.lineJoin); }),
    lineDashPattern: IO(function () { return List.apply(void 0, λ.ctx.getLineDash()); }),
    lineDashOffset: IO(function () { return λ.ctx.lineDashOffset; }),
    miterLimit: IO(function () { return λ.ctx.miterLimit; }),
    font: IO(function () { return λ.ctx.font; }),
    fontSize: IO(function () { return parseFloat(λ.ctx.font); }),
    fontFamily: IO(function () { return λ.ctx.font.slice(λ.ctx.font.indexOf(" ") + 1); }),
    textAlign: IO(function () { return mappingTextAlign.domain(λ.ctx.textAlign); }),
    textBaseline: IO(function () { return mappingTextBaseline.domain(λ.ctx.textBaseline); }),
    shadowBlurAmount: IO(function () { return λ.ctx.shadowBlur; }),
    shadowColor: IO(function () { return λ.ctx.shadowColor; }),
    shadowDX: IO(function () { return λ.ctx.shadowOffsetX; }),
    shadowDY: IO(function () { return λ.ctx.shadowOffsetY; }),
    shadowDP: IO(function () { return Pair(λ.ctx.shadowOffsetX, λ.ctx.shadowOffsetY); }),
    shadowDV: IO(function () { return Vector2(λ.ctx.shadowOffsetX, λ.ctx.shadowOffsetY); }),
    isInEvenOddPathP: function (x) { return function (y) { return IO(function () { return λ.ctx.isPointInPath(x, y, 'evenodd'); }); }; },
    isInEvenOddPathV: function (v) { return IO(function () { return λ.ctx.isPointInPath(v.x, v.y, 'evenodd'); }); },
    isInNonZeroPathP: function (x) { return function (y) { return IO(function () { return λ.ctx.isPointInPath(x, y, 'nonzero'); }); }; },
    isInNonZeroPathV: function (v) { return IO(function () { return λ.ctx.isPointInPath(v.x, v.y, 'nonzero'); }); },
    isInStrokeP: function (x) { return function (y) { return IO(function () { return λ.ctx.isPointInStroke(x, y); }); }; },
    isInStrokeV: function (v) { return IO(function () { return λ.ctx.isPointInStroke(v.x, v.y); }); },
    transformationMatrix: IO(function () {
        var m = λ.ctx.getTransform();
        return Matrix3(m.a, m.c, m.e, m.b, m.d, m.f, 0, 0, 1);
    }),
    alpha: IO(function () { return λ.ctx.globalAlpha; }),
    composition: IO(function () { return mappingComposition.domain(λ.ctx.globalCompositeOperation); }),
    wasd: IO(function () {
        return Vector2(BIT(isD(λ.keyboard['KeyD'])) - BIT(isD(λ.keyboard['KeyA'])), BIT(isD(λ.keyboard['KeyS'])) - BIT(isD(λ.keyboard['KeyW']))).pipe(V2.normalize);
    }),
    wasdY: IO(function () {
        return Vector3(BIT(isD(λ.keyboard['KeyD'])) - BIT(isD(λ.keyboard['KeyA'])), BIT(isD(λ.keyboard['Space'])) - BIT(isD(λ.keyboard['ShiftLeft'])), BIT(isD(λ.keyboard['KeyS'])) - BIT(isD(λ.keyboard['KeyW']))).pipe(V3.normalize);
    }),
    arrows: IO(function () {
        return Vector2(BIT(isD(λ.keyboard['ArrowRight'])) - BIT(isD(λ.keyboard['ArrowLeft'])), BIT(isD(λ.keyboard['ArrowDown'])) - BIT(isD(λ.keyboard['ArrowUp']))).pipe(V2.normalize);
    })
};
var Reput = {
    Norm: {
        canvasW: function (w) {
            return IO(function () { return (λ.ctx.canvas.width = w * innerWidth, null); });
        },
        canvasH: function (h) {
            return IO(function () { return (λ.ctx.canvas.height = h * innerHeight, null); });
        },
        canvasWH: function (w) { return function (h) {
            return IO(function () { return (λ.ctx.canvas.width = w * innerWidth, λ.ctx.canvas.height = h * innerHeight, null); });
        }; },
        canvasP: function (p) {
            return IO(function () { return (λ.ctx.canvas.width = p.fst * innerWidth, λ.ctx.canvas.height = p.snd * innerHeight, null); });
        },
        canvasV: function (v) {
            return IO(function () { return (λ.ctx.canvas.width = v.x * innerWidth, λ.ctx.canvas.height = v.y * innerHeight, null); });
        },
        lineThickness: function (t) {
            return IO(function () { return (λ.ctx.lineWidth = t * λ.ctx.canvas.width, null); });
        },
        lineDashPattern: function (pattern) {
            return IO(function () { return (λ.ctx.setLineDash(array(pattern).map(function (x) { return x * λ.ctx.canvas.width; })), null); });
        },
        lineDashOffset: function (offset) {
            return IO(function () { return (λ.ctx.lineDashOffset = offset * λ.ctx.canvas.width, null); });
        },
        fontSize: function (size) {
            return IO(function () { return (λ.ctx.font = size * λ.ctx.canvas.width + "px" + λ.ctx.font.slice(λ.ctx.font.indexOf(" ")), null); });
        },
        fillRGBA: function (r) { return function (g) { return function (b) { return function (a) {
            return IO(function () { return (λ.ctx.fillStyle = "rgba(" + r * 255 + "," + g * 255 + "," + b * 255 + "," + a + ")", null); });
        }; }; }; },
        fillRGBAV: function (v) {
            return IO(function () { return (λ.ctx.fillStyle = "rgba(" + v.x * 255 + "," + v.y * 255 + "," + v.z * 255 + "," + v.w + ")", null); });
        },
        strokeRGBA: function (r) { return function (g) { return function (b) { return function (a) {
            return IO(function () { return (λ.ctx.strokeStyle = "rgba(" + r * 255 + "," + g * 255 + "," + b * 255 + "," + a + ")", null); });
        }; }; }; },
        strokeRGBAV: function (v) {
            return IO(function () { return (λ.ctx.strokeStyle = "rgba(" + v.x * 255 + "," + v.y * 255 + "," + v.z * 255 + "," + v.w + ")", null); });
        },
        shadowRGBA: function (r) { return function (g) { return function (b) { return function (a) {
            return IO(function () { return (λ.ctx.shadowColor = "rgba(" + r * 255 + "," + g * 255 + "," + b * 255 + "," + a + ")", null); });
        }; }; }; },
        shadowRGBAV: function (v) {
            return IO(function () { return (λ.ctx.shadowColor = "rgba(" + v.x * 255 + "," + v.y * 255 + "," + v.z * 255 + "," + v.w + ")", null); });
        },
        shadowOffsetX: function (x) {
            return IO(function () { return (λ.ctx.shadowOffsetX = x * λ.ctx.canvas.width, null); });
        },
        shadowOffsetY: function (y) {
            return IO(function () { return (λ.ctx.shadowOffsetY = y * λ.ctx.canvas.height, null); });
        },
        shadowOffsetXY: function (x) { return function (y) {
            return IO(function () { return (λ.ctx.shadowOffsetX = x * λ.ctx.canvas.width,
                λ.ctx.shadowOffsetY = y * λ.ctx.canvas.height,
                null); });
        }; },
        shadowOffsetP: function (p) {
            return IO(function () { return (λ.ctx.shadowOffsetX = p.fst * λ.ctx.canvas.width,
                λ.ctx.shadowOffsetY = p.snd * λ.ctx.canvas.height,
                null); });
        },
        shadowOffsetV: function (v) {
            return IO(function () { return (λ.ctx.shadowOffsetX = v.x * λ.ctx.canvas.width,
                λ.ctx.shadowOffsetY = v.y * λ.ctx.canvas.height,
                null); });
        },
        transformationMatrix: function (m) {
            return IO(function () { return (λ.ctx.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx * λ.ctx.canvas.width, m.ky * λ.ctx.canvas.height),
                null); });
        }
    },
    layer: function (index) {
        return IO(function () {
            if (λ.ctxs[index])
                λ.ctx = λ.ctxs[index];
            else
                THROW("'(Reput.layer)' only accepts integers in interval [0, " + λ.ctxs.length + "); instead received '" + index + "'");
            return null;
        });
    },
    canvasW: function (w) {
        return IO(function () { return (λ.ctxs.forEach(function (ctx) { return ctx.canvas.width = w; }), null); });
    },
    canvasH: function (h) {
        return IO(function () { return (λ.ctxs.forEach(function (ctx) { return ctx.canvas.height = h; }), null); });
    },
    canvasWH: function (w) { return function (h) {
        return IO(function () { return (λ.ctxs.forEach(function (ctx) { return (ctx.canvas.width = w, ctx.canvas.height = h); }), null); });
    }; },
    canvasP: function (p) {
        return IO(function () { return (λ.ctxs.forEach(function (ctx) { return (ctx.canvas.width = p.fst, ctx.canvas.height = p.snd); }), null); });
    },
    canvasV: function (v) {
        return IO(function () { return (λ.ctxs.forEach(function (ctx) { return (ctx.canvas.width = v.x, ctx.canvas.height = v.y); }), null); });
    },
    lineThickness: function (t) {
        return IO(function () { return (λ.ctx.lineWidth = t, null); });
    },
    lineCap: function (cap) {
        return IO(function () { return (λ.ctx.lineCap = mappingLineCap.codomain(cap), null); });
    },
    lineJoin: function (joining) {
        return IO(function () { return (λ.ctx.lineJoin = mappingLineJoin.codomain(joining), null); });
    },
    lineDashPattern: function (pattern) {
        return IO(function () { return (λ.ctx.setLineDash(array(pattern)), null); });
    },
    lineDashOffset: function (offset) {
        return IO(function () { return (λ.ctx.lineDashOffset = offset, null); });
    },
    miterLimit: function (limit) {
        return IO(function () { return (λ.ctx.miterLimit = limit, null); });
    },
    font: function (fontInfo) {
        return IO(function () { return (λ.ctx.font = fontInfo, null); });
    },
    fontSize: function (size) {
        return IO(function () { return (λ.ctx.font = size + "px" + λ.ctx.font.slice(λ.ctx.font.indexOf(" ")), null); });
    },
    fontFamily: function (family) {
        return IO(function () { return (λ.ctx.font = parseFloat(λ.ctx.font) + "px " + family, null); });
    },
    textAlign: function (align) {
        return IO(function () { return (λ.ctx.textAlign = mappingTextAlign.codomain(align), null); });
    },
    textBaseline: function (baseline) {
        return IO(function () { return (λ.ctx.textBaseline = mappingTextBaseline.codomain(baseline), null); });
    },
    fillColor: function (color) {
        return IO(function () { return (λ.ctx.fillStyle = color, null); });
    },
    fillRGBA: function (r) { return function (g) { return function (b) { return function (a) {
        return IO(function () { return (λ.ctx.fillStyle = "rgba(" + r + "," + g + "," + b + "," + a + ")", null); });
    }; }; }; },
    fillRGBAV: function (v) {
        return IO(function () { return (λ.ctx.fillStyle = "rgba(" + v.x + "," + v.y + "," + v.z + "," + v.w + ")", null); });
    },
    strokeColor: function (color) {
        return IO(function () { return (λ.ctx.strokeStyle = color, null); });
    },
    strokeRGBA: function (r) { return function (g) { return function (b) { return function (a) {
        return IO(function () { return (λ.ctx.strokeStyle = "rgba(" + r + "," + g + "," + b + "," + a + ")", null); });
    }; }; }; },
    strokeRGBAV: function (v) {
        return IO(function () { return (λ.ctx.strokeStyle = "rgba(" + v.x + "," + v.y + "," + v.z + "," + v.w + ")", null); });
    },
    shadowBlurAmount: function (amount) {
        return IO(function () { return (λ.ctx.shadowBlur = amount, null); });
    },
    shadowColor: function (color) {
        return IO(function () { return (λ.ctx.shadowColor = color, null); });
    },
    shadowRGBA: function (r) { return function (g) { return function (b) { return function (a) {
        return IO(function () { return (λ.ctx.shadowColor = "rgba(" + r + "," + g + "," + b + "," + a + ")", null); });
    }; }; }; },
    shadowRGBAV: function (v) {
        return IO(function () { return (λ.ctx.shadowColor = "rgba(" + v.x + "," + v.y + "," + v.z + "," + v.w + ")", null); });
    },
    shadowOffsetX: function (x) {
        return IO(function () { return (λ.ctx.shadowOffsetX = x, null); });
    },
    shadowOffsetY: function (y) {
        return IO(function () { return (λ.ctx.shadowOffsetY = y, null); });
    },
    shadowOffsetXY: function (x) { return function (y) {
        return IO(function () { return (λ.ctx.shadowOffsetX = x, λ.ctx.shadowOffsetY = y, null); });
    }; },
    shadowOffsetV: function (v) {
        return IO(function () { return (λ.ctx.shadowOffsetX = v.x, λ.ctx.shadowOffsetY = v.y, null); });
    },
    transformationMatrix: function (m) {
        return IO(function () { return (λ.ctx.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null); });
    },
    alpha: function (opacity) {
        return IO(function () { return (λ.ctx.globalAlpha = opacity, null); });
    },
    compositionOperation: function (composition) {
        return IO(function () { return (λ.ctx.globalCompositeOperation = mappingComposition.codomain(composition), null); });
    }
};
var Output = {
    Norm: {
        drawImage: function (path) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null); });
        }; }; },
        drawImageP: function (path) { return function (xy) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
        }; },
        drawImageV: function (path) { return function (xy) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null); });
        }; },
        drawCroppedImage: function (path) {
            return function (cx) { return function (cy) { return function (cw) { return function (ch) {
                return function (x) { return function (y) { return function (w) { return function (h) {
                    return IO(function () { return (λ.ctx.drawImage(λ.image[path], cx * λ.image[path].width, cy * λ.image[path].height, cw * λ.image[path].width, ch * λ.image[path].height, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null); });
                }; }; }; };
            }; }; }; };
        },
        drawCroppedImageP: function (path) {
            return function (cxy) { return function (cwh) {
                return function (xy) { return function (wh) {
                    return IO(function () {
                        λ.ctx.drawImage(λ.image[path], cxy.fst * λ.image[path].width, cxy.snd * λ.image[path].width, cwh.fst * λ.image[path].width, cwh.snd * λ.image[path].width, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height);
                        return null;
                    });
                }; };
            }; };
        },
        drawCroppedImageV: function (path) {
            return function (cxy) { return function (cwh) {
                return function (xy) { return function (wh) {
                    return IO(function () { return (λ.ctx.drawImage(λ.image[path], cxy.x * λ.image[path].width, cxy.y * λ.image[path].width, cwh.x * λ.image[path].height, cwh.y * λ.image[path].height, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null); });
                }; };
            }; };
        },
        drawFullImage: function (path) { return function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null); });
        }; }; }; }; },
        drawFullImageP: function (path) { return function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height), null); });
        }; }; },
        drawFullImageV: function (path) { return function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null); });
        }; }; },
        drawSquareImage: function (path) { return function (k) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, k, k), null); });
        }; }; }; },
        drawSquareImageP: function (path) { return function (k) { return function (xy) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, k, k), null); });
        }; }; },
        drawSquareImageV: function (path) { return function (k) { return function (xy) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, k, k), null); });
        }; }; },
        drawScaledImage: function (path) { return function (k) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.drawImage(λ.image[path], x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, λ.image[path].width * k, λ.image[path].height * k), null); });
        }; }; }; },
        drawScaledImageP: function (path) { return function (k) { return function (xy) {
            return IO(function () {
                λ.ctx.drawImage(λ.image[path], xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, λ.image[path].width * k, λ.image[path].height * k);
                return null;
            });
        }; }; },
        drawScaledImageV: function (path) { return function (k) { return function (xy) {
            return IO(function () {
                λ.ctx.drawImage(λ.image[path], xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, λ.image[path].width * k, λ.image[path].height * k);
                return null;
            });
        }; }; },
        clearRectangle: function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () { return (λ.ctx.clearRect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.width, w * λ.ctx.canvas.width, h * λ.ctx.canvas.width), null); });
        }; }; }; },
        clearRectangleP: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.clearRect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
        }; },
        clearRectangleV: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.clearRect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null); });
        }; },
        rotate: function (angle) { return IO(function () { return (λ.ctx.rotate(angle * TAU), null); }); },
        scaleAxisP: function (kxy) {
            return IO(function () { return (λ.ctx.scale(kxy.fst * λ.ctx.canvas.width, kxy.snd * λ.ctx.canvas.height), null); });
        },
        scaleAxisV: function (kxy) {
            return IO(function () { return (λ.ctx.scale(kxy.x * λ.ctx.canvas.width, kxy.y * λ.ctx.canvas.height), null); });
        },
        translateX: function (dx) {
            return IO(function () { return (λ.ctx.translate(dx * λ.ctx.canvas.width, 0), null); });
        },
        translateY: function (dy) {
            return IO(function () { return (λ.ctx.translate(0, dy * λ.ctx.canvas.height), null); });
        },
        translate: function (dx) { return function (dy) {
            return IO(function () { return (λ.ctx.translate(dx * λ.ctx.canvas.width, dy * λ.ctx.canvas.height), null); });
        }; },
        translateP: function (dxy) {
            return IO(function () { return (λ.ctx.translate(dxy.fst * λ.ctx.canvas.width, dxy.snd * λ.ctx.canvas.height), null); });
        },
        translateV: function (dxy) {
            return IO(function () { return (λ.ctx.translate(dxy.x * λ.ctx.canvas.width, dxy.y * λ.ctx.canvas.height), null); });
        },
        transformation: function (m) {
            return IO(function () { return (λ.ctx.transform(m.ix, m.iy, m.jx, m.jy, m.kx * λ.ctx.canvas.width, m.ky * λ.ctx.canvas.height),
                null); });
        },
        moveTo: function (x) { return function (y) {
            return IO(function () { return (λ.ctx.moveTo(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null); });
        }; },
        moveToP: function (xy) {
            return IO(function () { return (λ.ctx.moveTo(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
        },
        moveToV: function (xy) {
            return IO(function () { return (λ.ctx.moveTo(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null); });
        },
        lineTo: function (x) { return function (y) {
            return IO(function () { return (λ.ctx.lineTo(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null); });
        }; },
        lineToP: function (xy) {
            return IO(function () { return (λ.ctx.lineTo(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
        },
        lineToV: function (xy) {
            return IO(function () { return (λ.ctx.lineTo(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null); });
        },
        bezierCurveTo: function (cx0) { return function (cy0) {
            return function (cx1) { return function (cy1) {
                return function (x) { return function (y) {
                    return IO(function () { return (λ.ctx.bezierCurveTo(cx0 * λ.ctx.canvas.width, cy0 * λ.ctx.canvas.height, cx1 * λ.ctx.canvas.width, cy1 * λ.ctx.canvas.height, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height),
                        null); });
                }; };
            }; };
        }; },
        bezierCurveToP: function (cxy0) {
            return function (cxy1) {
                return function (xy) {
                    return IO(function () { return (λ.ctx.bezierCurveTo(cxy0.fst * λ.ctx.canvas.width, cxy0.snd * λ.ctx.canvas.height, cxy1.fst * λ.ctx.canvas.width, cxy1.snd * λ.ctx.canvas.height, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
                };
            };
        },
        bezierCurveToV: function (cxy0) { return function (cxy1) { return function (xy) {
            return IO(function () { return (λ.ctx.bezierCurveTo(cxy0.x * λ.ctx.canvas.width, cxy0.y * λ.ctx.canvas.height, cxy1.x * λ.ctx.canvas.width, cxy1.y * λ.ctx.canvas.height, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null); });
        }; }; },
        quadraticCurveTo: function (cx) { return function (cy) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.quadraticCurveTo(cx * λ.ctx.canvas.width, cy * λ.ctx.canvas.width, x * λ.ctx.canvas.width, y * λ.ctx.canvas.width), null); });
        }; }; }; },
        quadraticCurveToP: function (cxy) { return function (xy) {
            return IO(function () { return (λ.ctx.quadraticCurveTo(cxy.fst * λ.ctx.canvas.width, cxy.snd * λ.ctx.canvas.height, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
        }; },
        quadraticCurveToV: function (cxy) { return function (xy) {
            return IO(function () { return (λ.ctx.quadraticCurveTo(cxy.x * λ.ctx.canvas.width, cxy.y * λ.ctx.canvas.height, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null); });
        }; },
        arcTo: function (r) { return function (cx0) { return function (cy0) { return function (cx1) { return function (cy1) {
            return IO(function () { return (λ.ctx.arcTo(cx0 * λ.ctx.canvas.width, cy0 * λ.ctx.canvas.height, cx1 * λ.ctx.canvas.width, cy1 * λ.ctx.canvas.height, r * λ.ctx.canvas.width), null); });
        }; }; }; }; },
        arcToP: function (r) { return function (cxy0) { return function (cxy1) {
            return IO(function () { return (λ.ctx.arcTo(cxy0.fst * λ.ctx.canvas.width, cxy0.snd * λ.ctx.canvas.height, cxy1.fst * λ.ctx.canvas.width, cxy1.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width), null); });
        }; }; },
        arcToV: function (r) { return function (cxy0) { return function (cxy1) {
            return IO(function () { return (λ.ctx.arcTo(cxy0.x * λ.ctx.canvas.width, cxy0.y * λ.ctx.canvas.height, cxy1.x * λ.ctx.canvas.width, cxy1.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width), null); });
        }; }; },
        rectangle: function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () { return (λ.ctx.rect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null); });
        }; }; }; },
        rectangleP: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.rect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.width, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.width), null); });
        }; },
        rectangleV: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.rect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null); });
        }; },
        fillRectangle: function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () { return (λ.ctx.fillRect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null); });
        }; }; }; },
        fillRectangleP: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.fillRect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height), null); });
        }; },
        fillRectangleV: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.fillRect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null); });
        }; },
        strokeRectangle: function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () { return (λ.ctx.strokeRect(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, w * λ.ctx.canvas.width, h * λ.ctx.canvas.height), null); });
        }; }; }; },
        strokeRectangleP: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.strokeRect(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height), null); });
        }; },
        strokeRectangleV: function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.strokeRect(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height), null); });
        }; },
        arc: function (r) { return function (a) { return function (b) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null); });
        }; }; }; }; },
        arcP: function (r) { return function (a) { return function (b) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null); });
        }; }; }; },
        arcV: function (r) { return function (a) { return function (b) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * 360), null); });
        }; }; }; },
        circle: function (r) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null); });
        }; }; },
        circleP: function (r) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null); });
        }; },
        circleV: function (r) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null); });
        }; },
        strokeCircle: function (r) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null); });
        }; }; },
        strokeCircleP: function (r) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null); });
        }; },
        strokeCircleV: function (r) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null); });
        }; },
        fillCircle: function (r) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.arc(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.fill(), null); });
        }; }; },
        fillCircleP: function (r) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.height, 0, TAU), λ.ctx.fill(), null); });
        }; },
        fillCircleV: function (r) { return function (xy) {
            return IO(function () { return (λ.ctx.arc(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.fill(), null); });
        }; },
        elliptic: function (r) { return function (a) { return function (b) {
            return function (x) { return function (y) { return function (kx) { return function (ky) {
                return IO(function () { return (λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null); });
            }; }; }; };
        }; }; },
        ellipticP: function (r) { return function (a) { return function (b) {
            return function (xy) { return function (wh) {
                return IO(function () { return (λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null); });
            }; };
        }; }; },
        ellipticV: function (r) { return function (a) { return function (b) {
            return function (xy) { return function (wh) {
                return IO(function () { return (λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, a * TAU, b * TAU), null); });
            }; };
        }; }; },
        ellipse: function (r) { return function (x) { return function (y) { return function (kx) { return function (ky) {
            return IO(function () { return (λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null); });
        }; }; }; }; },
        ellipseP: function (r) { return function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null); });
        }; }; },
        ellipseV: function (r) { return function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), null); });
        }; }; },
        strokeEllipse: function (r) { return function (x) { return function (y) { return function (kx) { return function (ky) {
            return IO(function () { return (λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU), λ.ctx.stroke(), null); });
        }; }; }; }; },
        strokeEllipseP: function (r) { return function (xy) { return function (wh) {
            return IO(function () {
                λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
                λ.ctx.stroke();
                return null;
            });
        }; }; },
        strokeEllipseV: function (r) { return function (xy) { return function (wh) {
            return IO(function () {
                λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
                λ.ctx.stroke();
                return null;
            });
        }; }; },
        fillEllipse: function (r) { return function (x) { return function (y) { return function (kx) { return function (ky) {
            return IO(function () {
                λ.ctx.ellipse(x * λ.ctx.canvas.width, y * λ.ctx.canvas.height, kx * λ.ctx.canvas.width, ky * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
                λ.ctx.fill();
                return null;
            });
        }; }; }; }; },
        fillEllipseP: function (r) { return function (xy) { return function (wh) {
            return IO(function () {
                λ.ctx.ellipse(xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height, wh.fst * λ.ctx.canvas.width, wh.snd * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
                λ.ctx.fill();
                return null;
            });
        }; }; },
        fillEllipseV: function (r) { return function (xy) { return function (wh) {
            return IO(function () {
                λ.ctx.ellipse(xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height, wh.x * λ.ctx.canvas.width, wh.y * λ.ctx.canvas.height, r * λ.ctx.canvas.width, 0, TAU);
                λ.ctx.fill();
                return null;
            });
        }; }; },
        strokeText: function (text) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.strokeText(text, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null); });
        }; }; },
        strokeTextP: function (text) { return function (xy) {
            return IO(function () { return (λ.ctx.strokeText(text, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
        }; },
        strokeTextV: function (text) { return function (xy) {
            return IO(function () { return (λ.ctx.strokeText(text, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null); });
        }; },
        fillText: function (text) { return function (x) { return function (y) {
            return IO(function () { return (λ.ctx.fillText(text, x * λ.ctx.canvas.width, y * λ.ctx.canvas.height), null); });
        }; }; },
        fillTextP: function (text) { return function (xy) {
            return IO(function () { return (λ.ctx.fillText(text, xy.fst * λ.ctx.canvas.width, xy.snd * λ.ctx.canvas.height), null); });
        }; },
        fillTextV: function (text) { return function (xy) {
            return IO(function () { return (λ.ctx.fillText(text, xy.x * λ.ctx.canvas.width, xy.y * λ.ctx.canvas.height), null); });
        }; },
        area: function (x0) { return function (y0) { return function (x1) { return function (y1) {
            return IO(function () {
                λ.ctx.rect(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height, (x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.height);
                return null;
            });
        }; }; }; },
        areaP: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.rect(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height, (xy1.fst - xy0.fst) * λ.ctx.canvas.width, (xy1.snd - xy0.snd) * λ.ctx.canvas.height);
                return null;
            });
        }; },
        areaV: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.rect(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height, (xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height);
                return null;
            });
        }; },
        strokeArea: function (x0) { return function (y0) { return function (x1) { return function (y1) {
            return IO(function () {
                λ.ctx.strokeRect(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height, (x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.width);
                return null;
            });
        }; }; }; },
        strokeAreaP: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.strokeRect(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height, (xy1.fst - xy0.fst) * λ.ctx.canvas.width, (xy1.snd - xy0.snd) * λ.ctx.canvas.height);
                return null;
            });
        }; },
        strokeAreaV: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.strokeRect(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height, (xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height);
                return null;
            });
        }; },
        fillArea: function (x0) { return function (y0) { return function (x1) { return function (y1) {
            return IO(function () {
                λ.ctx.fillRect(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height, (x1 - x0) * λ.ctx.canvas.width, (y1 - y0) * λ.ctx.canvas.width);
                return null;
            });
        }; }; }; },
        fillAreaP: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.fillRect(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height, (xy1.fst - xy0.fst) * λ.ctx.canvas.width, (xy1.snd - xy0.snd) * λ.ctx.canvas.height);
                return null;
            });
        }; },
        fillAreaV: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.fillRect(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height, (xy1.x - xy0.x) * λ.ctx.canvas.width, (xy1.y - xy0.y) * λ.ctx.canvas.height);
                return null;
            });
        }; },
        line: function (x0) { return function (y0) { return function (x1) { return function (y1) {
            return IO(function () {
                λ.ctx.moveTo(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height);
                λ.ctx.lineTo(x1 * λ.ctx.canvas.width, y1 * λ.ctx.canvas.height);
                return null;
            });
        }; }; }; },
        lineP: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.moveTo(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height);
                λ.ctx.lineTo(xy1.fst * λ.ctx.canvas.width, xy1.snd * λ.ctx.canvas.height);
                return null;
            });
        }; },
        lineV: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.moveTo(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height);
                λ.ctx.lineTo(xy1.x * λ.ctx.canvas.width, xy1.y * λ.ctx.canvas.height);
                return null;
            });
        }; },
        strokeLine: function (x0) { return function (y0) { return function (x1) { return function (y1) {
            return IO(function () {
                λ.ctx.moveTo(x0 * λ.ctx.canvas.width, y0 * λ.ctx.canvas.height);
                λ.ctx.lineTo(x1 * λ.ctx.canvas.width, y1 * λ.ctx.canvas.height);
                λ.ctx.stroke();
                return null;
            });
        }; }; }; },
        strokeLineP: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.moveTo(xy0.fst * λ.ctx.canvas.width, xy0.snd * λ.ctx.canvas.height);
                λ.ctx.lineTo(xy1.fst * λ.ctx.canvas.width, xy1.snd * λ.ctx.canvas.height);
                λ.ctx.stroke();
                return null;
            });
        }; },
        strokeLineV: function (xy0) { return function (xy1) {
            return IO(function () {
                λ.ctx.moveTo(xy0.x * λ.ctx.canvas.width, xy0.y * λ.ctx.canvas.height);
                λ.ctx.lineTo(xy1.x * λ.ctx.canvas.width, xy1.y * λ.ctx.canvas.height);
                λ.ctx.stroke();
                return null;
            });
        }; }
    },
    log: function (message) { return IO(function () { return (console.log(message), null); }); },
    warn: function (message) { return IO(function () { return (console.warn(message), null); }); },
    flush: IO(function () { return (console.clear(), null); }),
    debug: function (count) { return function (message) {
        return IO(function () {
            if (--λ.debugCounter < 0) {
                λ.debugCounter = count;
                console.debug(message);
            }
            return null;
        });
    }; },
    queue: function (io) { return IO(function () { return (requestAnimationFrame(io.INFO), null); }); },
    tick: IO(function () {
        for (var k in λ.keyboard)
            λ.keyboard[k] = relaxY(λ.keyboard[k]);
        for (var i in λ.mouseButtons)
            λ.mouseButtons[i] = relaxY(λ.mouseButtons[i]);
        λ.mouseScroll = Y.Rest;
        λ.mouseDeltaX = λ.mouseDeltaY = 0;
        λ.isResized = false;
        return null;
    }),
    activatePointerLock: IO(function () { return (onmouseup = function () { return λ.isPointerLocked || λ.ctx.canvas.requestPointerLock(); }, null); }),
    deactivatePointerLock: IO(function () { return λ.ctx.canvas.onmousedown = null; }),
    loadImage: function (path) {
        return IO(function () {
            λ.image[path] = new Image;
            λ.image[path].src = path;
            λ.image[path].onerror = function () { return THROW("'Output.loadImage' failed; could not load image: '" + path + "'"); };
            return null;
        });
    },
    drawImage: function (path) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], x, y), null); });
    }; }; },
    drawImageP: function (path) { return function (xy) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd), null); });
    }; },
    drawImageV: function (path) { return function (xy) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.x, xy.y), null); });
    }; },
    drawCroppedImage: function (path) {
        return function (cx) { return function (cy) { return function (cw) { return function (ch) {
            return function (x) { return function (y) { return function (w) { return function (h) {
                return IO(function () { return (λ.ctx.drawImage(λ.image[path], cx, cy, cw, ch, x, y, w, h), null); });
            }; }; }; };
        }; }; }; };
    },
    drawCroppedImageP: function (path) {
        return function (cxy) { return function (cwh) {
            return function (xy) { return function (wh) {
                return IO(function () {
                    λ.ctx.drawImage(λ.image[path], cxy.fst, cxy.snd, cwh.fst, cwh.snd, xy.fst, xy.snd, wh.fst, wh.snd);
                    return null;
                });
            }; };
        }; };
    },
    drawCroppedImageV: function (path) {
        return function (cxy) { return function (cwh) {
            return function (xy) { return function (wh) {
                return IO(function () { return (λ.ctx.drawImage(λ.image[path], cxy.x, cxy.y, cwh.x, cwh.y, xy.x, xy.y, wh.x, wh.y), null); });
            }; };
        }; };
    },
    drawFullImage: function (path) { return function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], x, y, w, h), null); });
    }; }; }; }; },
    drawFullImageP: function (path) { return function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd, wh.fst, wh.snd), null); });
    }; }; },
    drawFullImageV: function (path) { return function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.x, xy.y, wh.x, wh.y), null); });
    }; }; },
    drawSquareImage: function (path) { return function (k) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], x, y, k, k), null); });
    }; }; }; },
    drawSquareImageP: function (path) { return function (k) { return function (xy) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd, k, k), null); });
    }; }; },
    drawSquareImageV: function (path) { return function (k) { return function (xy) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], xy.x, xy.y, k, k), null); });
    }; }; },
    drawScaledImage: function (path) { return function (k) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.drawImage(λ.image[path], x, y, λ.image[path].width * k, λ.image[path].height * k), null); });
    }; }; }; },
    drawScaledImageP: function (path) { return function (k) { return function (xy) {
        return IO(function () {
            λ.ctx.drawImage(λ.image[path], xy.fst, xy.snd, λ.image[path].width * k, λ.image[path].height * k);
            return null;
        });
    }; }; },
    drawScaledImageV: function (path) { return function (k) { return function (xy) {
        return IO(function () {
            λ.ctx.drawImage(λ.image[path], xy.x, xy.y, λ.image[path].width * k, λ.image[path].height * k);
            return null;
        });
    }; }; },
    loadAudio: function (path) {
        return IO(function () {
            λ.audio[path] = new Audio(path);
            λ.audio[path].onerror = function () { return THROW("'Output.loadAudio' failed; could not load audio: '" + path + "'"); };
            return null;
        });
    },
    playAudio: function (path) {
        return IO(function () { var _a; return (((_a = λ.audio[path]) !== null && _a !== void 0 ? _a : THROW("'Output.playAudio' failed; audio not preloaded: '" + path + "'")).play(), null); });
    },
    playSFX: function (path) {
        return IO(function () {
            var _a;
            ((_a = λ.audio[path]) !== null && _a !== void 0 ? _a : THROW("'Output.playSFX' failed; audio not preloaded: '" + path + "'")).cloneNode().play();
            return null;
        });
    },
    loadFont: function (path) {
        return IO(function () {
            document.styleSheets[0].insertRule("@font-face{font-family:\"" + path.slice(path.lastIndexOf("/") + 1, path.lastIndexOf(".")) + "\";src:url(\"" + path + "\")}");
            return null;
        });
    },
    clearRectangle: function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (λ.ctx.clearRect(x, y, w, h), null); });
    }; }; }; },
    clearRectangleP: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.clearRect(xy.fst, xy.snd, wh.fst, xy.snd), null); });
    }; },
    clearRectangleV: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.clearRect(xy.x, xy.y, wh.x, wh.y), null); });
    }; },
    clearLayer: IO(function () { return (λ.ctx.clearRect(0, 0, λ.ctx.canvas.width, λ.ctx.canvas.height), null); }),
    clearCanvas: IO(function () { return (λ.ctxs.forEach(function (ctx) { return ctx.clearRect(0, 0, λ.ctx.canvas.width, λ.ctx.canvas.height); }, null)); }),
    fill: IO(function () { return (λ.ctx.fill(), null); }),
    stroke: IO(function () { return (λ.ctx.stroke(), null); }),
    save: IO(function () { return (λ.ctx.save(), null); }),
    restore: IO(function () { return (λ.ctx.restore(), null); }),
    clipEvenOdd: IO(function () { return (λ.ctx.clip('evenodd'), null); }),
    clipNonZero: IO(function () { return (λ.ctx.clip('nonzero'), null); }),
    rotate: function (angle) { return IO(function () { return (λ.ctx.rotate(angle), null); }); },
    scale: function (k) { return IO(function () { return (λ.ctx.scale(k, k), null); }); },
    scaleAxisX: function (kx) { return IO(function () { return (λ.ctx.scale(kx, 1), null); }); },
    scaleAxisY: function (ky) { return IO(function () { return (λ.ctx.scale(1, ky), null); }); },
    scaleAxis: function (kx) { return function (ky) { return IO(function () { return (λ.ctx.scale(kx, ky), null); }); }; },
    scaleAxisP: function (kxy) { return IO(function () { return (λ.ctx.scale(kxy.fst, kxy.snd), null); }); },
    scaleAxisV: function (kxy) { return IO(function () { return (λ.ctx.scale(kxy.x, kxy.y), null); }); },
    translateX: function (dx) { return IO(function () { return (λ.ctx.translate(dx, 0), null); }); },
    translateY: function (dy) { return IO(function () { return (λ.ctx.translate(0, dy), null); }); },
    translate: function (dx) { return function (dy) { return IO(function () { return (λ.ctx.translate(dx, dy), null); }); }; },
    translateP: function (dxy) { return IO(function () { return (λ.ctx.translate(dxy.fst, dxy.snd), null); }); },
    translateV: function (dxy) { return IO(function () { return (λ.ctx.translate(dxy.x, dxy.y), null); }); },
    transformation: function (m) { return IO(function () { return (λ.ctx.transform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky), null); }); },
    beginPath: IO(function () { return (λ.ctx.beginPath(), null); }),
    closePath: IO(function () { return (λ.ctx.closePath(), null); }),
    moveTo: function (x) { return function (y) { return IO(function () { return (λ.ctx.moveTo(x, y), null); }); }; },
    moveToP: function (xy) { return IO(function () { return (λ.ctx.moveTo(xy.fst, xy.snd), null); }); },
    moveToV: function (xy) { return IO(function () { return (λ.ctx.moveTo(xy.x, xy.y), null); }); },
    lineTo: function (x) { return function (y) { return IO(function () { return (λ.ctx.lineTo(x, y), null); }); }; },
    lineToP: function (xy) { return IO(function () { return (λ.ctx.lineTo(xy.fst, xy.snd), null); }); },
    lineToV: function (xy) { return IO(function () { return (λ.ctx.lineTo(xy.x, xy.y), null); }); },
    bezierCurveTo: function (cx0) { return function (cy0) {
        return function (cx1) { return function (cy1) {
            return function (x) { return function (y) {
                return IO(function () { return (λ.ctx.bezierCurveTo(cx0, cy0, cx1, cy1, x, y), null); });
            }; };
        }; };
    }; },
    bezierCurveToP: function (cxy0) {
        return function (cxy1) {
            return function (xy) {
                return IO(function () { return (λ.ctx.bezierCurveTo(cxy0.fst, cxy0.snd, cxy1.fst, cxy1.snd, xy.fst, xy.snd), null); });
            };
        };
    },
    bezierCurveToV: function (cxy0) { return function (cxy1) { return function (xy) {
        return IO(function () { return (λ.ctx.bezierCurveTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, xy.x, xy.y), null); });
    }; }; },
    quadraticCurveTo: function (cx) { return function (cy) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.quadraticCurveTo(cx, cy, x, y), null); });
    }; }; }; },
    quadraticCurveToP: function (cxy) { return function (xy) {
        return IO(function () { return (λ.ctx.quadraticCurveTo(cxy.fst, cxy.snd, xy.fst, xy.snd), null); });
    }; },
    quadraticCurveToV: function (cxy) { return function (xy) {
        return IO(function () { return (λ.ctx.quadraticCurveTo(cxy.x, cxy.y, xy.x, xy.y), null); });
    }; },
    arcTo: function (r) { return function (cx0) { return function (cy0) { return function (cx1) { return function (cy1) {
        return IO(function () { return (λ.ctx.arcTo(cx0, cy0, cx1, cy1, r), null); });
    }; }; }; }; },
    arcToP: function (r) { return function (cxy0) { return function (cxy1) {
        return IO(function () { return (λ.ctx.arcTo(cxy0.fst, cxy0.snd, cxy1.fst, cxy1.snd, r), null); });
    }; }; },
    arcToV: function (r) { return function (cxy0) { return function (cxy1) {
        return IO(function () { return (λ.ctx.arcTo(cxy0.x, cxy0.y, cxy1.x, cxy1.y, r), null); });
    }; }; },
    rectangle: function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (λ.ctx.rect(x, y, w, h), null); });
    }; }; }; },
    rectangleP: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.rect(xy.fst, xy.snd, wh.fst, wh.snd), null); });
    }; },
    rectangleV: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.rect(xy.x, xy.y, wh.x, wh.y), null); });
    }; },
    fillRectangle: function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (λ.ctx.fillRect(x, y, w, h), null); });
    }; }; }; },
    fillRectangleP: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.fillRect(xy.fst, xy.snd, wh.fst, wh.snd), null); });
    }; },
    fillRectangleV: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.fillRect(xy.x, xy.y, wh.x, wh.y), null); });
    }; },
    strokeRectangle: function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (λ.ctx.strokeRect(x, y, w, h), null); });
    }; }; }; },
    strokeRectangleP: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.strokeRect(xy.fst, xy.snd, wh.fst, wh.snd), null); });
    }; },
    strokeRectangleV: function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.strokeRect(xy.x, xy.y, wh.x, wh.y), null); });
    }; },
    arc: function (r) { return function (a) { return function (b) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.arc(x, y, r, a, b), null); });
    }; }; }; }; },
    arcP: function (r) { return function (a) { return function (b) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.fst, xy.snd, r, a, b), null); });
    }; }; }; },
    arcV: function (r) { return function (a) { return function (b) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.x, xy.y, r, a, b), null); });
    }; }; }; },
    circle: function (r) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.arc(x, y, r, 0, TAU), null); });
    }; }; },
    circleP: function (r) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.fst, xy.snd, r, 0, TAU), null); });
    }; },
    circleV: function (r) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), null); });
    }; },
    strokeCircle: function (r) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.arc(x, y, r, 0, TAU), λ.ctx.stroke(), null); });
    }; }; },
    strokeCircleP: function (r) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.fst, xy.snd, r, 0, TAU), λ.ctx.stroke(), null); });
    }; },
    strokeCircleV: function (r) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), λ.ctx.stroke(), null); });
    }; },
    fillCircle: function (r) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.arc(x, y, r, 0, TAU), λ.ctx.fill(), null); });
    }; }; },
    fillCircleP: function (r) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.fst, xy.snd, r, 0, TAU), λ.ctx.fill(), null); });
    }; },
    fillCircleV: function (r) { return function (xy) {
        return IO(function () { return (λ.ctx.arc(xy.x, xy.y, r, 0, TAU), λ.ctx.fill(), null); });
    }; },
    elliptic: function (r) { return function (a) { return function (b) {
        return function (x) { return function (y) { return function (kx) { return function (ky) {
            return IO(function () { return (λ.ctx.ellipse(x, y, kx, ky, r, a, b), null); });
        }; }; }; };
    }; }; },
    ellipticP: function (r) { return function (a) { return function (b) {
        return function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, a, b), null); });
        }; };
    }; }; },
    ellipticV: function (r) { return function (a) { return function (b) {
        return function (xy) { return function (wh) {
            return IO(function () { return (λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, a, b), null); });
        }; };
    }; }; },
    ellipse: function (r) { return function (x) { return function (y) { return function (kx) { return function (ky) {
        return IO(function () { return (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), null); });
    }; }; }; }; },
    ellipseP: function (r) { return function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, 0, TAU), null); });
    }; }; },
    ellipseV: function (r) { return function (xy) { return function (wh) {
        return IO(function () { return (λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU), null); });
    }; }; },
    strokeEllipse: function (r) { return function (x) { return function (y) { return function (kx) { return function (ky) {
        return IO(function () { return (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), λ.ctx.stroke(), null); });
    }; }; }; }; },
    strokeEllipseP: function (r) { return function (xy) { return function (wh) {
        return IO(function () {
            λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, 0, TAU);
            λ.ctx.stroke();
            return null;
        });
    }; }; },
    strokeEllipseV: function (r) { return function (xy) { return function (wh) {
        return IO(function () {
            λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU);
            λ.ctx.stroke();
            return null;
        });
    }; }; },
    fillEllipse: function (r) { return function (x) { return function (y) { return function (kx) { return function (ky) {
        return IO(function () { return (λ.ctx.ellipse(x, y, kx, ky, r, 0, TAU), λ.ctx.fill(), null); });
    }; }; }; }; },
    fillEllipseP: function (r) { return function (xy) { return function (wh) {
        return IO(function () {
            λ.ctx.ellipse(xy.fst, xy.snd, wh.fst, wh.snd, r, 0, TAU);
            λ.ctx.fill();
            return null;
        });
    }; }; },
    fillEllipseV: function (r) { return function (xy) { return function (wh) {
        return IO(function () {
            λ.ctx.ellipse(xy.x, xy.y, wh.x, wh.y, r, 0, TAU);
            λ.ctx.fill();
            return null;
        });
    }; }; },
    strokeText: function (text) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.strokeText(text, x, y), null); });
    }; }; },
    strokeTextP: function (text) { return function (xy) {
        return IO(function () { return (λ.ctx.strokeText(text, xy.fst, xy.snd), null); });
    }; },
    strokeTextV: function (text) { return function (xy) {
        return IO(function () { return (λ.ctx.strokeText(text, xy.x, xy.y), null); });
    }; },
    fillText: function (text) { return function (x) { return function (y) {
        return IO(function () { return (λ.ctx.fillText(text, x, y), null); });
    }; }; },
    fillTextP: function (text) { return function (xy) {
        return IO(function () { return (λ.ctx.fillText(text, xy.fst, xy.snd), null); });
    }; },
    fillTextV: function (text) { return function (xy) {
        return IO(function () { return (λ.ctx.fillText(text, xy.x, xy.y), null); });
    }; },
    area: function (x0) { return function (y0) { return function (x1) { return function (y1) {
        return IO(function () { return (λ.ctx.rect(x0, y0, x1 - x0, y1 - y0), null); });
    }; }; }; },
    areaP: function (xy0) { return function (xy1) {
        return IO(function () { return (λ.ctx.rect(xy0.fst, xy0.snd, xy1.fst - xy0.fst, xy1.snd - xy0.snd), null); });
    }; },
    areaV: function (xy0) { return function (xy1) {
        return IO(function () { return (λ.ctx.rect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null); });
    }; },
    strokeArea: function (x0) { return function (y0) { return function (x1) { return function (y1) {
        return IO(function () { return (λ.ctx.strokeRect(x0, y0, x1 - x0, y1 - y0), null); });
    }; }; }; },
    strokeAreaP: function (xy0) { return function (xy1) {
        return IO(function () { return (λ.ctx.strokeRect(xy0.fst, xy0.snd, xy1.fst - xy0.fst, xy1.snd - xy0.snd), null); });
    }; },
    strokeAreaV: function (xy0) { return function (xy1) {
        return IO(function () { return (λ.ctx.strokeRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null); });
    }; },
    fillArea: function (x0) { return function (y0) { return function (x1) { return function (y1) {
        return IO(function () { return (λ.ctx.fillRect(x0, y0, x1 - x0, y1 - y0), null); });
    }; }; }; },
    fillAreaP: function (xy0) { return function (xy1) {
        return IO(function () { return (λ.ctx.fillRect(xy0.fst, xy0.snd, xy1.fst - xy0.fst, xy1.snd - xy0.snd), null); });
    }; },
    fillAreaV: function (xy0) { return function (xy1) {
        return IO(function () { return (λ.ctx.fillRect(xy0.x, xy0.y, xy1.x - xy0.x, xy1.y - xy0.y), null); });
    }; },
    line: function (x0) { return function (y0) { return function (x1) { return function (y1) {
        return IO(function () {
            λ.ctx.moveTo(x0, y0);
            λ.ctx.lineTo(x1, y1);
            return null;
        });
    }; }; }; },
    lineP: function (xy0) { return function (xy1) {
        return IO(function () {
            λ.ctx.moveTo(xy0.fst, xy0.snd);
            λ.ctx.lineTo(xy1.fst, xy1.snd);
            return null;
        });
    }; },
    lineV: function (xy0) { return function (xy1) {
        return IO(function () {
            λ.ctx.moveTo(xy0.x, xy0.y);
            λ.ctx.lineTo(xy1.x, xy1.y);
            return null;
        });
    }; },
    strokeLine: function (x0) { return function (y0) { return function (x1) { return function (y1) {
        return IO(function () {
            λ.ctx.moveTo(x0, y0);
            λ.ctx.lineTo(x1, y1);
            λ.ctx.stroke();
            return null;
        });
    }; }; }; },
    strokeLineP: function (xy0) { return function (xy1) {
        return IO(function () {
            λ.ctx.moveTo(xy0.fst, xy0.snd);
            λ.ctx.lineTo(xy1.fst, xy1.snd);
            λ.ctx.stroke();
            return null;
        });
    }; },
    strokeLineV: function (xy0) { return function (xy1) {
        return IO(function () {
            λ.ctx.moveTo(xy0.x, xy0.y);
            λ.ctx.lineTo(xy1.x, xy1.y);
            λ.ctx.stroke();
            return null;
        });
    }; }
};
onload = function () {
    λ.ctxs = Array.from(document.querySelectorAll('canvas')).map(function (x) { return x.getContext('2d'); });
    λ.ctx = λ.ctxs[0];
    onmousemove = function (ev) {
        λ.mouseWindowX = ev.x,
            λ.mouseWindowY = ev.y,
            λ.mouseCanvasX = ev.clientX - λ.ctx.canvas.offsetLeft,
            λ.mouseCanvasY = ev.clientY - λ.ctx.canvas.offsetTop,
            λ.mouseScreenX = ev.screenX,
            λ.mouseScreenY = ev.screenY,
            λ.mouseDeltaX = ev.movementX,
            λ.mouseDeltaY = ev.movementY;
    };
    onmousedown = function (ev) { return λ.mouseButtons[ev.button] = Y.DD; };
    onmouseup = function (ev) { return λ.mouseButtons[ev.button] = Y.UU; };
    onkeyup = function (ev) { return λ.keyboard[ev.code] = Y.UU; };
    onkeydown = function (ev) {
        return λ.keyboard[ev.code] =
            ev.repeat
                ? λ.keyboard[ev.code]
                : Y.DD;
    };
    onwheel = function (ev) {
        return λ.mouseScroll =
            ev.deltaY < 0 ? Y.U :
                ev.deltaY > 0 ? Y.D : Y.Rest;
    };
    onresize = function () { return (clearTimeout(λ.resizeID), λ.resizeID = setTimeout(function () { return λ.isResized = true; }, 250)); };
    document.onpointerlockchange = function () {
        return λ.isPointerLocked = document.pointerLockElement === λ.ctx.canvas;
    };
};
