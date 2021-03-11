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
var __spreadArray = (this && this.__spreadArray) || function (to, from) {
    for (var i = 0, il = from.length, j = to.length; i < il; i++, j++)
        to[j] = from[i];
    return to;
};
var THROW = function (message) { throw new Error(message); };
var THROWTYPE = function (message) { throw new TypeError(message); };
var THROWRANGE = function (message) { throw new RangeError(message); };
var TAU = 6.283185307179586;
var IO = function (sideeffect) {
    return ({
        CONS: 'IO a',
        INFO: sideeffect,
        bind: function (f) { return IO(function () { return f(sideeffect()).INFO(); }); },
        fmap: function (f) { return IO(function () { return f(sideeffect()); }); },
        bindto: function (x) { return function (f) {
            return IO(function () {
                var _a;
                var $ = sideeffect();
                return __assign(__assign({}, $), (_a = {}, _a[x] = f($).INFO(), _a));
            });
        }; },
        fmapto: function (x) { return function (f) {
            return IO(function () {
                var _a;
                var $ = sideeffect();
                return __assign(__assign({}, $), (_a = {}, _a[x] = f($), _a));
            });
        }; },
        then: function (x) { return IO(function () { return (sideeffect(), x.INFO()); }); }
    });
};
var Nothing = {
    CONS: 'Nothing',
    bind: function (_) { return Nothing; },
    fmap: function (_) { return Nothing; },
    bindto: function (_) { return function (_) { return Nothing; }; },
    fmapto: function (_) { return function (_) { return Nothing; }; }
};
var Just = function (value) {
    return ({
        CONS: 'Just a',
        INFO: value,
        bind: function (f) {
            var x = f(value);
            return x.CONS === 'Nothing'
                ? Nothing
                : x;
        },
        fmap: function (f) { return Just(f(value)); },
        bindto: function (x) { return function (f) {
            var _a;
            var y = f(value);
            return y.CONS === 'Nothing'
                ? Nothing
                : Just(__assign(__assign({}, value), (_a = {}, _a[x] = y.INFO, _a)));
        }; },
        fmapto: function (x) { return function (f) {
            var _a;
            return Just(__assign(__assign({}, value), (_a = {}, _a[x] = f(value), _a)));
        }; }
    });
};
var State = function (statefulComputation) {
    return ({
        CONS: 'State (s -> (s, a))',
        INFO: statefulComputation,
        bind: function (f) {
            return State(function (x) {
                var _a = statefulComputation(x), y = _a[0], z = _a[1];
                return f(z).INFO(y);
            });
        },
        fmap: function (f) {
            return State(function (x) {
                var _a = statefulComputation(x), y = _a[0], z = _a[1];
                return [y, f(z)];
            });
        },
        bindto: function (k) { return function (f) {
            return State(function (x) {
                var _a;
                var _b = statefulComputation(x), y = _b[0], $ = _b[1];
                var _c = f($).INFO(y), z = _c[0], w = _c[1];
                return [z, __assign(__assign({}, $), (_a = {}, _a[k] = w, _a))];
            });
        }; },
        fmapto: function (k) { return function (f) {
            return State(function (x) {
                var _a;
                var _b = statefulComputation(x), y = _b[0], $ = _b[1];
                return [y, __assign(__assign({}, $), (_a = {}, _a[k] = f($), _a))];
            });
        }; },
        then: function (s) { return State(function (x) { return s.INFO(statefulComputation(x)[0]); }); }
    });
};
var List = function () {
    var elements = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        elements[_i] = arguments[_i];
    }
    return ({
        CONS: 'List a',
        INFO: elements,
        bind: function (f) { return List.apply(void 0, elements.flatMap(function (x) { return f(x).INFO; })); },
        fmap: function (f) { return List.apply(void 0, elements.map(function (x) { return f(x); })); },
        bindto: function (k) { return function (f) {
            return List.apply(void 0, elements.flatMap(function ($) { return f($).INFO.map(function (x) {
                var _a;
                return (__assign(__assign({}, $), (_a = {}, _a[k] = x, _a)));
            }); }));
        }; },
        fmapto: function (k) { return function (f) {
            return List.apply(void 0, elements.map(function ($) {
                var _a;
                return (__assign(__assign({}, $), (_a = {}, _a[k] = f($), _a)));
            }));
        }; },
        at: function (i) {
            return elements[i] === undefined
                ? THROWRANGE("Out of bounds index (" + i + ") occured with 'List' monad; indexing returned 'undefined'")
                : elements[i];
        }
    });
};
var Vector2D = function (x) { return function (y) {
    return ({
        CONS: 'Vector2D',
        x: x, y: y
    });
}; };
var Vector3D = function (x) { return function (y) { return function (z) {
    return ({
        CONS: 'Vector3D',
        x: x, y: y, z: z
    });
}; }; };
var Vector4D = function (x) { return function (y) { return function (z) { return function (w) {
    return ({
        CONS: 'Vector4D',
        x: x, y: y, z: z, w: w
    });
}; }; }; };
var Matrix2x2 = function (ix) { return function (jx) {
    return function (iy) { return function (jy) {
        return ({
            CONS: 'Matrix2x2',
            ix: ix, jx: jx,
            iy: iy, jy: jy,
            i: Vector2D(ix)(iy), j: Vector2D(jx)(jy),
            x: Vector2D(ix)(jx), y: Vector2D(iy)(jy)
        });
    }; };
}; };
var Matrix2D = function (i) { return function (j) {
    return Matrix2x2(i.x)(j.x)(i.y)(j.y);
}; };
var Matrix3x3 = function (ix) { return function (jx) { return function (kx) {
    return function (iy) { return function (jy) { return function (ky) {
        return function (iz) { return function (jz) { return function (kz) {
            return ({
                CONS: 'Matrix3x3',
                ix: ix, jx: jx, kx: kx,
                iy: iy, jy: jy, ky: ky,
                iz: iz, jz: jz, kz: kz,
                i: Vector3D(ix)(iy)(iz), j: Vector3D(jx)(jy)(jz), k: Vector3D(kx)(ky)(kz),
                x: Vector3D(ix)(jx)(kx), y: Vector3D(iy)(jy)(ky), z: Vector3D(iz)(jz)(kz)
            });
        }; }; };
    }; }; };
}; }; };
var Matrix3D = function (i) { return function (j) { return function (k) {
    return Matrix3x3(i.x)(j.x)(k.x)(i.y)(j.y)(k.y)(i.z)(j.z)(k.z);
}; }; };
var Matrix4x4 = function (ix) { return function (jx) { return function (kx) { return function (lx) {
    return function (iy) { return function (jy) { return function (ky) { return function (ly) {
        return function (iz) { return function (jz) { return function (kz) { return function (lz) {
            return function (iw) { return function (jw) { return function (kw) { return function (lw) {
                return ({
                    CONS: 'Matrix4x4',
                    ix: ix, jx: jx, kx: kx, lx: lx,
                    iy: iy, jy: jy, ky: ky, ly: ly,
                    iz: iz, jz: jz, kz: kz, lz: lz,
                    iw: iw, jw: jw, kw: kw, lw: lw,
                    i: Vector4D(ix)(iy)(iz)(iw), j: Vector4D(jx)(jy)(jz)(jw), k: Vector4D(kx)(ky)(kz)(kw), l: Vector4D(lx)(ly)(lz)(lw),
                    x: Vector4D(ix)(jx)(kx)(lx), y: Vector4D(iy)(jy)(ky)(ly), z: Vector4D(iz)(jz)(kz)(lz), w: Vector4D(iw)(jw)(kw)(lw)
                });
            }; }; }; };
        }; }; }; };
    }; }; }; };
}; }; }; };
var Matrix4D = function (i) { return function (j) { return function (k) { return function (l) {
    return Matrix4x4(i.x)(j.x)(k.x)(l.x)(i.y)(j.y)(k.y)(l.y)(i.z)(j.z)(k.z)(l.z)(i.w)(j.w)(k.w)(l.w);
}; }; }; };
var TextMeasurement = function (text) { return function (width) { return function (height) {
    return ({
        CONS: 'TextMeasurement',
        text: text, width: width, height: height
    });
}; }; };
var Switch = function (f) {
    return ({
        CONS: 'Switch',
        "case": function (x) { return function (y) {
            return Switch(function (z) {
                var w = f(z);
                return w === undefined && z === x ? y() : w;
            });
        }; },
        fall: function (x) { return Switch(function (y) {
            var z = f(y);
            return z === undefined ? x() : z;
        }); },
        "with": function (x) {
            var y = f(x);
            return y === undefined
                ? THROWRANGE("'Switch' did not cover all cases; missing case on value: '" + x + "'")
                : y;
        },
        thru: function (x) {
            var y = f(x);
            return y === undefined ? x : y;
        }
    });
};
Switch["case"] = function (domain) { return function (codomain) {
    return Switch(function (x) { return x === domain ? codomain() : undefined; });
}; };
var Bijection = function (pairs) {
    return ({
        CONS: 'Bijection',
        INFO: pairs,
        of: function (x) { return function (y) { return Bijection(__spreadArray(__spreadArray([], pairs), [[x, y]])); }; },
        domain: function (x) {
            var y = pairs.find(function (_a) {
                var z = _a[0], _ = _a[1];
                return z === x;
            });
            return y === undefined
                ? THROWRANGE("'Bijection' did not have a well-defined enough domain for value: '" + x + "'")
                : y[1];
        },
        codomain: function (x) {
            var y = pairs.find(function (_a) {
                var _ = _a[0], z = _a[1];
                return z === x;
            });
            return y === undefined
                ? THROWRANGE("'Bijection' did not have a well-defined enough codomain for value: '" + x + "'")
                : y[0];
        }
    });
};
Bijection.of = function (domainValue) { return function (codomainValue) {
    return Bijection([[domainValue, codomainValue]]);
}; };
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
    Vertical["CenterY"] = "None :: Vertical";
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
var relaxHorizontal = Switch["case"](Horizontal.Leftward)(function () { return Horizontal.Left; })["case"](Horizontal.Rightward)(function () { return Horizontal.Right; })
    .thru;
var relaxVertical = Switch["case"](Vertical.Downward)(function () { return Vertical.Down; })["case"](Vertical.Upward)(function () { return Vertical.Up; })
    .thru;
var relaxLateral = Switch["case"](Lateral.Backward)(function () { return Lateral.Back; })["case"](Lateral.Forward)(function () { return Lateral.Fore; })
    .thru;
var bijectionLineCap = Bijection
    .of(LineCap.Butt)('butt')
    .of(LineCap.Round)('round')
    .of(LineCap.Square)('square');
var bijectionLineJoin = Bijection
    .of(LineJoin.Round)('round')
    .of(LineJoin.Bevel)('bevel')
    .of(LineJoin.Miter)('miter');
var bijectionTextAlign = Bijection
    .of(TextAlign.Center)('center')
    .of(TextAlign.End)('end')
    .of(TextAlign.Left)('left')
    .of(TextAlign.Right)('right')
    .of(TextAlign.Start)('start');
var bijectionTextBaseline = Bijection
    .of(TextBaseline.Alphabetic)('alphabetic')
    .of(TextBaseline.Bottom)('bottom')
    .of(TextBaseline.Hanging)('hanging')
    .of(TextBaseline.Ideographic)('ideographic')
    .of(TextBaseline.Middle)('middle')
    .of(TextBaseline.Top)('top');
var bijectionCompositionOperation = Bijection
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
var Do = {
    IO: IO(function () { return ({}); }),
    Maybe: Just({}),
    State: State(function (s) { return [s, {}]; }),
    List: List({})
};
var __KEYBOARD_KEYS_ARRAY__ = [
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
var __EXTERNAL__ = {
    context: undefined,
    resizeID: undefined,
    isResized: false,
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
    keyboard: __KEYBOARD_KEYS_ARRAY__.reduce(function ($, k) {
        var _a;
        return (__assign(__assign({}, $), (_a = {}, _a[k] = Vertical.Up, _a)));
    }, {})
};
var Import;
(function (Import) {
    var Norm;
    (function (Norm) {
        Norm.mouseCanvasPosition = IO(function () { return [
            __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height
        ]; });
        Norm.mouseCanvasPositionVector = IO(function () {
            return Vector2D(__EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width)(__EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height);
        });
        Norm.mouseCanvasPositionX = IO(function () { return __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width; });
        Norm.mouseCanvasPositionY = IO(function () { return __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height; });
        Norm.mouseVelocity = IO(function () { return [
            __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height
        ]; });
        Norm.mouseVelocityVector = IO(function () {
            return Vector2D(__EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width)(__EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height);
        });
        Norm.mouseVelocityX = IO(function () { return __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width; });
        Norm.mouseVelocityY = IO(function () { return __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height; });
        Norm.textMeasurement = function (text) {
            return IO(function () {
                var _a = __EXTERNAL__.context.canvas, width = _a.width, height = _a.height;
                var _b = __EXTERNAL__.context.measureText(text), actualBoundingBoxLeft = _b.actualBoundingBoxLeft, actualBoundingBoxRight = _b.actualBoundingBoxRight, actualBoundingBoxAscent = _b.actualBoundingBoxAscent, actualBoundingBoxDescent = _b.actualBoundingBoxDescent;
                return TextMeasurement(text)((Math.abs(actualBoundingBoxLeft) + Math.abs(actualBoundingBoxRight)) / width)((Math.abs(actualBoundingBoxAscent) + Math.abs(actualBoundingBoxDescent)) / height);
            });
        };
        Norm.lineWidth = IO(function () { return __EXTERNAL__.context.lineWidth / __EXTERNAL__.context.canvas.width; });
        Norm.lineDashPattern = IO(function () { return List.apply(void 0, __EXTERNAL__.context.getLineDash().map(function (n) { return n / __EXTERNAL__.context.canvas.width; })); });
        Norm.lineDashOffset = IO(function () { return __EXTERNAL__.context.lineDashOffset / __EXTERNAL__.context.canvas.width; });
        Norm.fontSize = IO(function () { return parseFloat(__EXTERNAL__.context.font) / __EXTERNAL__.context.canvas.width; });
        Norm.shadowOffset = IO(function () { return [
            __EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height
        ]; });
        Norm.shadowOffsetVector = IO(function () {
            return Vector2D(__EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width)(__EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height);
        });
        Norm.shadowOffsetX = IO(function () { return __EXTERNAL__.context.shadowOffsetX / __EXTERNAL__.context.canvas.width; });
        Norm.shadowOffsetY = IO(function () { return __EXTERNAL__.context.shadowOffsetY / __EXTERNAL__.context.canvas.height; });
        Norm.isPointInEvenOddPath = function (x) { return function (y) {
            return IO(function () {
                return __EXTERNAL__.context.isPointInPath(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, 'evenodd');
            });
        }; };
        Norm.isPointInNonZeroPath = function (x) { return function (y) {
            return IO(function () {
                return __EXTERNAL__.context.isPointInPath(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, 'nonzero');
            });
        }; };
        Norm.isVectorInEvenOddPath = function (v) {
            return IO(function () {
                return __EXTERNAL__.context.isPointInPath(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height, 'evenodd');
            });
        };
        Norm.isVectorInNonZeroPath = function (v) {
            return IO(function () {
                return __EXTERNAL__.context.isPointInPath(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height, 'nonzero');
            });
        };
        Norm.isPointInStroke = function (x) { return function (y) {
            return IO(function () {
                return __EXTERNAL__.context.isPointInStroke(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
            });
        }; };
        Norm.isVectorInStroke = function (v) {
            return IO(function () {
                return __EXTERNAL__.context.isPointInStroke(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
            });
        };
        Norm.transformationMatrix = IO(function () {
            var m = __EXTERNAL__.context.getTransform();
            return Matrix3x3(m.a)(m.c)(m.e / __EXTERNAL__.context.canvas.width)(m.b)(m.d)(m.f / __EXTERNAL__.context.canvas.height)(0)(0)(1);
        });
    })(Norm = Import.Norm || (Import.Norm = {}));
    Import.timeSinceOpen = IO(function () { return performance.now(); });
    Import.timeSince1970 = IO(function () { return Date.now(); });
    Import.universalSeed = IO(function () { return __EXTERNAL__.seed; });
    Import.isWindowResized = IO(function () { return __EXTERNAL__.isResized; });
    Import.screenDimensions = IO(function () { return [screen.width, screen.height]; });
    Import.screenDimensionsVector = IO(function () { return Vector2D(screen.width)(screen.height); });
    Import.screenDimensionW = IO(function () { return screen.width; });
    Import.screenDimensionH = IO(function () { return screen.height; });
    Import.windowDimensions = IO(function () { return [innerWidth, innerHeight]; });
    Import.windowDimensionsVector = IO(function () { return Vector2D(innerWidth)(innerHeight); });
    Import.windowDimensionW = IO(function () { return innerWidth; });
    Import.windowDimensionH = IO(function () { return innerHeight; });
    Import.canvasDimensions = IO(function () { return [__EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height]; });
    Import.canvasDimensionsVector = IO(function () { return Vector2D(__EXTERNAL__.context.canvas.width)(__EXTERNAL__.context.canvas.height); });
    Import.canvasDimensionW = IO(function () { return __EXTERNAL__.context.canvas.width; });
    Import.canvasDimensionH = IO(function () { return __EXTERNAL__.context.canvas.height; });
    Import.mouseScreenPosition = IO(function () { return [__EXTERNAL__.mouse.screenX, __EXTERNAL__.mouse.screenY]; });
    Import.mouseScreenPositionVector = IO(function () { return Vector2D(__EXTERNAL__.mouse.screenX)(__EXTERNAL__.mouse.screenY); });
    Import.mouseScreenPositionX = IO(function () { return __EXTERNAL__.mouse.screenX; });
    Import.mouseScreenPositionY = IO(function () { return __EXTERNAL__.mouse.screenY; });
    Import.mouseWindowPosition = IO(function () { return [__EXTERNAL__.mouse.windowX, __EXTERNAL__.mouse.windowY]; });
    Import.mouseWindowPositionVector = IO(function () { return Vector2D(__EXTERNAL__.mouse.windowX)(__EXTERNAL__.mouse.windowY); });
    Import.mouseWindowPositionX = IO(function () { return __EXTERNAL__.mouse.windowX; });
    Import.mouseWindowPositionY = IO(function () { return __EXTERNAL__.mouse.windowY; });
    Import.mouseCanvasPosition = IO(function () { return [__EXTERNAL__.mouse.canvasX, __EXTERNAL__.mouse.canvasY]; });
    Import.mouseCanvasPositionVector = IO(function () { return Vector2D(__EXTERNAL__.mouse.canvasX)(__EXTERNAL__.mouse.canvasY); });
    Import.mouseCanvasPositionX = IO(function () { return __EXTERNAL__.mouse.canvasX; });
    Import.mouseCanvasPositionY = IO(function () { return __EXTERNAL__.mouse.canvasY; });
    Import.mouseVelocity = IO(function () { return [__EXTERNAL__.mouse.deltaX, __EXTERNAL__.mouse.deltaY]; });
    Import.mouseVelocityVector = IO(function () { return Vector2D(__EXTERNAL__.mouse.deltaX)(__EXTERNAL__.mouse.deltaY); });
    Import.mouseVelocityX = IO(function () { return __EXTERNAL__.mouse.deltaX; });
    Import.mouseVelocityY = IO(function () { return __EXTERNAL__.mouse.deltaY; });
    Import.mouseButtonLeft = IO(function () { return __EXTERNAL__.mouse.buttons[0]; });
    Import.mouseButtonMiddle = IO(function () { return __EXTERNAL__.mouse.buttons[1]; });
    Import.mouseButtonRight = IO(function () { return __EXTERNAL__.mouse.buttons[2]; });
    Import.mouseButtonA = IO(function () { return __EXTERNAL__.mouse.buttons[3]; });
    Import.mouseButtonB = IO(function () { return __EXTERNAL__.mouse.buttons[4]; });
    Import.keyboardKey = function (key) {
        return IO(function () { return __EXTERNAL__.keyboard[key]; });
    };
    Import.textMeasurement = function (text) {
        return IO(function () {
            var metrics = __EXTERNAL__.context.measureText(text);
            return TextMeasurement(text)(Math.abs(metrics.actualBoundingBoxLeft) + Math.abs(metrics.actualBoundingBoxRight))(Math.abs(metrics.actualBoundingBoxAscent) + Math.abs(metrics.actualBoundingBoxDescent));
        });
    };
    Import.lineWidth = IO(function () { return __EXTERNAL__.context.lineWidth; });
    Import.lineCap = IO(function () { return bijectionLineCap.codomain(__EXTERNAL__.context.lineCap); });
    Import.lineJoin = IO(function () { return bijectionLineJoin.codomain(__EXTERNAL__.context.lineJoin); });
    Import.lineDashPattern = IO(function () { return List.apply(void 0, __EXTERNAL__.context.getLineDash()); });
    Import.lineDashOffset = IO(function () { return __EXTERNAL__.context.lineDashOffset; });
    Import.miterLimit = IO(function () { return __EXTERNAL__.context.miterLimit; });
    Import.font = IO(function () { return __EXTERNAL__.context.font; });
    Import.fontSize = IO(function () { return parseFloat(__EXTERNAL__.context.font); });
    Import.fontFamily = IO(function () { return __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1); });
    Import.textAlign = IO(function () { return bijectionTextAlign.codomain(__EXTERNAL__.context.textAlign); });
    Import.textBaseline = IO(function () { return bijectionTextBaseline.codomain(__EXTERNAL__.context.textBaseline); });
    Import.shadowBlurAmount = IO(function () { return __EXTERNAL__.context.shadowBlur; });
    Import.shadowColor = IO(function () { return __EXTERNAL__.context.shadowColor; });
    Import.shadowOffset = IO(function () { return [__EXTERNAL__.context.shadowOffsetX, __EXTERNAL__.context.shadowOffsetY]; });
    Import.shadowOffsetVector = IO(function () { return Vector2D(__EXTERNAL__.context.shadowOffsetX)(__EXTERNAL__.context.shadowOffsetY); });
    Import.shadowOffsetX = IO(function () { return __EXTERNAL__.context.shadowOffsetX; });
    Import.shadowOffsetY = IO(function () { return __EXTERNAL__.context.shadowOffsetY; });
    Import.isPointInEvenOddPath = function (x) { return function (y) {
        return IO(function () { return __EXTERNAL__.context.isPointInPath(x, y, 'evenodd'); });
    }; };
    Import.isPointInNonZeroPath = function (x) { return function (y) {
        return IO(function () { return __EXTERNAL__.context.isPointInPath(x, y, 'nonzero'); });
    }; };
    Import.isVectorInEvenOddPath = function (v) {
        return IO(function () { return __EXTERNAL__.context.isPointInPath(v.x, v.y, 'evenodd'); });
    };
    Import.isVectorInNonZeroPath = function (v) {
        return IO(function () { return __EXTERNAL__.context.isPointInPath(v.x, v.y, 'nonzero'); });
    };
    Import.isPointInStroke = function (x) { return function (y) {
        return IO(function () { return __EXTERNAL__.context.isPointInStroke(x, y); });
    }; };
    Import.isVectorInStroke = function (v) {
        return IO(function () { return __EXTERNAL__.context.isPointInStroke(v.x, v.y); });
    };
    Import.transformationMatrix = IO(function () {
        var m = __EXTERNAL__.context.getTransform();
        return Matrix3x3(m.a)(m.c)(m.e)(m.b)(m.d)(m.f)(0)(0)(1);
    });
    Import.alpha = IO(function () { return __EXTERNAL__.context.globalAlpha; });
    Import.compositionOperation = IO(function () { return bijectionCompositionOperation.codomain(__EXTERNAL__.context.globalCompositeOperation); });
})(Import || (Import = {}));
var Mutate;
(function (Mutate) {
    var Norm;
    (function (Norm) {
        Norm.lineWidth = function (w) {
            return IO(function () {
                __EXTERNAL__.context.lineWidth = w * __EXTERNAL__.context.canvas.width;
                return null;
            });
        };
        Norm.lineDashPattern = function (pattern) {
            return IO(function () {
                __EXTERNAL__.context.setLineDash(pattern.INFO.map(function (n) { return n * __EXTERNAL__.context.canvas.width; }));
                return null;
            });
        };
        Norm.lineDashOffset = function (offset) {
            return IO(function () {
                __EXTERNAL__.context.lineDashOffset = offset * __EXTERNAL__.context.canvas.width;
                return null;
            });
        };
        Norm.fontSize = function (size) {
            return IO(function () {
                __EXTERNAL__.context.font =
                    size * __EXTERNAL__.context.canvas.width + "px " +
                        ("" + __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1));
                return null;
            });
        };
        Norm.fillRGBA = function (r) { return function (g) { return function (b) { return function (a) {
            return IO(function () {
                __EXTERNAL__.context.fillStyle = "rgba(" + r * 255 + "," + g * 255 + "," + b * 255 + "," + a + ")";
                return null;
            });
        }; }; }; };
        Norm.fillVector = function (v) {
            return IO(function () {
                __EXTERNAL__.context.fillStyle = "rgba(" + v.x * 255 + "," + v.y * 255 + "," + v.z * 255 + "," + v.w + ")";
                return null;
            });
        };
        Norm.strokeRGBA = function (r) { return function (g) { return function (b) { return function (a) {
            return IO(function () {
                __EXTERNAL__.context.strokeStyle = "rgba(" + r * 255 + "," + g * 255 + "," + b * 255 + "," + a * 255 + ")";
                return null;
            });
        }; }; }; };
        Norm.strokeVector = function (v) {
            return IO(function () {
                __EXTERNAL__.context.strokeStyle = "rgba(" + v.x * 255 + "," + v.y * 255 + "," + v.z * 255 + "," + v.w + ")";
                return null;
            });
        };
        Norm.shadowRGBA = function (r) { return function (g) { return function (b) { return function (a) {
            return IO(function () {
                __EXTERNAL__.context.shadowColor = "rgba(" + r * 255 + "," + g * 255 + "," + b * 255 + "," + a + ")";
                return null;
            });
        }; }; }; };
        Norm.shadowVector = function (v) {
            return IO(function () {
                __EXTERNAL__.context.shadowColor = "rgba(" + v.x * 255 + "," + v.y * 255 + "," + v.z * 255 + "," + v.w + ")";
                return null;
            });
        };
        Norm.shadowOffset = function (x) { return function (y) {
            return IO(function () {
                __EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width;
                __EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height;
                return null;
            });
        }; };
        Norm.shadowOffsetVector = function (v) {
            return IO(function () {
                __EXTERNAL__.context.shadowOffsetX = v.x * __EXTERNAL__.context.canvas.width;
                __EXTERNAL__.context.shadowOffsetY = v.y * __EXTERNAL__.context.canvas.height;
                return null;
            });
        };
        Norm.shadowOffsetX = function (x) {
            return IO(function () {
                __EXTERNAL__.context.shadowOffsetX = x * __EXTERNAL__.context.canvas.width;
                return null;
            });
        };
        Norm.shadowOffsetY = function (y) {
            return IO(function () {
                __EXTERNAL__.context.shadowOffsetY = y * __EXTERNAL__.context.canvas.height;
                return null;
            });
        };
        Norm.transformationMatrix = function (m) {
            return IO(function () {
                __EXTERNAL__.context.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx * __EXTERNAL__.context.canvas.width, m.ky * __EXTERNAL__.context.canvas.height);
                return null;
            });
        };
    })(Norm = Mutate.Norm || (Mutate.Norm = {}));
    Mutate.canvasDimensions = function (w) { return function (h) {
        return IO(function () {
            __EXTERNAL__.context.canvas.width = w;
            __EXTERNAL__.context.canvas.height = h;
            return null;
        });
    }; };
    Mutate.canvasDimensionVector = function (v) {
        return IO(function () {
            __EXTERNAL__.context.canvas.width = v.x;
            __EXTERNAL__.context.canvas.height = v.y;
            return null;
        });
    };
    Mutate.canvasDimensionW = function (w) {
        return IO(function () {
            __EXTERNAL__.context.canvas.width = w;
            return null;
        });
    };
    Mutate.canvasDimensionH = function (h) {
        return IO(function () {
            __EXTERNAL__.context.canvas.height = h;
            return null;
        });
    };
    Mutate.lineWidth = function (w) {
        return IO(function () {
            __EXTERNAL__.context.lineWidth = w;
            return null;
        });
    };
    Mutate.lineCap = function (cap) {
        return IO(function () {
            __EXTERNAL__.context.lineCap = bijectionLineCap.domain(cap);
            return null;
        });
    };
    Mutate.lineJoin = function (joining) {
        return IO(function () {
            __EXTERNAL__.context.lineJoin = bijectionLineJoin.domain(joining);
            return null;
        });
    };
    Mutate.lineDashPattern = function (pattern) {
        return IO(function () {
            __EXTERNAL__.context.setLineDash(pattern.INFO.slice());
            return null;
        });
    };
    Mutate.lineDashOffset = function (offset) {
        return IO(function () {
            __EXTERNAL__.context.lineDashOffset = offset;
            return null;
        });
    };
    Mutate.miterLimit = function (limit) {
        return IO(function () {
            __EXTERNAL__.context.miterLimit = limit;
            return null;
        });
    };
    Mutate.font = function (fontDescription) {
        return IO(function () {
            __EXTERNAL__.context.font = fontDescription;
            return null;
        });
    };
    Mutate.fontSize = function (size) {
        return IO(function () {
            __EXTERNAL__.context.font =
                size + "px " + __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1);
            return null;
        });
    };
    Mutate.fontFamily = function (family) {
        return IO(function () {
            __EXTERNAL__.context.font = parseFloat(__EXTERNAL__.context.font) + "px " + family;
            return null;
        });
    };
    Mutate.textAlign = function (align) {
        return IO(function () {
            __EXTERNAL__.context.textAlign = bijectionTextAlign.domain(align);
            return null;
        });
    };
    Mutate.textBaseline = function (baseline) {
        return IO(function () {
            __EXTERNAL__.context.textBaseline = bijectionTextBaseline.domain(baseline);
            return null;
        });
    };
    Mutate.fillColor = function (color) {
        return IO(function () {
            __EXTERNAL__.context.fillStyle = color;
            return null;
        });
    };
    Mutate.fillRGBA = function (r) { return function (g) { return function (b) { return function (a) {
        return IO(function () {
            __EXTERNAL__.context.fillStyle = "rgba(" + r + "," + g + "," + b + "," + a + ")";
            return null;
        });
    }; }; }; };
    Mutate.fillVector = function (v) {
        return IO(function () {
            __EXTERNAL__.context.fillStyle = "rgba(" + v.x + "," + v.y + "," + v.z + "," + v.w + ")";
            return null;
        });
    };
    Mutate.strokeColor = function (color) {
        return IO(function () {
            __EXTERNAL__.context.strokeStyle = color;
            return null;
        });
    };
    Mutate.strokeRGBA = function (r) { return function (g) { return function (b) { return function (a) {
        return IO(function () {
            __EXTERNAL__.context.strokeStyle = "rgba(" + r + "," + g + "," + b + "," + a + ")";
            return null;
        });
    }; }; }; };
    Mutate.strokeVector = function (v) {
        return IO(function () {
            __EXTERNAL__.context.strokeStyle = "rgba(" + v.x + "," + v.y + "," + v.z + "," + v.w + ")";
            return null;
        });
    };
    Mutate.shadowBlurAmount = function (amount) {
        return IO(function () {
            __EXTERNAL__.context.shadowBlur = amount;
            return null;
        });
    };
    Mutate.shadowColor = function (color) {
        return IO(function () {
            __EXTERNAL__.context.shadowColor = color;
            return null;
        });
    };
    Mutate.shadowRGBA = function (r) { return function (g) { return function (b) { return function (a) {
        return IO(function () {
            __EXTERNAL__.context.shadowColor = "rgba(" + r + "," + g + "," + b + "," + a + ")";
            return null;
        });
    }; }; }; };
    Mutate.shadowVector = function (v) {
        return IO(function () {
            __EXTERNAL__.context.shadowColor = "rgba(" + v.x + "," + v.y + "," + v.z + "," + v.w + ")";
            return null;
        });
    };
    Mutate.shadowOffset = function (x) { return function (y) {
        return IO(function () {
            __EXTERNAL__.context.shadowOffsetX = x;
            __EXTERNAL__.context.shadowOffsetY = y;
            return null;
        });
    }; };
    Mutate.shadowOffsetVector = function (v) {
        return IO(function () {
            __EXTERNAL__.context.shadowOffsetX = v.x;
            __EXTERNAL__.context.shadowOffsetY = v.y;
            return null;
        });
    };
    Mutate.shadowOffsetX = function (x) {
        return IO(function () {
            __EXTERNAL__.context.shadowOffsetX = x;
            return null;
        });
    };
    Mutate.shadowOffsetY = function (y) {
        return IO(function () {
            __EXTERNAL__.context.shadowOffsetY = y;
            return null;
        });
    };
    Mutate.transformationMatrix = function (m) {
        return IO(function () {
            __EXTERNAL__.context.setTransform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky);
            return null;
        });
    };
    Mutate.alpha = function (opacity) {
        return IO(function () {
            __EXTERNAL__.context.globalAlpha = opacity;
            return null;
        });
    };
    Mutate.compositionOperation = function (composition) {
        return IO(function () {
            __EXTERNAL__.context.globalCompositeOperation = bijectionCompositionOperation.domain(composition);
            return null;
        });
    };
})(Mutate || (Mutate = {}));
var Effect;
(function (Effect) {
    var Norm;
    (function (Norm) {
        Norm.drawImage = function (path) {
            return function (cropX) { return function (cropY) {
                return function (cropW) { return function (cropH) {
                    return function (x) { return function (y) {
                        return function (w) { return function (h) {
                            return IO(function () {
                                __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropX * __EXTERNAL__.image[path].width, cropY * __EXTERNAL__.image[path].height, cropW * __EXTERNAL__.image[path].width, cropH * __EXTERNAL__.image[path].height, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
                                return null;
                            });
                        }; };
                    }; };
                }; };
            }; };
        };
        Norm.drawImageVector = function (path) {
            return function (cropCoordinates) {
                return function (cropDimensions) {
                    return function (position) {
                        return function (dimensions) {
                            return IO(function () {
                                __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropCoordinates.x * __EXTERNAL__.image[path].width, cropCoordinates.y * __EXTERNAL__.image[path].height, cropDimensions.x * __EXTERNAL__.image[path].width, cropDimensions.y * __EXTERNAL__.image[path].height, position.x * __EXTERNAL__.context.canvas.width, position.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
                                return null;
                            });
                        };
                    };
                };
            };
        };
        Norm.drawFullImage = function (path) {
            return function (x) { return function (y) {
                return function (w) { return function (h) {
                    return IO(function () {
                        __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
                        return null;
                    });
                }; };
            }; };
        };
        Norm.drawFullImageVector = function (path) {
            return function (coordinates) {
                return function (dimensions) {
                    return IO(function () {
                        __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
                        return null;
                    });
                };
            };
        };
        Norm.drawFixedImage = function (path) { return function (x) { return function (y) { return function (k) {
            return IO(function () {
                __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, k * __EXTERNAL__.context.canvas.width, k * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; }; };
        Norm.drawFixedImageVector = function (path) { return function (coordinates) { return function (k) {
            return IO(function () {
                var l = k * __EXTERNAL__.context.canvas.width;
                __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, l, l);
                return null;
            });
        }; }; };
        Norm.drawScaledImage = function (path) { return function (x) { return function (y) { return function (k) {
            return IO(function () {
                __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, k * __EXTERNAL__.context.canvas.width * __EXTERNAL__.image[path].width, k * __EXTERNAL__.context.canvas.height * __EXTERNAL__.image[path].height);
                return null;
            });
        }; }; }; };
        Norm.drawScaledImageVector = function (path) { return function (coordinates) { return function (k) {
            return IO(function () {
                __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, k * __EXTERNAL__.context.canvas.width * __EXTERNAL__.image[path].width, k * __EXTERNAL__.context.canvas.height * __EXTERNAL__.image[path].height);
                return null;
            });
        }; }; };
        Norm.clearRectangle = function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () {
                __EXTERNAL__.context.clearRect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; }; };
        Norm.clearRectangleVector = function (coordinates) { return function (dimensions) {
            return IO(function () {
                __EXTERNAL__.context.clearRect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.rotate = function (angle) {
            return IO(function () {
                __EXTERNAL__.context.rotate(angle * TAU);
                return null;
            });
        };
        Norm.translate = function (dx) { return function (dy) {
            return IO(function () {
                __EXTERNAL__.context.translate(dx * __EXTERNAL__.context.canvas.width, dy * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.translateVector = function (v) {
            return IO(function () {
                __EXTERNAL__.context.translate(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        };
        Norm.transformation = function (m) {
            return IO(function () {
                __EXTERNAL__.context.transform(m.ix, m.iy, m.jx, m.jy, m.kx * __EXTERNAL__.context.canvas.width, m.ky * __EXTERNAL__.context.canvas.height);
                return null;
            });
        };
        Norm.moveTo = function (x) { return function (y) {
            return IO(function () {
                __EXTERNAL__.context.moveTo(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.moveToVector = function (v) {
            return IO(function () {
                __EXTERNAL__.context.moveTo(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        };
        Norm.lineTo = function (x) { return function (y) {
            return IO(function () {
                __EXTERNAL__.context.lineTo(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.lineToVector = function (v) {
            return IO(function () {
                __EXTERNAL__.context.lineTo(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        };
        Norm.bezierCurveTo = function (ix) { return function (iy) {
            return function (jx) { return function (jy) {
                return function (x) { return function (y) {
                    return IO(function () {
                        __EXTERNAL__.context.bezierCurveTo(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, jx * __EXTERNAL__.context.canvas.width, jy * __EXTERNAL__.context.canvas.height, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
                        return null;
                    });
                }; };
            }; };
        }; };
        Norm.bezierCurveToVector = function (i) { return function (j) { return function (v) {
            return IO(function () {
                __EXTERNAL__.context.bezierCurveTo(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, j.x * __EXTERNAL__.context.canvas.width, j.y * __EXTERNAL__.context.canvas.height, v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; };
        Norm.quadraticCurveTo = function (ix) { return function (iy) {
            return function (x) { return function (y) {
                return IO(function () {
                    __EXTERNAL__.context.quadraticCurveTo(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
                    return null;
                });
            }; };
        }; };
        Norm.quadraticCurveToVector = function (i) { return function (v) {
            return IO(function () {
                __EXTERNAL__.context.quadraticCurveTo(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.arcTo = function (ix) { return function (iy) {
            return function (jx) { return function (jy) {
                return function (r) {
                    return IO(function () {
                        __EXTERNAL__.context.arcTo(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, jx * __EXTERNAL__.context.canvas.width, jy * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width);
                        return null;
                    });
                };
            }; };
        }; };
        Norm.arcToVector = function (i) { return function (j) { return function (r) {
            return IO(function () {
                __EXTERNAL__.context.arcTo(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, j.x * __EXTERNAL__.context.canvas.width, j.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width);
                return null;
            });
        }; }; };
        Norm.rectangle = function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () {
                __EXTERNAL__.context.rect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; }; };
        Norm.rectangleVector = function (coordinates) { return function (dimensions) {
            return IO(function () {
                __EXTERNAL__.context.rect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.fillRectangle = function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () {
                __EXTERNAL__.context.fillRect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; }; };
        Norm.fillRectangleVector = function (coordinates) { return function (dimensions) {
            return IO(function () {
                __EXTERNAL__.context.fillRect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.strokeRectangle = function (x) { return function (y) { return function (w) { return function (h) {
            return IO(function () {
                __EXTERNAL__.context.strokeRect(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, w * __EXTERNAL__.context.canvas.width, h * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; }; };
        Norm.strokeRectangleVector = function (coordinates) { return function (dimensions) {
            return IO(function () {
                __EXTERNAL__.context.strokeRect(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; };
        Norm.arc = function (x) { return function (y) { return function (r) { return function (a) { return function (b) {
            return IO(function () {
                __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, a * TAU, b * TAU);
                return null;
            });
        }; }; }; }; };
        Norm.arcVector = function (v) { return function (r) { return function (a) { return function (b) {
            return IO(function () {
                __EXTERNAL__.context.arc(v.x * __EXTERNAL__.context.canvas.width, v.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, a * TAU, b * TAU);
                return null;
            });
        }; }; }; };
        Norm.circle = function (x) { return function (y) { return function (r) {
            return IO(function () {
                __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                return null;
            });
        }; }; };
        Norm.circleVector = function (coordinates) { return function (r) {
            return IO(function () {
                __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                return null;
            });
        }; };
        Norm.strokeCircle = function (x) { return function (y) { return function (r) {
            return IO(function () {
                __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                __EXTERNAL__.context.stroke();
                return null;
            });
        }; }; };
        Norm.strokeCircleVector = function (coordinates) { return function (r) {
            return IO(function () {
                __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                __EXTERNAL__.context.stroke();
                return null;
            });
        }; };
        Norm.fillCircle = function (x) { return function (y) { return function (r) {
            return IO(function () {
                __EXTERNAL__.context.arc(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                __EXTERNAL__.context.fill();
                return null;
            });
        }; }; };
        Norm.fillCircleVector = function (coordinates) { return function (r) {
            return IO(function () {
                __EXTERNAL__.context.arc(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                __EXTERNAL__.context.fill();
                return null;
            });
        }; };
        Norm.elliptic = function (x) { return function (y) {
            return function (kx) { return function (ky) {
                return function (a) { return function (b) {
                    return function (r) {
                        return IO(function () {
                            __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * TAU, a * TAU, b * TAU);
                            return null;
                        });
                    };
                }; };
            }; };
        }; };
        Norm.ellipticVector = function (coordinates) { return function (dimensions) {
            return function (a) { return function (b) { return function (r) {
                return IO(function () {
                    __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * TAU, a * TAU, b * TAU);
                    return null;
                });
            }; }; };
        }; };
        Norm.ellipse = function (x) { return function (y) {
            return function (kx) { return function (ky) {
                return function (r) {
                    return IO(function () {
                        __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                        return null;
                    });
                };
            }; };
        }; };
        Norm.ellipseVector = function (coordinates) {
            return function (dimensions) {
                return function (r) {
                    return IO(function () {
                        __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                        return null;
                    });
                };
            };
        };
        Norm.strokeEllipse = function (x) { return function (y) {
            return function (kx) { return function (ky) {
                return function (r) {
                    return IO(function () {
                        __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                        __EXTERNAL__.context.stroke();
                        return null;
                    });
                };
            }; };
        }; };
        Norm.strokeEllipseVector = function (coordinates) {
            return function (dimensions) {
                return function (r) {
                    return IO(function () {
                        __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                        __EXTERNAL__.context.stroke();
                        return null;
                    });
                };
            };
        };
        Norm.fillEllipse = function (x) { return function (y) {
            return function (kx) { return function (ky) {
                return function (r) {
                    return IO(function () {
                        __EXTERNAL__.context.ellipse(x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height, kx * __EXTERNAL__.context.canvas.width, ky * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                        __EXTERNAL__.context.fill();
                        return null;
                    });
                };
            }; };
        }; };
        Norm.fillEllipseVector = function (coordinates) {
            return function (dimensions) {
                return function (r) {
                    return IO(function () {
                        __EXTERNAL__.context.ellipse(coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.height, dimensions.x * __EXTERNAL__.context.canvas.width, dimensions.y * __EXTERNAL__.context.canvas.height, r * __EXTERNAL__.context.canvas.width, 0, TAU);
                        __EXTERNAL__.context.fill();
                        return null;
                    });
                };
            };
        };
        Norm.strokeText = function (text) { return function (x) { return function (y) {
            return IO(function () {
                __EXTERNAL__.context.strokeText(text, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; };
        Norm.strokeTextVector = function (text) { return function (coordinates) {
            return IO(function () {
                __EXTERNAL__.context.strokeText(text, coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.width);
                return null;
            });
        }; };
        Norm.fillText = function (text) { return function (x) { return function (y) {
            return IO(function () {
                __EXTERNAL__.context.fillText(text, x * __EXTERNAL__.context.canvas.width, y * __EXTERNAL__.context.canvas.height);
                return null;
            });
        }; }; };
        Norm.fillTextVector = function (text) { return function (coordinates) {
            return IO(function () {
                __EXTERNAL__.context.fillText(text, coordinates.x * __EXTERNAL__.context.canvas.width, coordinates.y * __EXTERNAL__.context.canvas.width);
                return null;
            });
        }; };
        Norm.area = function (ix) { return function (iy) { return function (jx) { return function (jy) {
            return IO(function () { return (__EXTERNAL__.context.rect(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, (jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height), null); });
        }; }; }; };
        Norm.areaVector = function (i) { return function (j) {
            return IO(function () { return (__EXTERNAL__.context.rect(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, (j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height), null); });
        }; };
        Norm.strokeArea = function (ix) { return function (iy) { return function (jx) { return function (jy) {
            return IO(function () { return (__EXTERNAL__.context.strokeRect(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, (jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height), null); });
        }; }; }; };
        Norm.strokeAreaVector = function (i) { return function (j) {
            return IO(function () { return (__EXTERNAL__.context.strokeRect(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, (j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height), null); });
        }; };
        Norm.fillArea = function (ix) { return function (iy) { return function (jx) { return function (jy) {
            return IO(function () { return (__EXTERNAL__.context.fillRect(ix * __EXTERNAL__.context.canvas.width, iy * __EXTERNAL__.context.canvas.height, (jx - ix) * __EXTERNAL__.context.canvas.width, (jy - iy) * __EXTERNAL__.context.canvas.height), null); });
        }; }; }; };
        Norm.fillAreaVector = function (i) { return function (j) {
            return IO(function () { return (__EXTERNAL__.context.fillRect(i.x * __EXTERNAL__.context.canvas.width, i.y * __EXTERNAL__.context.canvas.height, (j.x - i.x) * __EXTERNAL__.context.canvas.width, (j.y - i.y) * __EXTERNAL__.context.canvas.height), null); });
        }; };
    })(Norm = Effect.Norm || (Effect.Norm = {}));
    Effect.log = function (message) {
        return IO(function () { return (console.log(message), null); });
    };
    Effect.flush = IO(function () { return (console.clear(), null); });
    Effect.loadImage = function (path) {
        return IO(function () {
            __EXTERNAL__.image[path] = new Image;
            __EXTERNAL__.image[path].src = path;
            __EXTERNAL__.image[path].onerror = function () { return THROW("Could not load image: '" + path + "'"); };
            return null;
        });
    };
    Effect.drawImage = function (path) {
        return function (cropX) { return function (cropY) {
            return function (cropW) { return function (cropH) {
                return function (x) { return function (y) {
                    return function (w) { return function (h) {
                        return IO(function () {
                            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropX, cropY, cropW, cropH, x, y, w, h);
                            return null;
                        });
                    }; };
                }; };
            }; };
        }; };
    };
    Effect.drawImageVector = function (path) {
        return function (cropCoordinates) {
            return function (cropDimensions) {
                return function (position) {
                    return function (dimensions) {
                        return IO(function () {
                            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], cropCoordinates.x, cropCoordinates.y, cropDimensions.x, cropDimensions.y, position.x, position.y, dimensions.x, dimensions.y);
                            return null;
                        });
                    };
                };
            };
        };
    };
    Effect.drawFullImage = function (path) {
        return function (x) { return function (y) {
            return function (w) { return function (h) {
                return IO(function () { return (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x, y, w, h), null); });
            }; };
        }; };
    };
    Effect.drawFullImageVector = function (path) {
        return function (coordinates) {
            return function (dimensions) {
                return IO(function () {
                    __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x, coordinates.y, dimensions.x, dimensions.y);
                    return null;
                });
            };
        };
    };
    Effect.drawFixedImage = function (path) { return function (x) { return function (y) { return function (k) {
        return IO(function () { return (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x, y, k, k), null); });
    }; }; }; };
    Effect.drawFixedImageVector = function (path) { return function (coordinates) { return function (k) {
        return IO(function () { return (__EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x, coordinates.y, k, k), null); });
    }; }; };
    Effect.drawScaledImage = function (path) { return function (x) { return function (y) { return function (k) {
        return IO(function () {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], x, y, k * __EXTERNAL__.image[path].width, k * __EXTERNAL__.image[path].height);
            return null;
        });
    }; }; }; };
    Effect.drawScaledImageVector = function (path) { return function (coordinates) { return function (k) {
        return IO(function () {
            __EXTERNAL__.context.drawImage(__EXTERNAL__.image[path], coordinates.x, coordinates.y, k * __EXTERNAL__.image[path].width, k * __EXTERNAL__.image[path].height);
            return null;
        });
    }; }; };
    Effect.loadAudio = function (path) {
        return IO(function () {
            __EXTERNAL__.audio[path] = new Audio(path);
            __EXTERNAL__.audio[path].onerror = function () { return THROW("Could not load audio: '" + path + "'"); };
            return null;
        });
    };
    Effect.playAudio = function (path) {
        return IO(function () { return ((__EXTERNAL__.audio[path] || THROW("Audio not preloaded: '" + path + "'")).play(), null); });
    };
    Effect.playSFX = function (path) {
        return IO(function () { return ((__EXTERNAL__.audio[path] || THROW("Audio not preloaded: '" + path + "'")).cloneNode().play(), null); });
    };
    Effect.clearRectangle = function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (__EXTERNAL__.context.clearRect(x, y, w, h), null); });
    }; }; }; };
    Effect.clearRectangleVector = function (coordinates) { return function (dimensions) {
        return IO(function () { return (__EXTERNAL__.context.clearRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null); });
    }; };
    Effect.clearCanvas = IO(function () {
        __EXTERNAL__.context.clearRect(0, 0, __EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height);
        return null;
    });
    Effect.fill = IO(function () { return (__EXTERNAL__.context.fill(), null); });
    Effect.stroke = IO(function () { return (__EXTERNAL__.context.stroke(), null); });
    Effect.save = IO(function () { return (__EXTERNAL__.context.save(), null); });
    Effect.restore = IO(function () { return (__EXTERNAL__.context.restore(), null); });
    Effect.clipEvenOdd = IO(function () { return (__EXTERNAL__.context.clip('evenodd'), null); });
    Effect.clipNonZero = IO(function () { return (__EXTERNAL__.context.clip('nonzero'), null); });
    Effect.rotate = function (angle) {
        return IO(function () { return (__EXTERNAL__.context.rotate(angle), null); });
    };
    Effect.scale = function (k) {
        return IO(function () { return (__EXTERNAL__.context.scale(k, k), null); });
    };
    Effect.scaleAxis = function (kx) { return function (ky) {
        return IO(function () { return (__EXTERNAL__.context.scale(kx, ky), null); });
    }; };
    Effect.scaleAxisVector = function (v) {
        return IO(function () { return (__EXTERNAL__.context.scale(v.x, v.y), null); });
    };
    Effect.translate = function (dx) { return function (dy) {
        return IO(function () { return (__EXTERNAL__.context.translate(dx, dy), null); });
    }; };
    Effect.translateVector = function (v) {
        return IO(function () {
            __EXTERNAL__.context.translate(v.x, v.y);
            return null;
        });
    };
    Effect.transformation = function (m) {
        return IO(function () {
            __EXTERNAL__.context.transform(m.ix, m.iy, m.jx, m.jy, m.kx, m.ky);
            return null;
        });
    };
    Effect.beginPath = IO(function () { return (__EXTERNAL__.context.beginPath(), null); });
    Effect.closePath = IO(function () { return (__EXTERNAL__.context.closePath(), null); });
    Effect.moveTo = function (x) { return function (y) {
        return IO(function () { return (__EXTERNAL__.context.moveTo(x, y), null); });
    }; };
    Effect.moveToVector = function (v) {
        return IO(function () { return (__EXTERNAL__.context.moveTo(v.x, v.y), null); });
    };
    Effect.lineTo = function (x) { return function (y) {
        return IO(function () { return (__EXTERNAL__.context.lineTo(x, y), null); });
    }; };
    Effect.lineToVector = function (v) {
        return IO(function () { return (__EXTERNAL__.context.lineTo(v.x, v.y), null); });
    };
    Effect.bezierCurveTo = function (ix) { return function (iy) {
        return function (jx) { return function (jy) {
            return function (x) { return function (y) {
                return IO(function () { return (__EXTERNAL__.context.bezierCurveTo(ix, iy, jx, jy, x, y), null); });
            }; };
        }; };
    }; };
    Effect.bezierCurveToVector = function (i) { return function (j) { return function (v) {
        return IO(function () { return (__EXTERNAL__.context.bezierCurveTo(i.x, i.y, j.x, j.y, v.x, v.y), null); });
    }; }; };
    Effect.quadraticCurveTo = function (ix) { return function (iy) {
        return function (x) { return function (y) {
            return IO(function () { return (__EXTERNAL__.context.quadraticCurveTo(ix, iy, x, y), null); });
        }; };
    }; };
    Effect.quadraticCurveToVector = function (i) { return function (v) {
        return IO(function () { return (__EXTERNAL__.context.quadraticCurveTo(i.x, i.y, v.x, v.y), null); });
    }; };
    Effect.arcTo = function (ix) { return function (iy) {
        return function (jx) { return function (jy) {
            return function (r) {
                return IO(function () { return (__EXTERNAL__.context.arcTo(ix, iy, jx, jy, r), null); });
            };
        }; };
    }; };
    Effect.arcToVector = function (i) { return function (j) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.arcTo(i.x, i.y, j.x, j.y, r), null); });
    }; }; };
    Effect.rectangle = function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (__EXTERNAL__.context.rect(x, y, w, h), null); });
    }; }; }; };
    Effect.rectangleVector = function (coordinates) { return function (dimensions) {
        return IO(function () { return (__EXTERNAL__.context.rect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null); });
    }; };
    Effect.fillRectangle = function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (__EXTERNAL__.context.fillRect(x, y, w, h), null); });
    }; }; }; };
    Effect.fillRectangleVector = function (coordinates) { return function (dimensions) {
        return IO(function () { return (__EXTERNAL__.context.fillRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null); });
    }; };
    Effect.strokeRectangle = function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () { return (__EXTERNAL__.context.strokeRect(x, y, w, h), null); });
    }; }; }; };
    Effect.strokeRectangleVector = function (coordinates) { return function (dimensions) {
        return IO(function () { return (__EXTERNAL__.context.strokeRect(coordinates.x, coordinates.y, dimensions.x, dimensions.y), null); });
    }; };
    Effect.arc = function (x) { return function (y) { return function (r) { return function (a) { return function (b) {
        return IO(function () { return (__EXTERNAL__.context.arc(x, y, r, a, b), null); });
    }; }; }; }; };
    Effect.arcVector = function (v) { return function (r) { return function (a) { return function (b) {
        return IO(function () { return (__EXTERNAL__.context.arc(v.x, v.y, r, a, b), null); });
    }; }; }; };
    Effect.circle = function (x) { return function (y) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.arc(x, y, r, 0, TAU), null); });
    }; }; };
    Effect.circleVector = function (coordinates) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), null); });
    }; };
    Effect.strokeCircle = function (x) { return function (y) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.stroke(), null); });
    }; }; };
    Effect.strokeCircleVector = function (coordinates) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.stroke(), null); });
    }; };
    Effect.fillCircle = function (x) { return function (y) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.arc(x, y, r, 0, TAU), __EXTERNAL__.context.fill(), null); });
    }; }; };
    Effect.fillCircleVector = function (coordinates) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.arc(coordinates.x, coordinates.y, r, 0, TAU), __EXTERNAL__.context.fill(), null); });
    }; };
    Effect.elliptic = function (x) { return function (y) {
        return function (kx) { return function (ky) {
            return function (a) { return function (b) {
                return function (r) {
                    return IO(function () { return (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, a, b), null); });
                };
            }; };
        }; };
    }; };
    Effect.ellipticVector = function (coordinates) { return function (dimensions) {
        return function (a) { return function (b) { return function (r) {
            return IO(function () { return (__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, a, b), null); });
        }; }; };
    }; };
    Effect.ellipse = function (x) { return function (y) { return function (kx) { return function (ky) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), null); });
    }; }; }; }; };
    Effect.ellipseVector = function (coordinates) { return function (dimensions) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU), null); });
    }; }; };
    Effect.strokeEllipse = function (x) { return function (y) { return function (kx) { return function (ky) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), __EXTERNAL__.context.stroke(), null); });
    }; }; }; }; };
    Effect.strokeEllipseVector = function (coordinates) { return function (dimensions) { return function (r) {
        return IO(function () {
            __EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU);
            __EXTERNAL__.context.stroke();
            return null;
        });
    }; }; };
    Effect.fillEllipse = function (x) { return function (y) { return function (kx) { return function (ky) { return function (r) {
        return IO(function () { return (__EXTERNAL__.context.ellipse(x, y, kx, ky, r, 0, TAU), __EXTERNAL__.context.fill(), null); });
    }; }; }; }; };
    Effect.fillEllipseVector = function (coordinates) { return function (dimensions) { return function (r) {
        return IO(function () {
            __EXTERNAL__.context.ellipse(coordinates.x, coordinates.y, dimensions.x, dimensions.y, r, 0, TAU);
            __EXTERNAL__.context.fill();
            return null;
        });
    }; }; };
    Effect.strokeText = function (text) { return function (x) { return function (y) {
        return IO(function () { return (__EXTERNAL__.context.strokeText(text, x, y), null); });
    }; }; };
    Effect.strokeTextVector = function (text) { return function (coordinates) {
        return IO(function () { return (__EXTERNAL__.context.strokeText(text, coordinates.x, coordinates.y), null); });
    }; };
    Effect.fillText = function (text) { return function (x) { return function (y) {
        return IO(function () { return (__EXTERNAL__.context.fillText(text, x, y), null); });
    }; }; };
    Effect.fillTextVector = function (text) { return function (coordinates) {
        return IO(function () { return (__EXTERNAL__.context.fillText(text, coordinates.x, coordinates.y), null); });
    }; };
    Effect.area = function (ix) { return function (iy) { return function (jx) { return function (jy) {
        return IO(function () { return (__EXTERNAL__.context.rect(ix, iy, jx - ix, jy - iy), null); });
    }; }; }; };
    Effect.areaVector = function (i) { return function (j) {
        return IO(function () { return (__EXTERNAL__.context.rect(i.x, i.y, j.x - i.x, j.y - i.y), null); });
    }; };
    Effect.strokeArea = function (ix) { return function (iy) { return function (jx) { return function (jy) {
        return IO(function () { return (__EXTERNAL__.context.strokeRect(ix, iy, jx - ix, jy - iy), null); });
    }; }; }; };
    Effect.strokeAreaVector = function (i) { return function (j) {
        return IO(function () { return (__EXTERNAL__.context.strokeRect(i.x, i.y, j.x - i.x, j.y - i.y), null); });
    }; };
    Effect.fillArea = function (ix) { return function (iy) { return function (jx) { return function (jy) {
        return IO(function () { return (__EXTERNAL__.context.fillRect(ix, iy, jx - ix, jy - iy), null); });
    }; }; }; };
    Effect.fillAreaVector = function (i) { return function (j) {
        return IO(function () { return (__EXTERNAL__.context.fillRect(i.x, i.y, j.x - i.x, j.y - i.y), null); });
    }; };
})(Effect || (Effect = {}));
onload = function () {
    __EXTERNAL__.context = document.querySelector('canvas').getContext('2d');
    onkeydown = function (event) {
        if (!event.repeat)
            __EXTERNAL__.keyboard[event.code] = Vertical.Downward;
    };
    onkeyup = function (event) {
        __EXTERNAL__.keyboard[event.code] = Vertical.Upward;
    };
    onmousemove = function (event) {
        __EXTERNAL__.mouse.windowX = event.x;
        __EXTERNAL__.mouse.windowY = event.y;
        __EXTERNAL__.mouse.canvasX = event.clientX - __EXTERNAL__.context.canvas.offsetLeft;
        __EXTERNAL__.mouse.canvasY = event.clientY - __EXTERNAL__.context.canvas.offsetTop;
        __EXTERNAL__.mouse.screenX = event.screenX;
        __EXTERNAL__.mouse.screenY = event.screenY;
        __EXTERNAL__.mouse.deltaX = event.movementX;
        __EXTERNAL__.mouse.deltaY = event.movementY;
    };
    onmousedown = function (event) {
        __EXTERNAL__.mouse.buttons[event.button] = Vertical.Downward;
    };
    onmouseup = function (event) {
        __EXTERNAL__.mouse.buttons[event.button] = Vertical.Upward;
    };
    onwheel = function (event) {
        if (event.deltaY < 0)
            __EXTERNAL__.mouse.scroll = Vertical.Up;
        else
            (event.deltaY > 0);
        __EXTERNAL__.mouse.scroll = Vertical.Down;
    };
    onresize = function () {
        clearTimeout(__EXTERNAL__.resizeID);
        __EXTERNAL__.resizeID =
            setTimeout(function () { __EXTERNAL__.isResized = true; }, 250);
    };
};
