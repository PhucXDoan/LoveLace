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
var THROW = function (error) { throw error; };
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
                ? THROW(new RangeError("Out of bounds index (" + i + ") occured with 'List' monad; indexing returned 'undefined'"))
                : elements[i];
        }
    });
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
var TextAlignment;
(function (TextAlignment) {
    TextAlignment["Start"] = "Start :: TextAlignment";
    TextAlignment["End"] = "End :: TextAlignment";
    TextAlignment["Left"] = "Left :: TextAlignment";
    TextAlignment["Right"] = "Right :: TextAlignment";
    TextAlignment["Center"] = "Center :: TextAlignment";
})(TextAlignment || (TextAlignment = {}));
var TextBaseline;
(function (TextBaseline) {
    TextBaseline["Top"] = "Top :: TextBaseline";
    TextBaseline["Hanging"] = "Hanging :: TextBaseline";
    TextBaseline["Middle"] = "Middle :: TextBaseline";
    TextBaseline["Alphabetic"] = "Alphabetic :: TextBaseline";
    TextBaseline["Ideographic"] = "Ideographic :: TextBaseline";
    TextBaseline["Bottom"] = "Bottom :: TextBaseline";
})(TextBaseline || (TextBaseline = {}));
var relaxHorizontal = function (direction) {
    return direction === Horizontal.Leftward ? Horizontal.Left :
        direction === Horizontal.Rightward ? Horizontal.Right :
            direction;
};
var relaxVertical = function (direction) {
    return direction === Vertical.Downward ? Vertical.Down :
        direction === Vertical.Upward ? Vertical.Up :
            direction;
};
var relaxLateral = function (direction) {
    return direction === Lateral.Backward ? Lateral.Back :
        direction === Lateral.Forward ? Lateral.Fore :
            direction;
};
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
var Get;
(function (Get) {
    var Norm;
    (function (Norm) {
        Norm.mousePositionScreen = IO(function () { return [__EXTERNAL__.mouse.screenX / screen.width, __EXTERNAL__.mouse.screenY / screen.height]; });
        Norm.mousePositionScreenX = IO(function () { return __EXTERNAL__.mouse.screenX / screen.width; });
        Norm.mousePositionScreenY = IO(function () { return __EXTERNAL__.mouse.screenY / screen.height; });
        Norm.mousePositionWindow = IO(function () { return [__EXTERNAL__.mouse.windowX / innerWidth, __EXTERNAL__.mouse.windowY / innerHeight]; });
        Norm.mousePositionWindowX = IO(function () { return __EXTERNAL__.mouse.windowX / innerWidth; });
        Norm.mousePositionWindowY = IO(function () { return __EXTERNAL__.mouse.windowY / innerHeight; });
        Norm.mousePositionCanvas = IO(function () { return [
            __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height
        ]; });
        Norm.mousePositionCanvasX = IO(function () { return __EXTERNAL__.mouse.canvasX / __EXTERNAL__.context.canvas.width; });
        Norm.mousePositionCanvasY = IO(function () { return __EXTERNAL__.mouse.canvasY / __EXTERNAL__.context.canvas.height; });
        Norm.mousePositionScreenDelta = IO(function () { return [__EXTERNAL__.mouse.deltaX / screen.width, __EXTERNAL__.mouse.deltaY / screen.height]; });
        Norm.mousePositionScreenDeltaX = IO(function () { return __EXTERNAL__.mouse.deltaX / screen.width; });
        Norm.mousePositionScreenDeltaY = IO(function () { return __EXTERNAL__.mouse.deltaY / screen.height; });
        Norm.mousePositionWindowDelta = IO(function () { return [__EXTERNAL__.mouse.deltaX / innerWidth, __EXTERNAL__.mouse.deltaY / innerHeight]; });
        Norm.mousePositionWindowDeltaX = IO(function () { return __EXTERNAL__.mouse.deltaX / innerWidth; });
        Norm.mousePositionWindowDeltaY = IO(function () { return __EXTERNAL__.mouse.deltaY / innerHeight; });
        Norm.mousePositionCanvasDelta = IO(function () { return [
            __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width,
            __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height
        ]; });
        Norm.mousePositionCanvasDeltaX = IO(function () { return __EXTERNAL__.mouse.deltaX / __EXTERNAL__.context.canvas.width; });
        Norm.mousePositionCanvasDeltaY = IO(function () { return __EXTERNAL__.mouse.deltaY / __EXTERNAL__.context.canvas.height; });
    })(Norm = Get.Norm || (Get.Norm = {}));
    Get.timeSinceBeginning = IO(function () { return performance.now(); });
    Get.isWindowResized = IO(function () { return __EXTERNAL__.isResized; });
    Get.universalSeed = IO(function () { return __EXTERNAL__.seed; });
    Get.windowDimensions = IO(function () { return [innerWidth, innerHeight]; });
    Get.windowDimensionW = IO(function () { return innerWidth; });
    Get.windowDimensionH = IO(function () { return innerHeight; });
    Get.canvasDimensions = IO(function () { return [__EXTERNAL__.context.canvas.width, __EXTERNAL__.context.canvas.height]; });
    Get.canvasDimensionW = IO(function () { return __EXTERNAL__.context.canvas.width; });
    Get.canvasDimensionH = IO(function () { return __EXTERNAL__.context.canvas.height; });
    Get.mousePositionScreen = IO(function () { return [__EXTERNAL__.mouse.screenX, __EXTERNAL__.mouse.screenY]; });
    Get.mousePositionScreenX = IO(function () { return __EXTERNAL__.mouse.screenX; });
    Get.mousePositionScreenY = IO(function () { return __EXTERNAL__.mouse.screenY; });
    Get.mousePositionWindow = IO(function () { return [__EXTERNAL__.mouse.windowX, __EXTERNAL__.mouse.windowY]; });
    Get.mousePositionWindowX = IO(function () { return __EXTERNAL__.mouse.windowX; });
    Get.mousePositionWindowY = IO(function () { return __EXTERNAL__.mouse.windowY; });
    Get.mousePositionCanvas = IO(function () { return [__EXTERNAL__.mouse.canvasX, __EXTERNAL__.mouse.canvasY]; });
    Get.mousePositionCanvasX = IO(function () { return __EXTERNAL__.mouse.canvasX; });
    Get.mousePositionCanvasY = IO(function () { return __EXTERNAL__.mouse.canvasY; });
    Get.mousePositionDelta = IO(function () { return [__EXTERNAL__.mouse.deltaX, __EXTERNAL__.mouse.deltaY]; });
    Get.mousePositionDeltaX = IO(function () { return __EXTERNAL__.mouse.deltaX; });
    Get.mousePositionDeltaY = IO(function () { return __EXTERNAL__.mouse.deltaY; });
    Get.mouseScrollDirection = IO(function () { return __EXTERNAL__.mouse.scroll; });
    Get.mouseButtonLeft = IO(function () { return __EXTERNAL__.mouse.buttons[0]; });
    Get.mouseButtonMiddle = IO(function () { return __EXTERNAL__.mouse.buttons[1]; });
    Get.mouseButtonRight = IO(function () { return __EXTERNAL__.mouse.buttons[2]; });
    Get.mouseButtonEsotericA = IO(function () { return __EXTERNAL__.mouse.buttons[3]; });
    Get.mouseButtonEsotericB = IO(function () { return __EXTERNAL__.mouse.buttons[4]; });
    Get.keyboardKey = function (keyboardKeyName) {
        return IO(function () { return __EXTERNAL__.keyboard[keyboardKeyName]; });
    };
    Get.textMeasurement = function (message) {
        return IO(function () { return __EXTERNAL__.context.measureText(message); });
    };
    Get.lineWidth = IO(function () { return __EXTERNAL__.context.lineWidth; });
    Get.lineCap = IO(function () {
        return __EXTERNAL__.context.lineCap === 'butt' ? LineCap.Butt :
            __EXTERNAL__.context.lineCap === 'round' ? LineCap.Round :
                __EXTERNAL__.context.lineCap === 'square' ? LineCap.Square :
                    LineCap.Butt;
    });
    Get.lineJoin = IO(function () {
        return __EXTERNAL__.context.lineJoin === 'bevel' ? LineJoin.Bevel :
            __EXTERNAL__.context.lineJoin === 'miter' ? LineJoin.Miter :
                __EXTERNAL__.context.lineJoin === 'round' ? LineJoin.Round :
                    LineJoin.Miter;
    });
    Get.miterLimit = IO(function () { return __EXTERNAL__.context.miterLimit; });
    Get.lineDashPattern = IO(function () { return List.apply(void 0, __EXTERNAL__.context.getLineDash()); });
    Get.lineDashOffset = IO(function () { return __EXTERNAL__.context.lineDashOffset; });
    Get.font = IO(function () { return __EXTERNAL__.context.font; });
    Get.fontSize = IO(function () { return +__EXTERNAL__.context.font.slice(0, __EXTERNAL__.context.font.indexOf("px")); });
    Get.fontFamily = IO(function () { return __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf(" ") + 1); });
    Get.textAlignment = IO(function () {
        return __EXTERNAL__.context.textAlign === 'center' ? TextAlignment.Center :
            __EXTERNAL__.context.textAlign === 'end' ? TextAlignment.End :
                __EXTERNAL__.context.textAlign === 'left' ? TextAlignment.Left :
                    __EXTERNAL__.context.textAlign === 'right' ? TextAlignment.Right :
                        __EXTERNAL__.context.textAlign === 'start' ? TextAlignment.Start :
                            TextAlignment.Start;
    });
    Get.textBaseline = IO(function () {
        return __EXTERNAL__.context.textBaseline === 'alphabetic' ? TextBaseline.Alphabetic :
            __EXTERNAL__.context.textBaseline === 'bottom' ? TextBaseline.Bottom :
                __EXTERNAL__.context.textBaseline === 'hanging' ? TextBaseline.Hanging :
                    __EXTERNAL__.context.textBaseline === 'ideographic' ? TextBaseline.Ideographic :
                        __EXTERNAL__.context.textBaseline === 'middle' ? TextBaseline.Middle :
                            __EXTERNAL__.context.textBaseline === 'top' ? TextBaseline.Top :
                                null;
    });
})(Get || (Get = {}));
var Put;
(function (Put) {
    Put.lineWidth = function (w) {
        return IO(function () {
            __EXTERNAL__.context.lineWidth = w;
            return null;
        });
    };
    Put.lineCap = function (cap) {
        return IO(function () {
            __EXTERNAL__.context.lineCap =
                cap === LineCap.Butt ? 'butt' :
                    cap === LineCap.Round ? 'round' :
                        cap === LineCap.Square ? 'square' :
                            null;
            return null;
        });
    };
    Put.lineJoin = function (join) {
        return IO(function () {
            __EXTERNAL__.context.lineCap =
                join === LineJoin.Bevel ? 'bevel' :
                    join === LineJoin.Miter ? 'miter' :
                        join === LineJoin.Round ? 'round' :
                            null;
            return null;
        });
    };
    Put.miterLimit = function (limit) {
        return IO(function () {
            __EXTERNAL__.context.miterLimit = limit;
            return null;
        });
    };
    Put.lineDashPattern = function (pattern) {
        return IO(function () {
            __EXTERNAL__.context.setLineDash(pattern.INFO.slice());
            return null;
        });
    };
    Put.lineDashOffset = function (offset) {
        return IO(function () {
            __EXTERNAL__.context.lineDashOffset = offset;
            return null;
        });
    };
    Put.font = function (newfont) {
        return IO(function () {
            __EXTERNAL__.context.font = newfont;
            return null;
        });
    };
    Put.fontSize = function (size) {
        return IO(function () {
            __EXTERNAL__.context.font = size + __EXTERNAL__.context.font.slice(__EXTERNAL__.context.font.indexOf("px "));
            return null;
        });
    };
    Put.fontFamily = function (family) {
        return IO(function () {
            __EXTERNAL__.context.font = __EXTERNAL__.context.font.slice(0, __EXTERNAL__.context.font.indexOf("px ") + 3) + family;
            return null;
        });
    };
    Put.textAlignment = function (alignment) {
        return IO(function () {
            __EXTERNAL__.context.textAlign =
                alignment === TextAlignment.Center ? 'center' :
                    alignment === TextAlignment.End ? 'end' :
                        alignment === TextAlignment.Left ? 'left' :
                            alignment === TextAlignment.Right ? 'right' :
                                alignment === TextAlignment.Start ? 'start' :
                                    null;
            return null;
        });
    };
    Put.textBaseline = function (baseline) {
        return IO(function () {
            __EXTERNAL__.context.textBaseline =
                baseline === TextBaseline.Alphabetic ? 'alphabetic' :
                    baseline === TextBaseline.Bottom ? 'bottom' :
                        baseline === TextBaseline.Hanging ? 'hanging' :
                            baseline === TextBaseline.Ideographic ? 'ideographic' :
                                baseline === TextBaseline.Middle ? 'middle' :
                                    baseline === TextBaseline.Top ? 'top' :
                                        null;
            return null;
        });
    };
    Put.fill = function (color) {
        return IO(function () {
            __EXTERNAL__.context.fillStyle = color;
            return null;
        });
    };
    Put.stroke = function (color) {
        return IO(function () {
            __EXTERNAL__.context.strokeStyle = color;
            return null;
        });
    };
})(Put || (Put = {}));
var Act;
(function (Act) {
    Act.tickExternalState = IO(function () {
        __EXTERNAL__.mouse.scroll = Vertical.CenterY;
        __EXTERNAL__.isResized = false;
        for (var k in __EXTERNAL__.keyboard)
            __EXTERNAL__.keyboard[k] = relaxVertical(__EXTERNAL__.keyboard[k]);
        for (var k in __EXTERNAL__.mouse.buttons)
            __EXTERNAL__.mouse.buttons[k] = relaxVertical(__EXTERNAL__.mouse.buttons[k]);
        return null;
    });
    Act.clearRect = function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () {
            __EXTERNAL__.context.clearRect(x, y, w, h);
            return null;
        });
    }; }; }; };
    Act.fillRect = function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () {
            __EXTERNAL__.context.fillRect(x, y, w, h);
            return null;
        });
    }; }; }; };
    Act.strokeRect = function (x) { return function (y) { return function (w) { return function (h) {
        return IO(function () {
            __EXTERNAL__.context.strokeRect(x, y, w, h);
            return null;
        });
    }; }; }; };
    Act.fillText = function (message) { return function (x) { return function (y) {
        return IO(function () {
            __EXTERNAL__.context.fillText(message, x, y);
            return null;
        });
    }; }; };
    Act.strokeText = function (message) { return function (x) { return function (y) {
        return IO(function () {
            __EXTERNAL__.context.strokeText(message, x, y);
            return null;
        });
    }; }; };
})(Act || (Act = {}));
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
