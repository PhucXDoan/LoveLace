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
var REFRESH_RATE = 15;
var ASPECT_RATIO = 16 / 9;
var RESIZE_SPEED = 0.25;
var RESIZE_THRESHOLD = 1;
var Global = function (record) { return (__assign({ CONS: 'Global' }, record)); };
var State1 = {
    CONS: 'State1'
};
var iterateGlobal = function (global) {
    return Do.IO
        .bindto('present', function (_) { return Input.time; })
        .fmapto('refreshTime', function ($) { return (global.refreshTime > REFRESH_RATE ? 0 : global.refreshTime) + $.present - global.time; })
        .bindto('maxScalar', function (_) { return maximumCanvasScalar; })
        .bindto('isResized', function (_) { return Input.isWindowResized; })
        .fmapto('isResizing', function ($) { return ($.isResized || global.isResizing) && diff(global.scalar)($.maxScalar) > RESIZE_THRESHOLD; })
        .fmap(function ($) {
        return Global({
            time: $.present,
            refreshTime: $.refreshTime,
            isRefresh: $.refreshTime > REFRESH_RATE,
            scalar: $.isResizing ? lerp(RESIZE_SPEED)(global.scalar)($.maxScalar) : global.scalar,
            isResizing: $.isResizing
        });
    });
};
var maximumCanvasScalar = Do.IO
    .bindto('windowDimensions', function (_) { return Input.windowP; })
    .fmap(function ($) { return min($.windowDimensions.fst)($.windowDimensions.snd * ASPECT_RATIO); });
var scaleCanvas = function (scalar) {
    return Reput.canvasWH(scalar)(scalar / ASPECT_RATIO);
};
var main = Do.IO
    .bindto('present', function (_) { return Input.time; })
    .bindto('maxScalar', function (_) { return maximumCanvasScalar; })
    .also(function ($) { return scaleCanvas($.maxScalar); })
    .bind(function ($) {
    return loop(Global({
        time: $.present,
        refreshTime: 0,
        isRefresh: false,
        scalar: $.maxScalar,
        isResizing: false
    }))(State1);
});
var loop = function (global) { return function (local) {
    return Do.IO
        .bindto('nextGlobal', function (_) { return iterateGlobal(global); })
        .bindto('nextLocal', function ($) { return update($.nextGlobal)(local); })
        .also(function ($) {
        return $.nextGlobal.isRefresh && $.nextGlobal.isResizing
            ? scaleCanvas($.nextGlobal.scalar)
            : idle;
    })
        .also(function ($) {
        return $.nextGlobal.isRefresh
            ? render($.nextGlobal)($.nextLocal)
            : idle;
    })
        .side(Output.tick)
        .bind(function ($) { return Output.queue(loop($.nextGlobal)($.nextLocal)); });
}; };
var render = function (global) { return function (local) {
    return idle;
}; };
var update = function (global) { return function (local) {
    return unit.IO(local);
}; };
