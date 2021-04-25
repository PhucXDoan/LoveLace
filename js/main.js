"use strict";
const REFRESH_RATE = 15;
const ASPECT_RATIO = 16 / 9;
const RESIZE_SPEED = 0.25;
const RESIZE_THRESHOLD = 1;
const Global = (record) => (Object.assign({ CONS: 'Global' }, record));
const State1 = {
    CONS: 'State1'
};
const iterateGlobal = (global) => Do.IO
    .bindto('present', _ => Input.time)
    .fmapto('refreshTime', $ => (global.refreshTime > REFRESH_RATE ? 0 : global.refreshTime) + $.present - global.time)
    .bindto('maxScalar', _ => maximumCanvasScalar)
    .bindto('isResized', _ => Input.isWindowResized)
    .fmapto('isResizing', $ => ($.isResized || global.isResizing) && diff(global.scalar)($.maxScalar) > RESIZE_THRESHOLD)
    .fmap($ => Global({
    time: $.present,
    refreshTime: $.refreshTime,
    isRefresh: $.refreshTime > REFRESH_RATE,
    scalar: $.isResizing ? lerp(RESIZE_SPEED)(global.scalar)($.maxScalar) : global.scalar,
    isResizing: $.isResizing
}));
const maximumCanvasScalar = Do.IO
    .bindto('windowDimensions', _ => Input.windowP)
    .fmap($ => min($.windowDimensions.fst)($.windowDimensions.snd * ASPECT_RATIO));
const scaleCanvas = (scalar) => Reput.canvasWH(scalar)(scalar / ASPECT_RATIO);
const main = Do.IO
    .bindto('present', _ => Input.time)
    .bindto('maxScalar', _ => maximumCanvasScalar)
    .also($ => scaleCanvas($.maxScalar))
    .bind($ => loop(Global({
    time: $.present,
    refreshTime: 0,
    isRefresh: false,
    scalar: $.maxScalar,
    isResizing: false
}))(State1));
const loop = (global) => (local) => Do.IO
    .bindto('nextGlobal', _ => iterateGlobal(global))
    .bindto('nextLocal', $ => update($.nextGlobal)(local))
    .also($ => $.nextGlobal.isRefresh && $.nextGlobal.isResizing
    ? scaleCanvas($.nextGlobal.scalar)
    : idle)
    .also($ => $.nextGlobal.isRefresh
    ? render($.nextGlobal)($.nextLocal)
    : idle)
    .side(Output.tick)
    .bind($ => Output.queue(loop($.nextGlobal)($.nextLocal)));
const render = (global) => (local) => idle;
const update = (global) => (local) => unit.IO(local);
