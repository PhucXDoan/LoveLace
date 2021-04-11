"use strict";
const ASPECT_RATIO = Pair(16, 9);
const RESIZING_THRESHOLD = 1;
const RESIZING_SPEED = 0.1;
const REFRESH_TIME = 15;
const Core = ({ time, refreshTime, canvasScalar, isRefresh, isResizing }) => ({
    CONS: 'Core',
    get pipe() { return (f) => f(this); },
    time, refreshTime, canvasScalar, isRefresh, isResizing
});
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
const ProgramInitial = ({ time }) => ({
    CONS: 'ProgramInitial',
    get pipe() { return (f) => f(this); },
    time
});
const fetchMaxCanvasScalar = Import.windowDimensions.fmap(p => uncurry(min)(fpair(div)(p)(ASPECT_RATIO)));
const setCanvasScalar = (scalar) => uncurry(Mutate.canvasDimensions)(both(mul(scalar))(ASPECT_RATIO));
