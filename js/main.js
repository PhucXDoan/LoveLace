"use strict";
const ASPECT_RATIO = 16 / 9;
const RESIZING_THRESHOLD = 1;
const RESIZING_SPEED = 0.1;
const REFRESH_TIME = 15;
const ProgramInitial = ({ time }) => ({
    CONS: 'ProgramInitial',
    get pipe() { return (f) => f(this); },
    time
});
const fetchMaxCanvasScalar = Import.windowDimensions
    .fmap(fsnd(mul(ASPECT_RATIO)))
    .fmap(uncurry(min));
const setCanvasScalar = (scalar) => Mutate.canvasDimensions(scalar)(scalar / ASPECT_RATIO);
const main = Do.IO
    .bindto('present')(_ => Import.timeSinceOpen)
    .bindto('maxCanvasScalar')(_ => fetchMaxCanvasScalar)
    .also($ => uncurry(Mutate.canvasDimensions)(Pair($.maxCanvasScalar, $.maxCanvasScalar / ASPECT_RATIO)))
    .bind($ => loop(Core({
    time: $.present,
    refreshTime: 0,
    canvasScalar: $.maxCanvasScalar,
    isRefresh: false,
    isResizing: false
}))(ProgramInitial({
    time: $.present
})));
const loop = (core) => (program) => Do.IO
    .bindto('updatedCore')(_ => updateCore(core))
    .bindto('updatedProgram')(_ => updateProgram(program))
    .also($ => when($.updatedCore.isResizing && $.updatedCore.isRefresh)(setCanvasScalar($.updatedCore.canvasScalar)))
    .also($ => draw($.updatedCore)($.updatedProgram))
    .side(Effect.tick)
    .bind($ => Effect.queue(loop($.updatedCore)($.updatedProgram)));
const draw = (core) => (program) => idle;
const updateProgram = (program) => unit.IO(program);
