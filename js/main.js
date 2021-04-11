"use strict";
const main = Do.IO
    .bindto('present')(_ => Import.timeSinceOpen)
    .bindto('maxCanvasScalar')(_ => fetchMaxCanvasScalar)
    .also($ => uncurry(Mutate.canvasDimensions)(both(mul($.maxCanvasScalar))(ASPECT_RATIO)))
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
