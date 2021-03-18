"use strict";
const fetchMaxCanvasScalar = Import.windowDimensions.fmap(p => min(fst(p))(snd(p) / ASPECT_RATIO));
const scaleCanvas = (scalar) => Mutate.canvasDimensions(scalar)(scalar * ASPECT_RATIO);
const main = Do.IO
    .bindto('timeSinceOpen')(_ => Import.timeSinceOpen)
    .side(_ => fetchMaxCanvasScalar.bind(scaleCanvas))
    .bind($ => loop(Core({
    time: $.timeSinceOpen,
    updatecounter: 0,
    rendercounter: 0,
    isCanvasResizing: false
}))(ProgramA));
const loop = (core) => (program) => Do.IO
    .bindto('aCore')(_ => updateCore(core))
    .bindto('aProgram')(_ => updateProgram(core)(program))
    .side(_ => core.isCanvasResizing && core.isRender
    ? Import.canvasDimensionW
        .bind(currentScalar => fetchMaxCanvasScalar.fmap(lerp(RESIZE_SPEED)(currentScalar)))
        .bind(scaleCanvas)
    : nil)
    .side(_ => render(core)(program))
    .side(_ => Effect.fillText(`${core.time} | ${core.updatecounter} | ${core.rendercounter}`)(10)(10))
    .side(_ => Effect.tick)
    .bind($ => Effect.queue(loop($.aCore)($.aProgram)));
const updateProgram = (core) => (program) => program.CONS === 'ProgramA' ?
    Do.IO
        .bindto('mouseButtonLeft')(_ => Import.mouseButtonLeft)
        .fmap($ => $.mouseButtonLeft === Vertical.Downward
        ? ProgramB
        : ProgramA)
    :
        program.CONS === 'ProgramB' ?
            Do.IO
                .fmap(_ => ProgramB)
            :
                Do.IO
                    .fmap(_ => program);
const render = (core) => (program) => program.CONS === 'ProgramA' ?
    Do.IO
        .side(_ => Effect.clearCanvas)
        .side(_ => Mutate.fillColor(core.isCanvasResizing ? 'green' : 'red'))
        .side(_ => Effect.Norm.fillRectangle(0.1)(0.1)(0.1)(0.1))
        .side(_ => Effect.Norm.fillRectangle(0.8)(0.1)(0.1)(0.1))
        .side(_ => Effect.Norm.fillRectangle(0.8)(0.8)(0.1)(0.1))
        .side(_ => Effect.Norm.fillRectangle(0.1)(0.8)(0.1)(0.1))
        .fmap(_ => null)
    :
        program.CONS === 'ProgramB' ?
            Do.IO
                .side(_ => Effect.clearCanvas)
                .side(_ => Effect.fillRectangle(50)(25)(75)(100))
                .fmap(_ => null)
            :
                nil;
