"use strict";
const UPDATE_RATE = 15;
const RENDER_RATE = 15;
const ASPECT_RATIO = 9 / 16;
const RESIZE_SPEED = 0.25;
const Core = (record) => (Object.assign({ CONS: 'Core', isUpdate: record.updatecounter > UPDATE_RATE, isRender: record.rendercounter > RENDER_RATE }, record));
const updateCore = (core) => Do.IO
    .bindto('timeSinceOpen')(_ => Import.timeSinceOpen)
    .bindto('windowDimensions')(_ => Import.windowDimensions)
    .bindto('canvasDimensions')(_ => Import.canvasDimensions)
    .bindto('isWindowResized')(_ => Import.isWindowResized)
    .fmap($ => Core({
    time: $.timeSinceOpen,
    updatecounter: (core.updatecounter > UPDATE_RATE ? 0 : core.updatecounter) + $.timeSinceOpen - core.time,
    rendercounter: (core.rendercounter > RENDER_RATE ? 0 : core.rendercounter) + $.timeSinceOpen - core.time,
    isCanvasResizing: (core.isCanvasResizing || $.isWindowResized) &&
        (f => napprox(f($.windowDimensions))(f($.canvasDimensions))(4))(pick(fst($.windowDimensions) < snd($.windowDimensions) / ASPECT_RATIO))
}));
const ProgramA = {
    CONS: 'ProgramA'
};
const ProgramB = {
    CONS: 'ProgramB'
};
