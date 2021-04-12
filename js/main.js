"use strict";
const ASPECT_RATIO = 16 / 9;
const cubeVertices = List(Vector3D(-0.5)(-0.5)(-0.5), Vector3D(0.5)(-0.5)(-0.5), Vector3D(-0.5)(0.5)(-0.5), Vector3D(0.5)(0.5)(-0.5), Vector3D(-0.5)(-0.5)(0.5), Vector3D(0.5)(-0.5)(0.5), Vector3D(-0.5)(0.5)(0.5), Vector3D(0.5)(0.5)(0.5));
const Camera = (record) => (Object.assign({ CONS: 'Camera', get pipe() { return (f) => f(this); } }, record));
const ProgramInitial = (record) => (Object.assign({ CONS: 'ProgramInitial', get pipe() { return (f) => f(this); } }, record));
const fetchMaxCanvasScalar = Import.windowDimensions
    .fmap(fsnd(mul(ASPECT_RATIO)))
    .fmap(uncurry(min));
const setCanvasScalar = (scalar) => Mutate.canvasDimensions(scalar)(scalar / ASPECT_RATIO);
const projectVector3D = (camera) => (v) => Vector2D(0.5 - (v.x - camera.position.x) / (v.z - camera.position.z))(0.5 + (v.y - camera.position.y) / (v.z - camera.position.z) * ASPECT_RATIO);
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
    time: $.present,
    camera: Camera({
        position: Vector3D(0)(0)(0)
    })
})));
const loop = (core) => (program) => Do.IO
    .bindto('updatedCore')(_ => updateCore(core))
    .bindto('updatedProgram')($ => ($.updatedCore.isRefresh ? updateProgram($.updatedCore) : unit.IO)(program))
    .also($ => when($.updatedCore.isResizing && $.updatedCore.isRefresh)(setCanvasScalar($.updatedCore.canvasScalar)))
    .also($ => draw($.updatedCore)($.updatedProgram))
    .side(Effect.tick)
    .bind($ => Effect.queue(loop($.updatedCore)($.updatedProgram)));
const draw = (core) => (program) => Effect.clearCanvas
    .then(cubeVertices
    .fmap(projectVector3D(program.camera))
    .fmap(Effect.Norm.fillCircleVector(0.01))
    .fmap(Effect.beginPath.then)
    .pipe(executeIOs));
const updateProgram = (core) => (program) => Do.IO
    .bindto('keyW')(_ => Import.keyboardKey('KeyW'))
    .bindto('keyA')(_ => Import.keyboardKey('KeyA'))
    .bindto('keyS')(_ => Import.keyboardKey('KeyS'))
    .bindto('keyD')(_ => Import.keyboardKey('KeyD'))
    .bindto('space')(_ => Import.keyboardKey('Space'))
    .bindto('lctrl')(_ => Import.keyboardKey('ControlLeft'))
    .fmapto('cameraMovement')($ => Vector3D(0)(0)(0)
    .pipe(translate3D(isDown($.keyA) ? -1 : 0)(isDown($.lctrl) ? -1 : 0)(isDown($.keyW) ? -1 : 0))
    .pipe(translate3D(isDown($.keyD) ? 1 : 0)(isDown($.space) ? 1 : 0)(isDown($.keyS) ? 1 : 0))
    .pipe(rescale3D(0.1)))
    .fmap($ => ProgramInitial({
    time: core.time,
    camera: Camera({
        position: translateVector3D($.cameraMovement)(program.camera.position)
    })
}));
