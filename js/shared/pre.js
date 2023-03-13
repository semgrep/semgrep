var Module = {
    runtimeInitialized: false,
    onRuntimeInitialized: () => {
        if (!(typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope)) {
            return;
        }
        Module.runtimeInitialized = true;
        postMessage("initialized");
    }
}