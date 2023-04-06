import ParserModuleFactory from "./semgrep-parser";

export const ParserFactory = async () => {
  globalThis.ParserModule = await ParserModuleFactory();
  return require("./Parser.bc");
};

const exportedMethodNames = [
  "parseFile",
  "parsePattern",
  "writeFile",
  "readFile",
];

export const ParserWorkerFactory = (workerURL) => {
  return new Promise((resolve, reject) => {
    const worker = new Worker(workerURL);

    const makeMethod = (methodName) => {
      return (...args) =>
        new Promise((resolve, reject) => {
          worker.onmessage = (message) => resolve(message.data);
          worker.onerror = reject;
          worker.postMessage([methodName, ...args]);
        });
    };

    worker.onmessage = (message) => {
      if (message.data === "initialized") {
        const exportedMethods = {};
        exportedMethodNames.forEach(
          (name) => (exportedMethods[name] = makeMethod(name))
        );
        resolve(exportedMethods);
      }
    };
    worker.onerror = reject;
  });
};
