import ParserModuleFactory from "./semgrep-parser";

export const ParserFactory = async () => {
  globalThis.ParserModule = await ParserModuleFactory();
  return require("./Parser.bc");
};

export const ParserWorkerFactory = (workerURL) => {
  return new Promise((resolve, reject) => {
    const worker = new Worker(workerURL);

    const makeMethod = (methodName) => {
      return (args) =>
        new Promise((resolve, reject) => {
          worker.onmessage = (message) => resolve(message.data);
          worker.onerror = reject;
          worker.postMessage([methodName, ...args]);
        });
    };

    worker.onmessage = (message) => {
      if (message.data === "initialized") {
        resolve({
          parseFile: makeMethod("parseFile"),
          parsePattern: makeMethod("parsePattern"),
          writeFile: makeMethod("writeFile"),
          readFile: makeMethod("readFile"),
        });
      }
    };
    worker.onerror = reject;
  });
};
