import ParserModuleFactory from "./semgrep-parser";

export const ParserFactory = async (wasmPath) => {
  globalThis.ParserModule = await ParserModuleFactory({
    locateFile: (file) => {
      if (wasmPath && file === "semgrep-parser.wasm") {
        return wasmPath;
      }
      return file;
    },
  });
  return require("./Parser.bc");
};
