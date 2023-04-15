import ParserModuleFactory from "../dist/semgrep-parser";

const PARSER_PATH = "../../../../_build/default/js/languages/csharp/Parser.bc";
const DEFAULT_WASM_FILENAME = "semgrep-parser.wasm";

export type Mountpoint = object;
export type Lang = number;

export interface Parser {
  setMountPoints: (mountpoints: Mountpoint[]) => void;
  getLang: () => Lang;
  parseTarget: (filename: string) => any;
  parsePattern: (printErrors: boolean, pattern: string) => any;
  setParserWasmModule: (module: object) => void;
}

export const ParserFactory: (wasmPath?: string) => Promise<Parser> = async (
  wasmPath?: string
) => {
  const wasm = await ParserModuleFactory({
    locateFile: (file: string) => {
      if (wasmPath && file === DEFAULT_WASM_FILENAME) {
        return wasmPath;
      }
      return file;
    },
  });
  const parser = require(PARSER_PATH);
  parser.setParserWasmModule(wasm);
  return parser;
};
