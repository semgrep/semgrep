import WasmFactory from "../dist/semgrep-parser";
const {
  createParser,
} = require("../../../../_build/default/js/languages/python/Parser.bc");

export type Mountpoint = object;
export type Lang = number;

export interface Parser {
  getLang: () => Lang;
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (filename: string) => any;
  parsePattern: (printErrors: boolean, pattern: string) => any;
}

export const ParserFactory: () => Promise<Parser> = async () => {
  const wasm = await WasmFactory();
  return createParser(wasm);
};
