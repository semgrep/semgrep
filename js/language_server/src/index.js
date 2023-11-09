const SemgrepEngineWasm = require("../dist/language-server");

import { getDirname } from "cross-dirname";

const WASM_FILENAME = "language-server.wasm";

export const LSFactory = async (wasmUri) => {
  if (!wasmUri) {
    wasmUri = `${getDirname()}/${WASM_FILENAME}`;
  }
  const wasm = await SemgrepEngineWasm({
    locateFile: (uri) => (uri === WASM_FILENAME ? wasmUri : uri),
  });
  // libpcre regrettably must be global because semgrep eagerly compiles regexes
  globalThis.LibPcreModule = wasm;
  const {
    init,
    start,
  } = require("../../../_build/default/js/language_server/Main.bc");
  init(wasm);

  start();
};
