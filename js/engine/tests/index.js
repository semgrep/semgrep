const TestEngineWasm = require("../dist/semgrep-engine-test");

import { getDirname } from "cross-dirname";

const WASM_FILENAME = "semgrep-engine.wasm";

export const EngineTestFactory = async (wasmUri) => {
  if (!wasmUri) {
    wasmUri = `${getDirname()}/${WASM_FILENAME}`;
  }
  const wasm = await SemgrepEngineWasm({
    locateFile: (uri) => (uri === WASM_FILENAME ? wasmUri : uri),
  });
