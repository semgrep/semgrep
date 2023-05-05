const SemgrepEngineWasm = require("../dist/semgrep-engine");

import { getDirname } from "cross-dirname";

const WASM_FILENAME = "semgrep-engine.wasm";

export const EngineFactory = async (wasmUri) => {
  if (!wasmUri) {
    wasmUri = `${getDirname()}/${WASM_FILENAME}`;
  }
  const wasm = await SemgrepEngineWasm({
    locateFile: (uri) => (uri === WASM_FILENAME ? wasmUri : uri),
  });
  // libpcre regrettably must be global because semgrep eagerly compiles regexes
  globalThis.LibPcreModule = wasm;

  const {
    getMountpoints,
    setLibYamlWasmModule,
    setParsePattern,
    setJustParseWithLang,
    execute,
    getParserForLang,
    writeFile,
    deleteFile,
  } = require("../../../_build/default/js/engine/Main.bc");

  setLibYamlWasmModule(wasm);

  const languages = new Map();

  const parsePattern = (printErrors, lang, pattern) => {
    const parser = languages.get(lang);
    if (!parser) {
      throw new Error("No parser initialized for " + lang);
    }
    return parser.parsePattern(printErrors, lang, pattern);
  };

  const parseFile = (lang, str) => {
    const parser = languages.get(lang);
    if (!parser) {
      throw new Error("No parser initialized for " + lang);
    }
    return parser.parseTarget(lang, str);
  };

  setParsePattern(parsePattern);
  setJustParseWithLang(parseFile);

  return {
    getParserForLang,
    addParser: (parser) => {
      parser.setMountpoints(getMountpoints());
      parser.getLangs().forEach((lang) => {
        languages.set(lang, parser);
      });
    },
    hasParser: (lang) => languages.has(lang),
    execute,
    writeFile,
    deleteFile,
  };
};
