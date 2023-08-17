const SemgrepEngineWasm = require("../dist/semgrep-engine");

import { getDirname } from "cross-dirname";

const WASM_FILENAME = "semgrep-engine.wasm";

export class MissingParserError extends Error {
  constructor(lang) {
    super(`No parser initialized for language: ${lang}`);
    this.lang = lang;
  }
}

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
    lookupLang,
    writeFile,
    deleteFile,
  } = require("../../../_build/default/js/engine/Main.bc");
  setLibYamlWasmModule(wasm);

  const languages = new Map();

  const missingLanguages = new Set();

  const parsePattern = (printErrors, lang, pattern) => {
    const parser = languages.get(lang);
    if (!parser) {
      missingLanguages.add(lang);
      throw new MissingParserError(lang);
    }
    return parser.parsePattern(printErrors, lang, pattern);
  };

  const parseFile = (lang, str) => {
    const parser = languages.get(lang);
    if (!parser) {
      missingLanguages.add(lang);
      throw new MissingParserError(lang);
    }
    return parser.parseTarget(lang, str);
  };

  setParsePattern(parsePattern);
  setJustParseWithLang(parseFile);

  return {
    lookupLang,
    addParser: (parser) => {
      parser.setMountpoints(getMountpoints());
      parser.getLangs().forEach((lang) => {
        languages.set(lang, parser);
        missingLanguages.delete(lang);
      });
    },
    hasParser: (lang) => languages.has(lang),
    isMissingLanguages: () => missingLanguages.size > 0,
    getMissingLanguages: () => Array.from(missingLanguages),
    clearMissingLanguages: () => missingLanguages.clear(),
    execute,
    writeFile,
    deleteFile,
  };
};
