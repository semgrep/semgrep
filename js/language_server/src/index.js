const SemgrepEngineWasm = require("../dist/semgrep-engine");

import { getDirname } from "cross-dirname";

const WASM_FILENAME = "semgrep-engine.wasm";
const languages = ["python"];
const getParsers = async () => {
  const parsers = new Map();
  const addParser = async (parserName) => {
    const parser = require(`../../languages/${parserName}/dist/index.cjs`);
    const parserInstance = await parser.ParserFactory();
    parserInstance
      .getLangs()
      .forEach((lang) => parsers.set(lang, parserInstance));
  };
  for (const parserName of languages) {
    await addParser(parserName);
  }
  return parsers;
};

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
    getMountpoints,
    setJustParseWithLang,
    setParsePattern,
  } = require("../../../_build/default/js/language_server/Main.bc");
  init(wasm);
  const mountpoints = getMountpoints();
  const parsers = await getParsers();
  for (const parser of parsers.values()) {
    parser.setMountpoints(mountpoints);
  }
  const parseLang = (lang, str) => {
    try {
      return parsers.get(lang).parseTarget(lang, str);
    } catch (e) {
      console.log(`Error parsing target ${e}`);
      throw e;
    }
  };
  const parsePattern = (_, lang, pattern) => {
    try {
      return parsers.get(lang).parsePattern(true, lang, pattern);
    } catch (e) {
      console.log(`Error parsing pattern ${e}`);
      throw e;
    }
  };
  setJustParseWithLang(parseLang);
  setParsePattern(parsePattern);

  start();
};
