const SemgrepEngineWasm = require("../dist/semgrep-engine");

export class MissingParserError extends Error {
  constructor(lang) {
    super(`No parser initialized for language: ${lang}`);
    this.lang = lang;
  }
}

export const EngineFactory = async () => {
  const wasm = await SemgrepEngineWasm();
  // libpcre and libpcre2 regrettably must be global because semgrep eagerly
  // compiles regexes
  globalThis.LibPcreModule = wasm;
  globalThis.LibPcre2Module = wasm;
  const {
    init,
    getMountpoints,
    setParsePattern,
    setJustParseWithLang,
    setJsonnetParser,
    execute,
    lookupLang,
    writeFile,
    deleteFile,
  } = require("../../../_build/default/js/engine/Main.bc");
  init(wasm);

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
      // The Semgrep core engine eagerly uses the Jsonnet parser
      // because we support it as a format to write rules in.
      // Unlike other languages, there is no pure-OCaml (e.g. Pfff)
      // option that we can use instead, so we need to explicitly
      // pass a reference of the Jsonnet parser to the engine.
      if (parser.getLangs().includes("jsonnet")) {
        setJsonnetParser((file) => parser.parseTargetTsOnly(file));
      }
    },
    hasParser: (lang) => languages.has(lang),
    isMissingLanguages: () => missingLanguages.size > 0,
    getMissingLanguages: () => Array.from(missingLanguages),
    clearMissingLanguages: () => missingLanguages.clear(),
    execute,
    parsePattern,
    writeFile,
    deleteFile,
  };
};
