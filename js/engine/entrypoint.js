import LibYamlFactory from "../libyaml/dist/libyaml";

export const EngineFactory = async (libYamlWasmPath) => {
  globalThis.LibYamlModule = await LibYamlFactory({
    locateFile: (a) => {
      return libYamlWasmPath || a;
    },
  });
  const {
    getMountPoints,
    setParsePattern,
    setJustParseWithLang,
    execute,
    lookupLang,
  } = require("../../_build/default/js/engine/Main.bc");

  const languages = {};

  const parsePattern = (printErrors, lang, pattern) => {
    const parser = languages[lang];
    if (!parser) {
      throw new Error("No parser initialized for " + lang);
    }
    return parser.parsePattern(printErrors, pattern);
  };

  const parseFile = (lang, str) => {
    const parser = languages[lang];
    if (!parser) {
      throw new Error("No parser initialized for " + lang);
    }
    return parser.parseTarget(str);
  };

  setParsePattern(parsePattern);
  setJustParseWithLang(parseFile);

  return {
    lookupLang,
    addParser: (parser) => {
      parser.setMountPoints(getMountPoints()); // inherit engine's mount points
      const internalLang = parser.getLang();
      languages[internalLang] = parser;
    },
    hasParser: (lang) => {
      return !!languages[lang];
    },
    execute,
  };
};
