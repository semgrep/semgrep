import LibYamlFactory from "../../libyaml/dist/libyaml";

export type Lang = number;
export type Mountpoint = object;

export interface Parser {
  getLang: () => Lang;
  parsePattern: (printErrors: boolean, pattern: string) => any;
  parseTarget: (str: string) => any;
  setMountPoints: (mountpoints: Mountpoint[]) => void;
}

export interface Engine {
  lookupLang: (name: string) => Lang;
  addParser: (parser: Parser) => void;
  hasParser: (lang: Lang) => boolean;
  execute: (
    language: string,
    rulesFilename: string,
    targetFilename: string
  ) => string;
}

export const EngineFactory: (
  libYamlWasmPath?: string
) => Promise<Engine> = async (libYamlWasmPath?) => {
  const libyaml = await LibYamlFactory({
    locateFile: (a: string) => {
      return libYamlWasmPath || a;
    },
  });
  const {
    getMountPoints,
    setLibYamlWasmModule,
    setParsePattern,
    setJustParseWithLang,
    execute,
    lookupLang,
  } = require("../../../_build/default/js/engine/Main.bc");

  setLibYamlWasmModule(libyaml);

  const languages = new Map<Lang, Parser>();

  const parsePattern = (printErrors: boolean, lang: Lang, pattern: string) => {
    const parser = languages.get(lang);
    if (!parser) {
      throw new Error("No parser initialized for " + lang);
    }
    return parser.parsePattern(printErrors, pattern);
  };

  const parseFile = (lang: Lang, str: string) => {
    const parser = languages.get(lang);
    if (!parser) {
      throw new Error("No parser initialized for " + lang);
    }
    return parser.parseTarget(str);
  };

  setParsePattern(parsePattern);
  setJustParseWithLang(parseFile);

  return {
    lookupLang,
    addParser: (parser: Parser) => {
      parser.setMountPoints(getMountPoints()); // inherit engine's mount points
      languages.set(parser.getLang(), parser);
    },
    hasParser: (lang: Lang) => {
      return languages.has(lang);
    },
    execute,
  };
};
