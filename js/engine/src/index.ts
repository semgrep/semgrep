import LibYamlFactory from "../../libyaml/dist/libyaml";

export type Mountpoint = object;
export type Lang = number;

export interface Parser {
  getLang: () => Lang;
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (filename: string) => any;
  parsePattern: (printErrors: boolean, pattern: string) => any;
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
  writeFile: (filename: string, content: string) => void;
  deleteFile: (filename: string) => void;
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
    getMountpoints,
    setLibYamlWasmModule,
    setParsePattern,
    setJustParseWithLang,
    execute,
    lookupLang,
    writeFile,
    deleteFile,
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
      parser.setMountpoints(getMountpoints());
      languages.set(parser.getLang(), parser);
    },
    hasParser: (lang: Lang) => languages.has(lang),
    execute,
    writeFile,
    deleteFile,
  };
};
