import LibYamlFactory from "../../libyaml/dist/libyaml";

export type Mountpoint = object;
export type Lang = number;

export interface Parser {
  getLangs: () => Lang[];
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (lang: Lang, filename: string) => any;
  parsePattern: (printErrors: boolean, lang: Lang, pattern: string) => any;
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

export const EngineFactory: () => Promise<Engine> = async () => {
  const libyaml = await LibYamlFactory();
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
    return parser.parsePattern(printErrors, lang, pattern);
  };

  const parseFile = (lang: Lang, str: string) => {
    const parser = languages.get(lang);
    if (!parser) {
      throw new Error("No parser initialized for " + lang);
    }
    return parser.parseTarget(lang, str);
  };

  setParsePattern(parsePattern);
  setJustParseWithLang(parseFile);

  return {
    lookupLang,
    addParser: (parser: Parser) => {
      parser.setMountpoints(getMountpoints());
      parser.getLangs().forEach((lang) => {
        languages.set(lang, parser);
      });
    },
    hasParser: (lang: Lang) => languages.has(lang),
    execute,
    writeFile,
    deleteFile,
  };
};
