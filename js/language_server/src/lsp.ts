const YamlPCREWasm = require("../../dist/semgrep-engine.js");

const languages = [
  "bash",
  "c",
  "cairo",
  "csharp",
  "dart",
  "dockerfile",
  "elixir",
  "go",
  "hack",
  "html",
  "java",
  "json",
  "jsonnet",
  "kotlin",
  "lisp",
  "lua",
  "ocaml",
  "php",
  "protobuf",
  "promql",
  "python",
  "r",
  "rust",
  "scala",
  "solidity",
  "swift",
  "terraform",
  "typescript",
  "vue",
];
const getParsers = async () => {
  const parsers = new Map();
  const addParser = async (parserName: string) => {
    const parser = require(`@semgrep/lang-${parserName}`);
    const parserInstance = await parser.ParserFactory();
    parserInstance
      .getLangs()
      .forEach((lang: string) => parsers.set(lang, parserInstance));
  };
  for (const parserName of languages) {
    await addParser(parserName);
  }
  return parsers;
};

export interface LS {
  handleClientMessage: (packet: any) => Promise<any | undefined>;
  setWriteRef: (func: (packet: any) => void) => void;
}

declare global {
  var LibPcreModule: any;
}

export const LSFactory = async () => {
  const loadedYamlPCRE = await YamlPCREWasm();
  // libpcre regrettably must be global because semgrep eagerly compiles regexes
  globalThis.LibPcreModule = loadedYamlPCRE;
  const {
    init,
    handleClientMessage,
    setWriteRef,
    getMountpoints,
    setJustParseWithLang,
    setParsePattern,
  } = require("../../dist/Main.bc");
  init(loadedYamlPCRE);
  const mountpoints = getMountpoints();
  const parsers = await getParsers();
  for (const parser of parsers.values()) {
    parser.setMountpoints(mountpoints);
  }
  const parseLang = (lang: string, str: string) => {
    try {
      return parsers.get(lang).parseTarget(lang, str);
    } catch (e) {
      console.log(`Error parsing target ${e}`);
      throw e;
    }
  };
  const parsePattern = (_: any, lang: string, pattern: string) => {
    try {
      return parsers.get(lang).parsePattern(true, lang, pattern);
    } catch (e) {
      console.log(`Error parsing pattern ${e}`);
      throw e;
    }
  };
  setJustParseWithLang(parseLang);
  setParsePattern(parsePattern);
  const clientMessageHandler = async (packet: any) => {
    const json = JSON.stringify(packet);
    const result = await handleClientMessage(json);
    if (result) {
      // Parse LSP Response and get result
      return JSON.parse(result)["result"];
    }
    return undefined;
  };

  return {
    handleClientMessage: clientMessageHandler,
    setWriteRef: setWriteRef,
  };
};
