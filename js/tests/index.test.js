const SemgrepEngine = require("./dist/semgrep-engine");

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
  const addParser = async (parserName) => {
    const parser = require(`../languages/${parserName}/dist/index.cjs`);
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
const entrypoint = async () => {
  try {
    console.log("Running tests");
    const wasm = await SemgrepEngine({
      locateFile: (_) => "./dist/semgrep-engine.wasm",
    });
    globalThis.LibPcreModule = wasm;
    const {
      init,
      getMountpoints,
      setParsePattern,
      setJustParseWithLang,
      setJsonnetParser,
      run,
    } = require("../../_build/default/js/tests/test_jsoo.bc");
    init(wasm);
    const mountpoints = getMountpoints();
    const parsers = await getParsers();
    for (const parser of parsers.values()) {
      parser.setMountpoints(mountpoints);
      if (parser.getLangs().includes("jsonnet")) {
        setJsonnetParser((file) => parser.parseTargetTsOnly(file));
      }
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

    run("");
    process.exit(0);
  } catch (e) {
    console.log("Error running tests:\n");
    console.log(e);
    process.exit(1);
  } finally {
    console.log("Finished running tests");
  }
};

entrypoint();
