const TestWasm = require("./dist/test");

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
  "julia",
  "kotlin",
  "lisp",
  "lua",
  "ocaml",
  "php",
  "protobuf",
  "promql",
  "python",
  "r",
  "ruby",
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
const run = async () => {
  try {
    const wasm = await TestWasm({
      locateFile: (_) => "./dist/test.wasm",
    });
    globalThis.LibPcreModule = wasm;
    const {
      init,
      getMountpoints,
      setParsePattern,
      setJustParseWithLang,
      run,
    } = require("../../_build/default/js/tests/test_jsoo.bc");
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

    const testErrors = run("Dart");
    for (const e of testErrors) {
      console.log(e);
    }
  } catch (e) {
    console.log("Error running tests:\n");
    console.log(e);
  }
};
run();
