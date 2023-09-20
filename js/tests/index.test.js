const TestWasm = require("./dist/test");

const languages = [
  "bash",
  "c",
  "cpp",
  "csharp",
  "dockerfile",
  "elixir",
  "go",
  "html",
  "java",
  "json",
  "jsonnet",
  "kotlin",
  "lisp",
  "lua",
  "ocaml",
  "php",
  "python",
  "r",
  "ruby",
  "rust",
  "scala",
  "solidity",
  "swift",
  "terraform",
  "typescript",
];
const getParsers = async () => {
  const parsers = new Map();
  const addParser = async (parserName) => {
    const parser = require(`../languages/${parserName}/dist/index.cjs`);
    const parserInstance = await parser.ParserFactory();
    parsers.set(parserName, parserInstance);
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
        console.log(e);
      }
    };
    const parsePattern = (_, lang, pattern) => {
      try {
        return parsers.get(lang).parsePattern(true, lang, pattern);
      } catch (e) {
        console.log(e);
      }
    };
    setJustParseWithLang(parseLang);
    setParsePattern(parsePattern);

    run("Java");
  } catch (e) {
    console.log(e);
  }
};
run();
