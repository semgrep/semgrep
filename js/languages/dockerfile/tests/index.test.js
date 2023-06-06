const LibPcreModule = require("../../../libpcre/dist/libpcre");

// we have to jump through these hoops because semgrep eagerly uses pcre
const parserPromise = (async () => {
  globalThis.LibPcreModule = await LibPcreModule();
  const { ParserFactory } = require("../dist/index.cjs");
  return ParserFactory();
})();

const LANG = "dockerfile";
const EXPECTED_LANGS = [LANG];

test("getLangs", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual(EXPECTED_LANGS);
});

test("it parses a pattern", async () => {
  const parser = await parserPromise;
  const pattern = parser.parsePattern(false, LANG, "COPY $X $Y");
  expect(typeof pattern).toEqual("object");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  parser.parseTarget(LANG, "tests/Dockerfile");
});
