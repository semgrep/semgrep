const LibPcreModule = require("../../../libpcre/dist/libpcre");

// we have to jump through these hoops because semgrep eagerly uses pcre
const parserPromise = (async () => {
  globalThis.LibPcreModule = await LibPcreModule();
  const { ParserFactory } = require("../dist/index.cjs");
  return ParserFactory();
})();

test("it has a lang value", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual([7]);
});

test("it parses a pattern", async () => {
  const parser = await parserPromise;
  parser.parsePattern(false, "COPY $X $Y");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  parser.parseTarget("tests/Dockerfile");
});
