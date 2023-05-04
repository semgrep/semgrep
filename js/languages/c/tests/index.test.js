const { ParserFactory } = require("../dist/index.cjs");

const parserPromise = ParserFactory();

test("it has a lang value", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual([2]);
});

test("it parses a pattern", async () => {
  const parser = await parserPromise;
  parser.parsePattern(false, "printf(...)");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  parser.parseTarget("tests/test.c");
});
