const { ParserFactory } = require("../dist/index.cjs");

const parserPromise = ParserFactory();

const LANG = "swift";
const EXPECTED_LANGS = [LANG];

test("getLangs", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual(EXPECTED_LANGS);
});

test("it parses a pattern", async () => {
  const parser = await parserPromise;
  const pattern = parser.parsePattern(false, LANG, "print($X)");
  expect(typeof pattern).toEqual("object");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  const target = parser.parseTarget(LANG, "tests/test.swift");
  expect(typeof target).toEqual("object");
});
