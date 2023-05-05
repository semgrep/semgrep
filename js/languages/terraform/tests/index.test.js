const { ParserFactory } = require("../dist/index.cjs");

const parserPromise = ParserFactory();

const LANG = "terraform";
const EXPECTED_LANGS = [LANG];

test("getLangKeys", async () => {
  const parser = await parserPromise;
  expect(parser.getLangKeys()).toEqual(EXPECTED_LANGS);
});

test("it parses a pattern", async () => {
  const parser = await parserPromise;
  const pattern = parser.parsePattern(false, LANG, "value = $X");
  expect(typeof pattern).toEqual("object");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  const target = parser.parseTarget(LANG, "tests/test.tf");
  expect(typeof target).toEqual("object");
});
