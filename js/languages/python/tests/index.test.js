const { ParserFactory } = require("../dist/index.cjs");

const parserPromise = ParserFactory();

const LANG = "python";
const EXPECTED_LANGS = [LANG, "python2", "python3"];

test("getLangKeys", async () => {
  const parser = await parserPromise;
  expect(parser.getLangKeys()).toEqual(EXPECTED_LANGS);
});

test("it parses a pattern", async () => {
  const parser = await parserPromise;
  const pattern = parser.parsePattern(false, LANG, "print($X)");
  expect(typeof pattern).toEqual("object");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  const target = parser.parseTarget(LANG, "tests/test.py");
  expect(typeof target).toEqual("object");
});
