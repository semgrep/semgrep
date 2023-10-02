const { ParserFactory } = require("../dist/index.cjs");

const parserPromise = ParserFactory();

const LANG = "cairo";
const EXPECTED_LANGS = [LANG];

test("getLangs", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual(EXPECTED_LANGS);
});

// TODO
// test("it parses a pattern", async () => {
//   const parser = await parserPromise;
//   const pattern = parser.parsePattern(false, LANG, "sum by (foo) $X");
//   expect(typeof pattern).toEqual("object");
// });

test("it parses a file", async () => {
  const parser = await parserPromise;
  const target = parser.parseTarget(LANG, "tests/test.cairo");
  expect(typeof target).toEqual("object");
});
