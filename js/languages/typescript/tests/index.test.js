const { ParserFactory } = require("../dist/index.cjs");

const parserPromise = ParserFactory();

const EXPECTED_LANGS = ["js", "ts"];

test("getLangs", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual(EXPECTED_LANGS);
});

test("it successfully parses a pattern", async () => {
  const parser = await parserPromise;
  const pattern = parser.parsePattern(false, "js", "console.log($X)");
  expect(typeof pattern).toEqual("object");
});

test("it parses a js file", async () => {
  const parser = await parserPromise;
  const target = parser.parseTarget("js", "tests/example.ts");
  expect(typeof target).toEqual("object");
});

test("it parses a ts file", async () => {
  const parser = await parserPromise;
  const target = parser.parseTarget("ts", "tests/example.ts");
  expect(typeof target).toEqual("object");
});

test("it parses a tsx file", async () => {
  const parser = await parserPromise;
  const target = parser.parseTarget("ts", "tests/example.tsx");
  expect(typeof target).toEqual("object");
});
