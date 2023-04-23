const { ParserFactory } = require("../dist/index.cjs");

const parserPromise = ParserFactory();

test("it has a lang", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual([13, 33]);
});

test("it successfully parses a pattern", async () => {
  const parser = await parserPromise;
  parser.parsePattern(false, "console.log($X)");
});

test("it parses a js file", async () => {
  const parser = await parserPromise;
  parser.parseTarget(13, "tests/example.ts");
});

test("it parses a ts file", async () => {
  const parser = await parserPromise;
  parser.parseTarget(33, "tests/example.ts");
});

test("it parses a tsx file", async () => {
  const parser = await parserPromise;
  parser.parseTarget(33, "tests/example.tsx");
});
