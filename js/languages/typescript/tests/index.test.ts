import { expect, test } from "@jest/globals";

import { ParserFactory } from "../src/index";

const parserPromise = ParserFactory();

test("it has a lang", async () => {
  const parser = await parserPromise;
  expect(parser.getLangs()).toEqual([13, 33]);
});

test("it successfully parses a pattern", async () => {
  const parser = await parserPromise;
  parser.parsePattern(false, "console.log($X)");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  parser.parseTarget("tests/example.ts");
});
