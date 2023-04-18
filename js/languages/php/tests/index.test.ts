import { expect, test } from "@jest/globals";

import { ParserFactory } from "../src/index";

const parserPromise = ParserFactory();

test("it has a lang value", async () => {
  const parser = await parserPromise;
  expect(parser.getLang()).toBe(21);
});

test("it parses a pattern", async () => {
  const parser = await parserPromise;
  parser.parsePattern(true, "echo $X;");
});

test("it parses a file", async () => {
  const parser = await parserPromise;
  parser.parseTarget("tests/test.php");
});
