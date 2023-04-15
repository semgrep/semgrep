import { expect, test } from "@jest/globals";

import { ParserFactory } from "./index";

const parserPromise = ParserFactory();

test("it has a lang", async () => {
  const parser = await parserPromise;
  expect(parser.getLang()).toBe(19);
});

test("it successfully parses a pattern", async () => {
  const parser = await parserPromise;
  parser.parsePattern(false, "hello");
});
