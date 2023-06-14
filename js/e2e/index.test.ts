import { expect, test } from "@jest/globals";

const { EngineFactory } = require("../engine/dist/index");
const { ParserFactory } = require("../languages/go/dist/index.cjs");

test("it can parse a golang file", async () => {
  const engine = await EngineFactory();
  const parser = await ParserFactory();

  engine.addParser(parser);

  const result = JSON.parse(
    engine.execute("go", "rules.json", "../languages/go/tests/test.go")
  );

  expect(result.matches.length).toBe(1);
  expect(result.matches[0].extra.metavars["$X"].abstract_content).toBe("hello");
});
