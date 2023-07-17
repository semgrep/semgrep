const path = require("path");

const { EngineFactory } = require("../dist/index.cjs");

const enginePromise = EngineFactory("./dist/semgrep-engine.wasm");

describe("engine", () => {
  test("handles valid language", async () => {
    const engine = await enginePromise;
    expect(engine.lookupLang("java")).toEqual("java");
  });
  test("handles language aliases", async () => {
    const engine = await enginePromise;
    expect(engine.lookupLang("py")).toEqual("python");
  });
  test("handles invalid languages", async () => {
    const engine = await enginePromise;
    expect(engine.lookupLang("fake-language")).toBeNull();
  });
  test("detects an uninitialized language", async () => {
    const engine = await enginePromise;
    engine.execute(
      "python",
      `${__dirname}/test-rule-python.json`,
      `${__dirname}/../../languages/python/tests/test.py`
    );
    expect(engine.isMissingLanguages()).toBe(true);
    expect(engine.getMissingLanguages()).toEqual(["python"]);

    engine.clearMissingLanguages();
    expect(engine.isMissingLanguages()).toBe(false);
  });
});

describe("yaml parser", () => {
  test("parses a simple pattern", async () => {
    const engine = await enginePromise;
    const rulePath = path.resolve(`${__dirname}/test-rule-yaml.json`);
    const targetPath = path.resolve(`${__dirname}/test.yaml`);

    const result = JSON.parse(
      engine
        .execute("yaml", rulePath, targetPath)
        .replaceAll(rulePath, "test-rule-yaml.json")
        .replaceAll(targetPath, "test.yaml")
    );

    expect(result).toMatchSnapshot();
  });
  test("parses a pattern with pattern-regex", async () => {
    const engine = await enginePromise;
    const rulePath = path.resolve(`${__dirname}/test-rule-yaml-regex.json`);
    const targetPath = path.resolve(`${__dirname}/test.yaml`);

    const result = JSON.parse(
      engine
        .execute("yaml", rulePath, targetPath)
        .replaceAll(rulePath, "test-rule-yaml.json")
        .replaceAll(targetPath, "test.yaml")
    );

    expect(result).toMatchSnapshot();
  });
});
