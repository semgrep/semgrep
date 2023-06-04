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
  test("parses a pattern", async () => {
    const engine = await enginePromise;
    const result = JSON.parse(
      engine.execute(
        "yaml",
        `${__dirname}/test-rule-yaml.json`,
        `${__dirname}/test.yaml`
      )
    );

    // it took a lot of work to get libyaml working, so let's confirm start/end locations are correct
    expect(result.stats.okfiles).toBe(1);
    expect(result.matches.length).toBe(1);
    const match = result.matches[0];
    expect(match.location.start).toEqual(
      expect.objectContaining({ offset: 0, line: 1, col: 1 })
    );
    expect(match.location.end).toEqual(
      expect.objectContaining({ offset: 8, line: 1, col: 9 })
    );
    expect(match.extra.metavars["$X"].start).toEqual(
      expect.objectContaining({ offset: 5, line: 1, col: 6 })
    );
    expect(match.extra.metavars["$X"].end).toEqual(
      expect.objectContaining({ offset: 8, line: 1, col: 9 })
    );
    expect(match.extra.metavars["$X"].abstract_content).toEqual("bar");
  });
});
