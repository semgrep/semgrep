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
