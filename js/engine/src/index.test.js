const { EngineFactory } = require("../dist/index");

const enginePromise = EngineFactory("./dist/semgrep-engine.wasm");

describe("lookupParserName", () => {
  test("handles base case", async () => {
    const engine = await enginePromise;
    expect(engine.lookupLang("java")).toEqual("java");
  });
  test("handles invalid languages", async () => {
    const engine = await enginePromise;
    expect(engine.lookupLang("fake-language")).toBeNull();
  });
});
