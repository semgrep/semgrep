const { EngineFactory } = require("../dist/index");

const enginePromise = EngineFactory("./dist/semgrep-engine.wasm");

describe("lookupParserName", () => {
  test("handles base case", async () => {
    const engine = await enginePromise;
    expect(engine.lookupParserName("java")).toEqual("java");
  });
  test("handles invalid languages", async () => {
    const engine = await enginePromise;
    expect(engine.lookupParserName("fake-language")).toBeNull();
  });
  test("handles overridden languages", async () => {
    const engine = await enginePromise;
    expect(engine.lookupParserName("python")).toEqual("python");
    expect(engine.lookupParserName("python2")).toEqual("python");
    expect(engine.lookupParserName("python3")).toEqual("python");
    expect(engine.lookupParserName("py")).toEqual("python");
  });
});
