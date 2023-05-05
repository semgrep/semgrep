const { EngineFactory } = require("../dist/index");

const enginePromise = EngineFactory("./dist/semgrep-engine.wasm");

describe("lookupParserName", () => {
  test("handles base case", async () => {
    const engine = await enginePromise;
    expect(engine.getParserForLang("java")).toEqual("java");
  });
  test("handles invalid languages", async () => {
    const engine = await enginePromise;
    expect(engine.getParserForLang("fake-language")).toBeNull();
  });
  test("handles overridden languages", async () => {
    const engine = await enginePromise;
    expect(engine.getParserForLang("python")).toEqual("python");
    expect(engine.getParserForLang("python2")).toEqual("python");
    expect(engine.getParserForLang("python3")).toEqual("python");
    expect(engine.getParserForLang("py")).toEqual("python");
  });
});
