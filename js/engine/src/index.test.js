const { EngineFactory } = require("../dist/index");

test("it loads the engine", async () => {
  const engine = await EngineFactory("./dist/semgrep-engine.wasm");
  expect(engine.lookupLang("python")).toBe(24);
});
