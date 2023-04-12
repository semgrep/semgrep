import { EngineFactory } from "./entrypoint";

test("it loads the engine", async () => {
  const engine = await EngineFactory("./dist/libyaml.wasm");

  expect(engine.lookupLang("python")).toBe(24);
});
