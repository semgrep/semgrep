import { expect, test } from "@jest/globals";

import { EngineFactory } from "./index";

test("it loads the engine", async () => {
  const engine = await EngineFactory();

  expect(engine.lookupLang("python")).toBe(24);
});
