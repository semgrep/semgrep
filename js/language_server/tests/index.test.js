const { LSFactory } = require("../dist/index.cjs");

describe("language server", () => {
  test("starts and stops", async () => {
    const ls = await LSFactory();
    ls.start();
  });
});
