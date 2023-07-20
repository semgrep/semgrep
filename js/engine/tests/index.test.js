const SEMGREP_PRO = process.env.SEMGREP_PRO;
const path = require("path");
const { EngineFactory } = require("../dist/index.cjs");

if (SEMGREP_PRO) {
  const java = require("../../languages/java/dist/index.cjs");
  const js = require("../../languages/typescript/dist/index.cjs");
}

const enginePromise = EngineFactory("./dist/semgrep-engine.wasm");

const engineExec = async () => {
  const engine = await enginePromise;
  if (SEMGREP_PRO) {
    const a = await java.ParserFactory();
    engine.addParser(a);
    const b = await js.ParserFactory();
    engine.addParser(b);
  }
  return engine;
};

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
    const engine = await engineExec();
    engine.execute(
      "python",
      `${__dirname}/test-rule-python.json`,
      `${__dirname}`,
      [`dirname}/../../languages/python/tests/test.py`]
    );
    expect(engine.isMissingLanguages()).toBe(true);
    expect(engine.getMissingLanguages()).toEqual(["python"]);
    engine.clearMissingLanguages();
    expect(engine.isMissingLanguages()).toBe(false);
  });
});

describe("yaml parser", () => {
  test("parses a simple pattern", async () => {
    const engine = await engineExec();
    const rulePath = path.resolve(`${__dirname}/test-rule-yaml.json`);
    const targetPath = path.resolve(`${__dirname}/test.yaml`);
    const result = JSON.parse(
      engine
        .execute("yaml", rulePath, `${__dirname}`, [targetPath])
        .replaceAll(rulePath, "test-rule-yaml.json")
        .replaceAll(targetPath, "test.yaml")
        .replaceAll("PRO", "OSS")
    );
    expect(result).toMatchSnapshot();
  });
  test("parses a pattern with pattern-regex", async () => {
    const engine = await enginePromise;
    const rulePath = path.resolve(`${__dirname}/test-rule-yaml-regex.json`);
    const targetPath = path.resolve(`${__dirname}/test.yaml`);

    const result = JSON.parse(
      engine
        .execute("yaml", rulePath, `${__dirname}`, [targetPath])
        .replaceAll(rulePath, "test-rule-yaml.json")
        .replaceAll(targetPath, "test.yaml")
        .replaceAll("PRO", "OSS")
    );

    expect(result).toMatchSnapshot();
  });
});

if (SEMGREP_PRO) {
  describe("deep", () => {
    test("parses a pattern", async () => {
      const engine = await engineExec();
      let paths = [
        path.resolve(`${__dirname}/A.java`),
        path.resolve(`${__dirname}/B.java`),
        path.resolve(`${__dirname}/C.java`),
      ];
      const result = JSON.parse(
        engine
          .execute(
            "java",
            `${__dirname}/test-rule-java.json`,
            `${__dirname}`,
            paths
          )
          .replaceAll(path[0], "A.java")
          .replaceAll(path[1], "B.java")
          .replaceAll(path[2], "C.java")
      );
      expect(result).toMatchSnapshot();
    });
  });
}
