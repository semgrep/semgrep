const SEMGREP_PRO = process.env.SEMGREP_PRO;
const path = require("path");
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
      `${__dirname}`,
      [`${__dirname}/test.py`]
    );
    expect(engine.isMissingLanguages()).toBe(true);
    expect(engine.getMissingLanguages()).toEqual(["python"]);
    engine.clearMissingLanguages();
    expect(engine.isMissingLanguages()).toBe(false);
  });
});

describe("yaml parser", () => {
  test("parses a simple pattern", async () => {
    const engine = await enginePromise;
    const rulePath = path.resolve(`${__dirname}/test-rule-yaml.json`);
    const targetPath = path.resolve(`${__dirname}/test.yaml`);
    const result = JSON.parse(
      engine
        .execute("yaml", rulePath, `${__dirname}`, [targetPath])
        .replaceAll(rulePath, "test-rule-yaml.json")
        .replaceAll(targetPath, "test.yaml")
        .replaceAll("PRO", "OSS")
    );
    result["version"] = "<MASKED>";
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
    result["version"] = "<MASKED>";
    expect(result).toMatchSnapshot();
  });
});

if (SEMGREP_PRO) {
  describe("deep", () => {
    test("parses a pattern", async () => {
      const engine = await enginePromise;
      const java = require("../../languages/java/dist/index.cjs");
      engine.addParser(await java.ParserFactory());
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
      // Can't use snapshots here as if we skip them it fails in CI :(
      expect(result.stats.okfiles).toBe(3);
      expect(result.matches.length).toBe(3);
      expect(result.explanations.length).toBe(3);
      // We'll assume match content is ok, just make sure it's the right file
      for (const match of result.matches) {
        expect(match.location.path).toMatch(/C.java$/);
      }
    });
  });
}
