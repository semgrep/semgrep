const path = require("path");
const fs = require("fs");

const SemgrepEngine = require("../../engine/dist/semgrep-engine");

exports.createParser = async (parserFilename) => {
  const wasm = await SemgrepEngine({
    locateFile: (_) => `${__dirname}/../../engine/dist/semgrep-engine.wasm`,
  });
  globalThis.LibPcreModule = wasm;

  const { ParserFactory } = require(parserFilename);

  return ParserFactory();
};

exports.testGetLangs = (expectedLangs, parserPromise) => {
  describe("getLangs()", () => {
    test("returns expected language(s)", async () => {
      const parser = await parserPromise;
      expect(parser.getLangs().sort()).toEqual(expectedLangs.sort());
    });
  });
};

const getParsingFiles = (lang) => {
  const PARSING_BASE_PATH = `${__dirname}/../../tests/tests/parsing/${lang}/`;
  return fs
    .readdirSync(PARSING_BASE_PATH)
    .map((filename) => path.join(PARSING_BASE_PATH, filename))
    .filter((filename) => fs.lstatSync(filename).isFile());
};

exports.testParseTarget = (lang, parserPromise) => {
  getParsingFiles(lang).forEach((filename) => {
    test(filename, async () => {
      const parser = await parserPromise;
      const target = parser.parseTarget(lang, filename);
      expect(typeof target).toEqual("object");
    });
  });
};

const getPatternsFiles = (lang) => {
  const PATTERNS_BASE_PATH = `${__dirname}/../../tests/tests/patterns/${lang}/`;
  return fs
    .readdirSync(PATTERNS_BASE_PATH)
    .filter((filename) => filename.endsWith(".sgrep"))
    .map((filename) => path.join(PATTERNS_BASE_PATH, filename));
};

exports.testParseTarget = (lang, parserPromise) => {
  describe("parseTarget()", () => {
    getParsingFiles(lang).forEach((filename) => {
      test(filename, async () => {
        const parser = await parserPromise;
        const target = parser.parseTarget(lang, filename);
        expect(typeof target).toEqual("object");
      });
    });
  });
};

exports.testParsePattern = (lang, parserPromise) => {
  describe("parsePattern()", () => {
    getPatternsFiles(lang).forEach((filename) => {
      test(filename, async () => {
        const parser = await parserPromise;
        const patternContent = fs.readFileSync(filename, "utf8");
        const pattern = parser.parsePattern(true, lang, patternContent);
        expect(typeof pattern).toEqual("object");
      });
    });
  });
};

exports.testParser = (
  lang,
  parserPromise,
  additionalExpectedLangs = [],
  testLangOverride = null
) => {
  const expectedLangs = [lang].concat(additionalExpectedLangs);
  const langForTests = testLangOverride || lang;

  exports.testGetLangs(expectedLangs, parserPromise);
  exports.testParseTarget(langForTests, parserPromise);
  exports.testParsePattern(langForTests, parserPromise);
};
