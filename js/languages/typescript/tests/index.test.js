const {
  createParser,
  testGetLangs,
  testParseTarget,
  testParsePattern,
} = require("../../shared/parser");

const JS_LANG = "js";
const TS_LANG = "ts";

describe(`js/ts parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testGetLangs([JS_LANG, TS_LANG], parserPromise);

  describe(`${JS_LANG} parser`, () => {
    testParseTarget(JS_LANG, parserPromise);
    testParsePattern(JS_LANG, parserPromise);
  });

  describe(`${TS_LANG} parser`, () => {
    testParseTarget(TS_LANG, parserPromise);
    testParsePattern(TS_LANG, parserPromise);
  });
});
