const { createParser, testParser } = require("../../shared/parser");

const LANG = "bash";
const EXPECTED_LANGS = [LANG];

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testParser(LANG, parserPromise);
});
