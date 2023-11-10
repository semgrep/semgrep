const { createParser, testParser } = require("../../shared/parser");

const LANG = "swift";

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testParser(LANG, parserPromise);
});
