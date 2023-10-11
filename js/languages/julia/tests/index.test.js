const { createParser, testParser } = require("../../shared/parser");

const LANG = "julia";

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testParser(LANG, parserPromise);
});
