const { createParser, testParser } = require("../../shared/parser");

const LANG = "move_on_sui";

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testParser(LANG, parserPromise);
});
