const { createParser, testParser } = require("../../shared/parser");

const LANG = "python";

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testParser(LANG, parserPromise, ["python2", "python3"]);
});
