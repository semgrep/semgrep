const { createParser, testParser } = require("../../shared/parser");

const LANG = "lisp";

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testParser(LANG, parserPromise, ["scheme", "clojure"], "clojure");
});
