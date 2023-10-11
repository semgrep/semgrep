const {
  createParser,
  testGetLangs,
  testParseTarget,
} = require("../../shared/parser");

const LANG = "protobuf";

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testGetLangs([LANG], parserPromise);

  // TODO: figure out why no protobuf patterns work

  testParseTarget(LANG, parserPromise);
});
