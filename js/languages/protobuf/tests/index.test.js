const {
  createParser,
  testGetLangs,
  testParseTarget,
} = require("../../shared/parser");

const LANG = "protobuf";

describe(`${LANG} parser`, () => {
  const parserPromise = createParser(`${__dirname}/../dist/index.cjs`);

  testGetLangs([LANG], parserPromise);

  // TODO: we need to finish the protobuf parser
  // see https://github.com/returntocorp/semgrep/issues/8088

  testParseTarget(LANG, parserPromise);
});
