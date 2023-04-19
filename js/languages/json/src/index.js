const {
  createParser,
} = require("../../../../_build/default/js/languages/json/Parser.bc");

export const ParserFactory = async () => {
  return createParser(null);
};
