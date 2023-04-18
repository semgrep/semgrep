const {
  createParser,
} = require("../../../../_build/default/js/languages/scala/Parser.bc");

export const ParserFactory = async () => {
  return createParser(null);
};
