const {
  createParser,
} = require("../../../../_build/default/js/languages/json/Parser.bc");

export type Mountpoint = object;
export type Lang = number;

export interface Parser {
  getLang: () => Lang;
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (filename: string) => any;
  parsePattern: (printErrors: boolean, pattern: string) => any;
}

export const ParserFactory: () => Promise<Parser> = async () => {
  return createParser(null);
};
