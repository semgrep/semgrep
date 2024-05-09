export type Mountpoint = object;

//coupling: see also js/languages/shared/index.d.ts
export interface Parser {
  getLangs: () => string[];
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (lang: string, filename: string) => any;
  parsePattern: (lang: string, pattern: string) => any;
}

//coupling: this interface is implemented in index.js and ../Main.ml
export interface Engine {
  lookupLang: (name: string) => string | null;
  addParser: (parser: Parser) => void;
  hasParser: (lang: string) => boolean;
  execute: (
    language: string,
    rulesFilename: string,
    root: string,
    targetFilenames: string[]
  ) => string;
  writeFile: (filename: string, content: string) => void;
  deleteFile: (filename: string) => void;
  isMissingLanguages: () => boolean;
  getMissingLanguages: () => string[];
  clearMissingLanguages: () => void;
}
export declare const EngineFactory: () => Promise<Engine>;
