export type Mountpoint = object;
export interface Parser {
  getLangs: () => string[];
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (lang: string, filename: string) => any;
  parsePattern: (printErrors: boolean, lang: string, pattern: string) => any;
}
export interface Engine {
  lookupLang: (name: string) => string | null;
  addParser: (parser: Parser) => void;
  hasParser: (lang: string) => boolean;
  execute: (
    language: string,
    rulesFilename: string,
    targetFilename: string
  ) => string;
  writeFile: (filename: string, content: string) => void;
  deleteFile: (filename: string) => void;
  isMissingLanguages: () => boolean;
  getMissingLanguages: () => string[];
  clearMissingLanguages: () => void;
}
export declare const EngineFactory: (wasmUri?: string) => Promise<Engine>;
