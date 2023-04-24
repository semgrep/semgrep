export type Mountpoint = object;
export type Lang = number;
export interface Parser {
  getLangs: () => Lang[];
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (lang: Lang, filename: string) => any;
  parsePattern: (printErrors: boolean, lang: Lang, pattern: string) => any;
}
export interface Engine {
  lookupLang: (name: string) => Lang;
  addParser: (parser: Parser) => void;
  hasParser: (lang: Lang) => boolean;
  execute: (
    language: string,
    rulesFilename: string,
    targetFilename: string
  ) => string;
  writeFile: (filename: string, content: string) => void;
  deleteFile: (filename: string) => void;
}
export declare const EngineFactory: (wasmUri?: string) => Promise<Engine>;
