export type Mountpoint = object;
export type Lang = number;

export interface Parser {
  getLangs: () => Lang[];
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (lang: Lang, filename: string) => any;
  parsePattern: (printErrors: boolean, lang: Lang, pattern: string) => any;
}

export declare const ParserFactory: (wasmUri?: string) => Promise<Parser>;
