export type Mountpoint = object;
export type Lang = number;

export interface Parser {
  getLangs: () => Lang[];
  setMountpoints: (mountpoints: Mountpoint[]) => void;
  parseTarget: (lang: Lang, filename: string) => any;
  parsePattern: (lang: Lang, pattern: string) => any;
  parseTargetTsOnly: (file: string) => any;
}

export declare const ParserFactory: (wasmUri?: string) => Promise<Parser>;
