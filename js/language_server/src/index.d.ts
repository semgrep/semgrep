export interface LanguageServer {
  start(): void;
}
export interface LSFactory {
  (wasmUri?: string): Promise<LanguageServer>;
}
