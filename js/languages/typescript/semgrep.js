//Provides: octs_create_parser_tsx
function octs_create_parser_tsx() {
  const wasm = globalThis.ParserModule;
  const parser_ptr = wasm._ts_parser_new();
  wasm._ts_parser_set_language(parser_ptr, wasm._tree_sitter_typescript());  // TODO: does tsx have its own parser?
  return { wasm, parser_ptr };
}

//Provides: octs_create_parser_typescript
function octs_create_parser_typescript() {
  const wasm = globalThis.ParserModule;
  const parser_ptr = wasm._ts_parser_new();
  wasm._ts_parser_set_language(parser_ptr, wasm._tree_sitter_typescript());
  return { wasm, parser_ptr };
}