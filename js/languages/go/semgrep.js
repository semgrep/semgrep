//Provides: octs_create_parser_go
function octs_create_parser_go() {
  const wasm = globalThis.ParserModule;
  const parser_ptr = wasm._ts_parser_new();
  wasm._ts_parser_set_language(parser_ptr, wasm._tree_sitter_go());
  return { wasm, parser_ptr };
}
