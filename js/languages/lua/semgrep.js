//Provides: octs_create_parser_lua
function octs_create_parser_lua() {
  const wasm = globalThis.ParserModule;
  const parser_ptr = wasm._ts_parser_new();
  wasm._ts_parser_set_language(parser_ptr, wasm._tree_sitter_lua());
  return { wasm, parser_ptr };
}
