//Provides: octs_create_parser_php
//Requires: wasm,lazy_instantiate_parser
function octs_create_parser_php() {
  return lazy_instantiate_parser(() => {
    const ptr = wasm._ts_parser_new();
    wasm._ts_parser_set_language(ptr, wasm._tree_sitter_php());
    return ptr;
  });
}
