//Provides: octs_create_parser_dockerfile
//Requires: wasm,lazy_instantiate_parser
function octs_create_parser_dockerfile() {
  return lazy_instantiate_parser(() => {
    const ptr = wasm._ts_parser_new();
    wasm._ts_parser_set_language(ptr, wasm._tree_sitter_dockerfile());
    return ptr;
  });
}
