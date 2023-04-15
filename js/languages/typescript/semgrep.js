//Provides: octs_create_parser_tsx
//Requires: octs_create_parser_typescript
function octs_create_parser_tsx() {
  return octs_create_parser_typescript();
}

//Provides: octs_create_parser_typescript
//Requires: wasm,lazy_instantiate_parser
function octs_create_parser_typescript() {
  return lazy_instantiate_parser(() => {
    const ptr = wasm._ts_parser_new();
    wasm._ts_parser_set_language(ptr, wasm._tree_sitter_typescript());
    return ptr;
  });
}
