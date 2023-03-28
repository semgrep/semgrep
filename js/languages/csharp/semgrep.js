//Provides: octs_create_parser_c_sharp
function octs_create_parser_c_sharp() {
    const wasm = globalThis.ParserModule;
    const parser_ptr = wasm._ts_parser_new();
    wasm._ts_parser_set_language(parser_ptr, wasm._tree_sitter_c_sharp());
    return { wasm, parser_ptr };
}
  