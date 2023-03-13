//Provides: octs_create_parser_c_sharp
function octs_create_parser_c_sharp() {
    if (!globalThis.TreeSitterModule) {
      throw new Error("TreeSitterModule not loaded");
    }
    const wasm = globalThis.TreeSitterModule;
    const parser_ptr = wasm._ts_parser_new();
    wasm._ts_parser_set_language(parser_ptr, wasm._tree_sitter_c_sharp());
    return { wasm, parser_ptr };
  }
  