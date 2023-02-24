//Provides: create_parser
//Requires: tree_sitter_wasm_module
function create_parser(language) {
  const parser = tree_sitter_wasm_module._ts_parser_new();
  tree_sitter_wasm_module._ts_parser_set_language(parser, language);
  return parser;
}

//Provides: octs_create_parser_go
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_go() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_go());
}

//Provides: octs_create_parser_ocaml
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_ocaml() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_ocaml());
}

//Provides: octs_create_parser_hcl
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_hcl() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_hcl());
}

//Provides: octs_create_parser_clojure
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_clojure() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_clojure());
}

//Provides: octs_create_parser_python
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_python() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_python());
}

//Provides: octs_create_parser_elixir
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_elixir() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_elixir());
}

//Provides: octs_create_parser_dockerfile
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_dockerfile() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_dockerfile());
}

//Provides: octs_create_parser_typescript
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_typescript() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_typescript());
}

//Provides: octs_create_parser_tsx
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_tsx() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_tsx());
}

//Provides: octs_create_parser_rust
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_rust() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_rust());
}

//Provides: octs_create_parser_r
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_r() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_r());
}

//Provides: octs_create_parser_java
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_java() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_java());
}

//Provides: octs_create_parser_kotlin
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_kotlin() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_kotlin());
}

//Provides: octs_create_parser_php
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_php() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_php());
}

//Provides: octs_create_parser_lua
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_lua() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_lua());
}

//Provides: octs_create_parser_bash
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_bash() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_bash());
}

//Provides: octs_create_parser_solidity
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_solidity() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_solidity());
}

//Provides: octs_create_parser_cpp
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_cpp() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_cpp());
}

//Provides: octs_create_parser_swift
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_swift() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_swift());
}

//Provides: octs_create_parser_jsonnet
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_jsonnet() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_jsonnet());
}

//Provides: octs_create_parser_c_sharp
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_c_sharp() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_c_sharp());
}

//Provides: octs_create_parser_julia
//Requires: create_parser
function octs_create_parser_julia() {
  throw new Error(
    "TODO: re-incorporate julia once we figure out why the build is so slow"
  ); // TODO
  //return create_parser(SEMGREP_LANGUAGES._tree_sitter_python())
}

//Provides: octs_create_parser_c
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_c() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_c());
}

//Provides: octs_create_parser_ruby
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_ruby() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_ruby());
}

//Provides: octs_create_parser_vue
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_vue() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_vue());
}

//Provides: octs_create_parser_sqlite
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_sqlite() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_sqlite());
}

//Provides: octs_create_parser_html
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_html() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_html());
}

//Provides: octs_create_parser_hack
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_hack() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_hack());
}

//Provides: octs_create_parser_dart
//Requires: create_parser, tree_sitter_wasm_module
function octs_create_parser_dart() {
  return create_parser(tree_sitter_wasm_module._tree_sitter_dart());
}
