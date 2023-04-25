#!/usr/bin/env bash
set -eu

for LANG in "$@"; do
cat <<EOF
//Provides: octs_create_parser_${LANG}
//Requires: wasm,lazy_instantiate_parser
function octs_create_parser_${LANG}() {
  return lazy_instantiate_parser(() => {
    const ptr = wasm._ts_parser_new();
    wasm._ts_parser_set_language(ptr, wasm._tree_sitter_${LANG}());
    return ptr;
  });
}
EOF
done
