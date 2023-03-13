#!/bin/bash -eu

shopt -s globstar

# TODO: fix hardcoded tree-sitter paths
emcc -O3 \
    -I"${TREESITTER_INCDIR}" \
    -I../libs/ocaml-tree-sitter-core/downloads/tree-sitter-0.20.6/lib/src \
    "${1}"/**/semgrep-"${2}"/**/{parser,scanner}.c* \
    ../libs/ocaml-tree-sitter-core/downloads/tree-sitter-0.20.6/lib/src/lib.c \
    -sEXPORTED_FUNCTIONS="_tree_sitter_${2}",_ts_parser_new,_ts_parser_set_language,_ts_parser_parse_string,_ts_tree_root_node,_ts_node_type,_ts_node_start_point,_ts_node_end_point,_ts_node_is_named,_ts_node_child_count,_ts_node_child,_ts_tree_delete \
    -sEXPORTED_RUNTIME_METHODS=AsciiToString,stringToAscii,stringToUTF8,getValue,setValue \
    -sMODULARIZE \
    -o "${3}"
