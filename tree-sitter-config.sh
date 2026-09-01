# shellcheck shell=bash
# Paths to dune's tree-sitter-out (headers + libtree-sitter).
#
#   source path/to/tree-sitter-config.sh          # bash
#   include path/to/tree-sitter-config.mk         # make (thin wrapper)
#   bash path/to/tree-sitter-config.sh            # print TREESITTER_OUT

# This file always lives at the OSS root (standalone clone or OSS/ in the
# proprietary monorepo). Nested iff that directory is named OSS.
_ts_oss_root=$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
if [ "$(basename "$_ts_oss_root")" = "OSS" ]; then
  _ts_workspace=$(dirname "$_ts_oss_root")
  _ts_relpath=OSS/libs/ocaml-tree-sitter-semgrep/core/tree-sitter-out
else
  _ts_workspace=$_ts_oss_root
  _ts_relpath=libs/ocaml-tree-sitter-semgrep/core/tree-sitter-out
fi

export TREESITTER_OUT="$_ts_workspace/_build/default/$_ts_relpath"
export TREESITTER_INCDIR="$TREESITTER_OUT/include"
export TREESITTER_LIBDIR="$TREESITTER_OUT/lib"
export TREESITTER_BINDIR="$TREESITTER_OUT/bin"
# So pkg-config (e.g. src/main/flags.sh) finds the dune-built libtree-sitter.
export PKG_CONFIG_PATH="$TREESITTER_OUT/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

unset _ts_oss_root _ts_workspace _ts_relpath

# Only when executed directly (not sourced): print TREESITTER_OUT for
# tree-sitter-config.mk, which derives the other paths itself.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  echo "$TREESITTER_OUT"
fi
