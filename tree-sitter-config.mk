# Make wrapper for tree-sitter-config.sh (single source of truth for
# TREESITTER_OUT; the other paths are fixed suffixes of it, derived here).
#   include path/to/tree-sitter-config.mk
export TREESITTER_OUT := $(shell bash "$(dir $(abspath $(lastword $(MAKEFILE_LIST))))tree-sitter-config.sh")
export TREESITTER_INCDIR := $(TREESITTER_OUT)/include
export TREESITTER_LIBDIR := $(TREESITTER_OUT)/lib
export TREESITTER_BINDIR := $(TREESITTER_OUT)/bin
# So pkg-config (e.g. src/main/flags.sh) finds the dune-built libtree-sitter.
export PKG_CONFIG_PATH := $(TREESITTER_OUT)/lib/pkgconfig$(if $(PKG_CONFIG_PATH),:$(PKG_CONFIG_PATH))
