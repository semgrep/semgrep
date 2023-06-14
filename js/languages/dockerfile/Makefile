SEMGREP_LANG := dockerfile

# dockerfile parser also needs bash parser
EXTRA_TREE_SITTER_SOURCES := $(shell find ../../../languages/bash/tree-sitter -name 'scanner.c' -o -name 'scanner.cc' -o -name 'parser.c')
TS_EXPORTED_FUNCTIONS := _tree_sitter_dockerfile,_tree_sitter_bash

include ../shared/Makefile.include
