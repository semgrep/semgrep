SEMGREP_LANG := dockerfile

# dockerfile parser also needs bash parser
#EXTRA_TREE_SITTER_SOURCES := $(shell find ../../../languages/bash/tree-sitter -name 'scanner.c' -o -name 'scanner.cc' -o -name 'parser.c')
EXTRA_PARSER_DEPS := ../bash/dist/semgrep-parser.o
TS_EXPORTED_FUNCTIONS := _tree_sitter_dockerfile,_tree_sitter_bash

include ../shared/Makefile.include
