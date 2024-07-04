SEMGREP_LANG := hack

TREE_SITTER_SOURCES := $(shell find ../../../languages/hack/tree-sitter/semgrep-$(SEMGREP_LANG)/lib -name 'scanner.c' -o -name 'scanner.cc' -o -name 'parser.c')

include ../shared/Makefile.include
