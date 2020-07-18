# semgrep-core

# This extracts the folder where the libtree-sitter.{so|a} files are located.
# It is needed to build the code in ocaml-tree-sitter-lang.
# This is not elegant. Alternatives include:
# - Having a 'configure' script in ocaml-tree-sitter-lang, which would
#   tweak the dune setup.
# - Having a 'configure' script that produces a makefile to be included in
#   this makefile.
# - Having a wrapper script set the environment variables and call dune,
#   e.g. './scripts/with-config dune build'.
#
TREESITTER_LIBDIR ?= $(shell pkg-config --libs-only-L tree-sitter \
                           | sed -e 's/^-L//')
export TREESITTER_LIBDIR

all:
    # Update output of semgrep-core -version
	echo 'let version = "$(shell git describe --dirty --tags --long)"' > ./bin/version.ml

	dune build
	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest -f
e2etest:
	python3 tests/e2e/test_target_file.py
install:
	dune install
dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

.PHONY: all clean install test dump
