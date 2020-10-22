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
	./scripts/make-version > ./cli/version.ml
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

# Developer rules

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b develop -r mjambon

push:
	git push origin `git rev-parse --abbrev-ref HEAD`

merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout develop && git pull && git branch -D $$A


#coupling: .circleci/config.yml
#semgrep must be in your PATH (run 'pipenv shell' from semgrep/semgrep/)
check:
	@semgrep --version
	@semgrep-core -version
	semgrep --config ../semgrep.yml --config https://semgrep.dev/p/ocaml --error --strict --exclude tests ..

index:
	codegraph_build -lang cmt -derived_data .

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .

.PHONY: check index visual
