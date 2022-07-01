###############################################################################
# Major targets
###############################################################################

# This extracts the folder where the libtree-sitter.{so|a} files are located.
# It is needed to build the code in tree-sitter-lang.
# This is not elegant. Alternatives include:
# - Having a 'configure' script in tree-sitter-lang, which would
#   tweak the dune setup.
# - Having a 'configure' script that produces a makefile to be included in
#   this makefile.
# - Having a wrapper script set the environment variables and call dune,
#   e.g. './scripts/with-config dune build'.
#
TREESITTER_LIBDIR ?= $(shell pkg-config --libs-only-L tree-sitter \
                           | sed -e 's/^-L//')
export TREESITTER_LIBDIR

.PHONY: all
all:
	rm -f bin
	dune build
	dune build ./_build/default/tests/test.bc
	# make executables easily accessible for manual testing:
	test -e bin || ln -s _build/install/default/bin .

.PHONY: clean
clean:
	dune clean
	rm -f bin

# I put 'all' as a dependency because sometimes you modify a test file
# and dune runtest -f does not see this new file, probably because
# the cached file under _build/.../tests/ is still the old one.
#
.PHONY: test
test: all
	$(MAKE) -C src/spacegrep test
	dune runtest -f --no-buffer

# This may install more than you want.
# See the 'dev' target if all you need is access to the semgrep-core
# executable for testing.
.PHONY: install
install:
	dune install
	rm -f ../cli/src/semgrep/bin/semgrep-core
	cp _build/install/default/bin/semgrep-core ../cli/src/semgrep/bin/

###############################################################################
# Developer targets
###############################################################################

# Build semgrep-core and place it where semgrep expects it.
# This is for development purposes only as I'm not sure if a symlink is ok
# for packaging things up on the python side.
.PHONY: dev
dev:
	$(MAKE) all
	rm -f ../cli/src/semgrep/bin/semgrep-core
	ln -s ../../../../semgrep-core/bin/semgrep-core \
	  ../cli/src/semgrep/bin/semgrep-core

.PHONY: e2etest
e2etest:
	python3 tests/e2e/test_target_file.py

.PHONY: dump
dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

#coupling: .circleci/config.yml
#semgrep must be in your PATH (run 'pipenv shell' from cli/src/semgrep/)
.PHONY: check
check:
	@semgrep --version
	@semgrep-core -version
	semgrep --config ../semgrep.yml --config p/ocaml --error --strict --verbose --exclude tests ..

# Run matching performance tests
.PHONY: perf
perf:
	@echo "--- default settings ---"
	cd ./perf && ./run-perf-suite
	@echo "--- no caching ---"
	cd ./perf && ./run-perf-suite --no-cache
	@echo "--- maximum caching ---"
	cd ./perf && ./run-perf-suite --max-cache

# Run matching performance tests and post them to the semgrep dashboard
# at https://dashboard.semgrep.dev/
#
# This is meant for CI, which hopefully runs on similar machines each time.
#
.PHONY: report-perf
report-perf:
	cd ./perf && ./run-perf-suite --upload

.PHONY: update_semgrep_rules
update_semgrep_rules:
	cd tests/semgrep-rules; git checkout develop; git pull

###############################################################################
# Pad's targets
###############################################################################

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b develop -r returntocorp/pa

push:
	git push origin `git rev-parse --abbrev-ref HEAD`

merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout develop && git pull && git branch -D $$A


# see https://github.com/aryx/codegraph for information on codegraph_build
index:
	codegraph_build -lang cmt -derived_data .

# see https://github.com/aryx/codecheck for information on codecheck
check2:
	codecheck -lang ml -with_graph_code graph_code.marshall -filter 3 .

# see https://github.com/aryx/codemap for information on codemap
visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .
visual2:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null src
