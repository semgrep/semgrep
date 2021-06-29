# semgrep-core

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

all:
	rm -f bin
	./scripts/make-version > ./src/cli/version.ml
	dune build
	dune build ./_build/default/tests/test.bc
	# make executables easily accessible for manual testing:
	test -e bin || ln -s _build/install/default/bin .
clean:
	dune clean
	rm -f bin
# I put all as a dependency because sometimes you modify a test file
# and dune runtest -f does not see this new file, probably because
# the cached file under _build/.../tests/ is still the old one.
test: all
	$(MAKE) -C src/spacegrep test
	dune runtest -f
e2etest:
	python3 tests/e2e/test_target_file.py
install:
	dune install
	cp -f _build/install/default/bin/semgrep-core ../semgrep/semgrep/bin/
	cp -f _build/install/default/bin/spacegrep ../semgrep/semgrep/bin/
dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

.PHONY: all clean install test dump

# Developer rules

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b develop -r mjambon -r IagoAbal -r emjin

push:
	git push origin `git rev-parse --abbrev-ref HEAD`

merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout develop && git pull && git branch -D $$A


#coupling: .circleci/config.yml
#semgrep must be in your PATH (run 'pipenv shell' from semgrep/semgrep/)
check:
	@semgrep --version
	@semgrep-core -version
	semgrep --config ../semgrep.yml --config p/ocaml --error --strict --verbose --exclude tests ..

index:
	codegraph_build -lang cmt -derived_data .

visual:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null .
visual2:
	codemap -screen_size 3 -filter pfff -efuns_client efuns_client -emacs_client /dev/null src

.PHONY: check index visual

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
