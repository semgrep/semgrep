all:
	dune build
	dune build ./_build/default/tests/test.bc
lint:
	./sgrep_lint/build.sh ./build/
clean:
	dune clean
	rm -rf ./sgrep_lint/build/
test:
	dune runtest
	./sgrep_lint/tests/run-lint-tests.sh
install:
	dune install
install-lint:
	cp -rv ./sgrep_lint/build/sgrep.dist/ /usr/local/bin/sgrep-lint-files/
	ln -sf /usr/local/bin/sgrep-lint-files/sgrep-lint /usr/local/bin/sgrep-lint
dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

.PHONY: all clean install test dump
