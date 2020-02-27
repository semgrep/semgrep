all:
	dune build
	dune build ./_build/default/tests/test.bc
	./sgrep_lint/build.sh ./build/
clean:
	dune clean
	rm -rf ./build/
test:
	dune runtest
	./testlint/run-lint-tests.sh
install:
	dune install
	./sgrep_lint/build.sh /usr/local/bin/

dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

.PHONY: all clean install test dump
