all:
	dune build
	dune build ./_build/default/tests/test.bc
clean:
	dune clean
test:
	dune runtest
	./testlint/run-lint-tests.sh
install:
	dune install

dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

.PHONY: all clean install test dump
