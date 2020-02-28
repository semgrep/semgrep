all:
	dune build
	dune build ./_build/default/tests/test.bc
lint:
	./sgrep_lint/build.sh ./build/
clean:
	dune clean
	rm -rf ./build/
test:
	dune runtest
	./testlint/run-lint-tests.sh
install:
	dune install
	mkdir -p /usr/local/bin/sgrep-lint/
install-lint:
	cp -rv ./sgrep_lint/build/sgrep.dist/ /usr/local/bin/sgrep-lint/

dump:
	./_build/default/tests/test.bc -dump_ast tests/lint/stupid.py

.PHONY: all clean install test dump
