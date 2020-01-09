all:
	dune build
clean:
	dune clean
test:
	dune runtest
install:
	dune install

.PHONY: all clean install test
