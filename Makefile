all:
	dune build
clean:
	dune clean

test:
	dune runtest

.PHONY: all clean test
