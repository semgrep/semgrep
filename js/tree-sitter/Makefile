include Makefile.include

.PHONY: build
build: dist/tree-sitter.o

.PHONY: clean
clean:
	rm -rf dist

dist/tree-sitter.o: $(TREESITTER_SRCDIR)/lib.c
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		-I$(TREESITTER_SRCDIR) \
		-I${TREESITTER_INCDIR} \
		-shared \
		-o $@
