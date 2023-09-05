include ./Makefile.include

EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=UTF8ToString,stringToUTF8,lengthBytesUTF8,getValue,setValue \
   -sMODULARIZE

.PHONY: build
build: dist/libyaml.js

.PHONY: test
test: build
	npm ci
	npm test

.PHONY: clean
clean:
	rm -rf dist

.PHONY: distclean
distclean:
	rm -rf dist node_modules

dist/libyaml.o: *.c
	mkdir -p dist
	emcc \
		-O3 \
		-I. \
		-DHAVE_CONFIG_H \
		$^ \
		-shared \
		-o $@

dist/libyaml.js dist/libyaml.wasm: dist/libyaml.o
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,$(YAML_EXPORTED_METHODS) \
		-o $(word 1,$@)
