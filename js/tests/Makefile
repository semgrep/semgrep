EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=UTF8ToString,stringToUTF8,stringToAscii,lengthBytesUTF8,getValue,setValue \
   -sMODULARIZE

include ../libyaml/Makefile.include
include ../libpcre/Makefile.include

default: test

clean:
	rm -rf dist

test: dist/test.js dist/test.wasm
	node index.test.js


../libyaml/dist/libyaml.o:
	$(MAKE) -C ../libyaml dist/libyaml.o

../libpcre/dist/libpcre.a:
	$(MAKE) -C ../libpcre dist/libpcre.a
languages:
	SEMGREP_JS_TEST_BUILD=1 $(MAKE) -C .. languages

dist/test.js dist/test.wasm: ../libyaml/dist/libyaml.o ../libpcre/dist/libpcre.a languages
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,$(YAML_EXPORTED_METHODS),$(PCRE_EXPORTED_METHODS) \
		-o $(word 1,$@)
