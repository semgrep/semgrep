include ./Makefile.include

EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=UTF8ToString,stringToUTF8,stringToAscii,lengthBytesUTF8,getValue,setValue \
   -sMODULARIZE

.PHONY: build
build: dist/libpcre.js

.PHONY: clean
clean:
	rm -rf dist
	cd downloads/pcre-8.45; $(MAKE) clean

.PHONY: distclean
distclean:
	rm -rf dist downloads node_modules

.PHONY: test
test: build
	npm ci
	npm test

downloads/pcre-8.45.tar.gz:
	mkdir -p downloads
	curl -L -o $@ "https://sourceforge.net/projects/pcre/files/pcre/8.45/pcre-8.45.tar.gz/download"

downloads/pcre-8.45/configure: downloads/pcre-8.45.tar.gz
	cd downloads; tar xzf pcre-8.45.tar.gz

downloads/pcre-8.45/Makefile: downloads/pcre-8.45/configure
	cd downloads/pcre-8.45; emconfigure ./configure --enable-utf8

downloads/pcre-8.45/.libs/libpcre.a: downloads/pcre-8.45/Makefile
	cd downloads/pcre-8.45; emmake make libpcre.la

dist/libpcre.a: downloads/pcre-8.45/.libs/libpcre.a
	mkdir -p dist
	cp $^ $@

dist/libpcre.js dist/libpcre.wasm: dist/libpcre.a
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,$(PCRE_EXPORTED_METHODS) \
		-o $(word 1,$@)
