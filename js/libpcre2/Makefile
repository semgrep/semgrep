include ./Makefile.include

LIBRARY_NAME=pcre2
LIBRARY_VERSION=10.43
FULL_LIBRARY_NAME=$(LIBRARY_NAME)-$(LIBRARY_VERSION)

EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=UTF8ToString,stringToUTF8,stringToAscii,lengthBytesUTF8,intArrayFromString,writeArrayToMemory,getValue,setValue \
   -sMODULARIZE

.PHONY: build
build: dist/lib$(LIBRARY_NAME).js

.PHONY: clean
clean:
	rm -rf dist
	cd downloads/$(FULL_LIBRARY_NAME); $(MAKE) clean

.PHONY: distclean
distclean:
	rm -rf dist downloads node_modules

.PHONY: test
test: build
	npm ci
	npm test

downloads/$(FULL_LIBRARY_NAME).tar.gz:
	mkdir -p downloads
	curl -L -o $@ "https://github.com/PCRE2Project/pcre2/releases/download/$(FULL_LIBRARY_NAME)/$(FULL_LIBRARY_NAME).tar.gz"

downloads/$(FULL_LIBRARY_NAME)/configure: downloads/$(FULL_LIBRARY_NAME).tar.gz
	cd downloads; tar xzf $(FULL_LIBRARY_NAME).tar.gz

downloads/$(FULL_LIBRARY_NAME)/Makefile: downloads/$(FULL_LIBRARY_NAME)/configure
	cd downloads/$(FULL_LIBRARY_NAME); emconfigure ./configure --enable-utf8

downloads/$(FULL_LIBRARY_NAME)/.libs/lib$(LIBRARY_NAME)-8.a: downloads/$(FULL_LIBRARY_NAME)/Makefile
	cd downloads/$(FULL_LIBRARY_NAME); emmake make lib$(LIBRARY_NAME)-8.la

dist/lib$(LIBRARY_NAME)-8.a: downloads/$(FULL_LIBRARY_NAME)/.libs/lib$(LIBRARY_NAME)-8.a
	mkdir -p dist
	cp $^ $@

dist/lib$(LIBRARY_NAME).js dist/lib$(LIBRARY_NAME).wasm: dist/lib$(LIBRARY_NAME)-8.a
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,$(PCRE2_EXPORTED_FUNCTIONS) \
		-o $(word 1,$@)
