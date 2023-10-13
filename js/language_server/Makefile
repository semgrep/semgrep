EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=UTF8ToString,stringToUTF8,stringToAscii,lengthBytesUTF8,getValue,setValue \
   -sMODULARIZE

include ../libyaml/Makefile.include
include ../libpcre/Makefile.include

.PHONY: default
default: build

.PHONY: build
build: dist/index.cjs dist/index.mjs dist/index.d.ts

.PHONY: test
test: build
	npm test

.PHONY: server
server: build
	npm run server

clean:
	rm -rf dist


../libyaml/dist/libyaml.o:
	$(MAKE) -C ../libyaml dist/libyaml.o

../libpcre/dist/libpcre.a:
	$(MAKE) -C ../libpcre dist/libpcre.a


dist/semgrep-engine.js dist/semgrep-engine.wasm: ../libyaml/dist/libyaml.o ../libpcre/dist/libpcre.a
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,$(YAML_EXPORTED_METHODS),$(PCRE_EXPORTED_METHODS) \
		-o $(word 1,$@)

package-lock.json:
	npm install

node_modules/.package-lock.json: package-lock.json
	npm ci

dist/index.cjs: src/index.js node_modules/.package-lock.json dist/semgrep-engine.js ../../_build/default/js/engine/Main.bc.js
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --format=cjs --outfile=$@

dist/index.mjs: src/index.js node_modules/.package-lock.json dist/semgrep-engine.js ../../_build/default/js/engine/Main.bc.js
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --format=esm --outfile=$@

dist/index.d.ts: src/index.d.ts
	mkdir -p dist
	cp $^ $@
