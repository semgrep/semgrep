.PHONY: default
default: build

include ../engine/Makefile.include
include ../tree-sitter/Makefile.include

# TODO: manage this better
LANGUAGES := bash c cairo cpp csharp dart dockerfile elixir go hack html java jsonnet kotlin lisp lua ocaml php promql protobuf python r rust solidity terraform typescript vue
TS_LANGS := bash c cairo clojure cpp c_sharp dart dockerfile elixir go hack html java jsonnet kotlin lua ocaml php promql proto python r rust solidity hcl typescript vue

empty:=
space:= $(empty) $(empty)
comma:=,
PARSER_EXPORTED_FUNCTIONS := $(subst $(space),$(comma),$(TS_LANGS:%=_tree_sitter_%))

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

package-lock.json:
	npm install

node_modules/.package-lock.json: package-lock.json
	npm ci

../tree-sitter/dist/tree-sitter.o:
	$(MAKE) -C ../tree-sitter dist/tree-sitter.o

dist/language-server.js dist/language-server.wasm: ../libyaml/dist/libyaml.o ../libpcre/dist/libpcre.a ../tree-sitter/dist/tree-sitter.o $(LANGUAGES:%=../languages/%/dist/semgrep-parser.o)
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		$(EMCC_DEFAULTS) \
		-s INITIAL_MEMORY=31457280 \
		-sEXPORTED_FUNCTIONS=_malloc,_free,$(YAML_EXPORTED_FUNCTIONS),$(PCRE_EXPORTED_FUNCTIONS),$(TREESITTER_EXPORTED_FUNCTIONS),$(PARSER_EXPORTED_FUNCTIONS) \
		-o $(word 1,$@)

dist/index.cjs: src/index.js node_modules/.package-lock.json dist/language-server.js ../../_build/default/js/language_server/Main.bc.js
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --format=cjs --outfile=$@

dist/index.mjs: src/index.js node_modules/.package-lock.json dist/language-server.js ../../_build/default/js/language_server/Main.bc.js
	mkdir -p dist
	npx esbuild $< $(ESBUILD_DEFAULTS) --format=esm --outfile=$@

dist/index.d.ts: src/index.d.ts
	mkdir -p dist
	cp $^ $@
