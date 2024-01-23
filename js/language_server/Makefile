.PHONY: default
default: build

include ../engine/Makefile.include
include ../tree-sitter/Makefile.include

# TODO: manage this better

# All languages that have a C parser we need to compile w/emcc
LANGUAGES := bash c cairo cpp csharp dart dockerfile go hack html java jsonnet kotlin lisp lua ocaml php promql protobuf python r rust solidity terraform typescript vue

# Tree Sitter parser names so we can properly export them in EMCC
TS_LANGS := bash c cairo clojure cpp c_sharp dart dockerfile go hack html java jsonnet kotlin lua ocaml php promql proto python r rust solidity hcl typescript tsx vue

PARSER_EXPORTED_FUNCTIONS := $(shell echo $(TS_LANGS:%=_tree_sitter_%) | sed 's/ /,/g')

.PHONY: build
build: dist/semgrep-ls.js

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

dist/language-server-wasm.js: ../libyaml/dist/libyaml.o ../libpcre/dist/libpcre.a ../tree-sitter/dist/tree-sitter.o $(LANGUAGES:%=../languages/%/dist/semgrep-parser.o)
	mkdir -p dist
	emcc \
		-O3 \
		$^ \
		$(EMCC_DEFAULTS) \
		-s INITIAL_MEMORY=31457280 \
		-s SINGLE_FILE=1 \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,$(YAML_EXPORTED_FUNCTIONS),$(PCRE_EXPORTED_FUNCTIONS),$(TREESITTER_EXPORTED_FUNCTIONS),$(PARSER_EXPORTED_FUNCTIONS) \
		-o $@

.PHONY: dist/Main.bc.js
dist/Main.bc.js: ../../_build/default/js/language_server/Main.bc.js
	mkdir -p dist
	cp -rf $< $@

dist/semgrep-ls.js: node_modules/.package-lock.json dist/language-server-wasm.js dist/Main.bc.js
	mkdir -p dist
	npm run build
