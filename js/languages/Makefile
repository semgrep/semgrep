TREESITTER_SRCDIR := ../../libs/ocaml-tree-sitter-core/downloads/tree-sitter/lib/src

ESBUILD_DEFAULTS = \
	--bundle \
	--minify \
	--platform=neutral \
	--log-override:duplicate-case=silent \
	--external:path \
	--external:fs \
	--external:constants \
	--external:tty \
	--external:child_process

TS_EXPORTED_FUNCTIONS = _malloc,_free,_ts_parser_new,_ts_parser_set_language,_ts_parser_parse_string,_ts_tree_root_node,_ts_node_type,_ts_node_start_point,_ts_node_end_point,_ts_node_is_named,_ts_node_child_count,_ts_node_child,_ts_tree_delete

EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=AsciiToString,stringToAscii,stringToUTF8,getValue,setValue \
   -sSINGLE_FILE \
   -sMODULARIZE

SUPPORTED_LANGS = lua csharp go typescript

.PHONY: default
default:
	npm ci
	npm run build

.PHONY: test
test:
	npm run test

.PHONY: build
build: $(patsubst %,dist/%/index.cjs,$(SUPPORTED_LANGS)) $(patsubst %,dist/%/index.mjs,$(SUPPORTED_LANGS))

.PHONY: clean
clean:
	rm -rf dist

dist/lua/semgrep-parser.js: $(TREESITTER_SRCDIR)/lib.c ../../languages/lua/tree-sitter/semgrep-lua/lib/parser.c ../../languages/lua/tree-sitter/semgrep-lua/lib/scanner.cc
	mkdir -p dist/lua
	emcc \
		-O3 \
		-I$(TREESITTER_SRCDIR) \
		-I${TREESITTER_INCDIR} \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_tree_sitter_lua,$(TS_EXPORTED_FUNCTIONS) \
		-o $@

dist/csharp/semgrep-parser.js: $(TREESITTER_SRCDIR)/lib.c ../../languages/csharp/tree-sitter/semgrep-c-sharp/lib/parser.c ../../languages/csharp/tree-sitter/semgrep-c-sharp/lib/scanner.c
	mkdir -p dist/csharp
	emcc \
		-O3 \
		-I$(TREESITTER_SRCDIR) \
		-I${TREESITTER_INCDIR} \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_tree_sitter_c_sharp,$(TS_EXPORTED_FUNCTIONS) \
		-o $@

dist/go/semgrep-parser.js: $(TREESITTER_SRCDIR)/lib.c ../../languages/go/tree-sitter/semgrep-go/lib/parser.c
	mkdir -p dist/go
	emcc \
		-O3 \
		-I$(TREESITTER_SRCDIR) \
		-I${TREESITTER_INCDIR} \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_tree_sitter_go,$(TS_EXPORTED_FUNCTIONS) \
		-o $@

dist/typescript/semgrep-parser.js: $(TREESITTER_SRCDIR)/lib.c ../../languages/typescript/tree-sitter/semgrep-typescript/lib/parser.c ../../languages/typescript/tree-sitter/semgrep-typescript/lib/scanner.c
	mkdir -p dist/typescript
	emcc \
		-O3 \
		-I$(TREESITTER_SRCDIR) \
		-I${TREESITTER_INCDIR} \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_tree_sitter_typescript,$(TS_EXPORTED_FUNCTIONS) \
		-o $@

dist/%/Parser.bc.js: ../../_build/default/js/languages/%/Parser.bc.js
	cp $^ $@

dist/%/entrypoint.js: ../shared/entrypoint.js
	cp $^ $@

dist/%/index.cjs: dist/%/entrypoint.js dist/%/Parser.bc.js dist/%/semgrep-parser.js
	esbuild $< $(ESBUILD_DEFAULTS) --format=cjs --outfile=$@

dist/%/index.mjs: dist/%/entrypoint.js dist/%/Parser.bc.js dist/%/semgrep-parser.js
	esbuild $< $(ESBUILD_DEFAULTS) --format=esm --outfile=$@
