EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=UTF8ToString,stringToUTF8,lengthBytesUTF8,getValue,setValue \
   -sSINGLE_FILE \
   -sMODULARIZE

.PHONY: default
default: build

.PHONY: build
build: dist/libyaml.js dist/libyaml.d.ts

.PHONY: test
test: build
	npm test

.PHONY: clean
clean:
	rm -rf dist

node_modules/.package-lock.json:
	npm ci

dist/libyaml.js: *.c
	mkdir -p dist
	emcc \
		-O3 \
		-I. \
		-DHAVE_CONFIG_H \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,_get_sizeof_yaml_parser_t,_get_sizeof_yaml_event_t,_yaml_get_version_string,_yaml_get_version,_yaml_parser_initialize,_yaml_parser_set_input_string,_yaml_parser_parse,_yaml_event_delete,_yaml_parser_delete \
		-o $@

dist/libyaml.d.ts: node_modules/.package-lock.json dist/libyaml.js
	mkdir -p dist
	npx -p typescript tsc
