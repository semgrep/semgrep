EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=UTF8ToString,stringToUTF8,lengthBytesUTF8,getValue,setValue \
   -sSINGLE_FILE \
   -sMODULARIZE

.PHONY: default
default: build

.PHONY: build
build:
	npm ci
	npm run build

.PHONY: clean
clean:
	rm -rf dist

.PHONY: test
test: default
	npm run test

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
