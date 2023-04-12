EMCC_DEFAULTS = \
   -sALLOW_MEMORY_GROWTH=1 \
   -sEXPORTED_RUNTIME_METHODS=AsciiToString,stringToAscii,stringToUTF8,getValue,setValue \
   -sSINGLE_FILE \
   -sMODULARIZE

.PHONY: default
default: dist/libyaml.js

.PHONY: clean
clean:
	rm -rf dist

dist/libyaml.js: *.c
	mkdir -p dist
	emcc \
		-O3 \
		-I. \
		-DHAVE_CONFIG_H \
		$^ \
		$(EMCC_DEFAULTS) \
		-sEXPORTED_FUNCTIONS=_malloc,_free,_yaml_parser_initialize,_yaml_parser_set_input_string,_yaml_parser_parse \
		-o $@
