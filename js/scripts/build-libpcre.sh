#!/bin/bash -eu

pushd "${1}"
	test -f Makefile || emconfigure ./configure  # only run configure if the Makefile doesnt exist
	emmake make libpcre.la
popd

emcc -O3 "${1}/.libs/libpcre.a" \
		-sEXPORTED_FUNCTIONS=_malloc,_free,_pcre_version,_pcre_config,_pcre_compile,_pcre_fullinfo,_pcre_study,_pcre_exec \
		-sEXPORTED_RUNTIME_METHODS=AsciiToString,stringToAscii,stringToUTF8,getValue,setValue \
		-sMODULARIZE \
		-o "${2}"
