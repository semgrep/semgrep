#! /usr/bin/env bash
#
# Use static linking if platform allows.
#
set -eu

OS="$1"

# Force the use of static linking in these scenarios:
#
# - the name of the opam switch refers to musl,
#   e.g. '4.10.0+musl+static+flambda'
# - we're on alpine, in which case the opam switch doesn't have a special
#   name. It is assumed that the reason we're on alpine is to get
#   statically-linked executables.
if [[ "$(opam switch show)" == *+static* ]]; then
    FLAGS=()
	CCLIB=("-lssl" "-lcrypto" "-lz")
	CCOPT=("-static" "-no-pie")
else
	case "$OS" in
	linux)
		if [[ -e /etc/alpine-release ]]; then
			# The -cclib statically link in libcurl's dependencies.
			# This can be removed when we transition away from the ocurl otel collector
			#old: was just '--copt -static --copy -no-pie' before we dependended on libcurl
            FLAGS=()
			CCLIB=("-lssl" "-lcrypto" "-lz")
			CCOPT=("-static" "-no-pie")
		fi
		;;
	macosx)
		FLAGS=("-noautolink")
		CCLIB=("-lANSITerminal_stubs"
			"-lalcotest_stubs"
			"-lbase_internalhash_types_stubs"
			"-lbase_stubs"
			"-lbigstringaf_stubs"
			"-lca_certs_stubs"
			"-lcamlstr"
			"-lcheckseum_c_stubs"
			"-lcstruct_stubs"
			"-lctypes_stubs"
			"-lcurl-helper"
			"-lintegers_stubs"
			"-ljs_of_ocaml_stubs"
			"-ljsoo_runtime_stubs"
			"-llwt_unix_stubs"
			"-lmirage_crypto_ec_stubs"
			"-lmirage_crypto_rng_unix_stubs"
			"-lmirage_crypto_stubs"
			"-lmtime_clock_stubs"
			"-lmurmur3_stubs"
			"-lparmap_stubs"
			"-lpbrt_stubs"
			"-lpcre2_stubs"
			"-lpcre_stubs"
			"-lptime_clock_stubs"
			"-lstdc++"
			"-lterminal_size_stubs"
			"-lthreadsnat"
			"-ltime_now_stubs"
			"-ltree_sitter_bash_stubs"
			"-ltree_sitter_bindings_stubs"
			"-ltree_sitter_c_sharp_stubs"
			"-ltree_sitter_c_stubs"
			"-ltree_sitter_cairo_stubs"
			"-ltree_sitter_clojure_stubs"
			"-ltree_sitter_cpp_stubs"
			"-ltree_sitter_dart_stubs"
			"-ltree_sitter_dockerfile_stubs"
			"-ltree_sitter_go_stubs"
			"-ltree_sitter_hack_stubs"
			"-ltree_sitter_hcl_stubs"
			"-ltree_sitter_html_stubs"
			"-ltree_sitter_java_stubs"
			"-ltree_sitter_jsonnet_stubs"
			"-ltree_sitter_julia_stubs"
			"-ltree_sitter_kotlin_stubs"
			"-ltree_sitter_lua_stubs"
			"-ltree_sitter_ocaml_stubs"
			"-ltree_sitter_php_stubs"
			"-ltree_sitter_promql_stubs"
			"-ltree_sitter_proto_stubs"
			"-ltree_sitter_python_stubs"
			"-ltree_sitter_ql_stubs"
			"-ltree_sitter_r_stubs"
			"-ltree_sitter_ruby_stubs"
			"-ltree_sitter_rust_stubs"
			"-ltree_sitter_solidity_stubs"
			"-ltree_sitter_swift_stubs"
			"-ltree_sitter_tsx_stubs"
			"-ltree_sitter_typescript_stubs"
			"-ltree_sitter_vue_stubs"
			"-lunix"
			"-lyaml_c_stubs"
			"-lyaml_ffi_stubs"
			"-lzarith"
			"-lcurl"
			"$(pkg-config gmp --variable libdir)/libgmp.a"
			"$(pkg-config tree-sitter --variable libdir)/libtree-sitter.a"
			"$(brew --prefix libev)/lib/libev.a"
			"$(brew --prefix pcre)/lib/libpcre.a"
			"$(brew --prefix pcre2)/lib/libpcre2-8.a"
			"-lpthread")
            CCOPT=()
		;;
	windows)
        # not sure what we want to do here
        FLAGS=()
        CCLIB=()
        CCOPT=()
        ;;
	esac
fi

echo '('
for f in "${FLAGS[@]}"; do echo "  $f"; done
for f in "${CCLIB[@]}"; do echo "  -cclib $f"; done
for f in "${CCOPT[@]}"; do echo "  -ccopt $f"; done
echo ')'
