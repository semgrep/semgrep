#! /usr/bin/env bash
#
# Use static linking if platform allows.
#
# How to update this file
# =======================
#
# When adding a new dependency, you may need to update this file. This is
# required when additional objects must be linked. Examples include: a
# package with C bindings or system dependency.
#
# - If you are adding a new OCAML PACKAGE:
#
#   - In the macosx case below, add any STUBS the library relies on. These are
#     normally called something like <LIBRARY>_stubs. The name can be found in
#     the package's dune file as the value of the `names` field of the
#     `foreign_stubs` field under the library.
#
#   - In the macosx case below, add any SYSTEM LIBRARIES the library relies on.
#     This would be anything you needed to add to BREW_DEPS or have as a
#     conf-lib<SYSLIB> opam dependency.
#
#     If it would be installed by `brew`, specify the path using pkg-config
#     (see, e.g., gmp), or, if the package doesn't include a .pc file, use the
#     brew prefix. In these cases, you should specifically specify the static
#     archive (`.a`).
#
#   - Other cases should not typically require changes. If you have made
#     non-standard changes to the Dockerfile or Alpine build (e.g., building a
#     library from source), Alpine may require tweaks.

set -eu

OS="$1"
TREE_SITTER_LANGS="$2"

>&2 echo "Generating linking flags for OS ${OS} (!!!in case of linking errors, adjust src/main/flags.sh!!!)"

# Force the use of static linking in these scenarios:
#
# - the name of the opam switch refers to musl,
#   e.g. '4.10.0+musl+static+flambda'
# - we're on alpine, in which case the opam switch doesn't have a special
#   name. It is assumed that the reason we're on alpine is to get
#   statically-linked executables.


# Check if NIX_ENVIRONMENT is not set
# If it is set, we are in a nix-shell and we should not statically link
# This is first because opam won't exist in a nix build environment
if [[ -n "${SEMGREP_NIX_BUILD-}" ]]; then
    FLAGS=()
    CCLIB=()
    CCOPT=()
elif [[ "$(opam switch show)" == *+static* ]]; then
    FLAGS=()
    CCLIB=("-lssl" "-lcrypto" "-lz")
    CCOPT=("-static" "-no-pie")
else
    case "$OS" in
    linux)
        if [[ -e /etc/alpine-release ]]; then
            # The CCLIB flags statically (since we have CCOPT include -static)
            # link in libcurl's dependencies.
            # This can be removed when we transition away from the ocurl otel
            # collector.
            # old: was just '--copt -static --copy -no-pie' before we depended
            # on libcurl
            FLAGS=()
            CCLIB=("-lssl" "-lcrypto" "-lz")
            CCOPT=("-static" "-no-pie")
        else
            # On non-Alpine Linux distros (e.g., Ubuntu), we just dynamically
            # link for dev. Note that Alpine is used for our Docker builds.
            FLAGS=()
            CCLIB=()
            CCOPT=()
        fi
        ;;
    # In general, we want to statically link to make it easier to distribute
    # binaries since they won't carry runtime dependencies on various
    # libraries which would need to be separately installed or managed.
    # Additionally, this can be a bit fragile depending on which paths the
    # loader uses, and where dylibs are expected to be (and how possible
    # differences in the CI build versus other environments).
    #
    # However, on MacOS, it is a bit difficult to ensure libraries are linked
    # statically. It is not possible to link all libraries statically.
    # Notably, libSystem cannot (officially) be statically linked against,
    # since MacOS does not have a stable syscall ABI, but rather relies on
    # libSystem being stable. See, e.g.,
    # - <https://developer.apple.com/forums/thread/706419>
    # - <https://developer.apple.com/library/archive/qa/qa1118/_index.html>
    # - `man 1 ld`
    #
    # As a result, we cannot use a similar strategy to our Alpine build (which
    # works for Linux, since it is a rare case with a stable syscall ABI, See
    # <https://unix.stackexchange.com/questions/473137/do-other-unix-like-kernels-have-stable-syscall-abis>).
    # Previously we relied on a workaround where we deleted the `.dylib`
    # versions of libraries, leaving only the `.a` versions, thus forcing them
    # to be linked statically. However, this breaks the build in the case
    # where later build steps require programs which dynamically linked
    # against those libraries. Notably, this occurs for PCRE2. See
    # <https://github.com/semgrep/semgrep/pull/10064>.
    #
    # One might hope that we could specify the linker to use the `.a` version
    # without deleting the `.dylib`, but the only way to do this is to disable
    # autolinking and specify each library individually. Otherwise, on MacOS,
    # if both are present, the linker will link dynamically regardless.
    #
    # If you add a dependency which should be statically linked, it should be
    # added to this list. If it has a `.pc` file, ensure that the library is
    # appropriately added to PKG_CONFIG_PATH and use pkg-config to pick it up
    # below (see, e.g., gmp). If we expect it to always be installed via brew,
    # then use `brew --prefix <BREW PACKAGE NAME>`.
    macosx)
        langs=$(cat "${TREE_SITTER_LANGS}")
        FLAGS=("-noautolink")
        CCOPT=()
        # TODO: ideally much of this could be generated from dune/opam.
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
            "-lpcre_stubs"
            "-lpcre2_stubs"
            "-lptime_clock_stubs"
            "-lstdc++"
            "-lterminal_size_stubs"
            "-lthreadsnat"
            "-ltime_now_stubs"
            "-ltree_sitter_bindings_stubs"
            "-lunix"
            "-lyaml_c_stubs"
            "-lyaml_ffi_stubs"
            "-lzarith"
            "-lcurl"
            "$(pkg-config gmp --variable libdir)/libgmp.a"
            "$(pkg-config tree-sitter --variable libdir)/libtree-sitter.a"
            "$(pkg-config libpcre --variable libdir)/libpcre.a"
            "$(pkg-config libpcre2-8 --variable libdir)/libpcre2-8.a"
            "-lpthread")

        # Libev does not support pkg-config. See, e.g.,
        # <https://www.mail-archive.com/libev@lists.schmorp.de/msg02088.html>,
        # <http://lists.schmorp.de/pipermail/libev/2024q1/002940.html>. As a
        # result we still use the brew prefix, but with the option of an
        # environment variable for the build to override the location.
        if [ -z ${SEMGREP_LIBEV_ARCHIVE_PATH+set} ]; then
            CCLIB+=("$(brew --prefix libev)/lib/libev.a")
        else
            CCLIB+=("${SEMGREP_LIBEV_ARCHIVE_PATH}")
        fi

        for lang in $langs; do
            CCLIB+=("-ltree_sitter_${lang}_stubs")
        done
        ;;
    # TODO: dedicated branch for Windows?
    *)
        # not sure what we want to do here
        FLAGS=()
        CCLIB=()
        CCOPT=()
        ;;
    esac
fi

echo '('
# See <https://stackoverflow.com/questions/7577052/bash-empty-array-expansion-with-set-u>
# for why we do this cursed array expansion.
# Sadly GitHub Actions uses a bash <4.4-rc-2 which makes this mandatory instead
# of merely nice to have.
for f in ${FLAGS[@]+"${FLAGS[@]}"}; do echo "  $f"; done
for f in ${CCLIB[@]+"${CCLIB[@]}"}; do echo "  -cclib $f"; done
for f in ${CCOPT[@]+"${CCOPT[@]}"}; do echo "  -ccopt $f"; done
echo ')'
