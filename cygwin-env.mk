# Environment configuration when running under Cygwin on Windows
ifeq ($(shell uname -o),Cygwin)
  EXE := .exe

  # TODO: Remove when https://github.com/semgrep/testo/issues/124 is fixed
  # Run tests in serial on Windows to prevent deadlocks
  TEST_WORKERS := -j 0

  # Skip running some commands (e.g., tests) on Windows
  ifndef SKIP_ON_WINDOWS
    # Set to true only if it has not been overriden from the make invocation
    # environment. This allows overriding the skip by calling make
    # SKIP_ON_WINDOWS=false <some-target>
    SKIP_ON_WINDOWS := true
  endif

  # The make variables[1] $(CC) and $(AR) are commonly used to identify the C
  # compiler and the archive-maintaining program (ar), respectively. The names
  # of these tools may be prefixed by the target triplet in case of
  # cross-compilation, as in `x86_64-w64-mingw32-gcc`, with the triplet
  # (`x86_64`, `w64`, `mingw32`).  These cannot in general be determined from
  # the environment alone. However, since opam is a prerequisite for our
  # project, we can ask it which C compiler is used by the OCaml toolchain
  # installed in the current opam switch, and use it to infer the complete name
  # for the ar utility and the C compiler.
  #
  # Users invoking this Makefile can always set CC and AR with `make CC=...
  # AR=...` to override the configurations inferred here.
  #
  # Both CC and AR are exported to ensure they are propagated into sub-make
  # invocations.
  #
  # [1]: https://www.gnu.org/software/make/manual/html_node/Implicit-Variables.html
  ifneq ($(CC),)
	export CC = $(shell \
	  opam var sys-ocaml-cc 2>/dev/null || \
	  opam exec -- ocamlopt -config-var c_compiler 2>/dev/null || \
	  opam exec -- ocamlc -config-var c_compiler 2>/dev/null)
  endif
  ifneq ($(AR),)
	# Derive the name of the ar executable from the C compiler executable by
	  # suffixing `ar` to the compiler's target triplet. E.g., if the C compiler
	  # is `x86_64-w64-mingw32-gcc` the inferred ar is `x86_64-w64-mingw32-ar`
	export AR = $(shell printf "%s" "$(CC)" | sed -E 's/(-?)[^-]*$$/\1ar/')
  endif
endif

# This macro can be used to skip steps in a Makefile recipe.
# E.g., in the recipe
#
#	    foo: bar
#	        $(call skip_on_windows, ./non-portable-test.sh)
#	        ./portable-test.sh
#
# The `./non-portable-test.sh` will be skipped whenever
# SKIP_ON_WINDOWS is true.
ifeq ($(SKIP_ON_WINDOWS),true)
  skip_on_windows = @echo "Skipping $(1) on Windows"
else
  skip_on_windows = $(1)
endif
