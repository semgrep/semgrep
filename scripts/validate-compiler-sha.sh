#!/usr/bin/env bash
set -xeu

# usage
# ./scripts/validate-compiler-sha.sh [path/to/semgrep-core]
# Checks if the installed ocamlc compiler SHA matches the opam pin version SHA
# if a path was supplied to a semgrep-core binary, checks what the binary was
# built with instead.

# optional arg
PATH_TO_SEMGREP_CORE=${1-}

# what we've pinned in opam
SHA_PINNED_IN_OPAM=$(
  opam pin list --normalise |
  grep -oE 'semgrep/ocaml\.git#[0-9a-f]*' |
  cut -d'#' -f2
)

# the SHA infered from the compiler
SHA_IN_OCAMLC=$(
  opam exec -- ocamlc -version |
  grep -oE "\+semgrep-fork@[a-f0-9]*" |
  cut -d'@' -f2
)

echo "The SHA infered to be pinned in opam was $SHA_PINNED_IN_OPAM"
echo "The SHA infered from the installed ocamlc compiler was $SHA_IN_OCAMLC"

# check that they are non-empty
if [ -z "$SHA_PINNED_IN_OPAM" ] || [ -z "$SHA_IN_OCAMLC" ]; then
  echo "No SHA could be infered from either ocamlc or opam pin"
  exit 1
fi

# check that they're equal
if [ "$SHA_PINNED_IN_OPAM" != "$SHA_IN_OCAMLC" ]; then
    echo "They are not equal!"
    exit 1
fi

# if the path to a binary is not provided exit early
if [ -z "$PATH_TO_SEMGREP_CORE" ]; then
    exit 0
fi

# what semgrep was built with
SHA_SEMGREP_WAS_BUILT_WITH=$(
  ./"$PATH_TO_SEMGREP_CORE" -ocaml_version |
  grep -oE "\+semgrep-fork@[a-f0-9]*" |
  cut -d'@' -f2
)

echo "The SHA infered from the binary was $SHA_SEMGREP_WAS_BUILT_WITH"

if [ -n "$SHA_SEMGREP_WAS_BUILT_WITH" ] && [ "$SHA_PINNED_IN_OPAM" != "$SHA_SEMGREP_WAS_BUILT_WITH" ]; then
    echo "They are not equal!"
    exit 1
fi
