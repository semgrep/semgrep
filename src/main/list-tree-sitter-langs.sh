#! /usr/bin/env bash
#
# List of tree-sitter parsers into a config file
# This runs from the project root and looks for tree-sitter parsers
# in the standard locations.
#
# Now only used by src/experiments/forge/, which still links every
# tree-sitter language stub statically. The main semgrep-core binaries
# are now dynamically linked and no longer need this list.
set -eu

# Paths are relative to the project root which can be
# semgrep, semgrep-proprietary, or semgrep-proprietary/OSS depending
# on context.
paths=$(echo languages/*/tree-sitter/semgrep-*)

# Extract 'some_lang' from folders named 'semgrep-some-lang'
for path in $paths; do
  semgrep_lang=$(basename "$path")
  echo "${semgrep_lang#semgrep-}" | tr '-' '_' | sort
done
