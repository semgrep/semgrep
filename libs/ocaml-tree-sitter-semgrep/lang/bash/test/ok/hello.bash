#! /usr/bin/env bash
#
# This is a valid program.
#
set -eu -o pipefail

usage() {
  cat <<EOF
Somebody do $what.
EOF
}

what="something"

while [[ $# -gt 0 ]]; do
  case $1 in
    --help)
      usage
      exit 0
      ;;
    *)
      {
        echo "Invalid argument: $1"
        usage
      } >&2
      exit 1
  esac
  shift
done

(cd ~/..; ls)

: || echo "this wasn't true"

echo hello | tr 'a-z' 'A-Z'

echo goodbye &
wait
