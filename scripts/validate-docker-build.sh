#! /usr/bin/env bash
#
# Run a semgrep test on a Docker image to ensure it works.
#
set -eu -o pipefail

prog_name=$(basename "$0")
default_image=semgrep-dev

usage() {
  cat <<EOF
Usage: $prog_name [IMAGE]
Run basic tests to ensure a semgrep docker image is usable.
Default docker image to use: $default_image
EOF
}

error() {
  cat >&2 <<EOF
Error in $(basename "$0"): $*
EOF
  usage >&2
  exit 1
}

image=$default_image
if [[ $# -gt 0 ]]; then
  if [[ $# -gt 1 ]]; then
    error "too many arguments"
  fi
  case "$1" in
    --help)
      usage
      exit 0
      ;;
    *)
      image=$1
  esac
fi

echo "if 1 == 1: pass" \
    | docker run -i "$image" -l python -e '$X == $X' - \
    | grep -q "1 == 1"
