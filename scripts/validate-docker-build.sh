#! /usr/bin/env bash
#
# Run a semgrep test on a Docker image to ensure it works.
#
set -eu -o pipefail

prog_name=$(basename "$0")
default_image=semgrep

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
  if [[ $# -gt 2 ]]; then
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

platform=${2:-}

docker_args=()

if [[ -n $platform ]]; then
  docker_args+=(--platform "$platform")
fi

echo "Running just the image should print help without error"
docker run "${docker_args[@]}" "$image"
echo " -> OK"

echo "Random valid shell commands should run ok"
docker run "${docker_args[@]}" "$image" echo -l -a -t -r -v -e -f
echo " -> OK"

echo "Semgrep should run when a config is passed"
docker run "${docker_args[@]}" "$image" semgrep --config=p/ci --help
echo " -> OK"

echo "Semgrep should run when just help is requested"
docker run "${docker_args[@]}" "$image" semgrep --help
echo " -> OK"

echo "Semgrep should run when a subcommand is passed"
docker run "${docker_args[@]}" "$image" semgrep ci --help
docker run "${docker_args[@]}" "$image" semgrep publish --help
echo " -> OK"

echo "Semgrep should be able to return findings (stdin)"
result=$(echo "if 1 == 1: pass" | docker run "${docker_args[@]}" -i "$image" semgrep -l python -e '$X == $X' -)
echo "${result}" | grep -q "1 == 1"
echo " -> OK"

echo "Semgrep should be able to return findings (file)"
TEMP_DIR=$(mktemp -d)
echo "if 1 == 1: pass" > "${TEMP_DIR}/bar.py"
result=$(docker run "${docker_args[@]}" -v "${TEMP_DIR}:/src" -i "$image" semgrep -l python -e '$X == $X')
echo "${result}" | grep -q "1 == 1"
echo " -> OK"

#TODO: learn how to write for loops in bash :)
echo "Bash should be in the docker image"
docker run "${docker_args[@]}" "$image" bash --version
echo " -> OK"

echo "Jq should be in the docker image"
docker run "${docker_args[@]}" "$image" jq --version
echo " -> OK"

echo "Curl should be in the docker image"
docker run "${docker_args[@]}" "$image" curl --version
echo " -> OK"
