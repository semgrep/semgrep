#! /usr/bin/env bash
set -e
version="${GITHUB_REF/refs\/tags\//}"
version="${version:-$VERSION}"
version="${version:?Version must be set}"

# strip the v out
docker_tag="${version/v/}"

echo "Validating release with docker tag: $docker_tag"

echo "if 1 == 1: pass" \
    | docker run -i returntocorp/semgrep:"$docker_tag" -l python -e '$X == $X' - \
    | grep -q "1 == 1"

echo "Docker image OK!"
