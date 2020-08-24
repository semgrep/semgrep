#!/bin/bash -e
version="${GITHUB_REF/refs\/tags\//}"
version="${version:-$VERSION}"
version="${version:?Version must be set}"

# strip the v out
docker_tag="${version/v/}"

echo "Validating release with docker tag: $docker_tag"

echo "def silly_eq(a, b):" >> test.py
echo " return a + b == a + b" >> test.py

# shellcheck disable=SC2016
docker run -v "${PWD}:/src" returntocorp/semgrep:"$docker_tag" ./test.py -l python -e '$X == $X' | tee output

grep 'a + b == a + b' output

echo "Docker image OK!"
