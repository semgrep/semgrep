#!/bin/bash -e
version="${GITHUB_REF/refs\/tags\//}"
version="${version:-$VERSION}"
version="${version:?Version must be set}"

docker_tag="$(echo $version | sed 's/v//')"

echo "Validating release with docker tag: $docker_tag"

echo "def silly_eq(a, b):" >> test.py
echo " return a + b == a + b" >> test.py

# shellcheck disable=SC2016
docker run -v "${PWD}:/home/repo/" returntocorp/semgrep:"$docker_tag" ./test.py -l python -e '$X == $X' | tee output

cat output | grep 'a + b == a + b'

echo "Docker image OK!"
