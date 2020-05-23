#!/bin/bash
set -e
brew install opam pkg-config coreutils
opam init --no-setup --bare;
opam switch create 4.10.0;
opam switch 4.10.0;
git submodule update --init --recursive
eval "$(opam env)" && opam install -y ./pfff
mkdir -p artifacts
cd semgrep-core && eval "$(opam env)" && opam install --deps-only -y . && make all && make test && make install && cd ..
if [[ -z "$SKIP_NUITKA" ]]; then
  cd semgrep && sudo make all && cd ..
  cp -r ./semgrep/build/semgrep.dist/* artifacts/
fi
cp ./semgrep-core/_build/default/bin/Main.exe artifacts/semgrep-core
zip -r artifacts.zip artifacts
