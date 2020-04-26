#!/bin/bash
set -e
brew install opam pkg-config coreutils
opam init --no-setup --bare;
opam switch create 4.07.1;
opam switch 4.07.1;
eval "$(opam env)"; opam install -y reason dune ocamlfind camlp4 num ocamlgraph json-wheel conf-perl yaml
git submodule update --init --recursive
eval "$(opam env)" && opam install -y ./pfff
cd semgrep-core && eval "$(opam env)" && opam install -y . && make all && make test && make install && cd ..
mkdir -p artifacts
cp ./semgrep-core/_build/default/bin/main_semgrep_core.exe artifacts/semgrep-core
zip -r artifacts.zip artifacts
