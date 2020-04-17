#!/bin/bash
set -e

eval "$(opam env --root /home/opam/.opam --set-root)"; opam install -y reason dune ocamlfind camlp4 num ocamlgraph json-wheel conf-perl yaml
git submodule update --init --recursive
eval "$(opam env --root /home/opam/.opam --set-root)" && opam install -y ./pfff
eval "$(opam env --root /home/opam/.opam --set-root)" && cd sgrep && opam install -y . && make all && cd ..
eval "$(opam env --root /home/opam/.opam --set-root)" && cd sgrep_lint && export PATH=/github/home/.local/bin:$PATH && sudo make all && cd ..
mkdir -p semgrep-lint-files
cp ./sgrep/_build/default/bin/main_sgrep.exe sgrep-lint-files/semgrep
cp -r ./sgrep_lint/build/sgrep.dist/* sgrep-lint-files
chmod +x sgrep-lint-files/semgrep
chmod +x sgrep-lint-files/semgrep-lint
chmod +x sgrep-lint-files/__main__
tar -cvzf artifacts.tar.gz semgrep-lint-files/
