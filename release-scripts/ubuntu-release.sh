#!/bin/bash
set -e

echo "here's some help"
ls
echo "---------"
sudo apt-get install -y --no-install-recommends libcurl4-openssl-dev libexpat1-dev gettext libz-dev libssl-dev build-essential autoconf
opam switch --root /home/opam/.opam 4.07;

eval "$(opam env --root /home/opam/.opam --set-root)"; opam install -y reason dune ocamlfind camlp4 num ocamlgraph json-wheel conf-perl yaml
eval "$(opam env --root /home/opam/.opam --set-root)" && opam install -y ./pfff
eval "$(opam env --root /home/opam/.opam --set-root)" && cd semgrep-core && opam install -y . && make all && cd ..
eval "$(opam env --root /home/opam/.opam --set-root)" && cd semgrep && export PATH=/github/home/.local/bin:$PATH && sudo make all && cd ..
mkdir -p semgrep-files
cp ./semgrep-core/_build/default/bin/main_sgrep.exe semgrep-files/semgrep-core
cp -r ./semgrep/build/semgrep.dist/* semgrep-files
ls semgrep-files
# TODO: remove once the Python build job makes something named `semgrep`
chmod +x semgrep-files/semgrep-core
chmod +x semgrep-files/semgrep
tar -cvzf artifacts.tar.gz semgrep-files/
