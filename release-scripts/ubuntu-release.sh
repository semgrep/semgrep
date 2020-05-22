#!/bin/bash
set -e

echo "here's some help"
ls
echo "---------"
sudo apt-get install -y --no-install-recommends libcurl4-openssl-dev libexpat1-dev gettext libz-dev libssl-dev build-essential autoconf musl-tools
opam switch create --root /home/opam/.opam 4.10.0+musl+static+flambda;

eval "$(opam env --root /home/opam/.opam --set-root)" && opam install -y ./pfff
eval "$(opam env --root /home/opam/.opam --set-root)" && cd semgrep-core && opam install --deps-only -y . && make all && cd ..

if [[ -z "$SKIP_NUITKA" ]]; then
  eval "$(opam env --root /home/opam/.opam --set-root)" && cd semgrep && export PATH=/github/home/.local/bin:$PATH && sudo make all && cd ..
fi
mkdir -p semgrep-files
cp ./semgrep-core/_build/default/bin/Main.exe semgrep-files/semgrep-core
cp -r ./semgrep/build/semgrep.dist/* semgrep-files
ls semgrep-files
chmod +x semgrep-files/semgrep-core
chmod +x semgrep-files/semgrep
tar -cvzf artifacts.tar.gz semgrep-files/
