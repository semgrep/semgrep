#!/bin/bash
set -e

sudo apt-get update && sudo apt-get install -y --no-install-recommends make m4 perl wget swi-prolog mercurial pkg-config build-essential
sudo apt-get install -y zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libbz2-dev
sudo wget https://www.python.org/ftp/python/3.7.7/Python-3.7.7.tar.xz
sudo tar xvf Python-3.7.7.tar.xz
cd Python-3.7.7
sudo ./configure --enable-shared
sudo make altinstall
sudo ldconfig /usr/local/lib
python3.7 --version
which python3.7
/usr/local/bin/python3.7 -c "import bz2; print(bz2.__doc__)"
sudo curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
sudo -H python3.7 get-pip.py
pip3 --version
ldd --version
echo "Pls help me make this at least 3.7"
sudo apt-get install -y --no-install-recommends libcurl4-openssl-dev libexpat1-dev gettext libz-dev libssl-dev build-essential autoconf
cd /usr/src/;
sudo wget https://github.com/git/git/archive/v2.18.0.tar.gz -O git.tar.gz;
sudo tar -xf git.tar.gz;
cd git-*;
sudo make prefix=/usr/local all;
sudo make prefix=/usr/local install;
git --version;
sudo chmod -R 777 . /github
opam switch --root /home/opam/.opam 4.07;
eval "$(opam env --root /home/opam/.opam --set-root)"; opam install -y reason dune ocamlfind camlp4 num ocamlgraph json-wheel conf-perl yaml
git submodule update --init --recursive
eval "$(opam env --root /home/opam/.opam --set-root)" && opam install -y ./pfff
eval "$(opam env --root /home/opam/.opam --set-root)" && cd sgrep && make all
eval "$(opam env --root /home/opam/.opam --set-root)" && cd sgrep_lint && export PATH=/github/home/.local/bin:$PATH && sudo make all
mkdir -p semgrep-lint-files
cp ./sgrep/_build/default/bin/main_sgrep.exe sgrep-lint-files/semgrep
cp -r ./sgrep_lint/build/sgrep.dist/* sgrep-lint-files
chmod +x sgrep-lint-files/semgrep
chmod +x sgrep-lint-files/semgrep-lint
chmod +x sgrep-lint-files/semgrep-lint-exe
tar -cvzf artifacts.tar.gz semgrep-lint-files/
