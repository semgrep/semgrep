sudo apt-get update && sudo apt-get install -y --no-install-recommends make m4 perl wget swi-prolog mercurial pkg-config build-essential
sudo apt-get install -y zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libbz2-dev
cd /usr/src/
sudo wget https://github.com/git/git/archive/v2.18.0.tar.gz -O git.tar.gz
sudo tar -xf git.tar.gz
cd git-*
sudo make prefix=/usr/local all;
sudo make prefix=/usr/local install;
git --version
