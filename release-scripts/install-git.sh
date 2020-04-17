cd /usr/src/
sudo wget https://github.com/git/git/archive/v2.18.0.tar.gz -O git.tar.gz
sudo tar -xf git.tar.gz
cd git-*
sudo make prefix=/usr/local all;
sudo make prefix=/usr/local install;
git --version
