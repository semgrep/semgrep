#!/usr/bin/env bash

# THIS SHOULD ONLY BE RUN ONCE! Since this is a remote runner, its environment doesn't get reset
# Assuming that brew is already installed
# Install all ocaml things
echo "Warning: This setup is interactive! You must add this machine as a github action runner manually"

echo "Adding current user to wheel group!"
echo "This is needed so the build process can be non-interactive"
sudo dseditgroup -o edit -a build -t user "$(id -un)"
echo "Modifying /usr/local/include/ group permission to be read write execute"
sudo chmod g+rwx /usr/local/include/
echo "Modifying /usr/local/bin/ group permission to be read write execute"
sudo chmod g+rwx /usr/local/bin/
echo "Modifying /usr/local/lib/ group permission to be read write execute"
sudo chmod g+rwx /usr/local/lib/

brew update # Needed to sidestep bintray brownout
# Usually we install python 3.7 for building wheels, but 3.7 isn't supported on m1 macs
brew install opam python
make install-deps-MACOS-for-semgrep-core

opam init --no-setup --bare;
#coupling: this should be the same version than in our Dockerfile
opam switch create 4.14.0;
opam switch 4.14.0;
git submodule update --init --recursive --depth 1

eval "$(opam env)"
# Remove pcre dynamically linked to force MacOS to use static
# This needs to be done before make setup since it is used there
rm /usr/local/opt/pcre/lib/libpcre.1.dylib
rm /usr/local/opt/libev/lib/libev.4.dylib
# Setup python symbolic links
# Brew install won't alias python3 to python :/
ln -s /opt/homebrew/bin/python3 /opt/homebrew/bin/python
ln -s /opt/homebrew/bin/pip3 /opt/homebrew/bin/pip
# Setup action runner in root
cd ~ || exit
# Create a folder
mkdir actions-runner && cd actions-runner || exit
# Download the latest runner package
# This may need to be updated in the future :)
curl -O -L https://github.com/actions/runner/releases/download/v2.292.0/actions-runner-osx-arm64-2.292.0.tar.gz
# Extract the installer
tar xzf ./actions-runner-osx-arm64-2.292.0.tar.gz

# Must be sudo to run jobs
./config.sh
./run.sh
