#!/usr/bin/env bash
# THIS SHOULD ONLY BE RUN ONCE! Since this is a remote runner, its environment doesn't get reset
# Assuming that brew is already installed
# Install all ocaml things
echo "Warning: This setup is interactive! You must add this machine as a github action runner manually"
brew update # Needed to sidestep bintray brownout
# Usually we install python 3.7 for building wheels, but 3.7 isn't supported on m1 macs
brew install opam pkg-config coreutils python
opam init --no-setup --bare;
#coupling: this should be the same version than in our Dockerfile
opam switch create 4.12.0;
opam switch 4.12.0;
git submodule update --init --recursive --depth 1

eval "$(opam env)"

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
sudo ./config.sh
sudo ./run.sh
