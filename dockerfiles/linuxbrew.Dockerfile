# Explore Homebrew on Linux.
#
# Usage:
#
#  $ docker build -t brew -f linuxbrew.Dockerfile .
#  $ docker run -it brew
#
# Homebrew is a git-based package manager widely used on MacOS.
# This dockerfile is meant for those of us who are on call and need to figure
# certain things about the Semgrep release for Homebrew.
#
# This file is excluded from hadolint checks due to too many irrelevant
# findings.

# Basic setup for brew
FROM debian
RUN apt-get -y update
RUN apt-get -y install curl git procps build-essential
RUN curl -O https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
RUN bash install.sh
RUN echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >> ~/.bashrc

# Example
RUN eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)" \
 && brew install tree-sitter
