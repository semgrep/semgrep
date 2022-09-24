###############################################################################
# Overview
###############################################################################
# First, build a *static* 'semgrep-core' binary on Alpine
# (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container which takes care
# of the 'semgrep-cli' Python wrapping.

###############################################################################
# Step1: build semgrep-core
###############################################################################
# The docker base image below in the FROM currently uses OCaml 4.14.0
# See https://github.com/returntocorp/ocaml-layer/blob/master/configs/alpine.sh
# coupling: if you modify the OCaml version there, you probably also need
# to modify:
# - scripts/{osx-release,osx-m1-release,setup-m1-builder}.sh
# - doc/SEMGREP_CORE_CONTRIBUTING.md
# - https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
# Note that many .github/workflows/ use returntocorp/ocaml:alpine, which should
# be the latest, but may differ from this one.
FROM returntocorp/ocaml:alpine-2022-09-24 as semgrep-core-container

# Here is why we need those apk packages:
# - pcre-dev: for ocaml-pcre now used in semgrep-core
# - python3: used during building for processing lang.json
# - python3-dev: for the semgrep Python bridge to build Python C extensions
# TODO: update root image to include python 3.9
RUN apk add --no-cache pcre-dev python3 python3-dev &&\
     pip install --no-cache-dir pipenv==2022.6.7

WORKDIR /src/semgrep
COPY . .
#TODO? git clean -dfX?
RUN make setup

# Let's build just semgrep-core
WORKDIR /src/semgrep/semgrep-core
# An alternative to the eval is to use 'opam exec -- ...'
RUN eval $(opam env) && make minimal-build &&\
     # Sanity check
     /src/semgrep/semgrep-core/_build/default/src/cli/Main.exe -version

###############################################################################
# Step2: Build the final docker image with Python wrapper and semgrep-core bin
###############################################################################
# We change container, bringing the 'semgrep-core' binary with us.

FROM python:3.10-alpine AS semgrep-cli

WORKDIR /semgrep

#???
ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
     PIP_NO_CACHE_DIR=true \
     PYTHONIOENCODING=utf8 \
     PYTHONUNBUFFERED=1

# Here is why we need those apk packages:
# - bash: for entrypoint.sh (see below) and probably many other things
# - git, git-lfs, openssh: so that the semgrep docker image can be used in
#   Github actions and get git submodules and use ssh to get those submodules
# - libstdc++: for the Python jsonnet binding now used in the semgrep CLI
#   note: do not put libstdc++6, you'll get 'missing library' or 'unresolved
#   symbol' errors
RUN apk add --no-cache --virtual=.run-deps bash git git-lfs openssh libstdc++

# We just need the Python code in cli/.
# The semgrep-core stuff would be copied from the other container
COPY cli ./

# Use pip to install semgrep
# Note the difference between .run-deps and .build-deps below.
# TODO? what does --virtual= mean? why all in one command below?
# TODO? why the mkdir -p /tmp/.cache?
# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base && \
     SEMGREP_SKIP_BIN=true pip install /semgrep && \
     # running this pre-compiles some python files for faster startup times
     semgrep --version && \
     apk del .build-deps && \
     mkdir -p /tmp/.cache

# TODO: we should remove this (we were supposed to get rid of it in June 2022)
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Let the user know how their container was built
COPY Dockerfile /Dockerfile

# Get semgrep-core from step1
COPY --from=semgrep-core-container /src/semgrep/semgrep-core/_build/default/src/cli/Main.exe /usr/local/bin/semgrep-core

# ???
ENV SEMGREP_IN_DOCKER=1 \
     SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version \
     SEMGREP_USER_AGENT_APPEND="Docker"

# /semgrep is not needed anymore;
# semgrep is now available /usr/local/bin thx to the 'pip install' command above
# TODO: this weirdly does not reduce the size of the docker image though
RUN rm -rf /semgrep

# In case of problems, if you need to debug the docker image, run 'docker build .',
# identify the SHA of the build image and run 'docker run -it <sha> /bin/bash'
# to interactively explore the docker image.
ENTRYPOINT ["/entrypoint.sh"]
CMD ["semgrep", "--help"]
LABEL maintainer="support@r2c.dev"
