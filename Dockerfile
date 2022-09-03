###############################################################################
# Overview
###############################################################################
#
# First, build a *static* 'semgrep-core' binary on Alpine because it comes set
# up for it (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container which takes care
# of the 'semgrep-python' wrapping.
#

###############################################################################
# Step1: build semgrep-core
###############################################################################
#
# The docker base image below in the FROM currently uses OCaml 4.14.0
# See https://github.com/returntocorp/ocaml-layer/blob/master/configs/alpine.sh
#
# coupling: if you modify the OCaml version there, you probably also need
# to modify:
# - scripts/{osx-release,osx-m1-release,setup-m1-builder}.sh
# - doc/SEMGREP_CORE_CONTRIBUTING.md
# - https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
# Note that many .github/workflows/ use returntocorp/ocaml:alpine, which should
# be the latest, but may differ from this one.
FROM returntocorp/ocaml:alpine-2022-06-09@sha256:99b453a838c9d94414991c0fd7be4711aa1bcc120f576e0f0653c7b921ea9718 as semgrep-core

USER root
# Here is why we need those apk packages:
# - pcre-dev: for ocaml-pcre now used in semgrep-core
# - python3: used during building for processing lang.json
# - python3-dev: for the semgrep Python bridge to build Python C extensions
# TODO: update root image to include python 3.9
RUN apk add --no-cache pcre-dev python3 python3-dev &&\
     pip install --no-cache-dir pipenv==2022.6.7 &&\
     # This mkdir/chown is needed on Arch Linux where the WORKDIR command does not honor
     # the USER directive and all the directories are created as root
     # (as it should be according to the spec, see https://github.com/moby/moby/issues/36677)
     # The COPY --chown=user below are correctly adjusting the permissions of the files
     # except the enclosing directory itself which would be owned by root. This would then
     # prevent some commands (e.g., install-tree-sitter-lib, dune) to create files
     # in the enclosing directory.
     mkdir -p /semgrep/semgrep-core/src/ocaml-tree-sitter-core &&\
     chown -R user /semgrep

USER user

ENV OPAMYES=true

#TODO? we could reduce this to
# COPY --chown=user . /semgrep
# RUN make setup
WORKDIR /semgrep/semgrep-core/src/ocaml-tree-sitter-core
COPY --chown=user semgrep-core/src/ocaml-tree-sitter-core/ .
RUN ./configure \
 && ./scripts/install-tree-sitter-lib

WORKDIR /semgrep/semgrep-core/src/pfff
COPY --chown=user semgrep-core/src/pfff/*.opam ./
WORKDIR /semgrep/semgrep-core/src/ocaml-tree-sitter-core
COPY --chown=user semgrep-core/src/ocaml-tree-sitter-core/*.opam ./
WORKDIR /semgrep/semgrep-core
COPY --chown=user semgrep-core/*.opam ./

RUN opam install --deps-only \
     /semgrep/semgrep-core/src/pfff \
     /semgrep/semgrep-core/src/ocaml-tree-sitter-core \
     /semgrep/semgrep-core

# Copy all the source files needed to build semgrep-core
WORKDIR /semgrep
COPY --chown=user semgrep-core/ ./semgrep-core
COPY --chown=user interfaces/ ./interfaces
COPY --chown=user cli/src/semgrep/lang ./cli/src/semgrep/lang
COPY --chown=user cli/src/semgrep/semgrep_interfaces ./cli/src/semgrep/semgrep_interfaces

# Let's build it
WORKDIR /semgrep/semgrep-core
RUN opam exec -- make minimal-build &&\
     # Sanity check
     /semgrep/semgrep-core/_build/default/src/cli/Main.exe -version

###############################################################################
# Step2: Build the final docker image with Python wrapper and semgrep-core bin
###############################################################################
#
# We change container, bringing the 'semgrep-core' binary with us.
#

FROM python:3.10-alpine AS semgrep-cli

WORKDIR /semgrep

ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
     PIP_NO_CACHE_DIR=true \
     PYTHONIOENCODING=utf8 \
     PYTHONUNBUFFERED=1

# Here is why we need those apk packages:
# - bash: for entrypoint.sh (see below) and probably many other things
# - git, git-lfs, openssh: so that the semgrep docker image can be used in
#   github actions and get git submodules and use ssh to get those submodules
# - libstdc++: for the Python jsonnet binding now used in the semgrep CLI
#   note: do not put libstdc++6, you'll get 'missing library' or 'unresolved
#   symbol' errors
RUN apk add --no-cache --virtual=.run-deps bash git git-lfs openssh libstdc++
COPY cli ./

# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base && \
     SEMGREP_SKIP_BIN=true pip install /semgrep && \
     # running this pre-compiles some python files for faster startup times
     semgrep --version && \
     apk del .build-deps && \
     mkdir -p /tmp/.cache

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Let the user know how their container was built
COPY Dockerfile /Dockerfile

# Get semgrep-core from step1
COPY --from=semgrep-core /semgrep/semgrep-core/_build/default/src/cli/Main.exe /usr/local/bin/semgrep-core

ENV SEMGREP_IN_DOCKER=1 \
     SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version \
     SEMGREP_USER_AGENT_APPEND="Docker"

WORKDIR /src

# /semgrep is not needed anymore;
# semgrep is now available /usr/local/bin thx to the 'pip install' command above
# (this weirdly does not reduce the size of the docker image though)
RUN rm -rf /semgrep

# In case of problems, if you need to debug the docker image, run 'docker build .',
# identify the SHA of the build image and run 'docker run -it <sha> /bin/bash'
# to interactively explore the docker image.
ENTRYPOINT ["/entrypoint.sh"]
CMD ["semgrep", "--help"]
LABEL maintainer="support@r2c.dev"
