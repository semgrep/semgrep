#
# First, build a *static* 'semgrep-core' binary on Alpine because it comes set
# up for it (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container which takes care
# of the 'semgrep-python' wrapping.
#

# The docker base image below in the FROM currently uses OCaml 4.12.0
# See https://github.com/returntocorp/ocaml-layer/blob/master/configs/alpine.sh
#
# coupling: if you modify the OCaml version there, you probably also need
# to modify:
# - scripts/osx-release.sh
# - doc/SEMGREP_CORE_CONTRIBUTING.md
# - https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
# Note that many .github/workflows/ use returntocorp/ocaml:alpine, which should
# be the latest, but may differ from this one.
FROM returntocorp/ocaml:alpine-2022-03-31@sha256:4a42d4c82000df13148a4162d1689b32e8568bc256bf12faa5d8669570ffe8b7 as build-semgrep-core

USER root
# for ocaml-pcre now used in semgrep-core
# TODO: update root image to include python 3.9
RUN apk add --no-cache pcre-dev python3 &&\
     pip install --no-cache-dir pipenv==2021.11.23

USER user
WORKDIR /home/user

COPY --chown=user .gitmodules /semgrep/.gitmodules
COPY --chown=user .git/ /semgrep/.git/
COPY --chown=user semgrep-core/ /semgrep/semgrep-core/
# some .atd files in semgrep-core are symlinks to files in interfaces/
COPY --chown=user interfaces/ /semgrep/interfaces/
# we need this lang/ subdirectory to generate Lang.ml. In theory the data
# should be in interfaces/ but Python does not like symlinks when making
# packages, so interfaces/lang/ is actually a symlink towards
# semgrep/semgrep/lang.
# Note that the 'git submodule --depth 1' below
# would actually checkout semgrep/semgrep/lang directory,
# needed to compile 'semgrep-core' and to run 'semgrep'.
COPY --chown=user semgrep /semgrep/semgrep/
COPY --chown=user scripts /semgrep/scripts

WORKDIR /semgrep

# Protect against dirty environment during development.
# (ideally, we should translate .gitignore to .dockerignore)
#coupling: if you add dependencies above, you probably also need to update:
#  - scripts/install-alpine-semgrep-core
#  - the setup target in Makefile
RUN git clean -dfX && \
     git submodule foreach --recursive git clean -dfX && \
     git submodule update --init --recursive --depth 1 && \
     eval "$(opam env)" && \
     ./scripts/install-tree-sitter-runtime && \
     opam install --deps-only -y semgrep-core/src/pfff/ && \
     opam install --deps-only -y semgrep-core/src/ocaml-tree-sitter-core && \
     opam install --deps-only -y semgrep-core/ && \
     make -C semgrep-core/ all

# Sanity checks
RUN ./semgrep-core/_build/install/default/bin/semgrep-core -version

#
# We change container, bringing the 'semgrep-core' binary and 'semgrep/semgrep' code with us.
#

FROM python:3.10-alpine

WORKDIR /src
LABEL maintainer="support@r2c.dev"
ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
     PIP_NO_CACHE_DIR=true \
     SEMGREP_IN_DOCKER=1 \
     SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version \
     SEMGREP_USER_AGENT_APPEND="Docker" \
     PYTHONIOENCODING=utf8 \
     PYTHONUNBUFFERED=1

COPY --from=build-semgrep-core \
     /semgrep/semgrep-core/_build/install/default/bin/semgrep-core /usr/local/bin/semgrep-core
COPY --from=build-semgrep-core /semgrep/semgrep /semgrep

# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base && \
     apk add --no-cache --virtual=.run-deps bash git git-lfs openssh && \
     SEMGREP_SKIP_BIN=true pip install /semgrep && \
     semgrep --version && \
     apk del .build-deps && \
     mkdir -p /tmp/.cache

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Let the user know how their container was built
COPY dockerfiles/semgrep.Dockerfile /Dockerfile

ENTRYPOINT ["/entrypoint.sh"]
CMD ["semgrep", "--help"]
