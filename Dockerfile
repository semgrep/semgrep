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
FROM returntocorp/ocaml:alpine-2021-07-15 as build-semgrep-core

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
# semgrep/semgrep/lang. Note that the 'git submodule --depth 1' below
# would actually checkout this directory, but it's better to be explicit here
# about all the things we need to compile semgrep-core.
COPY --chown=user semgrep/semgrep/lang /semgrep/semgrep/semgrep/lang
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
# We change container, bringing only the 'semgrep-core' binary with us.
#

FROM python:3.10.1-alpine3.15@sha256:dce56d40d885d2c8847aa2a278a29d50450c8e3d10f9d7ffeb2f38dcc1eb0ea4
LABEL maintainer="support@r2c.dev"
ENV PIP_DISABLE_PIP_VERSION_CHECK=true PIP_NO_CACHE_DIR=true

# ugly: circle CI requires valid git and ssh programs in the container
# when running semgrep on a repository containing submodules
RUN apk add --no-cache git openssh

COPY --from=build-semgrep-core \
     /semgrep/semgrep-core/_build/install/default/bin/semgrep-core /usr/local/bin/semgrep-core
RUN semgrep-core -version

COPY semgrep /semgrep
# hadolint ignore=DL3013
RUN SEMGREP_SKIP_BIN=true python -m pip install /semgrep && \
     semgrep --version && \
     mkdir -p /src && \
     chmod 777 /src && \
     mkdir -p /tmp/.cache && \
     chmod 777 /tmp/.cache

# Let the user know how their container was built
COPY dockerfiles/semgrep.Dockerfile /Dockerfile

RUN adduser -D -u 1000 semgrep
USER 1000
ENV SEMGREP_IN_DOCKER=1
ENV SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version
ENV SEMGREP_USER_AGENT_APPEND="(Docker)"
ENV PYTHONIOENCODING=utf8
ENV PYTHONUNBUFFERED=1
ENTRYPOINT ["semgrep"]
CMD ["--help"]
