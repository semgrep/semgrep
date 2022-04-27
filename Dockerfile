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
FROM returntocorp/ocaml:alpine-2022-03-31@sha256:4a42d4c82000df13148a4162d1689b32e8568bc256bf12faa5d8669570ffe8b7 as semgrep-core

ENV OPAMYES=true
WORKDIR /semgrep/semgrep-core/src/ocaml-tree-sitter-core
COPY --chown=user semgrep-core/src/ocaml-tree-sitter-core/ .
RUN scripts/install-tree-sitter-lib

WORKDIR /semgrep/semgrep-core/src/pfff
COPY --chown=user semgrep-core/src/pfff/*.opam .
RUN --mount=type=cache,target=~/.opam eval $(opam env) && opam install --deps-only .

WORKDIR /semgrep/semgrep-core/src/ocaml-tree-sitter-core/
COPY --chown=user semgrep-core/src/ocaml-tree-sitter-core/*.opam .
RUN --mount=type=cache,target=~/.opam eval $(opam env) && opam install --deps-only .

WORKDIR /semgrep/semgrep-core
COPY --chown=user semgrep-core/*.opam .
RUN --mount=type=cache,target=~/.opam eval $(opam env) && opam install --deps-only .

WORKDIR /semgrep
COPY --chown=user semgrep-core/ ./semgrep-core
COPY --chown=user interfaces/ ./interfaces
COPY --chown=user semgrep/semgrep/lang ./semgrep/semgrep/lang
ENV DUNE_CACHE_ROOT=~/.dune
# can cache across github actions after once is merged: https://github.com/docker/setup-buildx-action/pull/138
RUN --mount=type=cache,target=~/.dune eval $(opam env) && dune build --cache=enabled

RUN /semgrep/semgrep-core/_build/default/src/cli/Main.exe -version

#
# We change container, bringing the 'semgrep-core' binary with us.
#

FROM python:3.10-alpine AS semgrep-cli

WORKDIR /semgrep
ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
     PYTHONIOENCODING=utf8 \
     PYTHONUNBUFFERED=1

COPY semgrep ./

# hadolint ignore=DL3013
RUN --mount=type=cache,target=/var/cache/apk \
     --mount=type=cache,target=~/.cache/pip \
     apk update && \
     apk add --virtual=.build-deps build-base && \
     apk add --virtual=.run-deps bash git git-lfs openssh && \
     SEMGREP_SKIP_BIN=true pip install '/semgrep[tests]' && \
     semgrep --version && \
     apk del .build-deps && \
     mkdir -p /tmp/.cache

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Let the user know how their container was built
COPY dockerfiles/semgrep.Dockerfile /Dockerfile

COPY --from=semgrep-core /semgrep/semgrep-core/_build/default/src/cli/Main.exe /usr/local/bin/semgrep-core

WORKDIR /src
ENV SEMGREP_IN_DOCKER=1 \
     SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version \
     SEMGREP_USER_AGENT_APPEND="Docker"

ENTRYPOINT ["/entrypoint.sh"]
CMD ["semgrep", "--help"]
LABEL maintainer="support@r2c.dev"
