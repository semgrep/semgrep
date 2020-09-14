#
# First, build a static 'semgrep-core' binary on Alpine because it comes set up
# for it (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container with that takes care
# of the 'semgrep' wrapping.
#

FROM ocaml/opam2:alpine@sha256:4c2ce9a181b4b12442a68fc221d0b753959ec80e24eae3bf788eeca4dcb9a293 as build-semgrep-core

USER root
RUN apk add --no-cache m4
USER opam

WORKDIR /home/opam/opam-repository
RUN git pull && opam update && opam switch create 4.10.0+flambda

COPY --chown=opam .gitmodules /semgrep/.gitmodules
COPY --chown=opam .git/ /semgrep/.git/
COPY --chown=opam semgrep-core/ /semgrep/semgrep-core/
COPY --chown=opam scripts /semgrep/scripts

WORKDIR /semgrep

# Protect against dirty environment during development.
# (ideally, we should translate .gitignore to .dockerignore)
RUN git clean -dfX
RUN git submodule foreach --recursive git clean -dfX

RUN git submodule update --init --recursive
RUN eval "$(opam env)" && ./scripts/install-ocaml-tree-sitter
RUN eval "$(opam env)" && opam install --deps-only -y semgrep-core/pfff/
RUN eval "$(opam env)" && opam install --deps-only -y semgrep-core/ && make -C semgrep-core/ all
RUN ./semgrep-core/_build/install/default/bin/semgrep-core -version

#
# We change container, bringing only the 'semgrep-core' binary with us.
#

FROM python:3.7.7-alpine3.11
LABEL maintainer="support@r2c.dev"

# ugly: circle CI requires a valid git command when running semgrep
# on a repository containing submodules
RUN apk add --no-cache git

COPY --from=build-semgrep-core \
     /semgrep/semgrep-core/_build/install/default/bin/semgrep-core /usr/local/bin/semgrep-core
RUN semgrep-core -version

COPY semgrep /semgrep
RUN HOMEBREW_SYSTEM='NOCORE' python -m pip install /semgrep
RUN semgrep --version

RUN mkdir -p /src
RUN chmod 777 /src
RUN mkdir -p /tmp/.cache
RUN chmod 777 /tmp/.cache

RUN adduser -D -u 1000 semgrep
USER 1000
ENV SEMGREP_IN_DOCKER=1
ENV SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version
ENV PYTHONIOENCODING=utf8
ENV PYTHONUNBUFFERED=1
ENTRYPOINT ["semgrep"]
CMD ["--help"]
