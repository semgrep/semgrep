#
# First, build a static 'semgrep-core' binary on Alpine because it comes set up
# for it (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container with that takes care
# of the 'semgrep' wrapping.
#

# Pinned version of ocaml/opam2:alpine-3.12-ocaml-4.10 as of 8/27/2020
FROM ocaml/opam2@sha256:58d4f8d57b671b4f273c9e97466d1d594dae7ca2b3829a1573d5e97eba8597cb as build-semgrep-core

USER root
RUN apk add --no-cache perl m4
USER opam

WORKDIR /home/opam/opam-repository
RUN opam update && opam switch create 4.10.0+flambda

COPY --chown=opam .gitmodules /semgrep/.gitmodules
COPY --chown=opam .git/ /semgrep/.git/
COPY --chown=opam pfff/ /semgrep/pfff/
COPY --chown=opam semgrep-core/ /semgrep/semgrep-core/
COPY --chown=opam scripts /semgrep/scripts

WORKDIR /semgrep

# Protect against dirty environment during development.
# (ideally, we should translate .gitignore to .dockerignore)
RUN git clean -dfX
RUN git submodule foreach --recursive git clean -dfX

RUN git submodule update --init --recursive
RUN eval "$(opam env)" && ./scripts/install-ocaml-tree-sitter
RUN eval "$(opam env)" && opam install -y pfff/
RUN eval "$(opam env)" && opam install --deps-only -y semgrep-core/ && make -C semgrep-core/ all
RUN ./semgrep-core/_build/install/default/bin/semgrep-core -version

#
# We change container, bringing only the 'semgrep-core' binary with us.
#

FROM python:3.7.7-alpine3.11
LABEL maintainer="support@r2c.dev"

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
