#
# First, build a static 'semgrep-core' binary on Alpine because it comes set up
# for it (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container with that takes care
# of the 'semgrep' wrapping.
#

FROM returntocorp/ocaml:alpine-2020-09-11 as build-semgrep-core

USER user
WORKDIR /home/user

COPY --chown=user .gitmodules /semgrep/.gitmodules
COPY --chown=user .git/ /semgrep/.git/
COPY --chown=user semgrep-core/ /semgrep/semgrep-core/
COPY --chown=user scripts /semgrep/scripts

WORKDIR /semgrep

# Protect against dirty environment during development.
# (ideally, we should translate .gitignore to .dockerignore)
RUN git clean -dfX
RUN git submodule foreach --recursive git clean -dfX

RUN git submodule update --init --recursive

RUN eval "$(opam env)" && ./scripts/install-ocaml-tree-sitter
RUN eval "$(opam env)" && opam install --deps-only -y semgrep-core/pfff/
RUN eval "$(opam env)" && opam install --deps-only -y semgrep-core/
RUN eval "$(opam env)" && make -C semgrep-core/ all

# Sanity check
RUN ./semgrep-core/_build/install/default/bin/semgrep-core -version

#
# We change container, bringing only the 'semgrep-core' binary with us.
#

FROM python:3.7.7-alpine3.11
LABEL maintainer="support@r2c.dev"

# ugly: circle CI requires valid git and ssh programs in the container
# when running semgrep on a repository containing submodules
RUN apk add --no-cache git ssh

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
