#
# First, build a *static* 'semgrep-core' binary on Alpine because it comes set
# up for it (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container which takes care
# of the 'semgrep-python' wrapping.
#

# NOTE: This is just a test file for the Dockerfile parser !
FROM returntocorp/ocaml:fake-tag as build-semgrep-core

USER root
# for ocaml-pcre now used in semgrep-core
# TODO: update root image to include python 3.9
RUN apk add --update --no-cache pcre-dev python3
RUN pip install --no-cache-dir pipenv==2022.6.7

USER user
WORKDIR /home/user

COPY --chown=user .gitmodules /semgrep/.gitmodules
COPY --chown=user .git/ /semgrep/.git/
COPY --chown=user scripts /semgrep/scripts

WORKDIR /semgrep

# Protect against dirty environment during development.
# (ideally, we should translate .gitignore to .dockerignore)
RUN git clean -dfX
RUN git submodule foreach --recursive git clean -dfX

RUN git submodule update --init --recursive --depth 1

#coupling: if you add dependencies here, you probably also need to update:
#  - scripts/install-alpine-semgrep-core
#  - the setup target in Makefile
RUN eval "$(opam env)" && ./scripts/install-tree-sitter-runtime
RUN eval "$(opam env)" && opam install --deps-only -y src/ocaml-tree-sitter-core
RUN eval "$(opam env)" && opam install --deps-only -y .
RUN eval "$(opam env)" && make -C  all

# Sanity checks
RUN ./_build/install/default/bin/semgrep-core -version

#
# We change container, bringing only the 'semgrep-core' binary with us.
#

FROM python:3.9.1-alpine3.13
LABEL maintainer="support@semgrep.com"

# ugly: circle CI requires valid git and ssh programs in the container
# when running semgrep on a repository containing submodules
RUN apk add --no-cache git openssh

COPY --from=build-semgrep-core \
     /semgrep/_build/install/default/bin/semgrep-core /usr/local/bin/semgrep-core
RUN semgrep-core -version

COPY semgrep /semgrep
RUN SEMGREP_SKIP_BIN=true python -m pip install /semgrep
RUN semgrep --version

RUN mkdir -p /src
RUN chmod 777 /src
RUN mkdir -p /tmp/.cache
RUN chmod 777 /tmp/.cache

# Let the user know how their container was built
COPY dockerfiles/semgrep.Dockerfile /Dockerfile

RUN adduser -D -u 1000 semgrep

# The following triggers a parsing bug at the moment
# (see the 'numeric-user.dockerfile' test case)
#USER 1000

ENV SEMGREP_IN_DOCKER=1
ENV SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version
ENV SEMGREP_USER_AGENT_APPEND="(Docker)"
ENV PYTHONIOENCODING=utf8
ENV PYTHONUNBUFFERED=1
ENTRYPOINT ["semgrep"]
CMD ["--help"]
