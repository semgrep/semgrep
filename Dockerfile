###############################################################################
# Overview
###############################################################################
# First, we build a fully *static* 'semgrep-core' binary on Alpine. This
# binary does not even depend on Glibc because Alpine uses Musl instead
# which can be statically linked.
#
# Then 'semgrep-core' alone is copied to another Alpine-based container
# which takes care of the 'semgrep-cli' Python wrapping.
#
# We use Alpine because it allows to generate the smallest Docker images.
# We use this two-steps process because *building* semgrep-core itself
# requires lots of tools (ocamlc, gcc, make, etc.), with big containers,
# but those tools are not necessary when *running* semgrep.
# This is a standard practice in the Docker world.
# See https://docs.docker.com/build/building/multi-stage/

###############################################################################
# Step1: build semgrep-core
###############################################################################

# The Docker image below (after the 'FROM') is prepackaged with 'ocamlc',
# 'opam', and lots of packages that are used by semgrep-core and installed in
# the 'make setup' command further below.
# See https://github.com/returntocorp/ocaml-layer/blob/master/configs/alpine.sh
# for this list of packages.
# Thanks to this container, 'make setup' finishes very quickly because it's
# mostly a noop. Alternative base container candidates are:
#
#  - 'ocaml/opam:alpine', the official OCaml/opam Docker image,
#    but building our Docker image would take longer because
#    of all the necessary Semgrep dependencies installed in 'make setup'.
#
#    We build a new Semgrep Docker image on each pull-request (PR) so we don't
#    want to wait 30min each time just for 'docker build' to finish.
#
#    Note also that ocaml/opam:alpine default user is 'opam', not 'root', which
#    is not without problems when used inside Github actions (GHA) or even inside
#    this Dockerfile.
#
#  - 'alpine', the official Alpine Docker image, but this would require some
#    extra 'apk' commands to install opam, and extra commands to setup OCaml
#    with this opam from scratch, and more importantly this would take
#    far more time to finish. Moreover, it is not trivial to work from such
#    a base container as 'opam' itself requires lots of extra
#    tools like gcc, make, which are not provided by default on Alpine.
#
# Note that the Docker base image below currently uses OCaml 4.14.0
# coupling: if you modify the OCaml version there, you probably also need
# to modify:
# - scripts/{osx-release,osx-m1-release,setup-m1-builder}.sh
# - doc/SEMGREP_CORE_CONTRIBUTING.md
# - https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
#
# Note that many .github/workflows/ use returntocorp/ocaml:alpine, which should
# be the latest, but may differ from this one.
FROM returntocorp/ocaml:alpine-2022-09-24 as semgrep-core-container

# Here is why we need those apk packages below:
# - pcre-dev: for ocaml-pcre now used in semgrep-core
# - python3: used also during building semgrep-core for processing lang.json
# - python3-dev: for the semgrep Python bridge to build Python C extensions
# TODO: update the root image to include python 3.9 so those apk are fast too
RUN apk add --no-cache pcre-dev python3 python3-dev &&\
    pip install --no-cache-dir pipenv==2022.6.7

WORKDIR /src/semgrep
COPY . .
#TODO? git clean -dfX?
RUN make setup

# Let's build just semgrep-core
WORKDIR /src/semgrep/semgrep-core
# An alternative to the eval is to use 'opam exec -- ...'
RUN eval "$(opam env)" &&\
    make minimal-build &&\
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

# Here is why we need those apk packages below:
# - bash: for entrypoint.sh (see below) and probably many other things
# - git, git-lfs, openssh: so that the semgrep docker image can be used in
#   Github actions (GHA) and get git submodules and use ssh to get those submodules
# - libstdc++: for the Python jsonnet binding now used in the semgrep CLI
#   note: do not put libstdc++6, you'll get 'missing library' or 'unresolved
#   symbol' errors
RUN apk add --no-cache --virtual=.run-deps\
     bash git git-lfs openssh libstdc++

# We just need the Python code in cli/.
# The semgrep-core stuff would be copied from the other container
COPY cli ./

# Let's now simply use 'pip' to install semgrep.
# Note the difference between .run-deps and .build-deps below.
# TODO? what does --virtual= mean? why all in one command below?
# TODO? why the mkdir -p /tmp/.cache?
# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base &&\
     SEMGREP_SKIP_BIN=true pip install /semgrep &&\
     # running this pre-compiles some python files for faster startup times
     semgrep --version &&\
     apk del .build-deps &&\
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

# The command we tell people to run for testing semgrep in docker is
#   docker run --rm -v "${PWD}:/src" returntocorp/semgrep semgrep --config=auto
# (see https://semgrep.dev/docs/getting-started/ )
# hence this WORKDIR
WORKDIR /src

# semgrep is now available /usr/local/bin thx to the 'pip install' command
# above, so let's remove /semgrep which is not needed anymore. 
#
# Note that this is only a cleanup. This does not reduce the size of
# the Docker image. Indeed, this is how Docker images work. The state
# of the filesystem after each Docker instruction is called a layer
# and remains available in the final image, similarly to diffs in a
# git history. To save space, we'd have to start another docker build
# stage like we already do between the ocaml build and the python
# build.
RUN rm -rf /semgrep

# In case of problems, if you need to debug the docker image, run 'docker build .',
# identify the SHA of the build image and run 'docker run -it <sha> /bin/bash'
# to interactively explore the docker image.
ENTRYPOINT ["/entrypoint.sh"]
CMD ["semgrep", "--help"]
LABEL maintainer="support@r2c.dev"
