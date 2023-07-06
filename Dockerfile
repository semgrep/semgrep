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
# the 'make install-deps' command further below.
# See https://github.com/returntocorp/ocaml-layer/blob/master/configs/alpine.sh
# for this list of packages.
# Thanks to this container, 'make install-deps' finishes very quickly because it's
# mostly a noop. Alternative base container candidates are:
#
#  - 'ocaml/opam:alpine', the official OCaml/opam Docker image,
#    but building our Docker image would take longer because
#    of all the necessary Semgrep dependencies installed in 'make install-deps'.
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
# An alternative to ocaml-layer would be to use https://depot.dev/
#
# Note that the Docker base image below currently uses OCaml 4.14.0
# coupling: if you modify the OCaml version there, you probably also need
# to modify:
# - scripts/{osx-setup-for-release,setup-m1-builder}.sh
# - doc/SEMGREP_CORE_CONTRIBUTING.md
# - https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
#
# coupling: if you modify the FROM below, you probably need to modify also
# a few .github/workflows/ files. grep for returntocorp/ocaml there.

FROM busybox:stable as semgrep-core-files
WORKDIR /src/semgrep
COPY . .
RUN rm -rf cli js

FROM returntocorp/ocaml:alpine-2023-06-16 as semgrep-core-container

WORKDIR /src/semgrep
COPY --from=semgrep-core-files /src/semgrep .

#TODO: update the root image to include python 3.9 so the apk commands
# run internally in make 'install-deps-alpine-xxx' below are fast too
RUN make install-deps-ALPINE-for-semgrep-core &&\
    make install-deps-for-semgrep-core

# Let's build just semgrep-core
WORKDIR /src/semgrep
# An alternative to the eval is to use 'opam exec -- ...'
RUN eval "$(opam env)" &&\
    make minimal-build &&\
    # Sanity check
    /src/semgrep/_build/default/src/main/Main.exe -version

###############################################################################
# Step2: Build the final docker image with Python wrapper and semgrep-core bin
###############################################################################
# We change container, bringing the 'semgrep-core' binary with us.

FROM python:3.11.3-alpine AS semgrep-cli

WORKDIR /semgrep

#???
ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
    PIP_NO_CACHE_DIR=true \
    PYTHONIOENCODING=utf8 \
    PYTHONUNBUFFERED=1

# Update to the latest packages for the base image.
# This allows to get CVE fixes ASAP, without waiting for new builds of the base image.
# See docker-library/python#761 for an example of such an issue in the past
# where the time between the CVE was discovered and the package update was X days, but
# the new base image was updated only after Y days.
RUN apk update &&\
    apk upgrade


# Here is why we need the apk packages below:
# - libstdc++: for the Python jsonnet binding now used in pysemgrep
#   note: do not put libstdc++6, you'll get 'missing library' or 'unresolved
#   symbol' errors
#   TODO: remove once the osemgrep port is done
# - git, git-lfs, openssh: so that the semgrep docker image can be used in
#   Github actions (GHA) and get git submodules and use ssh to get those submodules
# - bash, curl, jq: various utilities useful in CI jobs (e.g., our benchmark jobs,
#   which needs to use the latest semgrep docker image, also need a few utilities called
#   in some of our bash and python scripts/)
#   alt: we used to have an alternate semgrep-dev.Dockerfile container to use
#   for our benchmarks, but it complicates things and the addition of those
#   packages do not add much to the size of the docker image (<1%).
RUN apk add --no-cache --virtual=.run-deps\
    libstdc++\
    git git-lfs openssh\
    bash curl jq

# We just need the Python code in cli/.
# The semgrep-core stuff would be copied from the other container
COPY cli ./

# Let's now simply use 'pip' to install semgrep.
# Note the difference between .run-deps and .build-deps below.
# We use a single command to install packages, install semgrep, and remove
# packages to keep a small Docker image (classic Docker trick).
# Here is why we need the apk packages below:
#  - build-base: ??
#  - make, g++: to compile the jsonnet C++ library which is installed
#    by 'pip install jsonnet'.
#    TODO: at some point we should not need the 'pip install jsonnet' because
#    jsonnet would be mentioned in the setup.py for semgrep as a dependency.
#    LATER: at some point we would not need at all because of osemgrep/ojsonnet
# TODO? why the mkdir -p /tmp/.cache?
# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base make g++ &&\
     pip install jsonnet &&\
     pip install /semgrep &&\
     # running this pre-compiles some python files for faster startup times
     SEMGREP_SKIP_ARM64_CHECK=1 semgrep --version &&\
     apk del .build-deps &&\
     mkdir -p /tmp/.cache

# Let the user know how their container was built
COPY Dockerfile /Dockerfile

# Get semgrep-core from step1
COPY --from=semgrep-core-container /src/semgrep/_build/default/src/main/Main.exe /usr/local/bin/semgrep-core

RUN ln -s semgrep-core /usr/local/bin/osemgrep

# ???
ENV SEMGREP_IN_DOCKER=1 \
    SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version \
    SEMGREP_USER_AGENT_APPEND="Docker" \
    SEMGREP_SKIP_ARM64_CHECK=1

# The command we tell people to run for testing semgrep in Docker is
#   docker run --rm -v "${PWD}:/src" returntocorp/semgrep semgrep --config=auto
# (see https://semgrep.dev/docs/getting-started/ ), hence the WORKDIR directive below
WORKDIR /src

# 'semgrep' is now available in /usr/local/bin thanks to the 'pip install' command
# above, so let's remove /semgrep which is not needed anymore.
#
# Note that this is only a cleanup. This does not reduce the size of
# the Docker image. Indeed, this is how Docker images work. The state
# of the filesystem after each Docker instruction is called a layer
# and remains available in the final image (similarly to diffs in a
# git history).
# TODO? to save space, we could have another docker build stage like we already
# do between the ocaml build and the Python build.
RUN rm -rf /semgrep

# In case of problems, if you need to debug the docker image, run 'docker build .',
# identify the SHA of the build image and run 'docker run -it <sha> /bin/bash'
# to interactively explore the docker image.
CMD ["semgrep", "--help"]
LABEL maintainer="support@semgrep.com"
