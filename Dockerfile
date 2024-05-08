###############################################################################
# Overview
###############################################################################
# First, we build a fully *static* 'semgrep-core' binary on Alpine. This
# binary does not even depend on Glibc because Alpine uses Musl instead
# which can be statically linked.
#
# Then 'semgrep-core' alone is copied to another Alpine-based container
# which takes care of the 'semgrep-cli' (a.k.a. pysemgrep) Python wrapping.
#
# We use Alpine because it allows to generate small Docker images.
# We use this two-steps process because *building* semgrep-core itself
# requires lots of tools (ocamlc, gcc, make, etc.), with big containers,
# but those tools are not necessary when *running* semgrep.
# This is a standard practice in the Docker world.
# See https://docs.docker.com/build/building/multi-stage/
#
# In case of problems, if you need to debug the docker image, run 'docker build .',
# identify the SHA of the build image and run 'docker run -it <sha> /bin/bash'
# to interactively explore the docker image.

###############################################################################
# Step0: collect files needed to build semgrep-core
###############################################################################

# The semgrep git repository contains the source code to multiple build artifacts
# (semgrep, semgrep-core, semgrep.js, etc...). In order to maximize Docker cache
# hits (and keep the build fast), we only copy over the folders needed to build
# semgrep-core. This is done in a multi-stage build so that the final COPY
# happens in a single layer.

FROM busybox:stable as semgrep-core-files
WORKDIR /src/semgrep

# copy over the entire semgrep repository
COPY . .

# remove files and folders that aren't necessary for the semgrep-core build
# coupling: see the (dirs ...) directive in the toplevel dune file for the list
# of directories containing OCaml code and which should not be added below
# (except js/ which contains OCaml code but is not used to build semgrep-core)
RUN rm -rf cli js .github .circleci Dockerfile

# we *do* need the cli's semgrep_interfaces folder, however
COPY cli/src/semgrep/semgrep_interfaces cli/src/semgrep/semgrep_interfaces

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
#    update: we recently started to cache the ~/.opam/ directory in CI so
#    in theory we could get rid of ocaml-layer and instead use the official
#    opam docker image combined with this ~/.opam/ caching to speedup things.
#
#  - 'alpine', the official Alpine Docker image, but this would require some
#    extra 'apk' commands to install opam, and extra commands to setup OCaml
#    with this opam from scratch, and more importantly this would take
#    far more time to finish. Moreover, it is not trivial to work from such
#    a base container as 'opam' itself requires lots of extra
#    tools like gcc, make, which are not provided by default on Alpine.
#
# An alternative to ocaml-layer would be to use https://depot.dev/
# update: we actually started to use depot.dev to speedup multi-arch (arm)
# docker image, so maybe we could use it to get rid of ocaml-layer
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

# This base image should be updated regularly to maximize the caching
# of opam packages. We don't use a rolling ':latest' tag to ensure
# reproducible builds and fix problems more easily.
#
# Visit https://hub.docker.com/r/returntocorp/ocaml/tags to see the latest
# images available.
#

# TODO: 3.12 is very very old; Alpine released 3.19.0 in 2024.
# But we actually had troubles switching to more recent version in ocaml-layer
# in https://github.com/semgrep/semgrep/pull/10213
# and https://github.com/semgrep/ocaml-layer/pull/43
# So I'm using the last version that is known to work, the one we used
# in returntocorp/ocaml:alpine-2023-10-17
FROM alpine:3.12.0 as semgrep-core-container

# Install the general build packages (independent of semgrep)
#TODO? move those apk commands in Makefile? so we can factorize later in GHA?
RUN apk add --no-cache bash build-base git make opam
RUN opam init --disable-sandboxing -v &&\
    opam switch create 4.14.0 -v

# Install semgrep-core build dependencies
WORKDIR /src/semgrep
COPY --from=semgrep-core-files /src/semgrep/Makefile ./Makefile
COPY --from=semgrep-core-files /src/semgrep/scripts ./scripts
COPY --from=semgrep-core-files /src/semgrep/dune-project ./dune-project
COPY --from=semgrep-core-files /src/semgrep/semgrep.opam ./semgrep.opam
COPY --from=semgrep-core-files /src/semgrep/libs/ocaml-tree-sitter-core/tree-sitter.opam ./libs/ocaml-tree-sitter-core/tree-sitter.opam
COPY --from=semgrep-core-files /src/semgrep/dev ./dev
# note that we do not run 'make install-deps-for-semgrep-core' here because it
# configures and builds ocaml-tree-sitter-core too; here we are
# just concerned about installing external packages to maximize docker caching.
RUN make install-deps-ALPINE-for-semgrep-core &&\
    make install-opam-deps

# Compile (and minimal test) semgrep-core
COPY --from=semgrep-core-files /src/semgrep ./
# Let's build just semgrep-core
#alt: use 'opam exec -- ...' instead of eval
RUN make install-deps-for-semgrep-core &&\
    eval "$(opam env)" &&\
    make minimal-build &&\
    # Sanity check
    /src/semgrep/_build/default/src/main/Main.exe -version

###############################################################################
# Step2: Combine the Python wrapper (pysemgrep) and semgrep-core binary
###############################################################################
# We change container, bringing the 'semgrep-core' binary with us.

#coupling: the 'semgrep-oss' name is used in 'make build-docker'
FROM python:3.11-alpine AS semgrep-oss

WORKDIR /semgrep

# Update to the latest packages for the base image. This allows to get CVE
# fixes ASAP, without waiting for new builds of the base image.
# See docker-library/python#761 for an example of such an issue in the past
# where the time between the CVE was discovered and the package update was
# X days, but the new base image was updated only after Y days.
RUN apk upgrade --no-cache && \
    apk add --no-cache --virtual=.run-deps\
# Try to limit to the minimum the number of packages to install; this reduces
# the attack surface.
#
# history: we used to install here various utilities needed by some of our
# scripts under scripts/. Indeed, those scripts are run from CI jobs using the
# returntocorp/semgrep docker image as the container because they rely on semgrep
# or semgrep-core. Those scripts must also perform different
# tasks that require utilities other than semgrep (e.g., compute parsing
# statistics and then run 'jq' to filter the JSON). It is convenient to add
# them to the docker image, especially because the addition of those packages
# does not add much to the size of the docker image (<1%). However, those utilities
# can have CVEs associated with them. However, some users are already relying on
# those utilities in their own CI workflows so we must strike a balance between
# reducing the attack surface and not breaking existing workflows.
# alt:
#  - we used to have an alternate semgrep-dev.Dockerfile container to use
#    for our benchmarks, but it complicates things
#
# If you need more utilities, it is better to install them in the workflow instead
# (see for example cron-parsing-stats.jsonnet).
#
# See https://docs.docker.com/develop/security-best-practices/ for more info.
#
# Here is why we need the apk packages below:
# - git, git-lfs, openssh: so that the semgrep docker image can be used in
#   Github actions (GHA) and get git submodules and use ssh to get those
#   submodules
# - bash: many users customize their call to semgrep via bash script
# - jq: useful to process the JSON output of semgrep
# - curl: useful to connect to some webhooks
	git git-lfs openssh \
	bash jq curl

# We just need the Python code in cli/.
# The semgrep-core stuff would be copied from the other container
COPY cli ./

#???
ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
    PIP_NO_CACHE_DIR=true \
    PYTHONIOENCODING=utf8 \
    PYTHONUNBUFFERED=1

# Let's now simply use 'pip' to install semgrep.
# Note the difference between .run-deps and .build-deps below.
# We use a single command to install packages, install semgrep, and remove
# packages to keep a small Docker image (classic Docker trick).
# Here is why we need the apk packages below:
#  - build-base: ??
# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base make &&\
     pip install /semgrep &&\
     apk del .build-deps

# Let the user know how their container was built
COPY Dockerfile /Dockerfile

# Get semgrep-core from step1
COPY --from=semgrep-core-container /src/semgrep/_build/default/src/main/Main.exe /usr/local/bin/semgrep-core

RUN ln -s semgrep-core /usr/local/bin/osemgrep

# There are a few places in the CLI where we do different things
# depending on whether we are run from a Docker container.
# See also Semgrep_envvars.ml and Metrics_.mli.
ENV SEMGREP_IN_DOCKER=1 \
    SEMGREP_USER_AGENT_APPEND="Docker"

# The command we tell people to run for testing semgrep in Docker is
#   docker run --rm -v "${PWD}:/src" returntocorp/semgrep semgrep --config=auto
# (see https://semgrep.dev/docs/getting-started/ ), hence this WORKDIR directive
WORKDIR /src

# We don't need the python source anymore; 'pip install /semgrep' above
# installed them under /usr/local/lib/python3.xx/site-packages/semgrep/
RUN rm -rf /semgrep

# It is better to avoid running semgrep as root
# See https://stackoverflow.com/questions/49193283/why-it-is-unsafe-to-run-applications-as-root-in-docker-container
# Note though that the actual USER directive is done in Step 3.
RUN adduser -D -u 1000 -h /home/semgrep semgrep \
    && chown semgrep /src
# Disabling defaulting to the user 'semgrep' for now
# See the nonroot build stage below.
#USER semgrep

# Workaround for rootless containers as git operations may fail due to dubious
# ownership of /src
RUN printf "[safe]\n	directory = /src"  > ~root/.gitconfig
RUN printf "[safe]\n	directory = /src"  > ~semgrep/.gitconfig && \
	chown semgrep:semgrep ~semgrep/.gitconfig

# Note that we just use CMD below. Why not using ENTRYPOINT ["semgrep"] ?
# so that people can simply run
# `docker run --rm -v "${PWD}:/src" returntocorp/semgrep --help` instead of
# `docker run --rm -v "${PWD}:/src" returntocorp/semgrep semgrep --help`?
# (It's even worse now that we've switched company name with
# `docker run --rm -v "${PWD}:/src" semgrep/semgrep semgrep --help`, we now
# have three semgrep, hmmm).
#
# This is mainly to play well with CI providers like Gitlab. Indeed,
# gitlab CI sets up all CI jobs by first running other commands in the
# container; setting an ENTRYPOINT would break those commands and cause jobs
# to fail on setup, and would require users to set a manual override of the
# image's entrypoint in a .gitlab-ci.yml.
# => Simpler to not have any ENTRYPOINT, even it means forcing the user
# to repeat multiple times semgrep in the docker command line.
CMD ["semgrep", "--help"]
LABEL maintainer="support@semgrep.com"

###############################################################################
# Step3: install semgrep-pro
###############################################################################
# This builds a semgrep docker image with semgrep-pro already included,
# to save time in CI as one does not need to wait 2min each time to
# download it (it also reduces our cost to S3).
# This step is valid only when run from Github Actions (it needs a secret)
# See .github/workflows/build-test-docker.jsonnet and release.jsonnet

#coupling: the 'semgrep-cli' name is used in release.jsonnet
FROM semgrep-oss AS semgrep-cli

RUN --mount=type=secret,id=SEMGREP_APP_TOKEN SEMGREP_APP_TOKEN=$(cat /run/secrets/SEMGREP_APP_TOKEN) semgrep install-semgrep-pro --debug

# Clear out any detritus from the pro install (especially credentials)
RUN rm -rf /root/.semgrep

# This was the final step! This is what we ship to users!

###############################################################################
# optional: nonroot variant
###############################################################################
# Additional build stage that sets a non-root user.
# We can't make this the default in the semgrep-cli stage above because of
# permissions errors on the mounted volume when using instructions for running
# semgrep with docker:
#   `docker run -v "${PWD}:/src" -i returntocorp/semgrep semgrep`

#coupling: the 'nonroot' name is used in release.jsonnet
FROM semgrep-cli AS nonroot

# We need to move the core binary out of the protected /usr/local/bin dir so
# the non-root user can run `semgrep install-semgrep-pro` and use Pro Engine
# alt: we could also do this work directly in the root docker image.
# TODO? now that we install semgrep-pro in step4, do we still need that?
RUN rm /usr/local/bin/osemgrep && \
    mkdir /home/semgrep/bin && \
    mv /usr/local/bin/semgrep-core /home/semgrep/bin && \
    ln -s semgrep-core /home/semgrep/bin/osemgrep && \
    chown semgrep:semgrep /home/semgrep/bin

# Update PATH with new core binary location
ENV PATH="$PATH:/home/semgrep/bin"

USER semgrep

###############################################################################
# Other target: Build the semgrep Python wheel
###############################################################################
# This is a target used for building Python wheels. Semgrep users
# don't need to use this.

#coupling: 'semgrep-wheel' is used in build-test-manylinux-aarch64.jsonnet
FROM python:3.11-alpine AS semgrep-wheel

WORKDIR /semgrep

# Install some deps:
#  - build-base because ruamel.yaml has native code
#  - libffi-dev is needed for installing Python dependencies in
#    scripts/build-wheels.sh on arm64
RUN apk add --no-cache build-base zip bash libffi-dev

# Copy in the CLI
COPY cli ./cli

# Copy in semgrep-core executable
COPY --from=semgrep-core-container /src/semgrep/_build/default/src/main/Main.exe cli/src/semgrep/bin/semgrep-core

# Copy in scripts folder
COPY scripts/ ./scripts/

# Build the source distribution and binary wheel, validate that the wheel
# installs correctly. We're only checking the musllinux wheel because this is
# an Alpine container. It should not be a problem because the content of the
# wheels are identical.
RUN scripts/build-wheels.sh && scripts/validate-wheel.sh cli/dist/*musllinux*.whl

###############################################################################
# Other target: performance testing
###############################################################################

# Build target that exposes the performance benchmark tests in perf/ for
# use in running performance benchmarks from a test build container, e.g., on PRs
#coupling: the 'performance-tests' name is used in tests.jsonnet
FROM semgrep-cli AS performance-tests

COPY perf /semgrep/perf

RUN apk add --no-cache make

WORKDIR /semgrep/perf

ENTRYPOINT ["make"]
