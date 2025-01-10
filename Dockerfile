###############################################################################
# Overview
###############################################################################
# Dockerfile to build the semgrep/semgrep:canary docker image
# (see .github/workflows/{build-test-docker,push-docker}.jsonnet).
#
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
# We use static linking because we can and removing external library
# dependencies is usually simpler (especially since the docker container
# where we build semgrep-core is not the same container where we run it).
#
# In case of problems, if you need to debug the docker image, run 'docker build .',
# identify the SHA of the build image and run 'docker run -it <sha> /bin/bash'
# to interactively explore the docker image before that failing point.

###############################################################################
# Step0: collect files needed to build semgrep-core
###############################################################################

# The semgrep git repository contains the source code to multiple build artifacts
# (pysemgrep, semgrep-core, etc...). In order to maximize Docker cache
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
RUN rm -rf cli .github .circleci Dockerfile

# we *do* need the cli's semgrep_interfaces folder, however
COPY cli/src/semgrep/semgrep_interfaces cli/src/semgrep/semgrep_interfaces

###############################################################################
# Step1: build semgrep-core
###############################################################################

# We're now using a simple alpine:3.19 image in the FROM below.
# TL;DR this used to be too slow but our use of https://depot.dev to accelerate
# our docker build made this a viable and simpler option.
#
# The possible base container candidates are:
#
#  - 'alpine', the official Alpine Docker image. This requires some
#    extra 'apk' commands to install opam, and extra commands to setup OCaml
#    with this opam from scratch. Moreover, 'opam' itself requires lots of extra
#    tools like gcc, make, which are not provided by default on Alpine.
#
#    In theory, this can make a docker build really slow, like 30min, especially
#    in Github Actions (GHA).
#    We build a new Semgrep Docker image on each pull-request (PR) so we don't
#    want to wait 30min each time just for 'docker build' to finish.
#    Fortunately, our use of https://depot.dev allows us to cache intermediate
#    steps which usually make the whole docker build to finish in a few minutes.
#
#  - 'ocaml/opam:alpine', the official OCaml/opam Docker image,
#    but building our Docker image would still take time without depot.dev because
#    of all the necessary Semgrep dependencies installed in 'make install-deps'.
#
#    Note also that ocaml/opam:alpine default user is 'opam', not 'root', which
#    is not without problems when used inside Github actions (GHA) or even inside
#    this Dockerfile.
#
#    update: we recently started to cache the ~/.opam/ directory in most of our
#    CI workflows and started to use the official actions/setup-ocaml@v2 which
#    works pretty well and allowed us to get rid of ocaml-layer.
#
#  - 'returntocorp/ocaml:alpine-xxx', which comes from
#    https://github.com/returntocorp/ocaml-layer/blob/master/configs/alpine.sh
#
#    We used this base container for a very long time (before switching to
#    basic alpine). This Docker image is prepackaged with 'ocamlc','opam', and
#    lots of packages that are used by semgrep-core and installed in the
#    'make install-deps' command. Thanks to this container, 'make install-deps'
#    was finishing very quickly because it was mostly a noop.
#
#    However, it was another repository to modify each time we wanted to
#    add a package or we wanted to switch to a different OCaml version.
#    Being able to control everything from a single Dockerfile is simpler.

FROM alpine:3.19 as semgrep-core-container

# Install opam and basic build tools
RUN apk add --no-cache bash build-base git make opam

# coupling: if you modify the OCaml version there, you probably also need
# to modify:
# - .github/workflows/libs/semgrep.libsonnet
# - scripts/{osx-setup-for-release,setup-m1-builder}.sh
# - doc/SEMGREP_CORE_CONTRIBUTING.md
# - https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
#TODO: switch to 5.2.0 at some point
RUN opam init --disable-sandboxing -v && opam switch create 4.14.0 -v

# Install semgrep-core build dependencies
WORKDIR /src/semgrep
# Just copy enough so that the `make install-xxx` below can work
COPY --from=semgrep-core-files /src/semgrep/Makefile ./Makefile
COPY --from=semgrep-core-files /src/semgrep/scripts ./scripts
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

# Start from scratch with a fresh Alpine image. We used to use
# `python:3.11-alpine` but want to avoid shipping a bunch of unneeded Python
# packages in our production image. Instead we'll install exactly what we need.
#
# TODO: Update beyond Alpine 3.19 to pick up Python versions newer than 3.11

#coupling: the 'semgrep-oss' name is used in 'make build-docker'
FROM alpine:3.19 AS semgrep-oss

WORKDIR /pysemgrep

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
# semgrep/semgrep docker image as the container because they rely on semgrep
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
# - python3: to run pysemgrep
# - py3-setuptools: necessary runtime dependency for opentelemetry
	git git-lfs openssh \
	bash jq curl python3 py3-setuptools

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
#
# Using --break-system-packages so that Semgrep is installed globally in this
# container. Given that Alpine doesn't even ship with Python off the shelf and
# we are installing it only to run Semgrep, the risk of unintended consequences
# here is minimal.
#
# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base make py3-pip && \
     pip install /pysemgrep --break-system-packages &&\
     apk del .build-deps

# Get semgrep-core from step1
COPY --from=semgrep-core-container /src/semgrep/_build/default/src/main/Main.exe /usr/local/bin/semgrep-core

# We don't need the python source anymore; 'pip install ...' above
# installed them under /usr/local/lib/python3.xx/site-packages/semgrep/
RUN ln -s semgrep-core /usr/local/bin/osemgrep && rm -rf /pysemgrep



# Let the user know how their container was built
COPY Dockerfile /Dockerfile

# There are a few places in the CLI where we do different things
# depending on whether we are run from a Docker container.
# See also Semgrep_envvars.ml and Metrics_.mli.
ENV SEMGREP_IN_DOCKER=1 \
    SEMGREP_USER_AGENT_APPEND="Docker"

# The command we tell people to run for testing semgrep in Docker is
#   docker run --rm -v "${PWD}:/src" semgrep/semgrep semgrep --config=auto
# (see https://semgrep.dev/docs/getting-started/ ), hence this WORKDIR directive
WORKDIR /src

# It is better to avoid running semgrep as root
# See https://stackoverflow.com/questions/49193283/why-it-is-unsafe-to-run-applications-as-root-in-docker-container
# Note though that the actual USER directive is done in Step 3.
RUN adduser -D -u 1000 -h /home/semgrep semgrep \
    && chown semgrep /src
# stay with ROOT for now (see the nonroot step below)
#USER semgrep

# Workaround for rootless containers as git operations may fail due to dubious
# ownership of /src
RUN printf "[safe]\n	directory = /src"  > ~root/.gitconfig
RUN printf "[safe]\n	directory = /src"  > ~semgrep/.gitconfig && \
	chown semgrep:semgrep ~semgrep/.gitconfig

# Note that we just use CMD below. Why not using ENTRYPOINT ["semgrep"] ?
# so that people can simply run
# `docker run --rm -v "${PWD}:/src" semgrep/semgrep --help` instead of
# `docker run --rm -v "${PWD}:/src" semgrep/semgrep semgrep --help`?
# (Yes, that's 3 semgrep in a row, hmmm)
#
# This is mainly to play well with CI providers like Gitlab. Indeed,
# gitlab CI sets up all CI jobs by first running other commands in the
# container; setting an ENTRYPOINT would break those commands and cause jobs
# to fail on setup, and would require users to set a manual override of the
# image's entrypoint in a .gitlab-ci.yml.
# => Simpler to not have any ENTRYPOINT, even it means forcing the user
# to repeat multiple times semgrep in the docker command line.
# Using CMD instead gives them a default command when nothing is
# passed to the container, but at the same time still allows users
# to run the container interactively.
# For example,
#   docker run semgrep/semgrep
# will show the help text, but
#   docker run -it semgrep/semgrep /bin/bash
# will let users bring up a bash session.
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

# Expects to find a secret named SEMGREP_APP_TOKEN in Github Actions. To run
# locally, set the SEMGREP_APP_TOKEN environment variable and then run:
#
# $ docker build --secret id=SEMGREP_APP_TOKEN ...
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
#   `docker run -v "${PWD}:/src" -i semgrep/semgrep semgrep`

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
