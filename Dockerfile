#
# First, build a *static* 'semgrep-core' binary on Alpine because it comes set
# up for it (requires using musl rather than glibc).
#
# Then 'semgrep-core' alone is copied to a container which takes care
# of the 'semgrep-python' wrapping.
#

# The docker base image below in the FROM currently uses OCaml 4.14.0
# See https://github.com/returntocorp/ocaml-layer/blob/master/configs/alpine.sh
#
# coupling: if you modify the OCaml version there, you probably also need
# to modify:
# - scripts/{osx-release,osx-m1-release,setup-m1-builder}.sh
# - doc/SEMGREP_CORE_CONTRIBUTING.md
# - https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb
# Note that many .github/workflows/ use returntocorp/ocaml:alpine, which should
# be the latest, but may differ from this one.
FROM returntocorp/ocaml:alpine-2022-06-09@sha256:99b453a838c9d94414991c0fd7be4711aa1bcc120f576e0f0653c7b921ea9718 as semgrep-core

USER root
# for ocaml-pcre now used in semgrep-core
# TODO: update root image to include python 3.9
RUN apk add --no-cache pcre-dev python3 python3-dev
RUN pip install --no-cache-dir pipenv==2022.6.7

USER user

ENV OPAMYES=true

# TODO: why copy the current dir to /semgrep and run the command from there?
COPY --chown=user . /semgrep
WORKDIR /semgrep
RUN make setup

WORKDIR /semgrep/semgrep-core
RUN opam exec -- make minimal-build

RUN /semgrep/semgrep-core/_build/default/src/cli/Main.exe -version

#
# We change container, bringing the 'semgrep-core' binary with us.
#

FROM python:3.10-alpine AS semgrep-cli

WORKDIR /semgrep

ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
     PIP_NO_CACHE_DIR=true \
     PYTHONIOENCODING=utf8 \
     PYTHONUNBUFFERED=1

RUN apk add --no-cache --virtual=.run-deps bash git git-lfs openssh
COPY cli ./

# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base
RUN SEMGREP_SKIP_BIN=true pip install /semgrep
# running this pre-compiles some python files for faster startup times
RUN semgrep --version && apk del .build-deps && mkdir -p /tmp/.cache

# those files are not needed anymore
RUN rm -rf /semgrep

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Let the user know how their container was built
COPY Dockerfile /Dockerfile

COPY --from=semgrep-core /semgrep/semgrep-core/_build/default/src/cli/Main.exe /usr/local/bin/semgrep-core

ENV SEMGREP_IN_DOCKER=1 \
     SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version \
     SEMGREP_USER_AGENT_APPEND="Docker"

WORKDIR /src

ENTRYPOINT ["/entrypoint.sh"]
CMD ["semgrep", "--help"]
LABEL maintainer="support@r2c.dev"
