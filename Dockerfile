FROM python:3.10-alpine@sha256:bddea3d56e850b245bb09e7197f25a60bedf71b4bf0ee50b0d70f8684dd0d9c3

WORKDIR /src
LABEL maintainer="support@r2c.dev"
ENV PIP_DISABLE_PIP_VERSION_CHECK=true \
     PIP_NO_CACHE_DIR=true \
     SEMGREP_IN_DOCKER=1 \
     SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version \
     SEMGREP_USER_AGENT_APPEND="(Docker)" \
     PYTHONIOENCODING=utf8 \
     PYTHONUNBUFFERED=1

COPY --from=returntocorp/semgrep:develop \
     /usr/local/bin/semgrep-core /usr/local/bin/semgrep-core
COPY semgrep /semgrep

# hadolint ignore=DL3013
RUN apk add --no-cache --virtual=.build-deps build-base && \
     apk add --no-cache --virtual=.run-deps git openssh && \
     SEMGREP_SKIP_BIN=true pip install /semgrep && \
     semgrep --version && \
     apk del .build-deps && \
     mkdir -p /tmp/.cache

# Let the user know how their container was built
COPY dockerfiles/semgrep.Dockerfile /Dockerfile

ENTRYPOINT ["semgrep"]
CMD ["--help"]
