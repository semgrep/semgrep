# semgrep-core build

FROM ocaml/opam2:alpine@sha256:4c2ce9a181b4b12442a68fc221d0b753959ec80e24eae3bf788eeca4dcb9a293 as build-semgrep-core

USER root
RUN apk add --no-cache perl m4
USER opam

WORKDIR /home/opam/opam-repository
RUN git pull && opam update && opam switch create 4.10.0+musl+static+flambda

COPY --chown=opam .gitmodules /semgrep/.gitmodules
COPY --chown=opam .git/ /semgrep/.git/
COPY --chown=opam pfff/ /semgrep/pfff/
COPY --chown=opam semgrep-core/ /semgrep/semgrep-core/

WORKDIR /semgrep
RUN git submodule update --init --recursive
RUN eval "$(opam env)" && opam install -y pfff/
RUN eval "$(opam env)" && opam install --deps-only -y semgrep-core/ && make -C semgrep-core/ all

# final output

FROM python:3.7.7-alpine3.11
LABEL maintainer="support@r2c.dev"

COPY --from=build-semgrep-core /semgrep/semgrep-core/_build/default/bin/Main.exe /bin/semgrep-core
RUN semgrep-core -version

COPY semgrep /semgrep
RUN HOMEBREW_SYSTEM='NOCORE' python -m pip install /semgrep
RUN semgrep --version

ENV SEMGREP_IN_DOCKER=1
ENV SEMGREP_VERSION_CACHE_PATH=/tmp/.cache/semgrep_version
ENV PYTHONIOENCODING=utf8
ENV PYTHONUNBUFFERED=1
ENTRYPOINT ["semgrep"]
CMD ["--help"]
