## semgrep build

FROM python:3.7.7-alpine3.11 as build-semgrep
RUN apk add git
COPY .git /home/pythonbuild/.git
COPY semgrep /home/pythonbuild/semgrep/
WORKDIR /home/pythonbuild/semgrep
RUN HOMEBREW_SYSTEM='NOCORE' pip install --user .
RUN /root/.local/bin/semgrep --version

## semgrep-core build

FROM ocaml/opam2:alpine@sha256:4c2ce9a181b4b12442a68fc221d0b753959ec80e24eae3bf788eeca4dcb9a293 as build-semgrep-core
USER root
RUN apk add --no-cache perl m4

USER opam

WORKDIR /home/opam/opam-repository
RUN git pull && opam update && opam switch create 4.10.0+musl+static+flambda

COPY --chown=opam . /home/opam/sgrep/
WORKDIR /home/opam/sgrep

RUN git submodule update --init --recursive
RUN eval $(opam env) && opam install -y ./pfff
RUN eval $(opam env) && cd semgrep-core && opam install -y . && make all
RUN semgrep-core/_build/default/bin/main_semgrep_core.exe -version


## final output, combining both

FROM python:3.7.7-alpine3.11
LABEL maintainer="sgrep@r2c.dev"

ENV PYTHONUNBUFFERED=1

COPY --from=build-semgrep /root/.local /root/.local
# Make sure scripts in .local are usable:
ENV PATH=/root/.local/bin:$PATH

RUN semgrep --help
COPY --from=build-semgrep-core /home/opam/sgrep/semgrep-core/_build/default/bin/main_semgrep_core.exe /bin/semgrep-core
RUN semgrep-core --help

ENV SEMGREP_IN_DOCKER=1
ENV PYTHONIOENCODING=utf8
ENTRYPOINT [ "/root/.local/bin/semgrep" ]
