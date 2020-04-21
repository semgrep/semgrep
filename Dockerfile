## sgrep build

FROM ocaml/opam2:alpine@sha256:4c2ce9a181b4b12442a68fc221d0b753959ec80e24eae3bf788eeca4dcb9a293 as build-semgrep-core
USER root
RUN apk add --no-cache perl m4

USER opam

WORKDIR /home/opam/opam-repository
RUN git pull && opam update && opam switch 4.07

COPY --chown=opam . /home/opam/sgrep/
WORKDIR /home/opam/sgrep

RUN git submodule update --init --recursive
RUN eval $(opam env) && opam install -y ./pfff
RUN eval $(opam env) && cd sgrep && opam install -y . && make all
RUN sgrep/_build/default/bin/main_sgrep.exe -version

## sgrep lint build

FROM python:3.7.7-alpine3.11 as build-semgrep
RUN apk add --no-cache python3-dev build-base chrpath
COPY sgrep_lint /home/pythonbuild/sgrep_lint/
WORKDIR /home/pythonbuild/sgrep_lint
RUN make all
RUN ls -al /home/pythonbuild/sgrep_lint/build/sgrep.dist/

## final output, combining both

FROM alpine:3.11.3@sha256:ddba4d27a7ffc3f86dd6c2f92041af252a1f23a8e742c90e6e1297bfa1bc0c45
LABEL maintainer="sgrep@r2c.dev"

ENV PYTHONUNBUFFERED=1

COPY --from=build-semgrep /home/pythonbuild/sgrep_lint/build/sgrep.dist/* /bin/sgrep-lint-files/
RUN ln -s /bin/sgrep-lint-files/sgrep-lint /bin/semgrep
# Keep the old link around for backwards compatibility
RUN ln -s /bin/sgrep-lint-files/sgrep-lint /bin/sgrep-lint

RUN ls -al  /bin/sgrep-lint-files/cacert.pem
RUN mkdir /bin/sgrep-lint-files/certifi/
RUN ln -sfn /bin/sgrep-lint-files/cacert.pem  /bin/sgrep-lint-files/certifi/cacert.pem
RUN ls -al /bin/sgrep-lint-files/

RUN semgrep --help
COPY --from=build-semgrep-core /home/opam/sgrep/sgrep/_build/default/bin/main_sgrep.exe /bin/semgrep-core
RUN semgrep-core --help
RUN semgrep --config=r2c /bin/sgrep-lint-files/


ENV SGREP_IN_DOCKER=1
ENV PYTHONIOENCODING=utf8
ENTRYPOINT [ "/bin/semgrep" ]
