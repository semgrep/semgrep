## semgrep-core build

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
RUN eval $(opam env) && cd semgrep-core && opam install -y . && make all
RUN semgrep-core/_build/default/bin/main_sgrep.exe -version

## semgrep build

FROM python:3.7.7-alpine3.11 as build-semgrep
RUN apk add --no-cache python3-dev build-base chrpath
COPY semgrep /home/pythonbuild/semgrep/
WORKDIR /home/pythonbuild/semgrep
RUN make all
RUN ls -al /home/pythonbuild/semgrep/build/semgrep.dist/

## final output, combining both

FROM alpine:3.11.3@sha256:ddba4d27a7ffc3f86dd6c2f92041af252a1f23a8e742c90e6e1297bfa1bc0c45
LABEL maintainer="sgrep@r2c.dev"

ENV PYTHONUNBUFFERED=1

COPY --from=build-semgrep /home/pythonbuild/semgrep/build/semgrep.dist/* /bin/semgrep-files/
RUN ln -s /bin/semgrep-files/semgrep /bin/semgrep

RUN ls -al  /bin/semgrep-files/cacert.pem
RUN mkdir /bin/semgrep-files/certifi/
RUN ln -sfn /bin/semgrep-files/cacert.pem  /bin/semgrep-files/certifi/cacert.pem
RUN ls -al /bin/semgrep-files/

RUN semgrep --help
COPY --from=build-semgrep-core /home/opam/sgrep/semgrep-core/_build/default/bin/main_sgrep.exe /bin/semgrep-core
RUN semgrep-core --help
RUN semgrep --config=r2c /bin/semgrep-files/


ENV SGREP_IN_DOCKER=1
ENV PYTHONIOENCODING=utf8
ENTRYPOINT [ "/bin/semgrep" ]
