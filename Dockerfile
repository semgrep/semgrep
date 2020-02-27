## sgrep build 

FROM ocaml/opam2:alpine@sha256:4c2ce9a181b4b12442a68fc221d0b753959ec80e24eae3bf788eeca4dcb9a293 as build-sgrep
USER root
RUN apk add --no-cache perl m4

USER opam
WORKDIR /home/opam/opam-repository
RUN git pull && opam update && opam switch 4.07 && opam install ocamlfind camlp4 num ocamlgraph json-wheel conf-perl dune yaml
WORKDIR /home/opam/

COPY --chown=opam . /home/opam/sgrep/

RUN git clone https://github.com/returntocorp/pfff
RUN eval $(opam env) && cd pfff && ./configure && make depend && make && make opt && make install-libs#

RUN eval $(opam env); cd sgrep; make all#
RUN /home/opam/sgrep/_build/default/bin/main_sgrep.exe -version

## sgrep lint build

FROM alpine:3.11.3@sha256:ddba4d27a7ffc3f86dd6c2f92041af252a1f23a8e742c90e6e1297bfa1bc0c45 as build-sgrep-lint
RUN apk add --no-cache python3-dev build-base chrpath
RUN pip3 install Nuitka==0.6.7
COPY ./scripts/sgrep_lint/requirements.txt /requirements.txt
RUN pip3 install -r /requirements.txt

# have to manually specify colorama for some reason, but others (yaml, etc) are working ok
RUN python3 -m nuitka --follow-imports --standalone --show-modules --recurse-to=colorama --output-dir /home/opam/sgrep/sgrep-lint-built/ /home/opam/sgrep/scripts/sgrep_lint/sgrep.py
RUN ls -al /home/opam/sgrep/sgrep-lint-built/sgrep.dist/

## final output, combining both

FROM alpine:3.11.3@sha256:ddba4d27a7ffc3f86dd6c2f92041af252a1f23a8e742c90e6e1297bfa1bc0c45
LABEL maintainer="sgrep@r2c.dev"

ENV PYTHONUNBUFFERED=1

COPY --from=build-sgrep-lint /home/opam/sgrep/sgrep-lint-built/sgrep.dist/* /bin/
RUN mv /bin/sgrep /bin/sgrep-lint
RUN sgrep-lint --help

COPY --from=build-sgrep /home/opam/sgrep/_build/default/bin/main_sgrep.exe /bin/sgrep
RUN sgrep --help


ENTRYPOINT [ "/bin/sgrep-lint" ]
