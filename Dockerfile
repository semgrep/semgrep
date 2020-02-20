FROM ocaml/opam2:alpine@sha256:4c2ce9a181b4b12442a68fc221d0b753959ec80e24eae3bf788eeca4dcb9a293 as build
USER root
RUN apk add --no-cache perl m4 python3
USER opam
WORKDIR /home/opam/opam-repository
RUN git pull && opam update && opam switch 4.07 && opam install ocamlfind camlp4 num ocamlgraph json-wheel conf-perl dune yaml
WORKDIR /home/opam/

COPY --chown=opam . /home/opam/sgrep/

RUN git clone https://github.com/returntocorp/pfff
RUN eval $(opam env) && cd pfff && ./configure && make depend && make && make opt && make install-libs

RUN eval $(opam env); cd sgrep; make all

RUN /home/opam/sgrep/_build/default/bin/main_sgrep.exe -version

FROM alpine:3.11.3@sha256:ddba4d27a7ffc3f86dd6c2f92041af252a1f23a8e742c90e6e1297bfa1bc0c45
RUN apk add --no-cache python3
COPY --from=build /home/opam/sgrep/requirements.txt requirements.txt
RUN pip3 install -r requirements.txt
LABEL maintainer="sgrep@r2c.dev"

ENV PYTHONUNBUFFERED=1

COPY --from=build /home/opam/sgrep/_build/default/bin/main_sgrep.exe /bin/sgrep
COPY --from=build /home/opam/sgrep/sgrep.py /bin/sgrep-lint

ENV SGREP_IN_DOCKER=1
ENTRYPOINT [ "/bin/sgrep-lint" ]
