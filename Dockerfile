FROM ocaml/opam2:alpine@sha256:4c2ce9a181b4b12442a68fc221d0b753959ec80e24eae3bf788eeca4dcb9a293 as build
USER root
RUN apk add --no-cache perl m4
USER opam
WORKDIR /home/opam/opam-repository
RUN git pull && opam update && opam switch 4.07 && opam install ocamlfind camlp4 num ocamlgraph json-wheel conf-perl dune yaml
WORKDIR /home/opam/

RUN git clone https://github.com/returntocorp/pfff
RUN eval $(opam env) && cd pfff && git checkout bb3d11515b435bf37bdd1863c2839ef8efde9d82 && ./configure && make depend && make && make opt && make install-libs

RUN git clone https://github.com/returntocorp/sgrep
RUN eval $(opam env) && cd sgrep && git checkout f4368b81ed6a5911943f12b462c630970d258e66 && dune build

FROM alpine:3.11.3@sha256:ddba4d27a7ffc3f86dd6c2f92041af252a1f23a8e742c90e6e1297bfa1bc0c45
LABEL maintainer="sgrep@r2c.dev"

COPY --from=build /home/opam/sgrep/_build/default/bin/main_sgrep.exe /bin/sgrep
ENTRYPOINT [ "/bin/sgrep" ]
