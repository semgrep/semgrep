FROM alpine:3.17 as emscripten-container

ARG EMSCRIPTEN_VERSION=3.1.35
RUN apk --no-cache add git python3 gcompat

RUN git clone https://github.com/emscripten-core/emsdk.git /emsdk && \
    /emsdk/emsdk install ${EMSCRIPTEN_VERSION} && \
    /emsdk/emsdk activate ${EMSCRIPTEN_VERSION} && \
    sed -i 's/NODE_JS = .*/NODE_JS = "\/usr\/bin\/node"/g' /emsdk/.emscripten

FROM returntocorp/ocaml:alpine-2023-04-03 as semgrep-core-container

RUN apk --no-cache add gcompat nodejs && \
    ln -s /lib /lib64

COPY --from=emscripten-container /emsdk /emsdk

RUN eval "$(opam env)"
