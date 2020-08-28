#
# Build pfff and set it up to run tests.
#
# This is meant as a reference on how to build the project. From the project's
# root, run:
#
#   docker build -t pfff .
#
# Upon success, you can start bash within the container as follows:
#
#   docker run -it pfff
#

FROM ocaml/opam2:debian-stable
COPY --chown=opam:opam . /home/opam/pfff
WORKDIR /home/opam/pfff

RUN ./scripts/setup-debian
RUN ./scripts/install-opam-deps
RUN ./scripts/build
RUN ./scripts/test
RUN ./scripts/install
