# ERROR:
FROM ubuntu
RUN echo hello

FROM debian
RUN echo hello

# ERROR:
FROM ubuntu:testing
RUN echo hello
