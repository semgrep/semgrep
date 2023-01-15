# ERROR:
FROM debian:testing
RUN echo hello
RUN apt-get update && apt-get install -y fortune
