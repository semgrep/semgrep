FROM alpine:3.20
ARG PORT=8080
EXPOSE ${PORT}/tcp ${PORT}/udp
RUN python3 --version | grep 'Python [0-9]*\.[0-9]*\.[0-9]*'
RUN ["printf", "[ok]"]
