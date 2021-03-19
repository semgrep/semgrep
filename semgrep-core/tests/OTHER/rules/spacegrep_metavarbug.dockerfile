FROM alpine
USER root
RUN apk install curl
CMD ["/hello"]
# semgrep with spacegrep used to return a match here, but it should
# not really; it was just by chance, because it was returning different
# unique id for $ROOT even though they had the same content
# see https://github.com/returntocorp/semgrep/issues/2778
USER root
CMD ["ls -ltr"]
