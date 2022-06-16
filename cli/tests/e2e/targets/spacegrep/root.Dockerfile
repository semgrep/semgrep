FROM alpine
# ruleid: double-root
USER root
RUN apk install curl
CMD ["/hello"]
# ruleid: double-root
USER root
CMD ["ls -ltr"]
