# A target dockerfile containing a mix of different string literals:
# unquoted, single-quoted, double-quoted, JSON.

FROM alpine AS builder

# MATCH:
ENV GOFLAGS='-tags=dynamic -buildvcs=false'

# MATCH:
ENV GOPROXY=https://proxy.example.com/api/go/go-all \
    GONOPROXY="none" \
    GOFLAGS='-tags=dynamic -buildvcs=false'

USER foo:foo

ENTRYPOINT [ "./app"]
