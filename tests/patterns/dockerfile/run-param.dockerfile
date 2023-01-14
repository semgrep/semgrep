# MATCH:
RUN --mount=type=secret echo

RUN echo

RUN --mount=type=cache echo

# MATCH:
RUN --mount=type=secret,target=/root/.cache/go-build echo

# MATCH:
RUN --mount=target=/root/.cache/go-build,type=secret echo
