# MATCH:
RUN --mount=type=secret,id=aws,target=/root/.aws/credentials echo

# MATCH:
RUN --mount=type=secret,id=aws,target=/root/.aws/credentials \
  echo

# MATCH:
RUN --mount=target=/root/.aws/credentials,type=secret echo

RUN --mount=type=secret echo

# MATCH:
RUN --foo=bar --mount=target=/root/.aws/credentials,type=secret echo

RUN --foo=bar echo

RUN echo
