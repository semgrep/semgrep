# MATCH:
RUN --foo=bar echo

# MATCH:
RUN --foo=hello echo

RUN echo

RUN --bar=hello echo
