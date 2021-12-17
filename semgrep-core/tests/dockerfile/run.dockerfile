FROM debian

# MATCH:
RUN echo hello

# MATCH:
RUN ls \
  | tac

# MATCH:
RUN ls \
  -l
