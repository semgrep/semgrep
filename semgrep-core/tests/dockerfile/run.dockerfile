FROM debian

# MATCH:
RUN echo hello

# MATCH:
RUN ls \
  | tac

# MATCH:
RUN ls \
  -l

# MATCH:
RUN ls \
  -l; # blah \
  echo yay
