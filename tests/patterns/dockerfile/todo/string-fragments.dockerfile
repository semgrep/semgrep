FROM alpine

# This needs to fixed in the tree-sitter-dockerfile parser
# MATCH:
ENV A "a"b'c'

RUN echo "$A"
