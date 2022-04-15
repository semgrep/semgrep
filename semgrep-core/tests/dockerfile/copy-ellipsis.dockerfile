# MATCH:
COPY foo bar

# the following should match when the grammar is fixed to support
# multiple files:
#   https://github.com/camdencheek/tree-sitter-dockerfile/issues/12 i
# COPY foo bar baz
