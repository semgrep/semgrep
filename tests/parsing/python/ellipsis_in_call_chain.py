# Test for gh-11545: ellipsis in dot-access chain patterns
# These are semgrep patterns, not valid Python code.
# They should parse correctly as patterns.

# basic dot-access ellipsis
a. ... .d

# call chain ellipsis
builder(). ... .compact()
