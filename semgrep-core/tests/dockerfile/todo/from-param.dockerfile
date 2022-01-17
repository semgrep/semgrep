# TODO: 'FROM --platform=a b' is parsed incorrectly
# MATCH:
FROM --platform=linux/arm64 alpine

# MATCH:
FROM alpine
