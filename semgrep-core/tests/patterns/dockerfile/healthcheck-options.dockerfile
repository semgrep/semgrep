HEALTHCHECK NONE

HEALTHCHECK CMD echo hello

# MATCH:
HEALTHCHECK --timeout=60s --retries=2 CMD echo hello
