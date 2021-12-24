# MATCH:
HEALTHCHECK NONE

# MATCH:
healthcheck none

HEALTHCHECK CMD echo hello

HEALTHCHECK --timeout=60s --retries=2 CMD echo hello
