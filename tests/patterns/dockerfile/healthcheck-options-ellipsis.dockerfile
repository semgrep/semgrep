# MATCH:
HEALTHCHECK CMD echo foo

# MATCH:
HEALTHCHECK --timeout=30s CMD echo foo

HEALTHCHECK --timeout=30s CMD echo bar
