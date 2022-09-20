# MATCH:
FROM a as b
FROM b

# MATCH:
FROM a as base
FROM base
