# LABEL (resp. ENV) instructions are now translated into one LABEL (resp. ENV)
# instruction per pair.

# MATCH:
LABEL a=b c=d

# MATCH:
LABEL a=b
LABEL c=d

# MATCH:
LABEL x=y a=b
LABEL c=d e=f

LABEL c=d a=b
