# doesn't match because it's not a concatenation
${x}

# MATCH:
a${x}

# MATCH:
${x}b

# MATCH:
a${x}c

# MATCH:
foo a${x}c

# MATCH:
a${x}c bar

# MATCH:
foo a${x}c bar

# doesn't match because it's not a concatenation
foo ${x} bar
