# MATCH:
foo;

# MATCH:
foo

# no match on 'foo' because pattern contains an explicit ';'
bar foo
