import sqlparse
# MATCH:
sqlparse.parse('[' * 10000 + ']' * 10000)
# OK:
sqlparse.parse(uhoh + "something")
