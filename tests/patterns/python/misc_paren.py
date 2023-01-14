# ERROR: match
cursor.execute("a".format("b"))

# ERROR: match
cursor.execute(("a") % ("b",))
