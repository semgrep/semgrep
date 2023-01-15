#ERROR: match
cursor.execute("a" % ("b",))

#ERROR: match
cursor.execute(("a") % ("b",))
