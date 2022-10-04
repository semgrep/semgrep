# ERROR: match
with foo():
    pass

# ERROR: match
with (foo(), bar()):
    print("Hello!")
