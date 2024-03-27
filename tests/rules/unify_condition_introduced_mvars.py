

# ruleid: unify-condition-introduced-mvars
foo((
    one(1, 2),
    two(2, 3)
))

# ruleid: unify-condition-introduced-mvars
foo((
    two(2, 3),
    one(1, 2)
))

foo((
    one(1, 3),
    two(2, 3)
))

foo((
    one(1, 3)
))
