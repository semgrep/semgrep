
# OLD BEHAVIOR:
# We should produce this finding, because the binding from the first `metavariable-pattern`
# should not express a constraint on the second `metavariable-pattern`.
# Because the binding is deferred, both `$B`s are understood to be separate.

# NEW BEHAVIOR:
# We no longer "defer" the introduced metavariables. When our conditions introduce $B, we
# unify them before introducing them into the match, meaning that we should not get a
# finding here.

foo(
  inside(
    bar(1),
    qux(2)
  )
)