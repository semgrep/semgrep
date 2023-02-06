
# We should produce this finding, because the binding from the first `metavariable-pattern`
# should not express a constraint on the second `metavariable-pattern`.

# Because the binding is deferred, both `$B`s are understood to be separate.

# ruleid: defer-persistent-binding 
foo(
  inside(
    bar(1),
    qux(2)
  )
)