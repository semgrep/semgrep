
// this example is weird because the parens surround the
// generators of the `for`
// if we don't explicitly whitelist a `for` as a sepRegion
// where we can emit newlines, we're not going to be able to
// separate the two generators, because we won't emit a NEWLINE
// before the `_`
def foo() =
  foo { x =>
    bar (for
      y <- 2 
      _ <- 3 
    yield ())
  } 
