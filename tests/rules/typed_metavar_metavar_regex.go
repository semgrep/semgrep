func f(c *http.Request) {
  // The combination of a typed metavariable and a regex does not always work
  // in pro interfile mode because the type is semantically resolved and often
  // contains fake tokens. In pro interfile mode, `metavariable-type` should be
  // used instead of relying on non-syntactic metavariable binding. See
  // CODE-7993 for more details.
  // ruleid: deeptodoruleid: typed-metavar-metavar-regex
  c.Foo()
}
