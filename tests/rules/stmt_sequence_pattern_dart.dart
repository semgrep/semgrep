// Multi-statement pattern rule, exercising the `semgrep_statement_list`
// path through the rule layer (not just the unit `match_pattern` path).

dynamic data;

void caller() {
  // ruleid: stmt-sequence-pattern-dart
  data = get();
  log("kickoff");
  doStuff();
  eval(data);

  // Immediate adjacency — no intermediate statements.
  // ruleid: stmt-sequence-pattern-dart
  data = get();
  eval(data);

  // Metavariable doesn't unify — `eval(other)` uses a different binding.
  // ok: stmt-sequence-pattern-dart
  data = get();
  log("step");
  var other = data;
  eval(other);
}
