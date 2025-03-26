## [1.115.0](https://github.com/semgrep/semgrep/releases/tag/v1.115.0) - 2025-03-26


### Added


- pro: Extended the `requires:` key for taint sinks to specify multiple conditions
  associated with different metavariables.

  For example:

      pattern-sinks:
      - patterns:
        - pattern: $OBJ.foo($SINK, $ARG1)
        - focus-metavariable: $SINK
        requires:
        - $SINK: TAINT
        - $OBJ: OBJ
        - $ARG1: ARG1

  With a regular `requires:` the condition can only apply to whatever the sink is
  matching, in this case, `$SINK`. With a "multi-requires" we are able to restrict
  `$SINK`, `$OBJ` and `$ARG1` independently, each one having its own condition.

  Note that `requires:` is part of the *experimental* taint labels feature. (code-7912)
- In the text output of `semgrep scan` and `semgrep ci`, a warning message
  announcing the upcoming Semgrepignore v2 is now displayed. Differences in
  target selection are shown. (semgrepignore-v2-warning)
