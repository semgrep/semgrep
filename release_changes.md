## [1.104.0](https://github.com/semgrep/semgrep/releases/tag/v1.104.0) - 2025-01-22


### Changed


- Supply chain diff scans now skip resolving dependencies for subprojects without changes. (SC-2026)


### Fixed


- pro: Fixed bug in inter-file matching of subtypes. When looking to match some
  type `A`, Semgrep will match any type `B` that is a subtype of `A`, but in certain
  situations this did not work. (code-7963)
- taint-mode: Make traces record assignments that transfer taint via shapes.

  For example, in code like:

      B b = new B(taint);
      B b1 = b;
      sink(b1.getTaintedData());

  The assignment `b1 = b` should be recorded in the trace but previously it was not. (code-7966)
- Python: Parser updated to the most recent tree-sitter grammar.
  Parse rate from 99.8% -> 99.998%. (saf-1810)
