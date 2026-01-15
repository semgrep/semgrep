## [1.148.0](https://github.com/semgrep/semgrep/releases/tag/v1.148.0) - 2026-01-14


### Added


- Performance: subproject discovery in Supply Chain scans is no longer
  significantly slowed down by the presence of Git-untracked files
  resulting in faster diff scans in such cases. (sc-subproject-speedup)


### Fixed


- pro: Improved virtual method resolution in Java (code-9174)
- pro: Improved handling of parse errors during inter-file analysis. Now, these
  errors should be adequately reported back to users and in the JSON output. (code-9216)
- Dataflow now accounts for Python for/else and while/else loops. (gh-8405)
- Fix rare "bad file descriptor" when performing Git operations on Windows (saf-2358)
