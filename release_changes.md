## [1.87.0](https://github.com/returntocorp/semgrep/releases/tag/v1.87.0) - 2024-09-12


### Added


- Semgrep's interfile analysis (available with the Pro Engine) now ships with information about Python's standard library, improving its ability to resolve names and types in Python code and therefore its ability to produce findings. (py-libdefs)
- Added support for comparing Golang pre-release versions. With this, strict
  core versions, pseudo-versions and pre-release versions can all be
  compared to each other. (sc-1739)


### Changed


- If there is an OOM error during interfile dataflow analysis (`--pro`) Semgrep will
  now try to recover from it and continue the interfile analysis without falling back
  immediately to intrafile analysis. This allows using `--max-memory` with `--pro` in
  a more effective way. (flow-81)
- Consolidates lockfile parsing logic to happen once, at the beginning of the scan. This consolidated parsing now considers both changed and unchanged lockfiles during all steps of diff scans. (gh-2051)


### Fixed


- pro: taint-mode: Restore missing taint findings after having improved index-
  sensitivity:

      def foo(t):
          x = third_party_func(t)
          return x

      def test1():
          t = ("ok", taint)
          y = foo(t)
          sink(y) # now it's found! (code-7486)
- The Semgrep proprietary engine added a new entropy analyzer `entropy_v2` that supports strictness options. (gh-1641)
