## [1.154.0](https://github.com/semgrep/semgrep/releases/tag/v1.154.0) - 2026-03-04

### ### Fixed

- Fix crash on Windows when running `semgrep ci` with `--debug` and no blocking findings. The Windows subprocess path incorrectly raised an exception for all pysemgrep exit codes (including 0), which was silently swallowed in normal mode but propagated as a fatal error when `--debug` was active. (ENGINE-2491)
- Changed default memory policy from "eager" to "balanced".  Scan times should
  noticably improve; however, scans may use 5-10% additional memory.  If running
  in a resource-constrained environment, consider setting the memory policy back
  to "aggressive". (engine-2055)
- When Semgrep decides which files to scan (targeting), it can take a long time (over 5 minutes) on very large repos (> 10k files). Semgrep will now parallelize this work according to the number of jobs passed (`-j`) (engine-2512)
- Fixed a performance issues where passing many scannign roots on the command
  line (e.g. `semgrep scan $(git ls-files '*.py')`) caused one semgrep-core
  subprocess to be spawned per file. Roots that are not directories are now
  handled directly in Python without any subprocess overhead. (gh-11404)
- Scala: Restored parse rate after mistaken bug introduced by implicit block parsing fix (lang-215)
