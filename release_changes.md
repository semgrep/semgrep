## [1.187.0](https://github.com/semgrep/semgrep/releases/tag/v1.187.0) - 2026-11-24

### ### Changed

- Reduce scheduling overhead when scanning many small files by assigning files to worker domains in small batches. (chunk-target-dispatch)
- Improved scan performance for taint rules. (dataflow-fixpoint-equality)
- File prefilters can answer case-sensitive needle predicates from a per-target prefilter carried by the extended target, keeping their default evaluation behavior otherwise. (prefilter-needle-index)
- Improved prefilter generation for patterns comprising regex which use character classes to achieve case-insensitivity, e.g., `[fF][oO][oO]`. (regexp-case-class-prefilter)
- Reduce scan startup overhead by skipping prefilter work that no rule needs and stopping project-wide checks once all required conditions have been found. (skip-idle-startup-work)

### ### Fixed

- Fixed unnecessary directory I/O, introduced in 1.164.0, when scanning only explicitly listed files. (CODE-10318)
