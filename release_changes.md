## [1.152.0](https://github.com/semgrep/semgrep/releases/tag/v1.152.0) - 2026-02-12

### ### Added

- Turned on DNS rebinding protection for the MCP server (dns-check)
- Environment variables can now be passed to third-party package managers invoked as part of `--allow-local-builds` dependency resolution via the environment variable `SEMGREP_LOCAL_BUILD_ENV`, which accepts a JSON object with string keys and string values. (SC-3163)
- Memory management policies

  A memory policy defines how OCaml's garbage collector should be configured for
  a scan.  There are two initial policies: "aggressive", the current behaviour,
  which trades longer scan times for lower memory use, and "balanced", which
  finds a middle ground between reclaiming heap memory in short order while
  limiting how often the garbage collector runs.  The policy can be configured
  via the `--x-mem-policy` CLI flag for the pro engine; this flag is unused in
  the OSS engine. (engine-2055)
- Blocking findings that are outputted in the CI output are now labelled as such. (#4394)

### ### Changed

- pro: There should be fewer FNs when the max number of fields to track per object
  is reached. (code-9224)
- Remove legacy combined symbol analysis computation and upload in favor of per-subproject symbol analysis (sc-3153)

### ### Fixed

- pro: Improved accuracy of taint tracking through assignments, this will help
  reduce FPs in some cases. (code-9220)
