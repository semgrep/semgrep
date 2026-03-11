## [1.155.0](https://github.com/semgrep/semgrep/releases/tag/v1.155.0) - 2026-03-11

### ### Added

- Added support for (agentic) hooks in Windsurf. (windsurf-hooks)
- scala: Improved support for Scala 3's optional braces. (LANG-218)
- Added PowerShell language support (beta) with parsing and pattern matching (lang-233)

### ### Changed

- Removed the experimental and undocumented command `semgrep install-ci`. (osemgrep-install-ci)
- Migrate from publishing a single Linux wheel with the platform tag `musllinux_1_0_<arch>.manylinux2014_<arch>` to publishing two separate wheels:

  - A wheel with the platform tag musllinux_1_0_<arch>
  - A wheel with the platform tag manylinux2014_<arch>

  (pypi-linux-tag)

### ### Fixed

- When performing parallel operations over a small number of input items, the
  engine no longer spawns more OCaml domains than we have items to process.  This
  assists with resource utilisation. (engine-2588)
- Fixed: Prevent SessionStart hook crash when inject-secure-defaults receives empty stdin (JSONDecodeError). (engine-2592)
- Semgrep secret validation now times out after 30 seconds instead of 15 minutes. Additionally this timeout is configurable via the `--secrets-timeout` flag. (engine-2593)
- Fixed permission errors during lockfileless Java (Gradle) dependency resolution by invoking gradlew via sh when the executable bit is not set (gh-5747)
