## [1.142.0](https://github.com/semgrep/semgrep/releases/tag/v1.142.0) - 2025-10-30


### Added


- Pro: improved taint handling of `match` expressions in Scala. In examples like
  ```scala
  val x = taint match {
      case Some(t) => t
      case None => return "example"
  }
  ```
  dataflow should now track taint from `taint` to `x`. (code-9085)
- pro: scala: http4s-specific support for `case $M -> ... :? ... +& test +& ... => ...` patterns. (code-9131)


### Fixed


- Supply Chain subproject resolution table is now shown even when no subprojects were successfully resolved (SC-2492)
- UV lockfiles that include editable and local dependencies without versions are now parsed correctly. The unversioned dependencies will be ignored. (SC-2888)
- Failures in parsing UV lockfiles are now correctly reported as "Failed" rather than "Unsupported" (SC-2895)
- build.gradle.kts files now resolve correctly when `--allow-local-builds` is passed. (SC-2899)
- Rule parsing in 1.139.0 was switched to happen solely in semgrep-core. This caused some users to exit with code 7, so this change has been reverted. (saf-2265)
