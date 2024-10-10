## [1.90.0](https://github.com/returntocorp/semgrep/releases/tag/v1.90.0) - 2024-09-25


### Added


- Expanded support for requirement lockfiles. Semgrep will now find any `*requirement*.txt`
  file and lockfiles in a requirements folder (`**/requirements/*.txt`). This functionality
  will be gated behind the `--enable-experimental-requirements` CLI flag. (sc-1752)


### Changed


- Security update for code snippet storage & access methods. (gh-2038)


### Fixed


- Errors that occur in semgrep scans with jobs > 1 will now have more detail (SAF-1628)
- Dockerfile matching: `CMD $...ARGS` now behaves like `CMD ...` and matches
  any CMD instruction that uses the array syntax such as `CMD ["ls"]`. This
  fix also applies to the other command-like instructions RUN
  and ENTRYPOINT. (gh-9726)
- Pro Engine: There is now improved type inference in Kotlin and Scala. Constructor invocations like
  `Foo()` will now be inferred properly to be of type `Foo`. (saf-1537)
