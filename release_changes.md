## [1.101.0](https://github.com/semgrep/semgrep/releases/tag/v1.101.0) - 2024-12-18


### Added


- Improved pnpm-lock.yaml parsing. (gh-2663)


### Changed


- Re-ordered some terminal output of `semgrep ci` to allow semgrep-app to block scans based on specific findings (SECW-2740)
- A few fields in the JSON output (e.g., "fingerprint", "metavars") require now
  the user to be logged in to see them.
  See https://semgrep.dev/docs/semgrep-appsec-platform/json-and-sarif#json
  for more information. (json)
- We're renaming semgrep OSS to Semgrep Community Edition.
  See https://semgrep.dev/blog/2024/important-updates-to-semgrep-oss/
  for more information. (rename)
- A few fields in the SARIF output (e.g., "fingerprints") require now
  the user to be logged in to see them.
  See https://semgrep.dev/docs/semgrep-appsec-platform/json-and-sarif#sarif
  for more information. (sarif)


### Fixed


- pro: Improved inter-file tracking of tainted global variables. (code-7054)
- Python (pro-only): Taint now correctly tracks through calls to class methods
  within a class, via the `cls` parameter.

  So for instance, we would be able to determine a source-to-sink
  vulnerability in the following code snippet:
  ```
  class A:
    def foo(self, x):
      sink(x)

    @classmethod
    def bar(cls):
      cls.foo(source)
  ``` (saf-1765)
- pro: Fixed bug when generating inter-procedural taint traces, that it could
  cause a call-step to be missing in the trace. (saf-1783)
- Restored the "rules" field in the SARIF output, even when logged out. (saf-1794)
