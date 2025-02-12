## [1.108.0](https://github.com/semgrep/semgrep/releases/tag/v1.108.0) - 2025-02-12


### Added


- pro: Semgrep can now dynamically resolve dependencies for Python projects using pip, allowing it to determine transitive dependencies automatically. (sc-2069)


### Changed


- Bump base Alpine docker image from 3.19 to 3.21. (alpine-version)
- The semgrep-appsec-platform specific metadata fields "semgrep.dev:" and
  "semgrep.policy:" are now filtered from the JSON output unless you
  are logged in with the Semgrep appsec platform.
  See https://semgrep.dev/docs/semgrep-appsec-platform/json-and-sarif#json for more information. (metadata-filter)
- The Semgrep Docker image now uses Python 3.12 (bumped from 3.11). (python-version)


### Fixed


- This PR changes the way we handle failures in `git worktree remove` more gracefully.
  Instead of erroring, we continue to scan so that the user can still get results, but
  log the error. It also adds a guard so that this failure is less likely to happen
  and will include more debugging information when it does. (sms-521)
