## [1.122.0](https://github.com/semgrep/semgrep/releases/tag/v1.122.0) - 2025-05-14


### Added


- Adds support for the UV package manager in Supply Chain scans. (SC-1900)


### Fixed


- pro: Fixed inter-file naming bug affecting Go's struct-methods that could result
  in false negatives.

  Previously, adding a `pattern-inside` like

      func ($THING $TYPE) $FUNC(...) $R { ... }

  to a taint rule could cause some findings to incorrectly stop being reported. (code-7767)
- PRO: Fixed the issue with type matching when a type has a type parameter, e.g., matching the pattern `std::vector<$T>` with the code `std::vector<int> v` in C++. (code-8443)
- Make Nuget dependency child parsing case insensitive (sc-2355)
- Fixed bug where direct dev depenencies were not marked as direct when parsing package-lock.json projects. (sc-dev)
