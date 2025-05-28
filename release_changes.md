## [1.123.0](https://github.com/semgrep/semgrep/releases/tag/v1.123.0) - 2025-05-28


### Fixed


- Fixed bug where supply chain reachability rules which match multiple dependencies could produce reachable findings on transitive dependencies even when the actually used direct dependency was not vulnerable. (SC-2088)
- Fixed documentation to reflect that, for --metrics="auto", pseudoanonymous metrics are sent when the user is logged in. (gh-11028)
