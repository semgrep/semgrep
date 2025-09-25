## [1.138.0](https://github.com/semgrep/semgrep/releases/tag/v1.138.0) - 2025-09-25


### Added


- pro: scala: Method dispatching through traits (code-9092)


### Changed


- Pro: additionally improved prefiltering for taint rules, especially when using
  taint labels. This allows for the generation of more specific conditions than
  the previously released version (v1.133.0). (code-9097)


### Fixed


- pro: python: Fix resolution of implicit namespace modules (code-9008)
- We now filter `SEMGREP_APP_TOKEN` from any request made to non semgrep URLs
  passed to `-f/-c/--config` during config/rules fetching. (gh-11016)
- Typescript: Made it so that the pattern `var $X = $FUNC($REQ, $RES, ...) {...}`
  no longer fails to parse. (saf-2159)
- pro: improved performance of `tsconfig.json` matching for Typescript projects
  that contain multiple `tsconfig.json`s. (saf-2163)
- Semgrep no longer fails to validate a config when a rule lang is capitalized (Introduced 1.137.0) (saf-2247)
