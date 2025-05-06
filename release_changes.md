## [1.121.0](https://github.com/semgrep/semgrep/releases/tag/v1.121.0) - 2025-05-06


### Added


- pro: Improved handling of `tsconfig.json` in instances where multiple
  typescript "projects" (i.e., separately rooted source directories with their
  own configurations *not* joined by a single `tsconfig.json` with project
  references) are being scanned as one project under semgrep. This should result
  in better name/module resolution in TypeScript. (code-7798)
- pro: Improved handling of `include`, `exclude` and `files` properties in
  `tsconfig.json`. Projects which use more than one tsconfig in a given directory
  which apply to different sets of files under that directory should see
  improvements in name/module resolution. (code-7798-a)
- Improved Supply Chain scan output and logging. (sc-2356)


### Changed


- Upgrade the Julia parser to the tree-sitter-julia 0.22.0 (gh-10820)


### Fixed


- Fix bug introduced in Semgrep 1.120.0 causing interfile analyses to run out of memory due to too many parallel jobs. The default setting had been accidentally set to the number of available CPUs which is often too much in interfile mode. It's now back to `-j1` and it can be overridden by the user. (interfile-num-jobs)
- Fixed CI output so it shows per-product links depending on what product is enabled in a scan. (pr-3776)
- CLI: Fixed a bug where `--disable-nosem` was not properly causing nosemgrep'd findings
  to be uploaded to the App. (saf-1982)
- Exempt large manifests & lockfiles from being ignored by semgrep's file size filtering.
  This fixes a regression introduced in 1.117.0 (sca-1705). (sc-1705)
