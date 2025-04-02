## [1.117.0](https://github.com/semgrep/semgrep/releases/tag/v1.117.0) - 2025-04-02


### Added


- Add temporary backward compatibility in Semgrepignore v2 for patterns
  that start with `./`. For example, the pattern `./*.py` should be written as
  `/*.py` to have the desired effect of excluding the `.py` files
  located in the same directory as the `.semgrepignore` file containing
  the pattern.
  To minimize surprises for users switching to Semgrepignore v2,
  we'll be interpreting automatically `./*.py` as `/*.py` for the time
  being so as to match the legacy Semgrepignore v1 behavior. Users should not
  rely on this since it doesn't comply with the Gitignore/Semgrepignore
  standard and will be removed in the future. (tolerate-semgrepignore-v1-dotslash)
- Target file selection now uses
  [Semgrepignore v2](https://semgrep.dev/docs/semgrepignore-v2-reference) by default. This brings the behavior of the Semgrepignore file
  exclusions closer to Git and `.gitignore` files. There can now
  be multiple `.semgrepignore` files in the project. The `.semgrepignore` file
  in the current folder is no longer consulted unless it in the project.
  Negated patterns are now supported such as `!scanme.py` as with Gitignore.
  Some bugs were fixed. (use-semgrepignore-v2)


### Changed


- Upgrade Semgrep from OCaml 5.2.1 to 5.3.0 (#3)


### Fixed


- In Semgrepignore v2, allow wildcards `*` and `?` to match file names with a leading period. This matches the behavior of Gitignore and Semgrepignore v1. (semgrepignore-dotfiles)
