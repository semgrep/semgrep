## [1.91.0](https://github.com/returntocorp/semgrep/releases/tag/v1.91.0) - 2024-10-10


### Added


- Type inference in the Pro engine has been improved for class fields in
  TypeScript that are assigned a new instance but lack an explicit type
  definition. When no explicit type is provided for a class field, its type is
  inferred from the type of the expression assigned to it. For example, in the
  class definition `class Foo { private readonly bar = new Bar(); }`, the type of
  `bar` is inferred to be `Bar`. (code-7635)
- Cargo.lock parser can now associate dependencies with lockfile line numbers (sc-1140)


### Fixed


- Address python `rich.errors.LiveError` where attempting to display multiple progress bars
  raises an exception as flagged in #10562. (grow-414)
- C: Fix a regression causing pattern `-n` to sometimes not match code `-n`. (saf-1592)
- When a scan runs into an exception, the app is appropriately notified
  about the failure. Previously, in the app, it would seem to the user
  that the scan is still in progress. (sms-502)
