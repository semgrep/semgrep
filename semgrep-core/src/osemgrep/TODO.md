# osemgrep to-do list and notes

The goal of this file is to track progress and share ideas about the
migration of the Python CLI wrapper to OCaml, known temporarily as
osemgrep.

## October 10, 2022

by Martin:

- `osemgrep scan` is the only subcommand that was (partially)
  implemented.
- The output of `semgrep scan` needs to be formatted to JSON and the
  text output needs to be turned off.
- Target management needs completion and testing.
- The config manager, whose job is to interpret the `--config`
  argument specifying how to obtain Semgrep rules, is unimplemented.
  Right now, it assumes the argument is a plain rule file. We need to
  pick a suitable HTTP client library/framework. Suggestions:
  - Cohttp client: relies on Lwt, which is ideal for making multiple requests
    concurrently and safely. Lwt provides the same
    functionality as JavaScript promises. Unlike JavaScript,
    it requires explictly calling the loop that manages event-driven
    tasks. This allows us to start the Lwt loop to make a bunch of
    HTTP calls concurrently and exit it when we're done.
    Cohttp is reliable. I can provide code to get started and tutoring
    on Lwt promises.
  - If learning Lwt is too much of a burden and we just need to make
    one HTTP call with a timeout, it's easy to provide client
    functions that will do this synchronously and completely bury Lwt
    under the hood.
  - I haven't used other HTTP libraries for OCaml in recent years. I'm
    not sure if there's a solid alternative that's worth using.
- A lot of the current code under `osemgrep/` is transitory and will
  disappear or get renamed as we progress in the migration.
- I found that small functionality can be translated directly from
  Python. However, for bigger features such as the whole target manager,
  everything is best designed from scratch without looking too closely
  at the Python implementation. In those cases, the interfaces of the Python
  classes and methods are what matter most.
- I didn't get around to implement the include/exclude filtering. I
  renamed them in the OCaml implementation where possible because I
  never found them intuitive. We need to filter a list of file paths
  based on Python-compatible glob patterns. Maybe this could be done
  with the Glob module of the ocaml-re library, or maybe we need to
  write our own parser to be fully backward-compatible.
- End-to-end/QA tests are run from `/cli/semgrep` using `make osemgrep-e2e` or `osemgrep-qa`. The latter takes longer. I suggest
  focusing on the former initially. See inside the makefile for more info.
- The `osemgrep-util/` source folder is a mixed bag. I wasn't sure which
  code was needed just for `semgrep scan` and what would be used by
  the other subcommands.
- `semgrep-scan` contains the code relevant to the `semgrep scan`
  subcommand. The module that defines the configuration type
  and the command-line interface (`Scan_CLI`) is independent from the
  code that takes the config and does something with it. That code is
  the `Semgrep_Scan.main` function. All the modules in semgrep-scan/
  may depend on the `Scan_CLI.conf` type, which is meant to be passed
  around. `Scan_CLI.default` is the record holding the default config.
  It can be inherited from using the `with` syntax
  e.g. `{ default with foo = 123 }`.
- The `semgrep-core` command-line config (`Runner_config.t`) now also
  provides a default config as a single `Runner_config.default`
  object. It makes it easy to change parameters for testing without
  going through command-line parsing.
- `Runner_config.t` is incomplete. Some options are still stored as
  globals in the `Flag` module. I'd rather have them in the config
  record. If we don't want to pass around a config record everywhere,
  then I would consolidate the various entries of the `Flag` module in
  a single record and a single global (`val conf : t option ref`) and provide
  functions to set and lock a config temporarily. Otherwise we don't
  have a clue whether a flag `Flag.filter_irrelevant_rules` is on or
  off or whether it may be incorrectly set due to a previous invocation.

Other long-term or vague ideas:

- consider making `osemgrep` the new `semgrep` command and have it fall back
  to calling `semgrep-py` (the old `semgrep`).
