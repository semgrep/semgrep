# Semgrep Docs

Welcome to the extended Semgrep docs! 🙌 The Director's Cut, if you will.

## Language Support

For every language that Semgrep supports, there are certain features that must
be implemented, like support for the ellipsis (`...`) operator, metavariables, and more.

See [here](matrix.md) for a more detailed breakdown of feature support by
language.

## Semgrep Output

Semgrep by default prints to STDOUT, but it can also output JSON or SARIF (Static Analysis Results Interchange Format). See [here](output.md) for more details.

## Integrations

It's easy to integrate Semgrep as a pre-commit hook or in one of many
continuous integration (CI) systems.

See [here](integrations.md) for example config files to run Semgrep using
tools like AppVeyor, CircleCI, TravisCI, GitHub Actions, and Gitlab.

## Writing Rules

[Pattern Features](pattern-features.md) describes what Semgrep patterns can do
  in detail, and provides many example use cases of metavariables, the ellipsis
  operator, and more.

Semgrep YAML rule files enable you to combine patterns in powerful ways,
  allowing you to find code patterns that do or don't match multiple sets of
  patterns. See the [config file spec](configuration-files.md) for more details.

You can write and share rules directly from the live editor at https://semgrep.live or run Semgrep via a standalone CLI or Docker.

See [Semgrep rule writing methodology](writing_rules/README.md) for details on
the thought process behind writing and rolling out a new Semgrep rule
sucessfully, and see [these docs](writing_rules/examples.md) for walkthroughs of
writing rules from scratch.

There's also an interactive, example-based Semgrep rule writing tutorial here:
https://semgrep.live/learn.

## Contributing to Semgrep

We'd love your help! See `CONTRIBUTING.md` for detailed steps on getting your dev environment set up, building Semgrep, and more.
