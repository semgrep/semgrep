# Integrations

> Note that all integrations are currently under development and are not officially supported. Please contact sgrep@r2c.dev or [join our slack](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA) for help getting started

## Github Action

See our [sgrep-lint](https://github.com/marketplace/actions/sgrep-lint) action for easy use of `sgrep` in CI. We dogfood it [here](https://github.com/returntocorp/sgrep/tree/f92e3b4a12f0fcd659e787894ef3de0619f21419/.github/workflows/sgrep-lint.yml)

## Pre-Commit Hook

You can run `sgrep` as a pre-commit hook using [pre-commit](https://pre-commit.com)

```yaml
repos:
  - repo: https://github.com/returntocorp/sgrep
    rev: '0.4.3'
    hooks:
      - id: sgrep
        args: ['--precommit', '--error'] # exit 1
```

