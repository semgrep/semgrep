# Integrations

> Note that all integrations are currently under development and are not officially supported. Please contact semgrep@r2c.dev or [join our slack](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA) for help getting started

## Github Action

See our [semgrep](https://github.com/marketplace/actions/semgrep-action) action for easy use of `semgrep` in CI. We dogfood it [here](https://github.com/returntocorp/semgrep/tree/develop/.github/workflows/semgrep-action.yml)

## Pre-Commit Hook

You can run `semgrep` as a pre-commit hook using [pre-commit](https://pre-commit.com)

```yaml
repos:
  - repo: https://github.com/returntocorp/semgrep
    rev: '0.5.0'
    hooks:
      - id: semgrep
        args: ['--precommit', '--error'] # exit 1
```

