# Integrations

This document describes integrating `semgrep` into other pieces of software.

Contents:

* [Pre-Commit Hook](#pre-commit-hook)
* [Continuous Integration](#continuous-integration)
  * [AppVeyor](#appveyor)
  * [CircleCI](#circleci)
  * [TravisCI](#travisci)
  * [GitHub Actions](#github-actions)
  * [GitLab](#gitlab)
  * [Jenkins](#jenkins)

## Pre-Commit Hook

The [pre-commit framework](https://pre-commit.com)
can run `semgrep` on your code changes
whenever you make a commit.
To set this up,
[install `pre-commit`](https://pre-commit.com/#install)
and add this in your `.pre-commit-config.yaml`:

```yaml
repos:
- repo: https://github.com/returntocorp/semgrep
  rev: 'v0.27.0'
  hooks:
    - id: semgrep
      args: ['--config', 'https://semgrep.dev/p/r2c', '--error']
```

This will default to using the [`r2c` ruleset](https://semgrep.dev/p/r2c).
To choose another ruleset, see https://semgrep.dev/rulesets.

## Continuous Integration

This section describes integrating `semgrep` into your continuous integration
(CI) system. This allows you to continuously search your codebase for bugs,
anti-patterns, and security issues.

### AppVeyor

Include `semgrep` in your `.appveyor.yml` configuration file:

```yaml
build: off
environment:
    matrix:
        - PYTHON: "C:\\Python36"
        - PYTHON: "C:\\Python37"
        - ...
install:
    - python -m pip install semgrep
test_script:
    - semgrep --config https://semgrep.dev/p/r2c /path/to/code
```

This will default to using the [`r2c` ruleset](https://semgrep.dev/p/r2c).
To choose another ruleset see https://semgrep.dev/rulesets.

### CircleCI

Include `semgrep` in your `.circleci/config.yml` configuration file:

```yaml
version: 2
jobs:
    build:
        docker:
            - image: circleci/python
        steps:
            - checkout
            - run: python -m pip install semgrep
            - run: semgrep --config https://semgrep.dev/p/r2c /path/to/code
```

This will default to using the [`r2c` ruleset](https://semgrep.dev/p/r2c).
To choose another ruleset see https://semgrep.dev/rulesets.

Another way to do it is to use the official Semgrep [Docker Image](https://hub.docker.com/r/returntocorp/semgrep)

```yaml
version: 2
jobs:
    build:
        docker:
            - image: returntocorp/semgrep:latest
        working_directory: /src
        steps:
            - checkout
            - run: semgrep --error --config https://semgrep.dev/p/r2c .
```


### TravisCI

Include `semgrep` in your `.travis.yml` configuration file:

```yaml
language: python
install:
    - python -m pip install semgrep
script:
    - semgrep --config https://semgrep.dev/p/r2c /path/to/code
```

This will default to using the [`r2c` ruleset](https://semgrep.dev/p/r2c).
To choose another ruleset see https://semgrep.dev/rulesets.

### GitHub Actions

Include `semgrep` in your `.github/workflows/semgrep.yml` configuration file:

```yaml
name: Semgrep
on: [push, pull_request]
jobs:
    semgrep:
        runs-on: ubuntu-latest
        name: Check
        steps:
            - uses: actions/checkout@v1
            - uses: returntocorp/semgrep-action@v1
              with:
                config: p/r2c
```

This will default to using the [`r2c` ruleset](https://semgrep.dev/p/r2c).
To choose another ruleset see https://semgrep.dev/rulesets.

For more information on the GitHub Action see https://github.com/marketplace/actions/semgrep-action.

### GitLab

Include `semgrep` in your `.gitlab-ci.yml` configuration file:

```yaml
include:
  - template: 'Workflows/MergeRequest-Pipelines.gitlab-ci.yml'

semgrep:
  image: returntocorp/semgrep-action:v1
  script:
    - python -m semgrep_agent --config p/r2c
```

This will default to using the [`r2c` ruleset](https://semgrep.dev/p/r2c).
To choose another ruleset see https://semgrep.dev/rulesets.

### Jenkins

Include `semgrep` in your `Jenkinsfile` configuration file:

```
pipeline {
  agent any
  stages {
    stage('Semgrep') {
      steps {
        sh '''/usr/local/bin/docker run --rm -v "${PWD}:/src" returntocorp/semgrep --error --config=https://semgrep.dev/p/r2c'''
      }
    }
  }
}
```

This will default to using the [`r2c` ruleset](https://semgrep.dev/p/r2c).
To choose another ruleset see https://semgrep.dev/rulesets.
