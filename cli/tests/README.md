# semgrep tests

## Usage

`make test` in the `cli/` directory

## Structure

Sections marked with :construction: have no tests added yet.

### `unit/`

Unit tests are meant to be the primary test type.
This directory has tests for modules or classes
that mock out any external dependencies.

This is the only directory that counts for test coverage.

### :construction: `unit/property/`

This directory is for property-based tests
which programmatically generate a wide array of inputs to ensure
our general-purpose utils aren't tripped up by any kind of possible input.

### `e2e/`

End-to-end tests use committed output snapshots as a sanity check
that cover code on all layers from CLI parsing
down to semgrep-core's AST manipulation.

These tests verify that basic usage of specific features
don't change their behavior from the user's point of view;
unit tests wouldn't necessarily catch when units don't connect properly.

Consider any errors from this directory to be a confirmation prompt:
`pytest` will print how what the users sees has changed,
and if it seems sensible, just update the snapshots with `make regenerate-tests`.

### `qa/`

QA tests are automated quality assurance scenarios,
such as running Semgrep on various real life repositories.

These need to run only when doing QA for releases,
and can be run with `make qa-test` instead of `make test`.

### :construction: `performance/`

This directory is for benchmarking tests
that ensure that Semgrep runs fast enough.

## Fixtures

We use [pytest fixtures](https://docs.pytest.org/en/latest/fixture.html)
to author boilerplate-free tests.
When a test function takes a positional argument,
chances are it's a fixture that pytest passes in to it
either from `pytest` itself a `conftest.py` file of ours.

Some common ones we use are:

- [`monkeypatch`](https://docs.pytest.org/en/latest/monkeypatch.html),
  to make temporary changes to the environment
- [`tmp_path`](https://docs.pytest.org/en/latest/tmpdir.html),
  to get a temporary workspace in the filesystem
- [`run_semgrep_in_tmp`](#run_semgrep_in_tmp),
  to easily run the `semgrep` command line tool

### `run_semgrep_in_tmp`

This function runs `semgrep` for you via the CLI,
raises a `subprocess.CalledProcessError` if it fails,
and returns the results as a pretty-formatted JSON string.

Every test using this runs in a fresh temporary directory,
with `e2e/targets` and `e2e/rules` available at `./targets` and `./rules`, respectively.

When calling this, you specify the `--config` value as the first parameter:

`run_semgrep_in_tmp("r2c/python")`

It runs on the files in `e2e/targets/basic/` by default,
but you can change that to another directory next to that one:

`run_semgrep_in_tmp("r2c/python", target="equivalence")`

The return value is the JSON output from stdout,
but you can ask for stderr to be merged into the returned string:

`run_semgrep_in_tmp("r2c/python", stderr=True)`

You can add any additional CLI flag you'd like like so:

`run_semgrep_in_tmp("r2c/python", options=["--exclude", "*.py"])`

To call semgrep without the `--json` flag:

`run_semgrep_in_tmp("r2c/python", output_format=OutputFormat.TEXT)`

To call semgrep with the `--junit-xml` flag:

`run_semgrep_in_tmp("r2c/python", output_format=OutputFormat.JUNIT_XML)`

To call semgrep with the `--sarif` flag instead of `--json`:

`run_semgrep_in_tmp("r2c/python", output_format=OutputFormat.SARIF)`

## pytest-split and `.test_durations`

The `cli/.test_durations` file is a snapshot of how long tests took to run at a given time.
This informs the pytest-split plugin on how to optimally distribute tests to parallel workers.
The durations file helps even if it's out of date and missing half the tests we're currently running.
If you ever want to update the durations, run this command:

```terminal
$ pipenv run pytest --store-durations tests/*(/)
================== test session starts ==================
platform darwin -- Python 3.11.0b3, pytest-7.1.2, pluggy-1.0.0
rootdir: /Users/underyx/r2c/semgrep/cli, configfile: pyproject.toml
plugins: snapshot-0.9.0, xdist-2.5.0, freezegun-0.4.2, forked-1.4.0, split-0.8.0, mock-3.7.0
collected 1251 items
```

The `tests/*(/)` glob is a zsh feature to match all directories.
We explicitly target each directory to undo the exclusion of `tests/qa` set in `pyproject.toml`.
