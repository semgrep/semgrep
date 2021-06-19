# `semgrep` Contributing

The following explains how to build `semgrep` so that you can make and test changes to the Python wrapper. You may want to read the README first to understand the relationship between `semgrep`, `semgrep-core`, and `spacegrep`.

Contents:

* [Getting `semgrep-core` and `spacegrep` binaries](#getting-semgrep-core-and-spacegrep-binaries)
* [Setting Up the Environment](#creating-the-environment)
* [Running `semgrep`](#running-semgrep)
* [Installing `semgrep`](#installing-semgrep)
* [Troubleshooting](#troubleshooting)
* [Testing `semgrep`](#testing-semgrep)

## Getting `semgrep-core` and `spacegrep` binaries

If you would like to install `semgrep-core` and `spacegrep` from source (for example, because you want to fix a parse error), follow the instructions in [Building `semgrep-core`](SEMGREP_CORE_CONTRIBUTING.md#building-semgrep-core) and skip this section.

Otherwise, visit the [releases page](https://github.com/returntocorp/semgrep/releases)
and grab the latest zipfile or tarball for your platform. Extract this archive
and inside should be the necessary binaries. You can confirm this by running:

```bash
$ ./semgrep-core --help
$ ./spacegrep --help
```

Copy these files to somewhere in your `$PATH` so `semgrep` can find them. For
example, you may create a `~/bin/` directory within the repository. [Include it in your `$PATH`](https://unix.stackexchange.com/questions/26047/how-to-correctly-add-a-path-to-path)
and run the binary from there.

Alternatively, you may include it somewhere like `/usr/local/bin/`.

## Setting Up the Environment

Once you have `semgrep-core` and `spacegrep` installed, you will be able to build `semgrep`. You will need Python >= 3.6 as well.

Most `semgrep` development will operate inside the `semgrep` directory (from the top level of this repo, `semgrep/semgrep/`):

```
$ cd semgrep
```

We use [`pipenv`](https://github.com/pypa/pipenv) to manage our virtual
environment. If you don't have `pipenv` installed, the following command will do
so for you:

```
$ python -m pip install pipenv
```

Next we need to initialize the environment:

```
$ SEMGREP_SKIP_BIN=true python -m pipenv install --dev
```

*`SEMGREP_SKIP_BIN` tells the installer that we will bring our own binaries.*
This command will install dev dependencies such as pytest and will also install semgrep in editable mode in the pipenv.

## Running `semgrep`

You will want to be in the pipenv environment whenever you run semgrep. Start a shell with

```
$ python -m pipenv shell
```

Make sure you are in `semgrep/semgrep/`. Within the shell, run:

```
$ python -m semgrep -h
```

To try a simple analysis, you can run:

```
$ echo 'if 1 == 1: pass' | python -m semgrep --lang python --pattern '$X == $X' -
/tmp/...
1:if 1 == 1: pass
```

Congratulations, you have Semgrep running locally!

## Installing `semgrep`

You can always run `semgrep` from `semgrep/semgrep/`, which will use your latest changes in that directory, but you may also want to install the `semgrep` binary. To do this, run

```
pipenv install --dev
```

Some people have encountered difficulties with the above. If it fails, you can also try (within the `semgrep/semgrep/` directory)

```
sudo pip install -e .
```

Now you can run `semgrep -h` from anywhere.

If you have installed `semgrep-core` and `spacegrep` from source, there are convenient targets in the root Makefile that let you update all binaries. After you pull, simply run

```
make rebuild
```

See the Makefile in `semgrep/`

## Troubleshooting

For a reference build that's known to work, consult the root `Dockerfile`
to build semgrep inside a container. You can check that it builds with

```
docker build -t semgrep .
```

## Testing

`semgrep` uses [`pytest`](https://docs.pytest.org/en/latest/) for testing.

To run tests, run the following command within the pipenv shell:

```
$ python -m pytest
```

This command will run comprehensize parse tests on many open source projects. To skip these slow tests run:

```sh
$ python -m pytest --ignore=tests/qa/test_public_repos.py
```

Running a single test file is simple too:

```
$ python -m pytest path/to/test.py
```

Or running an individual test function:

```
$ python -m pytest -k test_func_name path/to/test.py
```

`semgrep` also includes [`pytest-benchmark`](https://pytest-benchmark.readthedocs.io/en/latest/)
to allow for basic benchmarking functionality. This can be run like so:

```
$ python -m pytest --benchmark-only
```
