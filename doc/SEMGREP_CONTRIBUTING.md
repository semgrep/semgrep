# `semgrep` Contributing

The following explains how to build `semgrep` so that you can make and test changes to the Python wrapper. You may want to read the README first to understand the relationship between `semgrep`, `semgrep-core`, and `spacegrep`.

Contents:

* [Getting `semgrep-core` and `spacegrep` binaries](#getting-semgrep-core-and-spacegrep-binaries)
* [Building `semgrep`](#building-semgrep)
* [Testing `semgrep`](#testing-semgrep)

## Getting `semgrep-core` and `spacegrep` binaries

If you would like to install `semgrep-core` and `spacegrep` locally (for example, because you want to fix a parse error), follow the instructions in [Building `semgrep-core` and `spacegrep`](link) and skip this section.

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

### Building `semgrep`

Once you have `semgrep-core` and `spacegrep` installed, you will be able to build `semgrep`. You will need Python >= 3.6 as well.

Most `semgrep` development will operate inside the `semgrep` directory (from the top level of this repo, `./semgrep`):

```
$ cd semgrep
```

We use [`pipenv`](https://github.com/pypa/pipenv) to manage our virtual
environment. If you don't have `pipenv` installed the following command will do
so for you:

```
$ python -m pip install pipenv
```

Next we need to initialize the environment:

```
$ SEMGREP_SKIP_BIN=true python -m pipenv install --dev
```

*`SEMGREP_SKIP_BIN` tells the installer that we will bring our own binaries.*

(TODO is this ^ actually necessary? I've never done it)

From here, you can operate inside the virtual environment by running:

```
$ python -m pipenv shell
```

At this point we can test our installation with:

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

### Testing

`semgrep` uses [`pytest`](https://docs.pytest.org/en/latest/) for testing. 

(TODO isn't it necessary to do `pip install -e .`? And `pipenv shell`?)

Running the tests is as simple as running:

```
$ python -m pytest
```

Running a single test file is simple too:

```
$ python -m pytest path/to/test.py
```

Or running an individual test function:

```
$ python -m pytest -k test_func_name path/to/test.py
```

`semgrep` includes a more comprehensive QA test suite that can be run like so:

```
$ python -m pytest --qa
```

`semgrep` also includes [`pytest-benchmark`](https://pytest-benchmark.readthedocs.io/en/latest/)
to allow for basic benchmarking functionality. This can be run like so:

```
$ python -m pytest --benchmark-only
```
