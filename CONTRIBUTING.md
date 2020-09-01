# Contributing

Semgrep welcomes contributions from anyone. If you have an idea for a feature
or notice a bug please [open an issue](https://github.com/returntocorp/semgrep/issues/new/choose).
Creating an issue first is preferable to moving directly to a pull request so
that we can ensure you're on the right track without any wasted effort. This
is also a great way to contribute to Semgrep even if you're not making changes
yourself.

Contents:

* [Development Workflow](#development-workflow)
* [`semgrep` Development](#semgrep-development)
  * [Installing](#installing)
  * [Testing](#testing)
* [`semgrep-core` Development](#semgrep-core-development)
  * [Installing from Source](#installing-from-source)
  * [Running](#running)
  * [Development Environment](#development-environment)
  * [Profiling Code](#profiling-code)
  * [Testing](#testing-1)
* [Backwards Incompatible Changes](#backwards-incompatible-changes)

## Development Workflow

Semgrep runs on Python versions >= 3.6. If you don't have one of these
versions installed, please do so before proceeding.

Before each commit Semgrep will run [`pre-commit`](https://pre-commit.com/) to
ensure files are well-formatted and check for basic linting bugs. If you don't
have `pre-commit` installed the following command will do so for you:

```
$ python -m pip install pre-commit
```

Our `pre-commit` configuration uses Docker images. Please ensure you have
[Docker installed](https://docs.docker.com/get-docker/) before running
`pre-commit`. Install the `pre-commit` hooks with the following command:

```
$ pre-commit install
```

To ensure `pre-commit` is working as expected, run the following command:

```
$ pre-commit run --all
```

Once `pre-commit` is working you may commit code and create pull requests as
you would expect. Pull requests require approval of at least one maintainer and
[CI to be passing](https://github.com/returntocorp/semgrep/actions).

## `semgrep` Development

The `semgrep` directory contains mostly Python code and is the interface
between the user and `semgrep-core`. When running `semgrep` locally you
will either need to [build and install `semgrep-core` locally](#semgrep-core-development)
or install the most recently released binary. We will get into that more
later.

### Installing

Most `semgrep` development will operate inside the `semgrep` directory (`semgrep/semgrep`):

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
$ python -m pipenv install --dev
```

From here, you can operate inside the virtual environment by running:

```
$ python -m pipenv shell
```

At this point we can test our installation, but cannot run any analysis yet
because we don't have an accompanying `semgrep-core`:

```
$ python -m semgrep -h
```

Note that in the virtual environment `semgrep` was installed in "editable"
mode. This means any changes you make to the Python code will immediately take
effect when running `python -m semgrep`.

As mentioned previously, we need `semgrep-core` installed locally to perform
any kind of analysis. If you're also working on `semgrep-core` code you may
wish to [build and install `semgrep-core` locally](#semgrep-core-development),
otherwise we can simply grab the most recently released binary and use that. To
do so visit the [releases page](https://github.com/returntocorp/semgrep/releases)
and grab the latest zipfile or tarball for your platform. Extract this archive
and inside should be a `semgrep-core` binary that you can run. You can confirm
this by running:

```
$ ./semgrep-core -version
```

Copy this file to somewhere in your `$PATH` so `semgrep` can find it. For
example, you may create a `~/bin/` directory, [include it in your `$PATH`](https://unix.stackexchange.com/questions/26047/how-to-correctly-add-a-path-to-path)
and run the binary from there. Another option is including it somewhere like
`/usr/local/bin/`. Once `semgrep-core` is available we can check and make sure
`semgrep` is fully operational by running a simple analysis:

```
$ cat << EOF > test.py
if 5 == 5:
    print("Useless comparison")
EOF
```

```
$ python test.py
Useless comparison
```

```
$ python -m semgrep --lang python --pattern '$X == $X' test.py
test.py
1:if 5 == 5:
```

Congratulations, you have Semgrep running locally!

### Testing

`semgrep` uses [`pytest`](https://docs.pytest.org/en/latest/) for testing.
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

## `semgrep-core` Development

The `semgrep-core` directory contains mostly OCaml code and performs the heavy
lifting program analysis work that makes Semgrep so powerful.

### Installing from Source

To compile semgrep, you first need to [install OCaml](https://opam.ocaml.org/doc/Install.html)
and its package manager OPAM. On macOS, it should simply consist in doing:

```bash
brew install opam
opam init
opam switch create 4.10.0
opam switch 4.10.0
eval $(opam env)
```

Install `pkg-config` in your environment. On a mac this is

```bash
brew install pkg-config
```

The root `Makefile` contains targets that take care of building the
right things. It is commented. Please refer to it and keep it
up-to-date.

For a first installation or if you're told external dependencies have
changed, run

```
make setup
```

For a routine build after pulling in source code changes from git, run

```
make rebuild
```

If you're just editing source code locally, including code in the submodules,
the following is sufficient:

```
make build  # or just 'make'
```

For a reference build that's known to work, consult the root `Dockerfile`
to build semgrep inside a container. You can check that it builds with

```
docker build -t semgrep .
```

### Running

Then to test semgrep on a file, for example tests/GENERIC/test.py run:

```bash
$ cd semgrep-core
$ ./_build/default/bin/Main.exe -e foo -lang python tests/python
```

If you want to test semgrep on a directory with a set of given rules, run:

```bash
$ cp ./semgrep-core/_build/default/bin/Main.exe /usr/local/bin/semgrep_core
$ cd semgrep
$ pipenv install --dev
$ pipenv run semgrep --config <YAML_FILE_OR_DIRECTORY> <code to check>
```

Note pipenv run command must be run from semgrep directory. If you want to run on other directories
run `pipenv shell` to enter pipenv virtual environment.

### Development Environment

You can use Visual Studio Code \(vscode\) to edit the code of Semgrep. The [reason-vscode](https://marketplace.visualstudio.com/items?itemName=jaredly.reason-vscode) Marketplace extension adds support for OCaml/Reason.

The OCaml and Reason IDE extension by David Morrison is another valid extension, but it seems not as actively maintained as reason-vscode.

The source of Semgrep contains also a .vscode/ directory at its root containing a task file to automatically build Semgrep from vscode.

Note that dune and ocamlmerlin must be in your PATH for vscode to correctly build and provide cross-reference on the code. In case of problems, do:

```bash
$ cd /path/to/semgrep
$ eval $(opam env)
$ dune        --version # just checking dune is in your PATH
$ ocamlmerlin -version  # just checking ocamlmerlin is in your PATH
$ code .
```

### Profiling Code

You can pass the -profile command-line argument to semgrep-core to get
a short profile of the code, for example:

``` bash
$ cd semgrep-core
$ ./_build/default/bin/Main.exe -profile -e foo tests/python
---------------------
profiling result
---------------------
Main total                               :      1.975 sec          1 count
Parse_python.parse                       :      0.828 sec          1 count
...
```

You can also instead set the environment variable SEMGREP_CORE_PROFILE to 1 to get the same information:

```bash
cd semgrep-core
export SEMGREP_CORE_PROFILE=1
./_build/default/bin/Main.exe -e foo tests/python
---------------------
profiling result
---------------------
Main total                               :      1.975 sec          1 count
Parse_python.parse                       :      0.828 sec          1 count
...
```

This is especially useful when you don't call directly semgrep-core, but
instead use the python wrapper semgrep.

You can also use the SEMGREP_CORE_DEBUG environment variable to add debugging
information, for example:

```bash
export SEMGREP_CORE_DEBUG=1
export SEMGREP_CORE_PROFILE=1
pipenv run semgrep -f ../semgrep-core/tests/PERF/ajin.yaml ../semgrep-core/tests/PERF/three.js
Debug mode On
Executed as: semgrep-core -lang javascript -rules_file /tmp/tmpy5pzp3p_ -j 8 ../semgrep-core/tests/PERF/three.js
Profile mode On
disabling -j when in profiling mode
PARSING: ../semgrep-core/tests/PERF/three.js
saving rules file for debugging in: /tmp/semgrep_core_rule-97ae74.yaml
---------------------
profiling result
---------------------
Main total                               :      1.975 sec          1 count
Parse_js.parse                           :      0.828 sec          1 count
Semgrep.check                            :      0.791 sec          1 count
Semgrep.match_sts_sts                    :      0.559 sec     185064 count
...
```

### Testing

`make test` in the `semgrep-core` directory will run tests that check code is correctly parsed
and patterns perform as expected. To add a test in an appropriate language subdirectory, `semgrep-core/tests/LANGUAGE`, create a target file (expected file extension given language) and a .sgrep file with a pattern. The testing suite will check that all places with a comment with `ERROR` were matches found by the .sgrep file. See existing tests for more clarity.
