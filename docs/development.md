# Development

## Pre-Commit

semgrep uses [pre-commit](https://pre-commit.com/). [Install pre-commit](https://pre-commit.com/#installation) then:

```bash
pre-commit install
```

## Installation from source

To compile semgrep, you first need to [install
OCaml](https://opam.ocaml.org/doc/Install.html) and its package
manager OPAM. On macOS, it should simply consist in doing:

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

## Run

Then to test semgrep on a file, for example tests/GENERIC/test.py run:

```bash
cd semgrep-core
./bin/semgrep-core -e foo -l python tests/python
...
```

If you want to test semgrep on a directory with a set of given rules, run:

```bash
sudo cp ./semgrep_core/bin/semgrep-core /usr/local/bin/semgrep_core
cd semgrep
pipenv install --dev
# You need to BYO semgrep-core -- You can either:
# - Build it yourself (as you did above. Put it on your path somewhere as semgrep-core)
# - Download it from a release
# - Grab it from pip (pip install semgrep), then steal the binary and put it somewhere.

# Especially if you have multiple versions installed, use `pipenv run` to be sure you're running the
# semgrep you're trying to modify
pipenv run semgrep --config <YAML_FILE_OR_DIRECTORY> <code to check>
```

Note pipenv run command must be run from semgrep directory. If you want to run on other directories
run `pipenv shell` to enter pipenv virtual environment.

## Development Environment

You can use Visual Studio Code \(vscode\) to edit the code of semgrep. The [reason-vscode](https://marketplace.visualstudio.com/items?itemName=jaredly.reason-vscode) Marketplace extension adds support for OCaml/Reason.

The OCaml and Reason IDE extension by David Morrison is another valid extension, but it seems not as actively maintained as reason-vscode.

The source of semgrep contains also a .vscode/ directory at its root containing a task file to automatically build semgrep from vscode.

Note that dune and ocamlmerlin must be in your PATH for vscode to correctly build and provide cross-reference on the code. In case of problems, do:

```bash
cd /path/to/semgrep
eval $(opam env)
dune        --version # just checking dune is in your PATH
ocamlmerlin -version  # just checking ocamlmerlin is in your PATH
code .
```

For Python, Pycharm is good.

## Debugging code

Set the OCAMLRUNPARAM environment variable to 'b' for backtrace. You will get better backtrace information when an exception is thrown.

```bash
export OCAMLRUNPARAM=b
```

## Profiling code

You can pass the -profile command-line argument to semgrep-core to get
a short profile of the code, for example:
``` bash
cd semgrep_core
./_build/default/bin/Main.exe -profile -e foo tests/python
---------------------
profiling result
---------------------
Main total                               :      1.975 sec          1 count
Parse_python.parse                       :      0.828 sec          1 count
...
```

You can also instead set the environment variable SEMGREP_CORE_PROFILE to 1 to get the same information:

``` bash
cd semgrep_core
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

## Testing

### semgrep-core

`make test` in the `semgrep-core` directory will run tests that check code is correctly parsed
and patterns perform as expected. To add a test in an appropriate lannguage subdirectory, `semgrep-core/tests/LANGUAGE`, create a target file (expected file extension given language) and a .sgrep file with a pattern. The testing suite will check that all places with a comment with `ERROR` were matches found by the .sgrep file. See existing tests for more clarity.

### semgrep

`make test` will run unit tests while `make qa-tests` will run semgrep over a set of github repositories to check that there are no parse errors.

## Overriding expected integration test outputs

When you're confident in the changes and you want to overwrite the `*.expected.json` files you can do this for semgrep

```bash
OVERRIDE_EXPECTED=1 make test
```
