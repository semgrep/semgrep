# Development

[![CircleCI](https://circleci.com/gh/returntocorp/semgrep.svg?style=svg)](https://circleci.com/gh/returntocorp/semgrep)

## Pre-Commit

semgrep uses [pre-commit](https://pre-commit.com/). [Install pre-commit](https://pre-commit.com/#installation) then:

```bash
pre-commit install
```

## Installation from source

To compile semgrep, you first need to [install OCaml](https://opam.ocaml.org/doc/Install.html) and its package manager OPAM. On macOS, it should simply consist in doing:

```bash
brew install opam
opam init
opam switch create 4.07.1
opam switch 4.07.1
eval $(opam env)
```

Install `pfff` which is a dependency of `semgrep`

```bash
git submodule init && git submodule update --init --recursive
eval $(opam env) && opam install -y ./pfff
```

Then you can compile the program with:

```bash
cd semgrep_core
make all
```

You can also use the Dockerfile in this directory to build semgrep inside a container.

## Run

Then to test semgrep on a file, for example tests/GENERIC/test.py run:

```bash
cd semgrep_core
./_build/default/bin/main_sgrep.exe -e foo tests/python
...
```

If you want to test semgrep on a directory with a set of given rules, run:

```bash
cp ./semgrep_core/_build/default/bin/main_sgrep.exe /usr/local/bin/semgrep_core
cd semgrep
make && make install
semgrep --config <YAML_FILE_OR_DIRECTORY> <code to check>
```

It is also possible to run semgrep from source:

```text
cd semgrep
python3 -m semgrep
```

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

## Debugging code

Set the OCAMLRUNPARAM environment variable to 'b' for backtrace. You will get better backtrace information when an exception is thrown.

```bash
export OCAMLRUNPARAM=b
```

## Overriding expected integration test outputs

When you're confident in the changes and you want to overwrite the `*.expected.json` files you can do this for semgrep

```bash
OVERRIDE_EXPECTED=1 make test
```

