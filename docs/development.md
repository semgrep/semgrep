# sgrep

[![CircleCI](https://circleci.com/gh/returntocorp/sgrep.svg?style=svg)](https://circleci.com/gh/returntocorp/sgrep)

## Pre-Commit

sgrep uses [pre-commit](https://pre-commit.com/). [Install pre-commit](https://pre-commit.com/#installation) then:

```bash
pre-commit install
```

## Installation from source

To compile sgrep, you first need to [install OCaml](https://opam.ocaml.org/doc/Install.html) and its
package manager OPAM.
On macOS, it should simply consist in doing:

```bash
brew install opam
opam init
opam switch create 4.07.1
opam switch 4.07.1
eval $(opam env)
```

Once OPAM is installed, you need to install the library pfff,
the OCaml frontend reason, and the build system dune:

```bash
opam install pfff
opam install reason
opam install dune
```

sgrep probably needs the very latest features of pfff, which may not
be yet in the latest OPAM version of pfff. In that case, install pfff
manually by doing:

```bash
git submodule init && git submodule update --remote
cd pfff
./configure; make depend; make; make opt; make reinstall-libs
```

Then you can compile the program with:

```bash
cd sgrep
dune build
```

You can also use the Dockerfile in this directory to build sgrep
inside a container.

## Run

Then to test sgrep on a file, for example tests/GENERIC/test.py
run:

```bash
cd sgrep
./_build/default/bin/main_sgrep.exe -e foo tests/python
...
```

If you want to test sgrep on a directory with a set of given rules, run:

```bash
cp ./sgrep/_build/default/bin/main_sgrep.exe /usr/local/bin/sgrep
cd sgrep_lint
make && make install
sgrep-lint --config <YAML_FILE_OR_DIRECTORY> <code to check>
```

## Development Environment

You can use Visual Studio Code (vscode) to edit the code of sgrep.
The [reason-vscode](https://marketplace.visualstudio.com/items?itemName=jaredly.reason-vscode) Marketplace extension adds support for OCaml/Reason.

The OCaml and Reason IDE extension by David Morrison is another valid
extension, but it seems not as actively maintained as reason-vscode.

The source of sgrep contains also a .vscode/ directory at its root
containing a task file to automatically build sgrep from vscode.

Note that dune and ocamlmerlin must be in your PATH for vscode to correctly
build and provide cross-reference on the code. In case of problems, do:

```bash
cd /path/to/sgrep
eval $(opam env)
dune        --version # just checking dune is in your PATH
ocamlmerlin -version  # just checking ocamlmerlin is in your PATH
code .
```

## Debugging code

Set the OCAMLRUNPARAM environment variable to 'b' for backtrace.
You will get better backtrace information when an exception is thrown.

```bash
export OCAMLRUNPARAM=b
```
