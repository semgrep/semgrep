# sgrep

[![CircleCI](https://circleci.com/gh/returntocorp/sgrep.svg?style=svg)](https://circleci.com/gh/returntocorp/sgrep)

## Installation from source

To compile Sgrep, you first need to install OCaml and its
package manager OPAM. See https://opam.ocaml.org/doc/Install.html
On macOS, it should simply consist in doing:

```
$ brew install opam
$ opam init
$ opam switch create 4.07.1
$ opam switch 4.07.1
$ eval $(opam env)
```

Once OPAM is installed, you need to install the dependent
libraries pfff, the OCaml frontend reason, and the build system dune:

```
$ opam install pfff
$ opam install reason
$ opam install dune
```

Then you can compile the program with:

```
$ dune build
```

## Run 

Then to test sgrep on a file, for example tests/GENERIC/test.py
run:

```
$ ./_build/default/bin/main_sgrep.exe -e foo tests/GENERIC/test.py
...
```

## Development Environment

You can use Visual Studio Code (vscode) to edit the code of sgrep. 
The reason-vscode Marketplace extension adds support for OCaml/Reason
(see https://marketplace.visualstudio.com/items?itemName=jaredly.reason-vscode).
The OCaml and Reason IDE extension by David Morrison is another valid 
extension, but it seems not as actively maintained as reason-vscode.

The source of sgrep contains also a .vscode/ directory at its root
containing a task file to automatically build sgrep from vscode.

Note that dune and ocamlmerlin must be in your PATH for vscode to correctly
build and provide cross-reference on the code. In case of problems, do:

```
$ cd /path/to/sgrep
$ eval $(opam env)
$ dune        --version # just checking dune is in your PATH
$ ocamlmerlin -version  # just checking ocamlmerlin is in your PATH
$ code .
```

## Debugging code

Set the OCAMLRUNPARAM environment variable to 'b' for backtrace. 
You will get better backtrace information when an exception is thrown.

$ export OCAMLRUNPARAM=b

