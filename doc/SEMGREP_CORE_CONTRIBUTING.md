# `semgrep-core` Contributing

The following explains how to build `semgrep-core` (and `spacegrep`) so you can make and test changes to the OCaml code. Once you have `semgrep-core` and `spacegrep` installed, you can refer to SEMGREP_CONTRIBUTING.md to see how to build and run the Semgrep application.

Contents:

* [Building `semgrep-core` and `spacegrep`]

  * [Development Environment](#development-environment)
  * [Profiling Code](#profiling-code)
  * [Benchmarking Code](#benchmarking-code)
  * [Testing](#testing-1)
* [Adding Support for a Language](#adding-support-for-a-language)
  * [Legacy Parsers](#legacy-parsers)
  * [Tree-Sitter Parsers](#tree-sitter-parsers)
  * [Tips for converting CST to generic AST](cst-to-ast-tips.md)

## Building `semgrep-core` and `spacegrep`

### Requirements

Both `semgrep-core` and `spacegrep` are written primarily in OCaml. You will need to [install OCaml](https://opam.ocaml.org/doc/Install.html) and its package manager OPAM. On macOS, it should consist in doing:

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

### 

The root `Makefile` contains targets that take care of building the
right things. It is commented. Please refer to it and keep it
up-to-date.

For a first installation or if you're told external dependencies have
changed, run

```
make dev-setup
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

### Testing

`make test` in the `semgrep-core` directory will run tests that check code is correctly parsed
and patterns perform as expected. To add a test in an appropriate language subdirectory, `semgrep-core/tests/LANGUAGE`, create a target file (expected file extension given language) and a .sgrep file with a pattern. The testing suite will check that all places with a comment with `ERROR` were matches found by the .sgrep file. See existing tests for more clarity.


### Running

Then to test semgrep on a file, for example tests/GENERIC/test.py run:

```bash
$ cd semgrep-core
$ ./bin/semgrep-core -e foo -lang python tests/python
```

If you want to test semgrep on a directory with a set of given rules, run:

```bash
$ cp ./semgrep-core/bin/semgrep-core /usr/local/bin/semgrep_core
$ cd semgrep
$ pipenv install --dev
$ pipenv run semgrep --config <YAML_FILE_OR_DIRECTORY> <code to check>
```

Note pipenv run command must be run from semgrep directory. If you want to run on other directories
run `pipenv shell` to enter pipenv virtual environment.

### Development Environment

You can use Visual Studio Code \(vscode\) to edit the code of Semgrep. The [reason-vscode](https://marketplace.visualstudio.com/items?itemName=jaredly.reason-vscode) Marketplace extension adds support for OCaml/Reason.

The [OCaml and Reason IDE extension](https://github.com/reasonml-editor/vscode-reasonml) by @freebroccolo is another valid extension, but it seems not as actively maintained as reason-vscode.

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
$ ./bin/semgrep-core -profile -e foo tests/python
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
./bin/semgrep-core -e foo tests/python
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

### Benchmarking Code

We have two sets of benchmarks, one on a suite of real repos against real rulesets (real benchmarks), another that highlights specific slow (rule, file) pairs (micro benchmarks).

To run the micro benchmarks, go to `semgrep-core/perf`, and run `./run-perf-suite`.

To run the real benchmarks, go to `perf`, and run `./run-benchmarks`. See the perf [readme](https://github.com/returntocorp/semgrep/blob/develop/perf/README.md) for more details on how these are set up.

There are a number of flags (`./run-benchmarks --help` to see them) which may be helpful if you are using the benchmarks for local development. For example, `./run-benchmarks --plot_benchmarks` will output a graph of the benchmark results at the end.

If you are concerned about performance, the recommended way to test is to hide your change behind a flag and add that flag to run-benchmarks. Add a flag in `semgrep-core/src/core/Flag_semgrep.ml`. These are ref cells, so you can check whether the flag is enabled or not via `!Flag_semgrep.your_flag`. In `semgrep-core/src/cli/Main.ml`, go to options, and add a flag that sets the appropriate `Flag_semgrep`. Then, in `perf/run-benchmarks`, go to the `SemgrepVariants` list, and add your variant.

You can also test the impact of your change by running `./run_benchmarks --std_only` in `perf`, which will only run the default version of semgrep.

## Adding Support for a Language

The parsers used by semgrep fall into these categories:
* legacy parsers (pfff): implemented directly in OCaml via a parser generator
* tree-sitter parsers: third-party parsers implemented as
  [tree-sitter](https://tree-sitter.github.io/) grammars
* generic parser (spacegrep): fallback for unsupported languages,
  comes with its own matching engine

For each language, we need a parser for target files and a parser for
semgrep patterns. For a given language, ideally both would use the same
parser. For historical reasons, some languages use a legacy
parser for patterns and a tree-sitter parser for target code.
Here's the breakdown by language as of February 2021:

* legacy parser for both pattern and target:
  - OCaml
  - PHP
  - Python
* legacy parser for pattern, tree-sitter parser for target:
  - C
  - Go
  - Java
  - JavaScript, JSX, JSON
  - Ruby
  - TypeScript, TSX
* tree-sitter parser for both pattern and target:
  - C#
  - Kotlin
  - Lua
  - R
  - Rust

New languages should use tree-sitter.

### Legacy Parsers

These parsers are implemented in
[pfff](https://github.com/returntocorp/pfff), which is an OCaml
project that we plug into semgrep-core as a git submodule.

### Tree-Sitter Parsers

Tree-sitter parsers exist as individual public projects. They are
shared with other users of tree-sitter outside of semgrep. Our
[ocaml-tree-sitter](https://github.com/returntocorp/ocaml-tree-sitter)
project adds the necessary extensions for supporting semgrep patterns
(ellipsis `...` and such). It also contains the machinery for turning
a tree-sitter grammar into a usable, typed concrete syntax tree (CST).

For example, for the Kotlin language we have:
* input: [tree-sitter-kotlin](https://github.com/fwcd/tree-sitter-kotlin)
* output: [semgrep-kotlin](https://github.com/returntocorp/semgrep-kotlin)

Assuming the tree-sitter grammar works well enough, most of the work
consists in mapping the CST to the generic abstract syntax tree (AST)
shared by all languages in semgrep.

These guides go over the integration work in more details:

* [How to add support for a new language](https://github.com/returntocorp/ocaml-tree-sitter/blob/master/doc/adding-a-language.md)
* [How to upgrade the grammar for a language](https://github.com/returntocorp/ocaml-tree-sitter/blob/master/doc/updating-a-grammar.md)
* [Tips for converting CST to generic AST](cst-to-ast-tips.md)
