# `semgrep-core` Contributing

The following explains how to build `semgrep-core` so you can make and test changes to the OCaml code. This will also install the `spacegrep` binary so it can be called by `semgrep`. Once you have `semgrep-core` and `spacegrep` installed, you can refer to SEMGREP_CONTRIBUTING.md to see how to build and run the Semgrep application.

Contents:

* [Building `semgrep-core`](#building-semgrep-core)

  * [Development Environment](#development-environment)
  * [Profiling Code](#profiling-code)
  * [Benchmarking Code](#benchmarking-code)
  * [Testing](#testing-1)
* [Adding Support for a Language](#adding-support-for-a-language)
  * [Legacy Parsers](#legacy-parsers)
  * [Tree-Sitter Parsers](#tree-sitter-parsers)
  * [Tips for converting CST to generic AST](cst-to-ast-tips.md)

## Building `semgrep-core`

### Requirements

`semgrep-core` is written primarily in OCaml. You will need to [install OCaml](https://opam.ocaml.org/doc/Install.html) and its package manager OPAM. On macOS, it should consist in doing:

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

### Installing for the First Time

The root `Makefile` contains targets that take care of building the
right things. It is commented. Please refer to it and keep it
up-to-date.

To link all necessary dependencies, run (at the repository root `semgrep/`)

```
make dev-setup
```

Next, to install `semgrep-core`, run

```
make build-core
```

Finally, test the installation with 

```
semgrep-core -help
```

You should also be able to run

```
spacegrep --help
```

At this point, you have the `semgrep-core` and `spacegrep` binaries, so if you would like to finish the Semgrep installation, go to the [Python-side instructions](link).

### Installing After a Change

Unless there is a significant dependency change, you will not need to run `make dev-setup` again. 

We have provided useful targets to help you build and link the entire semgrep project, including both `semgrep-core` and `semgrep`. You may find these helpful.

To install the latest OCaml binaries and `semgrep` binary after pulling source code changes from git, run

```
make rebuild
```

To install after you make a change locally, run
```
make build    # or just `make`
```

After making either of these targets, `semgrep` will run with all your local changes, OCaml and Python both.

(Note: Because this updates the `semgrep` binary, if you do not have your Python environment configured properly, you will encounter errors when running these commands. Follow the procedure under [Development](Development))

## Development 

In practice, it is not always convenient to use `make build` or `make rebuild`. `make rebuild` will update everything within the project; `make build` will compile and install all the binaries. You can do this yourself in a more targeted fashion.

Below is a flow appropriate for frequent developers of `semgrep-core` 

After you pull, run

```
git submodule update --recursive
```

This will update internal dependencies. (We suggest aliasing it to `uu`)

After `tree-sitter` is updated, you may need to reconfigure it. If so, run

```
make config
```

### Developing `semgrep-core`

If you are developing `semgrep-core`, enter the `semgrep/semgrep-core/` directory. The `Makefile` for `semgrep-core`-specific targets is at `semgrep/semgrep-core/`; the code is primarily in `src/`.

The following assumes you are in `semgrep/semgrep-core/`.

After you pull or make a change, compile using

```
make
```

This will build an executable for `semgrep-core` in `semgrep/semgrep-core/_build/default/src/cli/Main.exe` (we suggest aliasing this to `sc`). Try it out by running

```
_build/default/src/cli/Main.exe -help
```

When you are done, test your changes with 

```
make test
```

Finally, to update the `semgrep-core` binary used by `semgrep`, run

```
make install
```

### Testing `semgrep-core`

`make test` in the `semgrep-core` directory will run tests that check code is correctly parsed
and patterns perform as expected. To add a test in an appropriate language subdirectory, `semgrep-core/tests/LANGUAGE`, create a target file (expected file extension given language) and a .sgrep file with a pattern. The testing suite will check that all places with a comment with `ERROR` were matches found by the .sgrep file. See existing tests for more clarity.

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

## Testing Performance

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

***

In these next sections we will give an overview of `semgrep-core` and then some tips for making common changes to `semgrep-core`. These are only tips; without seeing an error, we cannot know its cause and proper resolution, but hopefully it gives useful direction.

## `semgrep-core` Overview

### Entry Point

The entry point to `semgrep-core` is `Main.ml`, in `semgrep-core/src/cli`. Add command-line arguments there. It calls functions depending on the mode in which `semgrep-core` was invoked (`-config` for a yaml file, `-f` for a single pattern, etc.)

When invoked by `semgrep`, `semgrep-core` is called by default wih `-config`. This corresponds to the function `semgrep-with-rules-file`, which in turn calls `semgrep-with-rules`.

### Parsing

`semgrep-core` uses an external module to parse code into an augmented language-specific abstract syntax tree (AST). Though we call these ASTs, they additionally contain token information such as parentheses that are traditionally only present in concrete syntax trees (CSTs) so that we can output results in the correct range.

When `semgrep-core` receives a rule or a target, it will first need to parse it. The functions that do this are located in `semgrep-core/src/parsing/`. 

* If it reads a rule, it will go through `Parse_rule.ml`, which uses `Parse_pattern.ml` to parse the code-like portions of the rule
* If it reads a target, it will go through `Parse_target.ml`

Depending on the language, `Parse_pattern.ml` and `Parse_target.ml` will invoke parsers to parse the code. For example, if we have Java code, at this point it will be parsed into a Java-specific AST.

### Converting to the Generic AST

`semgrep-core` does not match based on that AST. It has a generic AST, defined in `AST_generic` (in `semgrep-core/src/core/ast`), which all other ASTs are converted to.

The functions for this conversion are in either `semgrep-core/src/parsing/pfff/` or `semgrep-core/src/parsing/tree-sitter/`. They are named with the appropriate language. We will talk about them more later.

### Matching

The matching functions are contained in `semgrep-core/src/engine` (`Match_rules.ml`, `Match_patterns.ml`) and `semgrep-core/src/matching` (`Generic_vs_generic.ml`). There are several possible matchers to invoke

* spacegrep (for generic mode)
* regexp (to match by regexp instead of semgrep patterns)
* comby (an experimental mode for C++)
* pattern (the main mode)

We will only talk about the last for now. In most cases, you will go to the `check` function in `Match_patterns.ml`. This will traverse (visit) the target AST and try to match the pattern to it at each point. If the pattern and the target node correspond, it will call the relevant function in `Generic_vs_generic.ml`.

The core of the matching is done by `Generic_vs_generic.ml`. The logic for whether two expressions, statements, etc. match is contained within this file. 

## Fixing a Parse Error 

Before you start fixing a parse error, you need to know what parser was used. This bears some explanation.

### Guide to Parsers

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

###

Within `semgrep-core/src/`, there are two folders, `pfff/` and `o

## Adding Support for a Language

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
