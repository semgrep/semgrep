# `semgrep-core` Contributing

The following explains how to build `semgrep-core` so you can make and test changes to the OCaml code. This will also install the `spacegrep` binary so it can be called by `semgrep`. Once you have `semgrep-core` and `spacegrep` installed, you can refer to SEMGREP_CONTRIBUTING.md to see how to build and run the Semgrep application.

Contents:

* [Building `semgrep-core`](#building-semgrep-core)
  * [Requirements](#requirements)
  * [Installing for the First Time](#installing-for-the-first-time)
  * [Installing After a Change](#installing-after-a-change)
  * [Development](#development)
  * [Developing `semgrep-core`](#developing-semgrep-core)
  * [Testing `semgrep-core`](#testing-semgrep-core)
  * [Development Environment](#development-environment)
* [Testing Performance](#testing-performance)
  * [Profiling Code](#profiling-code)
  * [Benchmarking Code](#benchmarking-code)

* [Cheatsheet](#cheatsheet)
* [`semgrep-core` Overview](#semgrep-core-overview)
  * [Entry Point](#entry-point)
  * [Parsing](#parsing)
  * [Converting to the Generic AST](#converting-to-the-generic-ast)
  * [Matching](#matching)
  * [Reporting Results](#reporting-results)
* [Fixing a Parse Error](#fixing-a-parse-error)
  * [Guide to Parsers](#guide-to-parsers)
  * [Fixing a `pfff` Parse Error](#fixing-a-pfff-parse-error)
    * [Parsing with `pfff`](#parsing-with-pfff)
    * [Identifying the Error](#identifying-the-error)
    * [Fixing the Error](#fixing-the-error)
    * [Commiting the Fix](#commiting-the-fix)
  * [Fixing a Tree-sitter Parse Error](#fixing-a-tree-sitter-parse-error)
* [Fixing a Match Error](#fixing-a-match-error)
* [Fixing an Autofix Error](#fixing-an-autofix-error)
* [Debugging Resources](#debugging-resources)
* [Adding Support for a Language](#adding-support-for-a-language)
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

At this point, you have the `semgrep-core` and `spacegrep` binaries, so if you would like to finish the Semgrep installation, go to the [Python-side instructions](SEMGREP_CONTRIBUTING.md).

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

(Note: Because this updates the `semgrep` binary, if you do not have your Python environment configured properly, you will encounter errors when running these commands. Follow the procedure under [Development](#development))

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

## Cheatsheet

The following assume you are in `semgrep/semgrep-core/`.

Compilation:

* To compile: `make`
* To run the test suite: `make test`
* To install the `semgrep-core` binary: `make install`
* The `semgrep-core` executable produced by `make`: `_build/default/src/cli/Main.exe` (alias `sc`)
* The `pfff` executable produced by `make`: `_build/default/src/pfff/cli/Main.exe` (alias `pf`)

Running (examples in Python):

* To match a rule file against a target: `sc -config [your-rule].yaml [your-target].py -lang python`
* To match a pattern against a target: `sc -f [your-pattern].sgrep [your-target].py -lang python`
* To dump a pattern AST: `sc -dump_pattern [your-pattern].sgrep -lang python`
* To dump a target AST: `sc -dump_ast [your-target].py -lang python` 
* To dump a pattern Python AST: `pf -dump_python [your_pattern].sgrep -sgrep_mode -lang python`
* To dump a target Python AST: `pf -dump_python [your_pattern].sgrep -lang python`

Try it out: `sc -f tests/python/dots_stmts.sgrep tests/python/dots_stmts.py -lang python`

## `semgrep-core` Overview

### Entry Point

The entry point to `semgrep-core` is `Main.ml`, in `semgrep-core/src/cli/`. This is where you add command-line arguments. It calls functions depending on the mode in which `semgrep-core` was invoked (`-config` for a yaml file, `-f` for a single pattern, etc.)

When invoked by `semgrep`, `semgrep-core` is called by default wih `-config`. This corresponds to the function `semgrep_with_rules_file`, which in turn calls `semgrep_with_rules`. These functions will parse and then match the rule and targets.

### Parsing

`semgrep-core` uses external modules to parse code into augmented language-specific abstract syntax trees (ASTs). Though we call these ASTs, they additionally contain token information such as parentheses that are traditionally only present in concrete syntax trees (CSTs) so that we can output results in the correct range.

When `semgrep-core` receives a rule or a target, it will first need to parse it. The functions that do this are located in `semgrep-core/src/parsing/`. 

* If it reads a rule, it will go through `Parse_rule.ml`, which uses `Parse_pattern.ml` to parse the code-like portions of the rule
* If it reads a target, it will go through `Parse_target.ml`

Depending on the language, `Parse_pattern.ml` and `Parse_target.ml` will invoke parsers to parse the code. For example, if we have Java code, it will first be parsed into a Java-specific AST.

### Converting to the Generic AST

`semgrep-core` does not match based on the Java AST. It has a generic AST, defined in `AST_generic.ml` (in `semgrep-core/src/core/ast`), which all language-specific ASTs are converted to.

The functions for this conversion are in either `semgrep-core/src/parsing/pfff/` or `semgrep-core/src/parsing/tree-sitter/`. They are named with the appropriate language in a consistent convention.

### Matching

The matching functions are contained in `semgrep-core/src/engine/` (e.g. `Match_rules.ml`, `Match_patterns.ml`) and `semgrep-core/src/matching/` (e.g. `Generic_vs_generic.ml`). There are several possible matchers to invoke

* spacegrep (for generic mode)
* regexp (to match by regexp instead of semgrep patterns)
* comby (an experimental mode for languages we don't yet support)
* pattern (the main mode)

We will only talk about the last for now. In most cases, `Match_rules.ml` will invoke the `check` function in `Match_patterns.ml`. This will visit the target AST and try to match the pattern to it at each point. If the pattern and the target node correspond, it will call the relevant function in `Generic_vs_generic.ml`.

The core of the matching is done by `Generic_vs_generic.ml`. The logic for whether two expressions, statements, etc. match is contained within this file. 

### Reporting results

The results of the match will be returned to the calling function in `Main.ml` (for example, `semgrep_with_rules`). From there, the results are formatted and outputted.

There are two modes for outputting: JSON and text. JSON output is processed by functions in `JSON_report.ml` in `semgrep-core/src/reporting/`

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

### Fixing a `pfff` Parse Error

#### Parsing With `pfff`

[`pfff`](https://github.com/returntocorp/pfff) is an OCaml project that we plug into `semgrep-core` as a git submodule. It uses menhir to generate parsers from a defined grammar.

Consider a Python pattern (or target). To parse it into a generic AST form, we transform the code as follows:

Text -- (via `Lexer_python.mll`) --> Tokens -- (via `Parser_python.mly`) --> `Ast_python` -- (via `Python_to_generic.ml`) --> `AST_generic`

These files live in different places. Specifically,

* `Lexer_python.mll` is in `semgrep-core/src/pfff/lang_python/parsing`
* `Parser_python.mly` is in `semgrep-core/src/pfff/lang_python/parsing`
* `AST_python.ml` is in `semgrep-core/src/pfff/lang_python/parsing`
* `Python_to_generic.ml` is in `semgrep-core/src/parsing/pfff`
* `AST_generic.ml` is in `semgrep-core/src/core/ast`

You will notice that the first three, `Lexer_python.mll`, `Parser_python.mly`, and `AST_python.ml` are in `semgrep-core/src/pfff/`, which is a submodule. This means that when you modify them, you modify the submodule rather than `semgrep-core`. You can develop as usual---`pfff` is compiled when you run `make` in `semgrep-core/`---but will need to go through an extra step to make a pull request (explained later).

When a language is particularly complicated, it can be convenient to first parse into a CST, then convert to the AST. Currently, we only do this for PHP. In this case, there is an extra step:

Tokens -- (via `Parser_php.mly`) --> `Cst_php` -- (via `Ast_php_build.ml`) --> `Ast_php`

The lexers and parsers apply for both patterns and targets of a given language. To avoid parsing invalid targets, we have a function `Flag_semgrep.sgrep_guard` which fails when parsing constructs that only appear in patterns if a target is being parsed.

#### Identifying the Error

The source of the error can be anywhere along the Text --> `AST_generic` path, so you will want to identify which file is causing it. 

First, create a minimum failing case. If you are debugging a rule, isolate this to an individual pattern if possible, saved in a `.sgrep` file. 

For simplicity, we will use Python in the examples, but you can substitute Python for any language parsed with `pfff`. 

If the problem is in `Lexer_python.mll`, you will probably get a helpful error message which should tell you what you need to change. 

If the problem is in `Parser_python.mly`, you will probably not get a helpful error message, because the error will be reported in the generated parser, not the grammar. To identify which production within the grammar is problematic, you will want to see what AST the parser is trying to produce. Modify the failing case minimally until it parses successfully.

Now, you need to see what generic AST is produced by this similar code. You can actually do this in the playground, by going to Tools -> Dump AST. On the command line, you can run

* For a pattern: 
   ```
   sc -dump_pattern -f [your_pattern].sgrep -lang python
   ```
* For a target: 
   ```
   sc -dump_ast [your_target].py -lang python
   ```

where `sc` is an alias for `semgrep-core/_build/default/src/cli/Main.exe`, the executable produced by running `make` in `semgrep-core/`. If you have installed `semgrep-core`, you can instead use `semgrep-core` here, but each time you make a change you will need to compile (`make`) and then install (`make install`).

By default, tokens are not shown in full in the dumped AST. Their presence is indicated by `()`.

You may also find it useful to see the Python AST representation of the pattern. Just as `make` produces an executable for `semgrep-core` in `semgrep-core/_build/default/src/cli/Main.exe`, it also produces one for `pfff` in `semgrep-core/_build/default/src/pfff/cli/Main.exe` (alias to `pf` for these docs).

To dump the Python AST, run

* For a pattern: 
  ```
  pf -dump_python [your_pattern].sgrep -sgrep_mode -lang python
  ```
* For a target: 
  ```
  pf -dump_python [your_target].py -lang python
  ```

(Note that `-sgrep_mode` does not always work with incomplete programs. You may need to wrap your pattern so that it is a valid program for that language, except for semgrep constructs such as `...`)

#### Fixing the Error

At this point, the relevant change you need to make will vary depending on your goal. It may be as simple as adding `...` as a possible case. It may require you to introduce a new construct and add it to `AST_generic` and `Ast_python`. As a rule of thumb, prefer to avoid changing `AST_generic` if possible. This will also make your life easier!

If you add a pattern-specific feature, remember to use `Flag_semgrep.sgrep_guard` so that an invalid target does not parse successfully.

When you change the grammar, it is important that you do not introduce conflicts. Check the conflicts before you start by forcing dune to compile the grammar. (You can either use `make clean` and read through the output or make a change in `Parser_python.mly`, run `make`, then remove the change and run `make` again.) Then, after you change the grammar, see if there are any more conflicts than there were before your change.

It can sometimes be okay to introduce a `shift/reduce` conflict, though avoid doing this if possible. It is never okay to introduce a `reduce/reduce` conflict. To understand why, read about [LR(1) parsers](https://en.wikipedia.org/wiki/Canonical_LR_parser).

If you do introduce a conflict, you can figure out how to resolve it by running

```
menhir --explain Parser_python.mly
```

This will produce the file `Parser_python.conflicts` in the same folder as `Parser_python.mly`, which will show the two possible interpretations Menhir is considering for each conflict.

Unfortunately, it will also produce `Parser_python.ml` and `Parser_python.mli`, which will confuse dune when it tries to build. Remove these files before you run `make` again.

#### Committing the Fix

Once you have made your desired pattern or target parse, you need to make sure it doesn't break anything else. In `semgrep-core/`, run `make test`. If at the end it says `Ok`, you can commit your fix!

First, if you have any changes in `pfff`, go into the `semgrep-core/src/pfff/` directory, checkout `develop`, pull, and then make a pull request as usual with your changes. This will make a PR to [`pfff`](https://github.com/returntocorp/pfff).

When you change files in `pfff`, `semgrep-core` will realize that `pfff` is different (though not which file within `pfff`). If you go back up to `semgrep-core/` and run `git status`, you will see `modified: src/pfff (modified content)`. To pin your latest `pfff` changes to `semgrep-core`, add `src/pfff`.

Now, make the rest of your pull request for `semgrep-core` as usual.

If you haven't changed `pfff`, don't worry about this. Just make a pull request with your changes.

Remember to add test cases so that future changes don't break your example! See [Testing `semgrep-core`](#testing-semgrep-core)

### Fixing a Tree-sitter Parse Error

There is more information in [Adding Support for a Language](#adding-support-for-a-language) on tree-sitter which will be helpful. Also, see `semgrep-core/src/parsing/tree-sitter/`.  

## Fixing a Match Error

The first thing you will need to do is understand what you expected and why you aren't getting that. If possible, reduce your rule to a single pattern that doesn't match. You may need to experiment with the clauses in your rule. For example, if you are getting too many matches, it may be because the pattern in `pattern-not` doesn't match what you expect.

If you are unable to do so, you may need to investigate `Match_rule.ml`.

Otherwise, produce a minimal failing pattern/target pair. You will need to compare the ASTs to see which portion is not matching as you expect. Run

```
sc -dump_ast [your_target].py -lang py
```

and then

```
sc -dump_pattern [your_pattern].sgrep -lang py
```

It can be hard to figure out where in the AST you are looking. You can make it easier by using a distinctive variable name in the section you're interested in. 

Once you've isolated the parts that aren't matching, try to figure out where they're different, taking into account special features like metavariables and ellipses. It is unlikely (though not impossible) that the problem would ever be that two identical code segments aren't matching or that there is some AST element that ellipses refuse to match. You might find it helpful to write out the AST parts you want to match on a whiteboard, indicating which part is matched by a special feature. Pare down the code as much as possible and try changing the bit you're interested in.

When you are sure you know what ought to have happened, make it happen. If two pieces of code should match but don't, change `Generic_vs_generic.ml` to tell it that pattern should match the target.

Oftentimes, a matching error is actually a parsing error. You may want to change how `Parser_python.mly` reduces the construct or how it gets converted in `Python_to_generic.ml`. Refer to [Fixing a Parse Error](#fixing-a-parse-error) for advice.

At the end, confirm the match with

```
sc -f [your_pattern].sgrep [your_target].py -lang py
```

## Fixing an Autofix Error

Autofix runs through both `semgrep-core` and `semgrep`, but the most common autofix error people encounter is some kind of incorrect range. This happens because `semgrep-core` determines the range of a match based on the locations of the tokens stored in the AST. When the range is incorrect, that usually means a token is missing. You can see token location information with

```
sc -full_token_info -dump_ast [your_target].py -lang py
```

See [Fixing a Parse Error](#fixing-a-parse-error) for more on parsing 

## Debugging Resources

In the process of debugging, you will probably want to print things. We provide a function `pr2` in `Common.ml` (in `semgrep-core/src/pfff/commons/`) to print strings. You can also use the `Printf` module.

If you would like to print an AST element, you can use a `show` function. For example, to print a node of type `any` in `AST_generic`, you can use

`pr2 (show_any your_node)`

Any type that includes `[@@deriving show]` in its definition can be converted to a string in this way.

We also provide some flags that are useful. If you run with `-debug`, you can see the steps `semgrep-core` is taking. You can see more information (and change what you want to see) using `-log_config_file`, which takes a file. You can use one of `semgrep-core/log_config.json.ex1` or `semgrep-core/log_config.json.ex2` to start.

Additionally, the [OCaml debugger](https://ocaml.org/manual/debugger.html) is a great resource.

## Adding Support for a Language

There are some cases where we have chosen to implement a new parser in `pfff`, but in general new languages should use tree-sitter.

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
