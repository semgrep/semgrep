# Contributing

Semgrep welcomes contributions from anyone. If you have an idea for a feature
or notice a bug please [open an issue](https://github.com/returntocorp/semgrep/issues/new/choose).
Creating an issue first is preferable to moving directly to a pull request so
that we can ensure you're on the right track without any wasted effort. This
is also a great way to contribute to Semgrep even if you're not making changes
yourself.

Contents:

* [File Structure](#file-structure)
* [Code Relationship](#code-relationship)
* [Making a Change](#making-a-change)
  * [Only `semgrep`](#only-semgrep) 
  * [Only `semgrep-core`](#only-semgrep-core) 
  * [Both `semgrep` and `semgrep-core`](#both-semgrep-and-semgrep-core) 
* [Development Workflow](#development-workflow)

This README gives an overview of the repository. For further information on building, you will be directed to [SEMGREP_CORE_CONTRIBUTING.md](SEMGREP_CORE_CONTRIBUTING.md) and/or [SEMGREP_CONTRIBUTING.md](SEMGREP_CONTRIBUTING.md) in [Making a Change](#making-a-change). 

## File Structure

Semgrep consists of a Python wrapper (`semgrep`) around an OCaml engine (`semgrep-core`) which performs the core parsing/matching work. Within `semgrep-core`, there are two sources of parsers, `pfff`, linked as a [submodule](https://github.com/returntocorp/pfff), and `tree-sitter-lang`, built using [tree-sitter](https://github.com/tree-sitter/tree-sitter). Additionally, `semgrep-core` contains a subengine, `spacegrep`, for generic matching.

You may also be interested in `perf`, which contains our code for running repositories against specific rulesets.

There are many other files, but the below diagram broadly displays the file structure. 

```
.
├── semgrep/  (Python wrapper)
│    └── semgrep/
│ 
├── semgrep-core/  (OCaml engine for matching)
│   └── src/
│       │── pfff/ [submodule](https://github.com/returntocorp/pfff)
│       │── tree-sitter-lang/
│       └── spacegrep/  (Generic matching)
│           └── src/
│ 
└── perf/  (Performance benchmarking)
```

Most of Semgrep's logic is in `semgrep/semgrep` or `semgrep-core/src`. 

## Code Relationship

The `semgrep-core` binary stands alone. Once built, it is possible to run `semgrep-core` on a semgrep rule for a given language with a file/directory and receive matches. 

For example, say you create the config file `unsafe-exec.yaml` and the program `unsafe-exec.py`:

```yaml
rules:
- id: unsafe-exec
  pattern: exec(...);
  message: Avoid use of exec; it can lead to a remote code execution.
  severity: WARNING
  languages: [python]
```

```python
exec("ls");
```

If you run `semgrep-core -config unsafe-exec.yaml unsafe-exec.py -lang python`, it will output

```
unsafe-exec.py:1 with rule unsafe-exec
 exec("ls");
```

If you run `semgrep --config unsafe-exec.yaml unsafe-exec.py`, it will output

```
running 1 rules...
unsafe-exec.py
severity:warning rule:unsafe-exec: Avoid use of exec; it can lead to a remote code execution.
1:exec("ls");
ran 1 rules on 1 files: 1 findings
```

The matched code is the same, but with `semgrep` the output is more polished and includes the message. 

`semgrep` invokes the `semgrep-core` binary as a subprocess, with a flag to request JSON output. It reads the `semgrep-core` output and transforms it appropriately.

Currently, depending on the flags used, `spacegrep` is invoked both independently by `semgrep` as a subprocess and by `semgrep-core` as a subfolder. Therefore, `semgrep` requires the `spacegrep` binary, but building `semgrep-core` will build `spacegrep` as well.

## Making a Change

Semgrep runs on Python versions >= 3.6. If you don't have one of these versions installed, please do so before proceeding.

Because the Python and OCaml development paths are relatively independent, the instructions are divided into Python ([SEMGREP_CONTRIBUTING.md](SEMGREP_CONTRIBUTING.md)) and OCaml ([SEMGREP_CORE_CONTRIBUTING.md](SEMGREP_CORE_CONTRIBUTING.md)).

To fully build Semgrep from source, start at [SEMGREP_CORE_CONTRIBUTING.md](SEMGREP_CORE_CONTRIBUTING.md). It will direct you to [SEMGREP_CONTRIBUTING.md](SEMGREP_CONTRIBUTING.md) when appropriate.

Depending on what change you want to make, it might be simpler to build only `semgrep` or only `semgrep-core`. For example, if you only want to modify Python code, you can skip installing OCaml by downloading binaries for the OCaml parts. Similarly, if you only want to modify OCaml code, you can work on `semgrep-core`/`spacegrep` directly.

If you only want to build `semgrep`, go straight to [SEMGREP_CONTRIBUTING.md](SEMGREP_CONTRIBUTING.md). Otherwise, follow the instructions in [SEMGREP_CORE_CONTRIBUTING](SEMGREP_CORE_CONTRIBUTING.md).

Below is a guide for what functionality each of `semgrep` and `semgrep-core` controls. 

### Only `semgrep`

The python code for Semgrep performs pre and post-processing work. You likely need to touch only `semgrep` if you want to affect

* How output is formatted
* What files are scanned for each language
* The message that is displayed

Go to [SEMGREP_CONTRIBUTING.md](SEMGREP_CONTRIBUTING.md)

### Only `semgrep-core`

The OCaml code for Semgrep performs all the parsing and matching work. You likely need to touch only `semgrep-core` if you want to

* Fix a parse error
* Fix a matching error
* Improve Semgrep's performance

Go to [SEMGREP_CORE_CONTRIBUTING.md](SEMGREP_CORE_CONTRIBUTING.md)

### Both `semgrep` and `semgrep-core`

There are some features that cross through both OCaml and Python code. You will likely need to touch both `semgrep` and `semgrep-core` if you want to

* Fix an autofix error
* Add a new language
* Change error reporting

Go to [SEMGREP_CORE_CONTRIBUTING.md](SEMGREP_CORE_CONTRIBUTING.md). It will direct you to [SEMGREP_CONTRIBUTING.md](SEMGREP_CONTRIBUTING.md) when appropriate. 

## Development Workflow

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
