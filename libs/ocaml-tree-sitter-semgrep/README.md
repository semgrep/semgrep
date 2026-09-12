ocaml-tree-sitter-semgrep
==

Generate OCaml parsers based on
[tree-sitter](https://tree-sitter.github.io/tree-sitter/) grammars,
for [Semgrep](https://github.com/semgrep/semgrep).

This tree lives under `libs/ocaml-tree-sitter-semgrep` in the Semgrep
repository. It contains the code generator (`core/`) and Semgrep-extended
grammar wrappers (`lang/`). Upstream tree-sitter grammars are fetched on
demand from pins in `lang/upstream-grammars.json` (not vendored).

Contributing
--

### Development setup

Use `make setup` from the Semgrep repository root. Grammar-tool dependencies
are installed through the repository's `semgrep.opam` and lockfiles; there is
no separate setup or installation in this directory.

From the Semgrep repository root, run:

```bash
make test-grammar-kotlin
make test-grammars           # all registered languages
make regen-grammar-kotlin
```

These targets build the current generator and provision the language's pinned
tree-sitter CLI on demand. For prerequisites and the full add/update workflow,
see [Add or update a language grammar](doc/add-or-update-grammar.md).

### Internal build targets

Use root `test-grammar-<name>`, `test-grammars`, and `regen-grammar-<dest>`
for complete workflows: they build the tools and fetch pinned upstream sources.
There is no aggregate Makefile under `lang/semgrep-grammars/` or its `src/`.

Per-wrapper `make` fetches the pinned upstream grammar and dependencies,
provisions its CLI, and generates the parser. `make test` builds it first.
Per-language `make` also builds the workspace generator and runtime before
producing the standalone OCaml parser; `make test` then runs its examples.
Generation, compilation, and statistics targets declare those same prerequisites.
`clean` removes local build products without fetching or building anything.

For CST parsing statistics, first run `make test-grammar-<name>`, then
`make -C libs/ocaml-tree-sitter-semgrep/lang/<name> stat`. Configure that
language's `projects.txt` and `extensions.txt`; statistics also need Bash 4+.
`stat-priv` additionally requires a local `projects-priv.txt`. The optional
`core/scripts/most-starred-for-language` utility uses Python dependencies from
`cli/`; invoke it with `uv run --project <semgrep-root>/cli`. It writes
`projects.txt` in the current directory.

### Python / ABI tests

From the Semgrep repository root, `make ots-test` runs the core OCaml tests
and Python suites. The Python suites use the existing `cli/` development environment and lockfile,
plus Node.js.
They automatically download pinned CLIs and fetch one representative grammar
per CLI version. Missing tools, unresolved pins, and failed builds fail the tests.

`make test-all` includes both suites; `make test` runs Semgrep core tests.
CI runs `ots-test-ocaml` and `ots-test-python` as separate steps.

To run just the Python suites from the Semgrep repository root:

```bash
make ots-test-python
```

### tree-sitter versions

`lang/upstream-grammars.json` selects the CLI used to generate each grammar.
`core/scripts/provision-tree-sitter` downloads release binaries into
`core/tree-sitter-<version>/bin`, checks their reported version, and replaces
them atomically. Verified cached binaries are reused; `FORCE=1` refreshes them.
From the Semgrep repository root, `make setup-tree-sitter-versions` downloads
all registry versions and is also a prerequisite of `make ots-test-python`.

All standalone OCaml parsers link the repository's shared runtime, using
`tree-sitter-config.sh` at the Semgrep repository root. Its version comes from
`core/tree-sitter-version`. Generated parser and scanner headers remain with
their grammar. Versioned CLI directories no longer supply headers or libraries;
leftovers from older installations are unused.

Core's own CLI build and the opam runtime package are unchanged.

### tree-sitter ABI 15

ABI 15 is **off by default**. To opt in for a build, set the
environment variable `SEMGREP_ENABLE_ABI15`. Accepted truthy values:
`1`, `true`, `yes`, `on` (and their uppercase forms).

Selection is implemented in `scripts/generate_abi_args.py` (CLI:
`scripts/ts-generate-abi-args`). End-to-end ABI 15 generation
requires tree-sitter >= 0.25.0.

The following table explains the decision matrix for the tree-sitter bindings/ABI
parameter.

| Condition | ABI |
|--|--|
| Grammar ships a `tree-sitter.json` **and** `SEMGREP_ENABLE_ABI15` is set **and** pinned tree-sitter >= 0.25.0 | 15 |
| tree-sitter >= 0.24.0 (otherwise) | 14 |
| older tree-sitter | `--no-bindings` (no ABI flag) |

### Adding a new language

See [Add or update a language grammar](doc/add-or-update-grammar.md) and
[How to add support for a new language](https://semgrep.dev/docs/contributing/adding-a-language/).

Documentation
--

We have limited [documentation](doc) which is mostly targeted at
early contributors. It's growing organically based on demand, so don't
hesitate to [file an issue](https://github.com/semgrep/semgrep/issues)
explaining what you're trying to do.

License
--

ocaml-tree-sitter is free software with contributors from multiple
organizations. The project is driven by [Semgrep](https://github.com/semgrep).

- The OCaml bindings to tree-sitter's C API were created by Bryan
  Phelps as part of the reason-tree-sitter project.
- The tree-sitter grammars for major programming languages are
  external projects. Each comes with its own license.
