# Semgrep OSS Codebase Overview

This is the home of Semgrep CE (Community Edition).
Semgrep is a fast static analysis tool that searches code,
finds bugs, and enforces secure guardrails and coding standards.

## Architecture

Semgrep is divided in two major components:
1. CLI: a Python command-line interface that handles orchestration, communication with the Semgrep AppSec Platform, and some analysis (notably for Supply Chain). Python code is incrementally being migrated to OCaml via RPC.
2. core: the OCaml engine that handles parsing, pattern matching, and most analysis (`semgrep-core`).

Semgrep core follows a multi-stage pipeline:
1. **Parsing** → Language-specific parsers convert source code to Generic AST
2. **Analysis** → AST is analyzed and optionally converted to IL (Intermediate Language)
3. **Matching** → Pattern matching against rules
4. **Reporting** → Results are filtered, formatted, and reported

### Key Concepts
- **Generic AST**: Unified abstract syntax tree enabling language-agnostic pattern matching
- **IL (Intermediate Language)**: Simplified, lower-level representation for analysis
- **Pattern Matching**: Semantic code matching with metavariables (`$X`) and ellipsis operators (`...`)

### Python-to-OCaml Migration Strategy

The `osemgrep` direct entry point is **deprecated**. The primary entry point for Semgrep is the Python CLI (`semgrep`), which calls into OCaml via RPC. Do not use or rely on `osemgrep` directly.

Functionality is incrementally migrated from Python to OCaml via the RPC library (`src/rpc/`). This allows individual components to be ported in isolation without requiring a full rewrite. When migrating a component, ensure a complete project plan is in place that includes deletion of the existing Python code.

**Language choice for new code:**
- **Default to OCaml** for new functionality.
- **OCaml is preferred** for: parsing, program analysis, performance-sensitive computations, and code that benefits from future refactoring.
- **Python is acceptable** for: interactions with external services (third-party SDKs), code that must closely interact with existing Python code, and similar cases where Python is the pragmatic choice.
- **Lack of OCaml familiarity alone is not a sufficient reason to write new Python code.**
- Do NOT duplicate work across Python and OCaml. Implement in one language only.

## Developer Setup

### Initial Setup

Prerequisites:
- **opam** (OCaml package manager)
- **Dune** (build system)
- **Python 3** and **pip**
- **uv** (Python dependency management)
- **pre-commit** (git hooks)
- **gcc/clang** and standard C toolchain
- **git**, **make**, **bash**

**IMPORTANT**: Run every one of these commands ONCE when initially setting up the
repository. Do not skip any.
```bash
make setup                      # Installs OCaml dependencies and builds tree-sitter runtime
pre-commit install              # Sets up the pre-commit hooks
make all                        # Build OCaml core + Python CLI (includes uv sync)
```

`make setup` must be re-run when dependencies change. It often resolves strange
build errors.

## Repository Etiquette

### Branch Naming
- Use format: `username/brief-description`
- Examples: `brandon/fix-code-actions-hanging`, `yosef/upgrade-cli-deps`
- **IMPORTANT**: All branches must be based off `develop` (or part of a Graphite stack that is ultimately based off `develop`)

### Commits
- Use conventional commits: `feat:`, `fix:`, `chore:`, `test:`, `docs:`
- Keep commits focused and atomic
- Reference issue numbers when applicable

### Changelog
For any nontrivial user-facing change, add an entry under `changelog.d/`. Name the file after the Linear ticket with an appropriate suffix (e.g. `changelog.d/ENGINE-1234.fixed`). Valid suffixes are `.added`, `.changed`, `.fixed`, and `.infra` — see `changelog.d/README` for details.

### Pull Requests
Before submitting:
- Update documentation if relevant
- Add a changelog entry (see above) for nontrivial user-facing changes
- Run `make test` and confirm tests pass
- Flag any security implications to the security team
- Ensure no sensitive information is logged at INFO/WARNING/ERROR levels

See [Contributing docs](https://semgrep.dev/docs/contributing/contributing-code/) for detailed guidelines.

## Development

### Common Commands

**Building:**
- `make core` - Fast build of semgrep-core (use during active development)
- `make all` - Full build including Python CLI setup

**Testing:**
- `make test` - Run OCaml core tests and rules tests
- `make test-all` - Run all tests including Python CLI tests
- `make build-core-test && ./test -s '<filter>'` - Run specific tests matching filter

**After making code changes:**

For OCaml code changes (`src/`):
1. `make core` - Rebuild binaries
2. `make test` - Verify tests pass
3. `make copy-core-for-cli` - Update CLI binaries if needed

For Python CLI changes (`cli/`):
1. Changes take effect immediately if using `cd cli && uv run`
2. `make -C cli test` - Run Python tests

For all changes:
1. `pre-commit run --files /path/to/changed/files` - Run formatters and other pre-commit checks

### Development Tools
This project uses **make** for build orchestration. **IMPORTANT: Never invoke `dune` directly for building or testing** — always use the Makefile targets above. The Makefiles handle path resolution for the monorepo layout. Direct `dune` usage is only appropriate for documentation generation:

- `dune build @doc` - Generate `ocamldoc` to `_build/default/_doc/_html`
- `dune build @doc-private` - Generate `ocamldoc` of module implementation to `_build/default/_doc/_html`

## OCaml Code Style Guidelines

### General

 - Avoid mutable data structures. Only use `ref`, `Hashtbl`, etc. for:
   - Interacting with existing code.
   - Performance-sensitive code.
   - Code where mutability genuinely makes it more readable and maintainable.
   - Code that is safe from data races.  Use `Hook` and `SharedMemo` in cases
     where sharing mutable state is truly unavoidable, but avoid if at all
     possible.
 - Avoid the implicit use of polymorphic compare, hash, etc. in complex data
   structures. Prefer instantiations of `Stdlib`'s `Map`/`Set` using
   ppx-generated compare functions, for example.

### `ocamldoc` Guidelines

 - ALWAYS have a preamble documentation that describes what the file does and the general approach / algorithm
   used in it.
 - NEVER document functions and types that are internal to a `.ml` file, unless that file's symbols
   are accessible to other `.ml` files.
 - ALWAYS document functions and types that are accessible to other source code.
 - NEVER document functions that are idiomatically converting from one type to another (e.g. `x_of_y`)

## Working with Tests

### Python Test Markers (CLI)

Every pytest test under `cli/tests/` must carry exactly one speed marker, or
CI's `check-markers` gate fails:

- `@pytest.mark.quick` — under 100 ms
- `@pytest.mark.kinda_slow` — up to 1–2 s
- `@pytest.mark.slow` — more than 1–2 s

### Test Annotations

| Annotation | Meaning | Action |
|------------|---------|--------|
| `ruleid: rule-name` | Expected true positive | Test passes when finding is reported here |
| `todoruleid: rule-name` | Known false negative | **Fix analyzer** to report, then change to `ruleid` |
| `ok: rule-name` | Expected true negative | Test passes when NO finding reported here |
| `todook: rule-name` | Known false positive | **Fix analyzer** to not report, then change to `ok` |

**Test failure messages:**
- "N false negatives" = Missing `ruleid` findings or unexpected passes on `todoruleid`
- "N false positives" = Reporting on `ok:` lines or on unannotated lines

### Debugging Failures

**Quick diagnosis:**
- Multiple similar `todoruleid` annotations → Missing analyzer capability (not a bug)
- Isolated failure → Likely a specific case bug
- Failures across architectural layers → Check full execution path before proposing fixes

## Isolation of Open-Source Components

This repository contains only Community Edition code. NEVER refer to proprietary/Pro code or features
in files in this repository. When working on code here, ALWAYS verify that this separation
is maintained.
