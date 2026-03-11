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

Functionality is incrementally migrated from Python to OCaml via the RPC library (`src/rpc/`). This allows individual components to be ported in isolation without requiring a full rewrite.

**Language choice for new code:**
- **Default to OCaml** for new functionality.
- **OCaml is preferred** for: parsing, program analysis, performance-sensitive computations, and code that benefits from future refactoring.
- **Python is acceptable** for: interactions with external services (third-party SDKs), code that must closely interact with existing Python code, and similar cases where Python is the pragmatic choice.
- Do NOT duplicate work across Python and OCaml. Implement in one language only.

## Directory Layout

| Directory | Purpose |
|-----------|---------|
| **Analysis Core** | |
| `src/ast_generic/` | Generic AST with visitors and mappers for traversal |
| `src/il/` | Intermediate Language representation |
| `src/analyzing/` | AST-to-IL transformation, dataflow, control-flow, constant propagation |
| `src/naming/` | Name resolution with scope management and symbol binding |
| `src/typing/` | Type inference and checking |
| **Pattern Matching & Engine** | |
| `src/engine/` | Core pattern matching with rule orchestration and metavariable unification |
| `src/matching/` | Pattern matching utilities and language-specific adaptations |
| `src/tainting/` | Taint analysis and tracking |
| **Language Support** | |
| `languages/` | Language parsers |
| `src/parsing/` | Language parsers with Tree-sitter integration |
| **Rules & Configuration** | |
| `src/rule/` | Rule parsing and multi-layer transformation (YAML → Rule.t → Mini_rule.t) |
| `src/configuring/` | Configuration for project settings, rules, path filtering |
| `src/prefiltering/` | Performance optimization by filtering files/rules before analysis |
| `src/metachecking/` | Rule quality validation and improvement suggestions |
| `src/spacegrep/` | Language-agnostic whitespace-aware pattern matching |
| `src/aliengrep/` | Regular expression-based code search |
| **Scanning & Execution** | |
| `src/osemgrep/` | OCaml CLI implementation (deprecated as direct entry point; code is used via RPC from Python CLI) |
| `src/core_scan/` | Scanning orchestration with parallel execution |
| `src/targeting/` | Target file selection with language detection |
| `src/target/` | Target representation (file and in-memory) |
| **Output & Reporting** | |
| `src/reporting/` | Result reporting (JSON, SARIF, text) |
| `src/fixing/` | Autofix capabilities for code rewriting |
| **Additional Features** | |
| `src/sca/` | Software Composition Analysis for dependency scanning |
| `src/rpc/` | RPC interface for Python <-> OCaml communication |
| `src/lsp_legacy/` | Legacy LSP implementation |
| **Foundation & Testing** | |
| `src/core/`, `src/core_cli/` | Foundation libraries with utilities, file system operations, logging |
| `src/printing/` | Pretty printing for AST, IL, error messages |
| `src/tests/` | Test cases |

## Developer Setup

### Initial Setup

**Option 1: Nix-based Setup (reproducible environment, recommended)**

Prerequisites: **Nix** (with flakes enabled) and **direnv**

```bash
direnv allow                           # Direnv will automatically load the Nix environment
make all                               # Build (direnv provides all dependencies)
```

The Nix shell provides all dependencies. Direnv will activate the environment automatically when you enter the project directory.

**Option 2: Standard Setup**

Prerequisites:
- **OCaml 5.3.0** (via opam)
- **opam** (OCaml package manager)
- **Dune** (build system)
- **Python 3** and **pip**
- **uv** (Python dependency management)
- **pre-commit** (git hooks)
- **gcc/clang** and standard C toolchain
- **git**, **make**, **bash**

Run ONLY ONCE or when dependencies change:
```bash
make setup                      # Installs OCaml dependencies and builds tree-sitter runtime
pre-commit install              # Sets up the pre-commit hooks
make all                        # Build OCaml core + Python CLI (includes uv sync)
```

## Core Files and Utility Functions

### Entry Points
- `src/core_cli/Core_CLI.mli` - Low-level semgrep-core CLI entry point with scan orchestration
- `src/osemgrep/cli/CLI.mli` - OCaml CLI dispatcher (deprecated as direct entry point; see Migration Strategy above)

### Core Data Types (src/core/)

**Results & Matches:**
- `Core_result.mli` - Aggregated scan results containing matches, errors, profiling data, and statistics
- `Core_match.mli` - Individual pattern match with location range and metavariable bindings
- `Range.mli` - File position tracking with character ranges (start/end positions)
- `Metavariable.mli` - Pattern variable bindings (`$X`, `$FOO`) and their captured values

**Error Handling:**
- `Core_error.mli` - Error representation with location, rule ID, severity, and error type
  - `mk_error` - Create errors from exceptions or rule errors
  - `exn_to_error` - Convert exceptions to structured errors
  - `string_of_error` - Format errors for display

**Type System:**
- `Type.mli` - Type inference representation with builtin types (Int, Float, String, Bool), records, functions, arrays, pointers
  - Supports language-specific type mappings
  - Includes visitor pattern for AST traversal

### Logging, Profiling & Telemetry (src/core/)

**Logging:**
- `Log_semgrep.mli` - Logging setup with OpenTelemetry integration
  - `Log_semgrep.Log.debug`, `.info`, `.warn`, `.err` - Log at different levels
  - Modules typically create: `module Log = Log_semgrep.Log`
  - Supports log-to-file and log-to-otel options

**Profiling:**
- `Core_profiling.mli` - Detailed timing profiling for `-json_time` flag
  - Tracks parse time and match time per rule and file
  - `rule_profiling` - Per-rule timing data
  - `file_profiling` - Per-file timing with rule breakdown
- `Core_quick_profiling.mli` - Quick profiling stats (parsing, matching, tainting, prefiltering)
- `Summary_stats.mli` - Summary statistics with "very slow" file/rule identification
  - Tracks count, mean, standard deviation
  - Reports top N slowest items

**Telemetry:**
- `Trace_data.mli` - OpenTelemetry trace data preparation
  - `analysis_flags` - Track which features are enabled
  - `get_resource_attrs` - Create telemetry tags for grouping traces/metrics

### CLI Configuration (src/core_cli/)

- `Core_CLI.mli` - Semgrep-core main entry point
  - `main` - CLI entry point processing command-line arguments
  - `mk_config` - Compute scan configuration from CLI flags
  - `output_core_results` - Format results as JSON or text
  - Command-line flags: `lang`, `num_jobs`, `debug`, `trace`, `symbol_analysis`
- `Core_exit_code.mli` - Exit codes for semgrep-core
  - `Success`, `False`, `Bad_command_line`, `Unknown_exception`
  - `exit_semgrep` - Clean exit with logging

### Output & Formatting (src/core/)

- `Semgrep_output_utils.mli` - Utilities for working with Semgrep output types
  - `lines_of_file_at_range` - Extract code lines for display
  - `content_of_file_at_range` - Extract content for metavar interpolation
  - `sort_core_matches`, `sort_cli_matches` - Sort results for display

### AST Printing (src/printing/)

- `Ugly_print_AST.mli` - Syntactically-correct AST-to-code printing (recommended)
  - `python_printer`, `jsts_printer`, `ocaml_printer` classes
  - `print_expr`, `print_arguments`, `print_name` - Print AST nodes as code
  - Used for autofix and code generation
- `Pretty_print_AST.mli` - DEPRECATED, use Ugly_print_AST instead

### Testing Utilities (src/core/)

- `Test_tags.mli` - Test filtering tags for Testo framework
  - `flaky` - Mark tests that sometimes fail
  - `e2e` - End-to-end tests
  - `tags_of_lang` - Language-specific test filtering

### Version & Metadata (src/core/)

- `Version_info.mli` - Semgrep version parsing and comparison

## Repository Etiquette

### Branch Naming
- Use format: `username/brief-description`
- Examples: `brandon/fix-code-actions-hanging`, `yosef/upgrade-cli-deps`

### Commits
- Use conventional commits: `feat:`, `fix:`, `chore:`, `test:`, `docs:`
- Keep commits focused and atomic
- Reference issue numbers when applicable

### Pull Requests
Before submitting:
- Update documentation if relevant
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

### Development Tools
This project uses **Dune** for build orchestration. **IMPORTANT: Never invoke `dune` directly for building or testing** — always use the Makefile targets above. The Makefiles handle path resolution for the monorepo layout. Direct `dune` usage is only appropriate for documentation generation:

- `dune build @doc` - Generate `ocamldoc` to `_build/default/_doc/_html`
- `dune build @doc-private` - Generate `ocamldoc` of module implementation to `_build/default/_doc/_html`


## OCaml Code Style Guidelines

### General

- OCaml with type annotations (recent focus)
- Extensive use of modules and functors
- Pattern matching and algebraic data types

### `ocamldoc` Guidelines

 - ALWAYS have a preamble documentation that describes what the file does and the general approach / algorithm
   used in it.
 - NEVER document functions and types that are internal to a `.ml` file, unless that file's symbols
   are accessible to other `.ml` files.
 - ALWAYS document functions and types that are accessible to other source code.
 - NEVER document functions that are idiomatically converting from one type to another (e.g. `x_of_y`)

### Pattern Matching Best Practices

When working with Option types and conditional logic:

**Prefer:** Direct pattern matching with guards
```ocaml
match get_attrs() with
| None -> false
| Some attrs when List.mem TargetAttr attrs -> true
| Some attrs -> (* other logic *)
```

**Avoid:** Intermediate booleans and nested if-then-else
```ocaml
let attrs_opt = get_attrs() in
let has_attr = match attrs_opt with Some attrs -> List.mem TargetAttr attrs | None -> false in
if has_attr then true else (* other logic *)
```

**Rationale:**
- Single traversal of the data structure
- Pattern guards (`when` clauses) are idiomatic OCaml
- Clear n-way branching structure
- Avoids redundant pattern matches
- More explicit about early exit conditions

## Debugging Tips

### General Tips
- Enable verbose logging with `-debug`

### Debugging with LLDB/GDB

**IMPORTANT: Apple Silicon Limitations**
- Binaries built on Apple Silicon (M1/M2/M3) do not include debug symbols
- **You must debug on Linux** if you need to sync execution with source code
- Use Docker or a Linux VM for proper debugging with symbols

**Using LLDB with OCaml:**
- OCaml function names are mangled - use regular expression breakpoints
- Example: `breakpoint set -r "camlCore_scan.*"` to break on Core_scan functions
- Example: `breakpoint set -r "camlMatching.*match_rules"` for specific functions
- Use `image lookup -rn <pattern>` to find available function names

### Inspecting AST Representations
- Use `semgrep-core -dump_tree_sitter_cst -lang $LANG $FILE` to see the raw TreeSitter CST
- Use `semgrep-core -dump-named-ast -lang $LANG $FILE` to see the Generic AST
- Use `semgrep-core -dump-il -lang $LANG $FILE` to see the IL representation
- Use `semgrep-core -cfg_il -lang $LANG $FILE` to see the CFG representation

## Working with Tests

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
