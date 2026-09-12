# Add or update a language grammar

Run all commands below from the **Semgrep repository root**, which contains
`languages/`, `src/`, and `libs/`. Paths in this guide are relative to that root.

```bash
ots=libs/ocaml-tree-sitter-semgrep
```

There are three parts to a grammar change:

| Part | Location | Purpose |
| --- | --- | --- |
| Upstream grammar | `$ots/lang/upstream-grammars.json` | Pins the source repository, commit, and tree-sitter CLI version. |
| Semgrep wrapper | `$ots/lang/semgrep-grammars/src/semgrep-<name>/` | Extends the upstream grammar with pattern syntax such as `$X` and `...`. |
| Generated parser | `languages/<language>/tree-sitter/semgrep-<dest>/` | Contains the C and OCaml parser files used by Semgrep. |

A registry **key** identifies an upstream grammar. A **destination** identifies
one generated parser. They are usually identical (`python`), but one key can
produce several destinations (`typescript` produces `typescript` and `tsx`).
`test-grammar` tests a key and its dialects; `regen-grammar` writes one destination
from that key's `regen` list.

## Prepare the tools

Complete the repository's normal development setup first (`make setup`,
`pre-commit install`, and `make all`). Grammar development also needs Node.js,
Cargo, clang/clang++, and Git LFS on `PATH`.

The normal setup installs the OCaml dependencies through `semgrep.opam` and its
lockfile, including the pinned `tree-sitter` package. No separate setup or
installation in the grammar directory is needed.

`make test-grammar-<name>` and `make regen-grammar-<dest>` build the current
generator automatically and provision the selected tree-sitter CLI under
`$ots/core/tree-sitter-<version>/` on demand. The registry's `tree_sitter` field
selects that CLI; `$ots/core/tree-sitter-version` independently selects Semgrep's
shared runtime library, which standalone OCaml parsers also use. Adding a CLI
version only requires updating the registry; the provisioner downloads its
published release binary automatically.

## Update an existing grammar

For example, to update Python:

1. Edit Python's `commit` in `$ots/lang/upstream-grammars.json`. Change
   `tree_sitter` only if the new grammar needs a different CLI version.
2. Adjust the wrapper in `$ots/lang/semgrep-grammars/src/semgrep-python/`
   if needed, and add regression cases to its `test/corpus/` directory.
3. Test the grammar, then regenerate the parser:

   ```bash
   make test-grammar-python
   make regen-grammar-python
   ```

4. Run `make core` and `make test`. Review and commit the registry change,
   wrapper changes, tests, and generated files under
   `languages/python/tree-sitter/semgrep-python/`.

For wrapper-only changes, skip step 1. For a key with multiple destinations,
run `regen-grammar` once for each destination. For example:

```bash
make test-grammar-typescript
make regen-grammar-typescript
make regen-grammar-tsx
```

Testing fetches the pinned upstream sources and rebuilds the wrapper before
checking its corpus and generated OCaml parser. Regeneration copies the parser
into `languages/` and refreshes its `fyi/` provenance files.

Entries with `regen: []` do not produce a parser for Semgrep. If you change a
grammar used by another entry's `depends_on`, also test and regenerate that
consumer (for example, `cpp` after changing `c`).

## Add a new language

The example below uses `mylang` for the registry key, grammar name, and
destination. Replace it with your language's name. This is the simplest case:
one upstream repository with a root `grammar.js` and one generated parser.
For multiple dialects or a different upstream layout, use the existing
TypeScript or PHP wrapper as a reference.

### 1. Register the upstream grammar

Add an entry to `$ots/lang/upstream-grammars.json`:

```json
"mylang": {
  "url": "https://github.com/OWNER/tree-sitter-mylang.git",
  "commit": "FULL_UPSTREAM_COMMIT_SHA",
  "tree_sitter": "0.26.3",
  "regen": ["mylang"]
}
```

Replace the URL and commit with the upstream repository and an immutable commit
SHA. Choose a supported CLI version compatible with that grammar. Optional
`depends_on` lists other registry keys that must be fetched first; `clone`
overrides the suffix of the local `tree-sitter-<clone>` directory.

Validate the registry and fetch the sources:

```bash
python3 "$ots/scripts/grammar_registry.py" validate
python3 "$ots/scripts/grammar_registry.py" fetch mylang
```

Sources are fetched into `$ots/lang/semgrep-grammars/src/tree-sitter-mylang/`.
Keep this clone gitignored; upstream grammars are fetched from the registry.

### 2. Create the wrapper and generator directories

Create the shared Makefile links, preparation script link, test directories,
and the link through which the OCaml generator reads the wrapper:

```bash
mkdir -p "$ots/lang/semgrep-grammars/src/semgrep-mylang/test/corpus"
mkdir -p "$ots/lang/mylang/test/ok"
ln -s ../Makefile.common "$ots/lang/semgrep-grammars/src/semgrep-mylang/Makefile"
ln -s ../prep.common "$ots/lang/semgrep-grammars/src/semgrep-mylang/prep"
ln -s ../src/semgrep-mylang "$ots/lang/semgrep-grammars/lang/mylang"
ln -s ../Makefile.common "$ots/lang/mylang/Makefile"
```

Create `$ots/lang/semgrep-grammars/src/semgrep-mylang/grammar.js`:

```javascript
const base = require('tree-sitter-mylang/grammar');

module.exports = grammar(base, {
  name: 'mylang',
  rules: {},
});
```

This initially inherits the upstream grammar unchanged. Extend `rules` to accept
Semgrep metavariables and ellipses in the appropriate language constructs. Add
corpus tests with input and expected syntax trees under the wrapper's
`test/corpus/`; the FGA wrapper is a small example. The shared `prep` script
imports the upstream scanner and corpus. Use a custom `prep` if the grammar
needs additional headers or has a different directory layout.

Add representative source files to `$ots/lang/mylang/test/ok/`. These exercise
the generated OCaml parser, separately from the tree-sitter corpus tests.

Create `$ots/lang/mylang/fyi.list` with these paths, adjusting the upstream
license filename if necessary. Paths **inside this file** are relative to
`$ots/lang/`:

```text
semgrep-grammars/src/tree-sitter-mylang/LICENSE
semgrep-grammars/src/tree-sitter-mylang/grammar.js
semgrep-grammars/src/semgrep-mylang/grammar.js
```

### 3. Test and generate the parser

```bash
make test-grammar-mylang
mkdir -p languages/mylang/tree-sitter/semgrep-mylang
make regen-grammar-mylang
```

Fix grammar or generation failures before continuing. Regeneration finds the
existing destination and writes `lib/`, `bin/`, `config/`, and `fyi/` inside it.
If the language directory should have another name, use
`languages/<language>/tree-sitter/semgrep-mylang/` in the `mkdir` command.

Register the generated C parser with Git LFS before staging it:

```bash
git lfs track 'languages/mylang/tree-sitter/semgrep-mylang/lib/parser.c'
```

Do not edit generated parser files by hand. Change the wrapper and regenerate.

### 4. Connect the parser to Semgrep

Generating a parser does **not** yet make the language available to Semgrep.
Use `languages/fga/` as an example of the remaining integration:

| Change | Where to work |
| --- | --- |
| Convert the generated concrete syntax tree to Generic AST | Add `Parse_mylang_tree_sitter.ml` and `.mli` under `languages/mylang/`; use generated `lib/Boilerplate.ml` as a starting point. |
| Build the converter and link the generated parser | Add a `dune` library like `languages/fga/generic/dune`, then add it to `src/parsing/dune`. |
| Register the language name and file extensions | Edit `cli/src/semgrep/semgrep_interfaces/generate.py`, then run `make -C cli/src/semgrep/semgrep_interfaces`. |
| Route target and pattern parsing to the converter | Add language cases in `src/parsing/Parse_target.ml` and `src/parsing/Parse_pattern.ml`; handle other language dispatch sites identified by the build. |
| Test actual Semgrep behavior | Add `tests/parsing/mylang/hello-world.<extension>` and matching tests under `tests/patterns/mylang/`. |

Implement both target-code and pattern parsing, including metavariables and
ellipses; the generated boilerplate alone does not implement this conversion.

### 5. Verify and commit

Run `make core`, `make test`, and pre-commit on the changed files. Commit the
registry entry, wrapper, generator links and tests, generated parser, language
integration, and `.gitattributes` change. Add a changelog entry for the new
language support.

`$ots/scripts/list-languages` automatically discovers `semgrep-mylang/` for
Test Grammars CI; no list edit is required. `projects.txt`, `extensions.txt`,
and the `STAT_LANGUAGES` lists are optional inputs for parsing benchmarks,
not requirements for grammar CI.

Never commit the fetched upstream clone or intermediate `lang/mylang/ocaml-src/`
and `src/` build outputs.
