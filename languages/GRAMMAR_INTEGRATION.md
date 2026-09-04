# In-tree tree-sitter grammar integration

## Background

Each `languages/<lang>/tree-sitter/` builds an OCaml library
`tree-sitter-lang.<lang>` from a **generated** tree-sitter parser (`lib/CST.ml`,
`lib/Boilerplate.ml`, `lib/Parse.ml`, `lib/parser.c`, `lib/scanner.c`, …). That
generated code is produced by the `ocaml-tree-sitter-semgrep` generator, now
vendored in-tree at `libs/ocaml-tree-sitter-semgrep/`.

Historically the generated code round-tripped through an external GitHub repo:

```
libs/ocaml-tree-sitter-semgrep/lang/           (grammar source + generator)
        │  make -C lang/<lang> gen  +  lang/release <lang>
        ▼
   github.com/semgrep/semgrep-<lang>            (generated code, released)
        │  git submodule
        ▼
   languages/<lang>/tree-sitter/semgrep-<lang>  (consumed by the build)
```

Now that the generator lives in the monorepo, we cut out the external repo:
generated code is **committed in-tree** under
`languages/<lang>/tree-sitter/semgrep-<lang>/` and regenerated on demand. A
grammar-source change and its regenerated parser can land in one reviewable
commit — no publish-to-external-repo / bump-submodule dance.

**Status:** grammars are being converted from git submodules to vendored
in-tree directories one batch at a time; see `.gitmodules` /
`OSS/.gitmodules` for which `tree-sitter/semgrep-*` paths are still gitlinks.
Each is converted in place (same path), so the dune wiring is unchanged.
Once the last grammar lands, the only submodules left in the repo will be the
non-grammar rules repos (`semgrep-rules`, `semgrep-rules-proprietary`).

## How to regenerate a language

```
make -C OSS regen-grammar-<lang>     # e.g. make -C OSS regen-grammar-python
# or, from the OSS root:  make regen-grammar-python
```

This runs `OSS/scripts/regen-grammar`, which:

1. provisions the upstream grammar (shallow-cloned at the commit recorded in the
   existing `semgrep-<lang>/fyi/versions`, if not already checked out);
2. looks up the language's tree-sitter CLI pin (`lang/scripts/ts-version-for-lang`)
   and runs `core/scripts/provision-tree-sitter` so
   `core/tree-sitter-<version>/bin` is on PATH (see **Tree-sitter versions**
   below);
3. runs `make -C libs/ocaml-tree-sitter-semgrep/lang/<lang> gen` with
   `PROJECT_ROOT` set to the ots checkout (so `lang/Makefile.common` does not
   treat the monorepo root as ots);
4. copies `ocaml-src/{lib,bin,config,.gitignore}` into
   `languages/<lang>/tree-sitter/semgrep-<lang>/`;
5. refreshes `fyi/` provenance (upstream grammar commit + tree-sitter version).

Prerequisites: the ots core built (`make -C libs/ocaml-tree-sitter-semgrep/core
setup install`) and network once per tree-sitter version (provision-tree-sitter
fetches the CLI). Review the diff, then commit `lib/`, `bin/`, `config/`, and
`fyi/`.

## Tree-sitter versions (generate vs runtime)

ocaml-tree-sitter-semgrep keeps **two** tree-sitter source trees, and they
are not interchangeable:

1. **Generate-time CLI (per language).** Each grammar is listed in one of
   `lang/languages-<version>` (e.g. bash → 0.22.6, python/ruby → 0.26.3).
   `lang/scripts/ts-version-for-lang` looks that up; `core/scripts/provision-tree-sitter`
   installs that CLI + headers + lib into `core/tree-sitter-<version>/`.
   `lang/Makefile.common` prepends that `bin/` on PATH for `make gen`.
2. **Inlined runtime (one version).** `core/tree-sitter-version` is a single
   pin. Dune downloads that tarball into `tree-sitter-repo` and builds
   `tree-sitter-out`. Semgrep-core and the OCaml bindings compile against
   this. It does **not** choose the grammar-build CLI.

The script provisions the pin from `languages-*` and prepends that binary before `make gen`. Do not regenerate grammars with
`tree-sitter-out/bin/tree-sitter`.

## Adding a new language (the conversion procedure)

While the migration is in progress, see `.gitmodules` / `OSS/.gitmodules` for
which `tree-sitter/semgrep-*` paths are still gitlinks. Once a language is
vendored in-tree, or when adding a brand-new language (or re-vendoring from an
external `semgrep-<lang>` repo), the in-place conversion is:

1. De-submodule `languages/<lang>/tree-sitter/semgrep-<lang>`: `git rm --cached`
   the gitlink, remove its `.git` pointer and `.git/modules/…` metadata, drop
   its stanza from the **root** `.gitmodules`, regenerate `OSS/.gitmodules`
   (`python3 scripts/generate_oss_gitmodules.py`), then `git add` the directory
   as regular files. Track `lib/parser.c` with git-lfs (see `.gitattributes`).
   Keep the path identical so no dune changes are needed. Do **not** port
   `.github/pull_request_template.md` (or any other GitHub PR template) or
   `README.md`: those described the standalone `semgrep-<lang>` repo, not
   this monorepo.
   (When scripting this across many submodules, stage `.gitmodules` *before* the
   next `git rm --cached` — `git rm` refuses to remove a gitlink while
   `.gitmodules` has unstaged edits.)
2. Confirm `make regen-grammar-<lang>` reproduces the committed `lib/` (ideally a
   no-op diff against the just-vendored files).
3. Build-verify (`make core`).

### Notes

- The upstream `tree-sitter-<lang>` grammar sources are intentionally **not**
  vendored; they are only needed at regeneration time and are fetched on demand,
  pinned from `fyi/versions`.
- `fyi/` is documentation (provenance), not a build input.
- GitHub PR templates and `README.md` from the old `semgrep-<lang>` repos are
  not vendored. `scripts/check-vendored-grammars` requires every other
  tracked file to be byte-identical to the replaced gitlink, and fails if
  either was ported.
- Pre-commit skips `languages/.*/semgrep-.*` (proprietary config too) so
  ocamlformat/prettier cannot rewrite generated files. Handwritten sources
  next to them (`Parse_<lang>_tree_sitter.ml`, `ast/`, `generic/`) are still
  formatted.
- `scripts/check-parser-c-lfs` requires every in-tree `parser.c` to be stored
  via git-lfs; grammar dirs that remain git submodules are exempt (their
  `parser.c` lives inside the gitlink).

## Testing regeneration

`check-vendored-grammars` only proves the in-tree copy matches the old
submodule. It does not run the generator.

`.github/workflows/regen-grammar.yml` exercises `OSS/scripts/regen-grammar`
end-to-end on a single **solidity** canary. Solidity is small and was
regenerated most recently against the current ots core. Most other vendored
grammars were copied from older standalone repos and cannot yet be reproduced
byte-for-byte (build-provenance drift in the generator, not a bug in the
vendored parsers), so looping over every newly-vendored language would be
unreliable. `ots.checkout_steps` always pulls real LFS content (every
caller builds the tree), so a plain `git diff` on `lib/`, `bin/`, and
`config/` correctly detects changes even though `lib/parser.c` is
LFS-tracked.
