# Scala grammar updates

These instructions apply to Scala grammar repair and subsequent CST-to-AST
conversion. Grammar repair handles scanner, preparation, and corpus work;
regeneration and converter repair follow as separate pipeline stages. Pending
converter work is not a blocker to completing the grammar stage. During converter
repair, expected compilation errors from changed CST types are in-scope work,
not grounds for `CANNOT_PROCEED`. Paths below
are relative to the OSS root unless stated otherwise.

Use the supplied pre-update revision and current generated CST as the repair
inputs. Do not transplant converter code or tests from stashes or another
unmerged update attempt; these notes carry the durable knowledge between runs.

For ambiguous metavariable-lambda probes, complete the comparison against the
exact shipped pre-update parser before deciding whether support regressed.
Use one source or one pattern entry point per probe; do not concatenate
`__SEMGREP_EXPRESSION` entry points into a compilation unit. Record inherited
limitations and continue; repair confirmed losses within the grammar scope.

| Area | Constraint and verification |
| --- | --- |
| Owned scanner | This wrapper owns `src/scanner.c`; compare it with both upstream pins, synchronize external-token order (including `COMMA_OUTDENT`) and upstream scanner behavior, and preserve Semgrep extensions. Cover comma outdent and leading infix continuation in native and Semgrep corpus cases. |
| Corpus preparation | This wrapper's `prep` copies and patches the upstream corpus. Grammar repair may update `prep` to suppress patch backups with `--no-backup-if-mismatch`; `.orig` and `.~1~` files become duplicate corpus tests. Fix preparation, not inherited expected trees. |
| Constructor annotations | Preserve the wrapper's single-argument-list annotation rule: class parameters must not become annotation arguments. Keep annotated multi-parameter constructor coverage. |
| Consecutive guards | Consecutive `if` guards in one enumerator are a known unsupported form through upstream v0.25.1. The old shipped parser silently misparsed the second `if`; successful old parsing alone does not establish correct translation. Keep this limitation explicit in `libs/ocaml-tree-sitter-semgrep/lang/scala/test/xfail/`, with a separate-enumerator passing control. Grammar repair may add these focused xfails; do not weaken existing passing tests or add an unrelated guard override. Recheck support for later pins. |
| Parser selection | Scala uses tree-sitter first and a handwritten Pfff fallback. Verify changed features with tree-sitter directly; engine acceptance through fallback does not establish converter coverage. |
| Match-type migration | In v0.25.1, `CST.indented_type_cases` is replaced by an inline choice in `Match_type`: `Indent_rep1_type_case_clause_outd` or `LCURL_rep1_type_case_clause_RCURL`. Extract the case list from either branch; convert each clause into both its left and right types and retain them with the scrutinee. Removing the obsolete type annotation alone is insufficient. |
| Changed CST payloads | In `languages/scala/generic/Parse_scala_tree_sitter.ml`, retain match-type scrutinee/case sides, lambda parameters and bodies, given signatures, enum modifiers, anonymous `using` parameters, and import aliases. Check concrete AST children and matching, not compilation alone. |
| Parent constructors | Preserve every regular and `using` argument clause on its owning parent, including clause boundaries and the `using` marker; flattening all arguments loses distinctions such as `A(1)(2)` versus `A(1, 2)`. Trailing clauses belong to the last comma-separated parent; `with` parents must retain constructor arguments too, while argument-free mixins retain their existing representation. Test a single parent, multiple comma-separated parents, and `with` parents with distinct arguments, plus nearby negative pattern matches that distinguish clause grouping as well as argument values. |
| Known upstream scanner limitation | The v0.25.1 scanner's ASCII-only operator classification does not cover Unicode symbol operators at continuation-line starts. This is inherited upstream behavior, not a new Semgrep scanner regression; report its provenance rather than expanding the update scope. |

Use `tests/parsing/scala/` and `tests/patterns/scala/` for translation regressions,
and this wrapper's `test/corpus/` for CST and scanner regressions. Existing
fixtures are evidence only after their actual parser route and AST are checked.
