// Guard grammar workflow path routing and Git pathspec translation.
//
// grammar paths select affected test-lang targets; all_languages paths force
// the full matrix; ignored paths must affect neither the trigger nor selection.
local workflow = import '../ocaml-tree-sitter-test-languages.jsonnet';
local paths = workflow.export.trigger_path_groups('libs/ocaml-tree-sitter-semgrep');
local expected_all_languages = std.set([
  '*.opam',
  '.github/workflows/libs/actions.libsonnet',
  '.github/workflows/libs/semgrep.libsonnet',
  '.github/workflows/libs/uses.libsonnet',
  '.github/workflows/ocaml-tree-sitter-test-languages.jsonnet',
  '.github/workflows/ocaml-tree-sitter-test-languages.yml',
  '.github/workflows/ocaml-tree-sitter.libsonnet',
  'Makefile',
  'cli/pyproject.toml',
  'cli/uv.lock',
  'dune-project',
  'dune-workspace',
  'libs/ocaml-tree-sitter-semgrep/Makefile',
  'libs/ocaml-tree-sitter-semgrep/core/**',
  'libs/ocaml-tree-sitter-semgrep/dune',
  'libs/ocaml-tree-sitter-semgrep/scripts/**',
  'opam-lockfiles/*.locked',
  'tree-sitter-config.mk',
  'tree-sitter-config.sh',
]);
local expected_ignored = [
  '!libs/ocaml-tree-sitter-semgrep/core/doc/**',
  '!libs/ocaml-tree-sitter-semgrep/core/test/**',
  '!libs/ocaml-tree-sitter-semgrep/core/README.md',
  '!libs/ocaml-tree-sitter-semgrep/scripts/test_*.py',
];
local nested_ots = 'OSS/libs/ocaml-tree-sitter-semgrep';
local nested_extra = ['OSS/.github/workflows/ocaml-tree-sitter.libsonnet'];
local nested = workflow.export.trigger_path_groups(nested_ots, nested_extra);

// Every trigger path has one deliberate matrix effect.
assert paths.grammar == ['libs/ocaml-tree-sitter-semgrep/lang/**'];
assert paths.all_languages == expected_all_languages;
assert paths.ignored == expected_ignored;
assert std.setInter(paths.grammar, paths.all_languages) == [];
assert workflow.export.trigger_paths('libs/ocaml-tree-sitter-semgrep')
       == std.set(paths.grammar + paths.all_languages) + paths.ignored;

// Git must use the same repository-rooted glob semantics as GitHub.
assert workflow.export.git_pathspec('*.opam') == "':(top,glob)*.opam'";
assert workflow.export.git_pathspec('libs/ocaml-tree-sitter-semgrep/lang/**')
       == "':(top,glob)libs/ocaml-tree-sitter-semgrep/lang/**'";
assert workflow.export.git_pathspec('!scripts/test_*.py')
       == "':(top,exclude,glob)scripts/test_*.py'";

// Nested checkouts prefix OTS paths while retaining repository workflow paths
// and explicitly supplied paths.
assert nested.grammar == [nested_ots + '/lang/**'];
assert std.member(nested.all_languages, nested_ots + '/scripts/**');
assert std.member(
  nested.all_languages,
  '.github/workflows/ocaml-tree-sitter.libsonnet',
);
assert std.member(nested.all_languages, nested_extra[0]);
assert nested.ignored == [
  '!' + nested_ots + '/core/doc/**',
  '!' + nested_ots + '/core/test/**',
  '!' + nested_ots + '/core/README.md',
  '!' + nested_ots + '/scripts/test_*.py',
];

true
