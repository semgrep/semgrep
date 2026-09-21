// Guard dependency cache isolation, manifest hashing, and grammar installation.
// Run with make -C .github/workflows test-opam-cache from the repository root.
local semgrep = import '../libs/semgrep.libsonnet';
local ots = import '../ocaml-tree-sitter.libsonnet';
local profiles = ['format', 'grammar', 'semgrep', 'interfaces', 'lockfiles'];
local keys = [semgrep.opam_setup(cache_profile=p).with['cache-prefix'] for p in profiles];
local grammar = ots.for_tree('libs/ocaml-tree-sitter-semgrep');

assert std.length(std.set(keys)) == std.length(profiles) : 'Dependency sets must have distinct cache keys';
assert std.all([std.startsWith(keys[i], 'v7-' + profiles[i] + '-') for i in std.range(0, std.length(profiles) - 1)]);
assert grammar.setup_ocaml_step.with['cache-prefix'] == keys[1];
assert std.member(grammar.build_core_steps, grammar.install_deps_step);
assert grammar.install_deps_step['working-directory'] == '.';
assert ots.for_tree('nested/libs/ocaml-tree-sitter-semgrep').install_deps_step['working-directory'] == 'nested';
assert grammar.install_deps_step.run == 'opam exec -- make install-grammar-deps';
assert std.length(std.findSubstr("'nested/opam-lockfiles/*.locked'", semgrep.opam_setup(checkout_path='nested').with['cache-prefix'])) == 1;
assert std.length(std.findSubstr('dev/required.opam', keys[0])) > 0;
assert std.length(std.findSubstr('core/tree-sitter.opam', keys[1])) > 0;
assert std.length(std.findSubstr('semgrep-interfaces.opam', keys[3])) > 0;
true
