// This is similar to build-test-core-x86.jsonnet but using OCaml 5
// and setup-ocaml@v2 instead of ocaml-layer.
// This is just to make sure Semgrep is ready to be migrated to OCaml 5.
local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

//TODO: currently fails because of conflicts with ocamlformat and jsoo
//TODO: local opam_switch = '5.2.0~alpha1';
local opam_switch = '5.1.0';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job = {
  'runs-on': 'ubuntu-latest',
  steps: [
    actions.checkout_with_submodules(),
    // this must be done after the checkout as opam installs itself
    // locally in the project folder (/home/runner/work/semgrep/semgrep/_opam)
    {
      uses: 'ocaml/setup-ocaml@v2',
      with: {
        'ocaml-compiler': opam_switch,
      },
    },
    semgrep.cache_opam.step(
      key=opam_switch + '-_opam-' + "${{ hashFiles('semgrep.opam') }}",
      path="_opam",
    ),
    // alt: no 'make install-deps-UBUNTU-for-semgrep-core'
    // possibly looks like opam and setup-ocaml@ can automatically install
    // depext dependencies, but ran into issues for git-unix with libev. Best if
    // we have install-deps-for-semgrep-core have the platform version in the
    // rule or as a prereq?
    {
      name: 'Install semgrep dependencies',
      run: |||
        eval $(opam env)
        make install-deps-UBUNTU-for-semgrep-core
        make install-deps-for-semgrep-core
      |||,
    },
    {
      name: 'Build semgrep',
      run: |||
        eval $(opam env)
        make
      |||,
    },
    // TODO: some tests are currently failing with OCaml5! but at least
    // we can still check whether it builds
    {
      name: 'Test semgrep',
      run: |||
        echo TODO
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'build-test-core-x86-ocaml5',
  // Here we differ from build-test-core-x86.jsonnet by not relying on
  // tests.yml for being included; this is an independent workflow instead.
  on: gha.on_classic,
  jobs: {
    job: job,
  },
}
