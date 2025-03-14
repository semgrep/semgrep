// The main goal of this workflow is to run pre-commit on every pull requests.
// Note that we run Semgrep inside pre-commit, so this is also dogfooding
// and testing how semgrep interact with pre-commit.
// We also run some Github Actions (GHA) lint checks.

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local pre_commit_steps(config='.pre-commit-config.yaml') = [
  gha.git_safedir,
  actions.setup_python_step(cache=false),
  semgrep.opam_setup(),
  { 'run' : 'opam install -y ocamlformat.0.26.2',},
  // note that in a CI context pre-commit runs the hooks with the '--all' flag, so
  // semgrep for example is passed all the files in the repository, not just
  // the one modifed in the PR (as it is the case when it's ran from git
  // hooks locally). This is why sometimes pre-commit passes locally but fails
  // in CI, for the same PR.
  {
    uses: 'pre-commit/action@v3.0.0',
    with: {
      'extra_args': '-c %s' % config,
    }
  },
];

// Running pre-commit in CI. See semgrep/.pre-commit-config.yaml for
// our pre-commit configuration.
local pre_commit_job(checkout_steps, config='.pre-commit-config.yaml') = {
  'runs-on': 'ubuntu-latest',
  steps:
    checkout_steps + pre_commit_steps(config),
};

// TODO: we should port those GHA checks to semgrep and add them in semgrep-rules
local action_lint_job(checkout_steps, dir="./") = {
  'runs-on': 'ubuntu-latest',
  steps: checkout_steps + [
    gha.git_safedir,
    {
      uses: 'actions/setup-go@v5',
      with: {
        'go-version': '1.19',
      },
    },
    {
      run: 'go install github.com/rhysd/actionlint/cmd/actionlint@v1.7.7',
    },
    {
      run: |||
        cd %s
        actionlint -shellcheck=''
      |||  % dir,
    },
  ],
};

local jsonnet_gha_job(checkout_steps, dir=".github/workflows") = {
  'runs-on': 'ubuntu-latest',
  steps: checkout_steps
    + [
    {
      name: 'Check GitHub workflow files are up to date',
      // yq (the good one) is actually pre-installed in GHA ubuntu image, see
      // https://github.com/actions/runner-images/blob/main/images/ubuntu/Ubuntu2204-Readme.md
      run: |||
        sudo apt-get update
        sudo apt-get install jsonnet
        cd %s
        make clean
        make
        git diff --exit-code
      ||| % dir,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'lint',
  on: gha.on_classic,
  jobs: {
    'pre-commit': pre_commit_job(actions.checkout_with_submodules()),
    'github-actions': action_lint_job(actions.checkout()),
    'jsonnet-gha': jsonnet_gha_job(actions.checkout()),
  },
  export::{
    // reused in semgrep-pro
    'github-actions': action_lint_job,
    'jsonnet-gha': jsonnet_gha_job,
    'pre-commit': pre_commit_job,
  },
}
