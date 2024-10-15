// The main goal of this workflow is to run pre-commit on every pull requests.
// Note that we run Semgrep inside pre-commit, so this is also dogfooding
// and testing how semgrep interact with pre-commit.
// We also run some Github Actions (GHA) lint checks.
//
// TODO: set up an environment that has both ocaml and python so we can run
//       all the pre-commit checks in one simple step like we do on our
//       dev machines.
//       At the time of writing, actions/setup-python@v4 fails for various
//       reasons when running it in our Alpine or Ubuntu containers
//       built with ocaml-layer that would provide ocamlformat.
//       Not sure why this is hard.

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local pre_commit_steps(dependencies_path) = [
  // pre-commit is a Python script, this speedup things from Xmin to Ymin?
  // Ensures we have a version of python which is acceptible to pre-commit,
  // even if we're on a runner which perhaps doesn't (e.g., Ubuntu 24.04).
  // See also <https://github.com/pre-commit/action/issues/210>.
  // TODO: factorize with actions.libsonnet setup_python_step
   {
    uses: 'actions/setup-python@v4',
    with: {
      'python-version': '3.11',
      // This caches the pip installed dependencies that pre-commit uses.
      // Otherwise it will need to reinstall everytime.
      // TODO? who will need to reinstall everytime? github action? How much does
      // this cache directive speedup this workflow?
      cache: 'pip',
      'cache-dependency-path': dependencies_path,
    },
  },
  // note that in a CI context pre-commit runs the hooks with the '--all' flag, so
  // semgrep for example is passed all the files in the repository, not just
  // the one modifed in the PR (as it is the case when it's ran from git
  // hooks locally). This is why sometimes pre-commit passes locally but fails
  // in CI, for the same PR.
  {
    uses: 'pre-commit/action@v3.0.0',
    env: {
      // Tell scripts/lint-ocaml to not bother with ocamlformat
      SKIP_OCAMLFORMAT: 'yes',
    },
  },
];

// Running pre-commit in CI. See semgrep/.pre-commit-config.yaml for
// our pre-commit configuration.
local pre_commit_job = {
  'runs-on': 'ubuntu-latest',
  steps: [
    actions.checkout(),
    gha.git_safedir,
    // We grab those submodules below because they are the one needed by 'mypy',
    // which runs as part of pre-commit to check our Python code.
    // alt: we could also use 'submodules: recursive' instead, but that would be slower
    {
      name: 'Fetch semgrep-cli submodules',
      run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces',
    },
  ] + pre_commit_steps('.github/workflows/lint.yml'),
};

// The 'extra_args:' directive runs semgrep/.pre-commit-config.yaml#L150,
// which runs Semgrep Bandit and Semgrep Python, which we don't run on normal pre-commit
// since they would slow down pre-commit on local development.
// An alternative would be to add the last step of this job in the pre-commit job above,
// but jobs can run in parallel, hence the copy-paste to speed things up.
// The intention is a test that runs semgrep-pre-commit.
// (TODO: actually it looks like we also run Semgrep Bandit/Python in normal pre-commit)
// TODO? should we split this out to a different config?
local pre_commit_manual_job = {
  'runs-on': 'ubuntu-latest',
  steps: [
    actions.checkout(),
    gha.git_safedir,
    // less: factorize with pre_commit_steps
    {
       uses: 'actions/setup-python@v4',
       with: {
         'python-version': '3.11',
        }
    },
    {
      uses: 'pre-commit/action@v3.0.0',
      with: {
        extra_args: '--hook-stage manual',
      },
    },
  ],
};

// Running the ocamlformat part of pre-commit, which requires a special container
local pre_commit_ocaml_job(checkout_steps) =
  {
    // Even if there's a 'container:' below, we still need a 'runs-on:', to say which VM will
    // run the Docker container. See https://github.com/orgs/community/discussions/25534
    // (this is not necessary in Circle CI which has sane defaults).
    'runs-on': 'ubuntu-latest',
    // This custom image provides 'ocamlformat' with a specific version needed to check
    // OCaml code (must be the same than the one in dev/required.opam)
    // See https://github.com/returntocorp/ocaml-layer/blob/master/configs/ubuntu.sh
    container: 'returntocorp/ocaml:ubuntu-2023-10-17',
    steps: checkout_steps +
      [
      // HOME in the container is tampered by GHA and modified from /root to /home/github
      // which then confuses opam below which can not find its ~/.opam (which is at /root/.opam)
      // hence the ugly use of 'env: HOME ...' below.
      // An alternative would be to run commands with 'sudo' or prefixed with 'HOME=root ...'
      {
        name: 'Check OCaml code with ocamlformat',
        env: {
          HOME: '/root',
        },
        // coupling: the version below must be the same than in dev/required.opam
        //
        // Without the 'git config' command below, we would get this error in CI:
        //   fatal: detected dubious ownership in repository at '/__w/semgrep/semgrep'
        //   To add an exception for this directory, call:
        //         git config --global --add safe.directory /__w/semgrep/semgrep
        // This is probably because the files are owned by root and git
        // by default returns an error. The 'safe.directory' directive just whitelists
        // the directory so pre-commit below (which internally runs some 'git' commands)
        // does not fail.
        // TODO: Not sure why we need to do that here but have no issue in the
        // other pre-commit jobs. Maybe because pre-commit/action@v3.0.0 does extra stuff?
        //
        // to debug errors in pre-commit, use instead:
        // opam exec -- pre-commit run --verbose --all lint-ocaml || cat /root/.cache/pre-commit/pre-commit.log
        //
        // TODO: get rid of apt-get autconf and opam update which is slow
        // but need a more recent container: above
        run: |||
          # When installing ocamlformat OPAM will try to rebuild some packages
          # and for that it requires 'autoconf'.
          apt-get install -y autoconf
          opam update
          opam install -y ocamlformat.0.26.2
          git config --global --add safe.directory "$GITHUB_WORKSPACE"
          opam exec -- pre-commit run --verbose --all lint-ocaml
        |||,
      },
    ],
  };

// TODO: we should port those GHA checks to semgrep and add them in semgrep-rules
local action_lint_job(checkout_steps) = {
  'runs-on': 'ubuntu-latest',
  steps: checkout_steps + [
    gha.git_safedir,
    {
      uses: 'actions/setup-go@v4',
      with: {
        'go-version': '1.19',
      },
    },
    {
      run: 'go install github.com/rhysd/actionlint/cmd/actionlint@v1.6.25',
    },
    {
      run: "actionlint -shellcheck=''",
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
    'pre-commit': pre_commit_job,
    'pre-commit-manual': pre_commit_manual_job,
    'pre-commit-ocaml': pre_commit_ocaml_job([actions.checkout()]),
    'github-actions': action_lint_job([actions.checkout()]),
    'jsonnet-gha': jsonnet_gha_job([actions.checkout()]),
  },
  export::{
    // reused in semgrep-pro
    'pre-commit-ocaml': pre_commit_ocaml_job,
    'github-actions': action_lint_job,
    'jsonnet-gha': jsonnet_gha_job,
    'pre-commit-steps': pre_commit_steps,
  },
}
