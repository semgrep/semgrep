// The main goal of this workflow is to run pre-commit on every pull requests.
// It also runs some Github Actions (GHA) lint checks

// Running pre-commit in CI. See semgrep/.pre-commit-config.yaml for
// our pre-commit configuration.
// Dogfood: note that we run Semgrep inside pre-commit, so this is also dogfooding
// and testing how semgrep interact with pre-commit.
local pre_commit = {
  'runs-on': 'ubuntu-latest',
  steps: [
    {
      uses: 'actions/checkout@v3',
      with: {
        'persist-credentials': false,
      },
    },
    // We grab those submodules below because they are the one needed by 'mypy',
    // which runs as part of pre-commit to check our Python code.
    // alt: we could also use 'submodules: recursive' instead, but that would be slower
    {
      name: 'Fetch semgrep-cli submodules',
      run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces',
    },
    // pre-commit is a Python script, this speedup things from Xmin to Ymin?
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.11',
        // This caches the pip installed dependencies that pre-commit uses.
        // Otherwise it will need to reinstall everytime.
        // TODO? who will need to reinstall everytime? github action? How much does
        // this cache directive speedup this workflow?
        cache: 'pip',
        'cache-dependency-path': '.github/workflows/lint.yml',
      },
    },
    // note that in a CI context pre-commit runs the hooks with the '--all' flag, so
    // semgrep for example is passed all the files in the repository, not just
    // the one modifed in the PR (as it is the case when it's ran from git
    // hooks locally). This is why sometimes pre-commit passes locally but fails
    // in CI, for the same PR.
    {
      uses: 'pre-commit/action@v3.0.0',
    },
  ],
};

// This is mostly a copy-paste of the pre-commit job above. The only difference
// is the 'extra_args:' directive, which runs semgrep/.pre-commit-config.yaml#L150,
// which runs Semgrep Bandit and Semgrep Python, which we don't run on normal pre-commit
// since they would slow down pre-commit on local development.
// An alternative would be to add the last step of this job in the pre-commit job above,
// but jobs can run in parallel, hence the copy-paste to speed things up.
// The intention is a test that runs semgrep-pre-commit.
// (TODO: actually it looks like we also run Semgrep Bandit/Python in normal pre-commit)
// TODO? should we split this out to a different config?
local pre_commit_manual = {
  'runs-on': 'ubuntu-latest',
  steps: [
    {
      uses: 'actions/checkout@v3',
      with: {
        'persist-credentials': false,
      },
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
local pre_commit_ocaml =
  {
    // Even if there's a 'container:' below, we still need a 'runs-on:', to say which VM will
    // run the Docker container. See https://github.com/orgs/community/discussions/25534
    // This is not necessary in Circle CI which has sane defaults.
    'runs-on': 'ubuntu-latest',
    // This custom image provides 'ocamlformat' with a specific version needed to check
    // OCaml code (must be the same than the one in dev/dev.opam)
    // See https://github.com/returntocorp/ocaml-layer/blob/master/configs/ubuntu.sh
    container: 'returntocorp/ocaml:ubuntu5.0-2023-09-29',
    // HOME in the container is tampered by GHA and modified from /root to /home/github
    // which then confuses opam below which can not find its ~/.opam (which is at /root/.opam)
    // hence the ugly use of 'env: HOME ...' below.
    // An alternative would be to run commands with 'sudo' or prefixed with 'HOME=root ...'
    steps: [
      {
        uses: 'actions/checkout@v3',
        with: {
          'persist-credentials': false,
        },
      },
      {
        name: 'Check OCaml code with ocamlformat',
        env: {
          HOME: '/root',
        },
        // coupling: the version below must be the same than in dev/dev.opam
        // Without this 'git config' command below, we would get this error in CI:
        //   fatal: detected dubious ownership in repository at '/__w/semgrep/semgrep'
        //   To add an exception for this directory, call:
        //         git config --global --add safe.directory /__w/semgrep/semgrep
        // This is probably because the files are owned by root and git
        // by default returns an error. The 'safe.directory' directive just whitelists
        // the directory so pre-commit below (which internally runs some 'git' commands)
        // does not fail.
        // TODO: Not sure why we need to do that here but have no issue in the
        // other pre-commit jobs. Maybe because pre-commit/action@v3.0.0 does extra stuff?
        // to debug errors in pre-commit, use instead:
        // opam exec -- pre-commit run --verbose --all lint-ocaml || cat /root/.cache/pre-commit/pre-commit.log
        run: |||
          opam install -y ocamlformat.0.26.1
          git config --global --add safe.directory "$GITHUB_WORKSPACE"
          opam exec -- pre-commit run --verbose --all lint-ocaml
        |||,
      },
    ],
  };

// TODO: we should port those GHA checks to semgrep and add them in semgrep-rules
local action_lint = {
  'runs-on': 'ubuntu-latest',
  steps: [
    {
      uses: 'actions/checkout@v3',
    },
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


{
  name: 'lint',
  on: {
    workflow_dispatch: null,
    pull_request: null,
    push: {
      branches: [
        'develop',
      ],
    },
  },
  jobs: {
    'pre-commit': pre_commit,
    'pre-commit-manual': pre_commit_manual,
    'pre-commit-ocaml': pre_commit_ocaml,
    'github-actions': action_lint,
  },
}
