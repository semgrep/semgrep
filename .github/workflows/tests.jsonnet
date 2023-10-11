// The goals of this workflow are to check that:
// - we can build semgrep-core and pysemgrep
// - all our semgrep-core and pysemgrep (and osemgrep) tests are passing
// - we can build a Docker image
// - we can build Linux and MacOS binaries and python "wheels" for pypi
// - we don't have any perf regressions in our benchmarks

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// some jobs rely on artifacts produced by this workflow
local core_x86 = import 'build-test-core-x86.jsonnet';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

// Bence's automatic snapshot update PR

local failure_and_right_event =
  "failure() && github.event_name == 'pull_request' && (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))";

local snapshot_update_pr_steps = [
  // because of the fail-fast setting, we expect only the fastest failing
  // job to get to the steps below
  {
    name: 'Prepare repo for snapshot commit',
    'if': 'failure()',
    run: |||
      # the commit step that follows will fail to fetch the pfff submodule
      # (perhaps because of the github token's permissions)
      # so we disable recursive fetching
      git config fetch.recurseSubmodules false
    |||,
  },
  {
    name: 'Configure git creds for push',
    id: 'configure-creds',
    'if': failure_and_right_event,
    run: |||
      echo "machine github.com" >> ~/.netrc
      echo "login ${{ github.repository }}" >> ~/.netrc
      echo "password ${{ secrets.GITHUB_TOKEN }}" >> ~/.netrc
    |||,
  },
  {
    name: 'Commit snapshot updates',
    id: 'snapshot-commit',
    'if': failure_and_right_event,
    uses: 'EndBug/add-and-commit@v9',
    with: {
      add: 'cli/tests/e2e/snapshots',
      default_author: 'github_actions',
      message: 'Update pytest snapshots',
      new_branch: 'snapshot-updates-${{ github.run_id }}-${{ github.run_attempt }}',
    },
  },
  {
    name: 'Remove Credentials',
    id: 'remove-creds',
    'if': failure_and_right_event,
    run: 'rm ~/.netrc',
  },
  {
    name: 'Comment about any snapshot updates',
    'if': "failure() && steps.snapshot-commit.outputs.pushed == 'true'",
    run: |||
      echo ":camera_flash: The pytest shapshots changed in your PR." >> /tmp/message.txt
      echo "Please carefully review these changes and make sure they are intended:" >> /tmp/message.txt
      echo >> /tmp/message.txt
      echo "1. Review the changes at https://github.com/returntocorp/semgrep/commit/${{ steps.snapshot-commit.outputs.commit_long_sha }}" >> /tmp/message.txt
      echo "2. Accept the new snapshots with" >> /tmp/message.txt
      echo >> /tmp/message.txt
      echo "       git fetch origin && git cherry-pick ${{ steps.snapshot-commit.outputs.commit_sha }} && git push" >> /tmp/message.txt

      gh pr comment ${{ github.event.pull_request.number }} --body-file /tmp/message.txt
    |||,
    env: {
      GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
    },
  },
];

// ----------------------------------------------------------------------------
// Semgrep-core and osemgrep jobs
// ----------------------------------------------------------------------------

// This is mostly the same that in build-test-core-x86.jsonnet
// but without the artifact creation and with more tests.
// alt: we could factorize buy copy-paste is ok.
local test_semgrep_core_job =
  semgrep.ocaml_alpine_container
  {
    steps: [
      actions.checkout_with_submodules(),
      {
        name: 'Build semgrep-core',
        run: |||
          eval $(opam env)
          make install-deps-ALPINE-for-semgrep-core
          make install-deps-for-semgrep-core
          make core
        |||,
      },
      {
        name: 'Test semgrep-core (and time it)',
        run: |||
          eval $(opam env)
          START=`date +%s`

          make core-test
          make core-test-e2e

          END=`date +%s`
          TEST_RUN_TIME=$((END-START))
          curl --fail -L -X POST "https://dashboard.semgrep.dev/api/metric/semgrep.core.test-run-time-seconds.num" -d "$TEST_RUN_TIME"
        |||,
      },
      {
        name: 'Report Number of Tests Stats',
        'if': "github.ref == 'refs/heads/develop'",
        run: './scripts/report_test_metrics.sh',
      },
      // TODO: move this to a stable host for more reliable results.
      // It's not clear how to push the stats only when "on the main
      // branch". The GitHub Actions documentation is unhelpful. So we
      // keep things simple and publish the results every time.
      {
        name: 'Publish match performance',
        // This runs a short test suite to track the match performance
        // of semgrep-core over time. The results are pushed to the
        // dashboard at https://dashboard.semgrep.dev/
        run: 'opam exec -- make report-perf-matching',
      },
    ],
  };

// alt: could factorize with previous job
local test_osemgrep_job =
  semgrep.ocaml_alpine_container
  {
    steps: [
      actions.checkout_with_submodules(),
      {
        name: 'Build semgrep-core',
        run: |||
          eval $(opam env)
          make install-deps-ALPINE-for-semgrep-core
          make install-deps-for-semgrep-core
          make core
        |||,
      },
      {
        name: 'Install osemgrep',
        run: |||
          eval $(opam env)
          make copy-core-for-cli
        |||,
      },
      {
        name: 'Install Python dependencies',
        run: |||
          make install-deps-ALPINE-for-pysemgrep
          (cd cli; pipenv install --dev)
        |||,
      },
      {
        name: 'Run pytest for osemgrep known passing tests',
        'working-directory': 'cli',
        run: 'make osempass',
      },
    ],
  };

// ----------------------------------------------------------------------------
// Pytests
// ----------------------------------------------------------------------------

// Factorize a few steps for the test_xxx jobs below

// alt: could just use submodule:true, not sure it's worth the opti
local fetch_submodules_step = {
  name: 'Fetch semgrep-cli submodules',
  run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces',
};
local pipenv_install_step = {
  run: 'pip install pipenv==2022.6.7',
};

local download_x86_artifacts = {
  uses: 'actions/download-artifact@v3',
  with: {
    name: core_x86.export.artifact_name,
  },
};
local install_x86_artifacts = {
  name: 'Install artifacts',
  run: |||
    tar xf ocaml-build-artifacts.tgz
    sudo cp ocaml-build-artifacts/bin/* /usr/bin
  |||,
};

local install_python_deps = {
  name: 'Install Python dependencies',
  'working-directory': 'cli',
  run: 'pipenv install --dev',
};

// Run pytest with many python versions
local test_cli_job = {
  name: 'test semgrep-cli',
  'runs-on': 'ubuntu-22.04',
  needs: [
    'build-test-core-x86',
  ],
  permissions: {
    contents: 'write',
    'pull-requests': 'write',
  },
  strategy: {
    matrix: {
      python: [
        '3.7',
        '3.8',
        '3.9',
        '3.10',
        '3.11',
      ],
    },
  },
  steps: [
    actions.checkout(),
    fetch_submodules_step,
    actions.setup_python('${{ matrix.python }}'),
    pipenv_install_step,
    download_x86_artifacts,
    install_x86_artifacts,
    install_python_deps,
    {
      name: 'Run pytest',
      'working-directory': 'cli',
      // the --snapshot-update below works with the snapshot_update_pr_steps
      run: |||
        # tests should simulate CI environment iff they need one
        unset CI
        unset "${!GITHUB_@}"

        pipenv run pytest -n auto -vv --snapshot-update --allow-snapshot-deletion
      |||,
    },
  ] + snapshot_update_pr_steps,
};

// These tests aren't run by default by pytest.
// To reproduce errors locally, use:
//   $ cd cli/tests
//   $ make qa

local test_qa_job = {
  name: 'quality assurance on semgrep',
  'runs-on': 'ubuntu-22.04',
  needs: [
    'build-test-core-x86',
  ],
  strategy: {
    'fail-fast': false,
    matrix: {
      split: [
        1,
        2,
        3,
        4,
      ],
    },
  },
  steps: [
    actions.checkout(),
    // Is it indented that we also fetch tests/semgrep-rules?
    {
      name: 'Fetch semgrep-cli submodules',
      run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces tests/semgrep-rules',
    },
    actions.setup_python('3.11'),
    pipenv_install_step,
    download_x86_artifacts,
    install_x86_artifacts,
    // TODO: mostly like install_python_deps with PATH adjustment
    {
      name: 'Install semgrep',
      'working-directory': 'cli',
      run: |||
        export PATH=/github/home/.local/bin:$PATH
        pipenv install --dev
      |||,
    },
    {
      uses: 'actions/cache@v3',
      with: {
        path: '~/.cache/qa-public-repos',
        key: "qa-public-repos-${{ hashFiles('semgrep/tests/qa/*public_repos*') }}-${{ matrix.split }}",
      },
    },
    {
      run: |||
        mkdir -p ~/.cache/qa-public-repos
        touch ~/.cache/qa-public-repos/ok
      |||,
    },
    {
      name: 'Test semgrep',
      'working-directory': 'cli',
      run: |||
        export PATH=/github/home/.local/bin:$PATH
        pipenv run pytest -n auto -vv --tb=short --splits 4 --group ${{ matrix.split }} tests/qa
      |||,
      env: {
        QA_TESTS_CACHE_PATH: '~/.cache/qa-public-repos',
      },
    },
  ],
};

// ----------------------------------------------------------------------------
// Benchmarks
// ----------------------------------------------------------------------------

local bench_prepare_steps = [
  actions.checkout(),
  fetch_submodules_step,
  actions.setup_python('3.7'),
  pipenv_install_step,
  download_x86_artifacts,
  install_x86_artifacts,
  install_python_deps,
];

// Run abbreviated version of benchmarks to check that they work
local benchmarks_lite_job = {
  'runs-on': 'ubuntu-22.04',
  needs: [
    'build-test-core-x86',
  ],
  steps: bench_prepare_steps + [
    {
      name: 'Test dummy benchmarks on latest',
      'working-directory': 'cli',
      run: |||
        pipenv run semgrep --version
        pipenv run semgrep-core -version
        pipenv run python3 ../perf/run-benchmarks --dummy
      |||,
    },
  ],
};

// Run each benchmark twice to decrease effect of natural variance
local benchmarks_full_job = {
  'runs-on': 'ubuntu-22.04',
  needs: [
    'build-test-core-x86',
  ],
  steps: bench_prepare_steps + [
    {
      name: 'Run perf benchmark',
      run: 'scripts/run-benchmarks.sh ${{ secrets.GITHUB_TOKEN }} ${{ github.event.number }}',
    },
    {
      name: 'Run python performance tests',
      'working-directory': 'cli',
      run: 'pipenv run pytest tests/performance',
    },
  ],
};

// ----------------------------------------------------------------------------
// Docker
// ----------------------------------------------------------------------------

// To make a comparison to git:
// - docker image == git repository
//   example: returntocorp/semgrep
//
// - docker tag == git ref
//   example: :latest, :canary
//
// - docker digest == git commit
//   example: sha256:98ea6e4f216f2fb4b69fff9b3a44842c38686ca685f3f55dc48c5d3fb1107be4

// You can see those tags in use here:
// https://hub.docker.com/r/returntocorp/semgrep/tags
local docker_tags = |||
  # tag image with full version (ex. "1.2.3")
  type=semver,pattern={{version}}
  # tag image with major.minor (ex. "1.2")
  type=semver,pattern={{major}}.{{minor}}
  # tag image with pr (ex. "pr-42", great for bisecting)
  type=ref,event=pr
  # ??? deleted those? useful?
  type=ref,event=branch
  type=sha,event=branch
  # ???
  type=edge
|||;

local build_test_docker_job = {
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  with: {
    'docker-flavor': |||
      latest=auto
    |||,
    'docker-tags': docker_tags,
    // ??
    'artifact-name': 'image-test',
    'repository-name': '${{ github.repository }}',
    file: 'Dockerfile',
    // see the Dockerfile, this is the name root variant
    target: 'semgrep-cli',
    'enable-tests': true,
  },
};

local build_test_docker_nonroot_job = {
  // We want to run build-test-docker-nonroot *after* build-test-docker so
  // that it reuses the warmed-up docker cache.
  needs: [
    'build-test-docker',
  ],
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  with: {
    // nonroot suffix here! which will be added for each tags
    'docker-flavor': |||
      latest=auto
      suffix=-nonroot,onlatest=true
    |||,
    'docker-tags': docker_tags,
    'artifact-name': 'image-test-nonroot',
    'repository-name': '${{ github.repository }}',
    file: 'Dockerfile',
    // see the Dockerfile, this is the name of the nonroot variant
    target: 'nonroot',
    // TODO: why false here?
    'enable-tests': false,
  },
};

local right_ref_and_right_event =
  "github.ref == 'refs/heads/develop' || (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))";

local push_docker_job = {
  needs: [
    'build-test-docker',
  ],
  uses: './.github/workflows/push-docker.yaml',
  'if': right_ref_and_right_event,
  secrets: 'inherit',
  with: {
    'artifact-name': 'image-test',
    'repository-name': '${{ github.repository }}',
    'dry-run': false,
  },
};

local push_docker_nonroot_job = {
  needs: [
    'build-test-docker-nonroot',
  ],
  uses: './.github/workflows/push-docker.yaml',
  'if': right_ref_and_right_event,
  secrets: 'inherit',
  with: {
    'artifact-name': 'image-test-nonroot',
    'repository-name': '${{ github.repository }}',
    'dry-run': false,
  },
};

// ----------------------------------------------------------------------------
// Semgrep Pro
// ----------------------------------------------------------------------------

local test_semgrep_pro_job = {
  needs: [
    'build-test-docker',
    'push-docker',
  ],
  uses: './.github/workflows/test-semgrep-pro.yaml',
  'if': "github.ref == 'refs/heads/develop' || github.event.pull_request.head.repo.full_name == github.repository",
  secrets: 'inherit',
  with: {
    'artifact-name': 'image-test',
    'repository-name': '${{ github.repository }}',
  },
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

// ??
local ignore_md = {
  'paths-ignore': [
    '**.md',
  ],
};

{
  name: 'tests',
  on: {
    workflow_dispatch: null,
    pull_request: ignore_md,
    push: {
      branches: [
        'develop',
      ],
    } + ignore_md,
  },
  jobs: {
    'test-semgrep-core': test_semgrep_core_job,
    'test-osemgrep': test_osemgrep_job,
    // Pysemgrep tests, requires build-test-core-x86 job
    'test-cli': test_cli_job,
    'test-qa': test_qa_job,
    'benchmarks-lite': benchmarks_lite_job,
    'benchmarks-full': benchmarks_full_job,
    // Docker stuff
    'build-test-docker': build_test_docker_job,
    // requires build-test-docker
    'push-docker': push_docker_job,
    'build-test-docker-nonroot': build_test_docker_nonroot_job,
    'push-docker-nonroot': push_docker_nonroot_job,
    // Semgrep-pro mismatch check
    'test-semgrep-pro': test_semgrep_pro_job,
    // The inherit jobs also included from releases.yml
    'build-test-core-x86': {
      uses: './.github/workflows/build-test-core-x86.yml',
      secrets: 'inherit',
    },
    'build-test-manylinux-x86': {
      needs: [
        'build-test-core-x86',
      ],
      uses: './.github/workflows/build-test-manylinux-x86.yml',
      secrets: 'inherit',
    },
    'build-test-manylinux-aarch64': {
      needs: [
        'build-test-docker',
      ],
      uses: './.github/workflows/build-test-manylinux-aarch64.yml',
      secrets: 'inherit',
    },
    'build-test-osx-x86': {
      uses: './.github/workflows/build-test-osx-x86.yml',
      secrets: 'inherit',
    },
    'build-test-osx-arm64': {
      uses: './.github/workflows/build-test-osx-arm64.yml',
      secrets: 'inherit',
    },
    'build-test-javascript': {
      uses: './.github/workflows/build-test-javascript.yml',
      secrets: 'inherit',
      // we limit artifact uploads to avoid filling the S3 bucket with tons of semgrep.js builds.
      // we will upload if one of these are true:
      // - the branch name is "develop" (so that we can test the bleeding edge)
      // - the branch name starts with "release-" (TODO: move this to release.yml instead)
      // - the PR is not a fork and has a "publish-js" label
      with: {
        'upload-artifacts': "${{ (github.ref == 'refs/heads/develop') || startsWith(github.ref, 'refs/heads/release-') || (!github.event.pull_request.head.repo.fork && contains(github.event.pull_request.labels.*.name, 'publish-js')) }}",
      },
    },
  },
}
