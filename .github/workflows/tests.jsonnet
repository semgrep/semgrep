// The goals of this workflow are to check that:
// - we can build semgrep-core and pysemgrep
// - all our semgrep-core and pysemgrep (and osemgrep) tests are passing
// - we can build a Docker image (for amd64 and arm64)
// - we can build Linux and MacOS binaries and python "wheels" for pypi
//   (also for amd64 and arm64)
// - we don't have any perf regressions in our benchmarks

local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// some jobs rely on artifacts produced by these workflow
local core_x86 = import 'build-test-core-x86.jsonnet';

// intermediate image produced by build-push-action
local docker_artifact_name = 'semgrep-docker-image-artifact';

local docker_repository_name = 'semgrep/semgrep';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

// Bence's automatic snapshot update PR

local failure_and_right_event =
  "failure() && github.event_name == 'pull_request' && (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))";

local snapshot_update_pr_steps(add_paths, repo_name) = [
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
      add: add_paths,
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
      echo "1. Review the changes at https://github.com/semgrep/%(repo_name)s/commit/${{ steps.snapshot-commit.outputs.commit_long_sha }}" >> /tmp/message.txt
      echo "2. Accept the new snapshots with" >> /tmp/message.txt
      echo >> /tmp/message.txt
      echo "       git fetch origin && git cherry-pick ${{ steps.snapshot-commit.outputs.commit_sha }} && git push" >> /tmp/message.txt

      gh pr comment ${{ github.event.pull_request.number }} --body-file /tmp/message.txt
    ||| % {repo_name: repo_name},
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
// alt: we could factorize
local test_semgrep_core_job =
  semgrep.containers.ocaml_alpine.job
  {
    steps: [
      gha.speedy_checkout_step,
      actions.checkout_with_submodules(),
      gha.git_safedir,
      semgrep.cache_opam.step(
        key=semgrep.containers.ocaml_alpine.opam_switch +
          "-${{hashFiles('semgrep.opam')}}"
       ),
      {
        name: 'Install dependencies',
        run: |||
          eval $(opam env)
          make install-deps-ALPINE-for-semgrep-core
          make install-deps-for-semgrep-core
          make -C interfaces/semgrep_interfaces setup-ALPINE setup
        |||,
      },
      {
        name: 'Build semgrep-core',
        run: 'opam exec -- make core',
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
  semgrep.containers.ocaml_alpine.job
  {
    steps: [
      gha.speedy_checkout_step,
      actions.checkout_with_submodules(),
    ] +
    semgrep.osemgrep_test_steps_after_checkout
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

local download_x86_artifacts =
  actions.download_artifact_step(core_x86.export.artifact_name);

local install_x86_artifacts = {
  name: 'Install artifacts',
  run: |||
    tar xf artifacts.tgz
    #alt: put it in cli/src/semgrep/bin/, like make copy-core-for-cli
    sudo cp artifacts/* /usr/bin
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
    // Needed for semgrep-core
    'build-test-core-x86',
  ],
  permissions: gha.pull_request_permissions,
  strategy: {
    matrix: {
      python: [
        '3.9',
        '3.10',
        '3.11',
      ],
    },
  },
  steps: [
    actions.checkout(),
    fetch_submodules_step,
    actions.setup_python_step('${{ matrix.python }}'),
    actions.pipenv_install_step,
    install_python_deps,
    download_x86_artifacts,
    install_x86_artifacts,
    {
      name: 'Run pytest',
      'working-directory': 'cli',
      // The --snapshot-update below works with the snapshot_update_pr_steps.
      //
      run: |||
        # tests should simulate CI environment iff they need one
        unset CI
        unset "${!GITHUB_@}"

        PYTEST_EXTRA_ARGS="--snapshot-update --allow-snapshot-deletion" make ci-test
      |||,
    },
  ] + snapshot_update_pr_steps(
    add_paths="cli/tests/default/e2e/snapshots",
    repo_name="semgrep"
  ),
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
    actions.setup_python_step('3.11'),
    actions.pipenv_install_step,
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
  actions.setup_python_step(semgrep.default_python_version),
  actions.pipenv_install_step,
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

local trigger_semgrep_comparison_argo = {
  'if': "${{ github.event_name == 'pull_request' && !startsWith(github.event.pull_request.base.ref, 'release') && !startsWith(github.head_ref, 'release') }}",
  secrets: 'inherit',
  needs: [
    'push-docker-returntocorp',
  ],
  uses: './.github/workflows/trigger-semgrep-comparison-argo.yml',
};

// ----------------------------------------------------------------------------
// Docker
// ----------------------------------------------------------------------------

// In the jobs below, we set certain docker "tags".
// To make a comparison to git:
// - docker image == git repository
//   example: semgrep/semgrep
// - docker digest == git commit
//   example: sha256:98ea6e4f216f2fb4b69fff9b3a44842c38686ca685f3f55dc48c5d3
// - docker tag == git ref
//   example: :latest, :canary, 1.2.3, pr-4434
//
// You can see those tags in use here:
// https://hub.docker.com/r/semgrep/semgrep/tags
//
// Example of docker tags:
// - # tag image with full version (ex. "1.2.3")
//   type=semver,pattern={{version}}
// - # tag image with major.minor (ex. "1.2")
//   type=semver,pattern={{major}}.{{minor}}
// - # tag image with pr (ex. "pr-42", great for bisecting)
//   type=ref,event=pr
// - # tag image with branch (ex: "develop")
//   type=ref,event=branch
// - # tag image with commit (ex: "sha-ab324a")
//   type=sha,event=branch
// - # not sure we need this one
//   type=edge

local build_test_docker_job = {
  uses: './.github/workflows/build-test-docker.yml',
  secrets: 'inherit',
  with: {
    'docker-flavor': |||
       latest=false
     |||,
    'docker-tags': |||
       type=ref,event=pr
       type=ref,event=branch
       type=sha,event=branch
     |||,
    'artifact-name': docker_artifact_name,
    'repository-name': docker_repository_name,
    file: 'Dockerfile',
    // see the Dockerfile, this is the name root variant
    target: 'semgrep-cli',
    'enable-tests': true,
  },
};

// The Dockerfile contain different steps and can build different "targets"
// (e.g., a "nonroot" variant of the official semgrep docker image)
local build_test_docker_other_target_job(suffix, target) = {
  // We want to run build-test-docker-other *after* build-test-docker so
  // that it reuses the warmed-up docker cache.
  needs: [
    'build-test-docker',
  ],
  uses: './.github/workflows/build-test-docker.yml',
  secrets: 'inherit',
  with: {
    // suffix here! which will be added for each tags
    'docker-flavor': |||
      latest=false
      suffix=%s
    ||| % suffix,
    'docker-tags': |||
       type=sha,event=branch
       type=ref,event=pr
      |||,
    'artifact-name': docker_artifact_name + suffix,
    'repository-name': docker_repository_name,
    file: 'Dockerfile',
    // see the Dockerfile, this is the name of a variant
    target: target,
    // TODO: why false here?
    'enable-tests': false,
  },
};

local right_ref_and_right_event =
  "github.ref == 'refs/heads/develop' || (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))";

local push_docker_job(artifact_name, repository_name) = {
  uses: './.github/workflows/push-docker.yml',
  'if': right_ref_and_right_event,
  secrets: 'inherit',
  with: {
    'artifact-name': artifact_name,
    'repository-name': repository_name,
    'dry-run': false,
  },
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

// Do not run all the tests if you only modify the README.md or other docs
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
  // These extra permissions are needed by some of the jobs
  // (e.g. build-test-javascript)
  permissions: gha.write_permissions,
  jobs: {
    'test-semgrep-core': test_semgrep_core_job,
    'test-osemgrep': test_osemgrep_job,
    // Pysemgrep tests that require check-semgrep-pro
    'test-cli': test_cli_job,
    // Pysemgrep tests that require build-test-core-x86
    'test-qa': test_qa_job,
    'benchmarks-lite': benchmarks_lite_job,
    // These 'benchmarks-full' use rule-ids and paths with difference prefixes
    // than the Pro benchmarks (OSS.perf and OSS/perf) thus causing problems with
    // masking. We quick-fix this by disabling 'benchmarks-full', given that we
    // have the Argo-based semgrep-compare benchmarks as a safeguard. It may be
    // more productive to integrate these benchmarks into the Pro's workflow.
    // 'benchmarks-full': benchmarks_full_job,
    // Docker stuff
    'build-test-docker': build_test_docker_job,
    'push-docker-returntocorp':
       push_docker_job(docker_artifact_name, docker_repository_name) +
       { needs: [ 'build-test-docker' ] },
    'build-test-docker-nonroot':
      build_test_docker_other_target_job("-nonroot", "nonroot"),
    // No need to push those variant docker images. This is useful in
    // release.jsonnet, but not so much here.
    // old:
    //  'push-docker-semgrep': push_docker_job(..., 'semgrep/semgrep') + { ... }
    //  'push-docker-nonroot-returntocorp': ...
    'build-test-docker-performance-tests':
      build_test_docker_other_target_job("-performance-tests", "performance-tests"),
    //'push-docker-performance-tests': ...
    // trigger argo workflows
    'trigger-semgrep-comparison-argo': trigger_semgrep_comparison_argo,
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
    'build-test-windows-x86': {
      uses: './.github/workflows/build-test-windows-x86.yml',
      secrets: 'inherit',
    },
  },
  export:: {
    // Used in semgrep-proprietary.
    snapshot_update_pr_steps: snapshot_update_pr_steps
  }
}
