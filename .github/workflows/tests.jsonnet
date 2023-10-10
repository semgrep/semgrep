// The goals of this workflow are to check that:
// - we can build semgrep-core and semgrep
// - we can build a Docker image as well as Linux and MacOS binaries
// - all our tests (the one in semgrep-core and the one in semgrep-cli) are passing
// - we don't have any perf regressions in our benchmarks

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------
local test_core_job = {
  name: 'test semgrep-core',
  'runs-on': 'ubuntu-22.04',
  container: 'returntocorp/ocaml:alpine-2023-06-16',
  env: {
    HOME: '/root',
  },
  steps: [
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
        'persist-credentials': false,
      },
    },
    {
      name: 'Build semgrep-core',
      run: 'eval $(opam env)\nmake install-deps-ALPINE-for-semgrep-core\nmake install-deps-for-semgrep-core\nmake core\n',
    },
    {
      name: 'Test semgrep-core',
      run: 'eval $(opam env)\nSTART=`date +%s`\nmake core-test\nmake core-test-e2e\nEND=`date +%s`\nTEST_RUN_TIME=$((END-START))\ncurl --fail -L -X POST "https://dashboard.semgrep.dev/api/metric/semgrep.core.test-run-time-seconds.num" -d "$TEST_RUN_TIME"\n',
    },
    {
      name: 'Report Number of Tests Stats',
      'if': "github.ref == 'refs/heads/develop'",
      run: './scripts/report_test_metrics.sh',
    },
    {
      name: 'Publish match performance',
      run: '# This runs a short test suite to track the match performance\n# of semgrep-core over time. The results are pushed to the\n# dashboard at https://dashboard.semgrep.dev/\n#\nopam exec -- make report-perf-matching\n',
    },
  ],
};

local test_osemgrep_job = {
  name: 'test osemgrep',
  'runs-on': 'ubuntu-22.04',
  container: 'returntocorp/ocaml:alpine-2023-06-16',
  env: {
    HOME: '/root',
  },
  steps: [
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
        'persist-credentials': false,
      },
    },
    {
      name: 'Build semgrep-core',
      run: 'eval $(opam env)\nmake install-deps-ALPINE-for-semgrep-core\nmake install-deps-for-semgrep-core\nmake core\n',
    },
    {
      name: 'Install osemgrep',
      run: 'eval $(opam env)\nmake copy-core-for-cli\n',
    },
    {
      name: 'Install Python dependencies',
      run: 'make install-deps-ALPINE-for-pysemgrep\n(cd cli; pipenv install --dev)\n',
    },
    {
      name: 'Run pytest for osemgrep known passing tests',
      'working-directory': 'cli',
      run: 'make osempass\n',
    },
  ],
};

local build_semgrep_js_job = {
  name: 'build semgrep js ocaml for tests',
  'runs-on': 'ubuntu-latest-16-core',
  container: 'returntocorp/ocaml:alpine-2023-06-16',
  env: {
    HOME: '/root',
  },
  steps: [
    {
      name: 'Make checkout speedy',
      run: 'git config --global fetch.parallel 50',
    },
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
      },
    },
    {
      name: 'Set up tree-sitter',
      run: '(cd libs/ocaml-tree-sitter-core && ./configure && ./scripts/install-tree-sitter-lib)\n',
    },
    {
      name: 'Cache git checkout',
      id: 'cache-git',
      uses: 'actions/cache/save@v3',
      with: {
        path: '.',
        key: 'semgrep-with-submodules-and-tree-sitter-${{ github.sha }}',
      },
    },
    {
      name: 'Build semgrep',
      run: 'eval $(opam env)\nmake install-deps-ALPINE-for-semgrep-core\nmake install-deps-for-semgrep-core\nmake build-semgrep-jsoo-debug\n',
    },
    {
      uses: 'actions/upload-artifact@v3',
      with: {
        name: 'semgrep-js-ocaml-test-${{ github.sha }}',
        'retention-days': 1,
        path: '_build/default/js/engine/*.bc.js\n_build/default/js/languages/*/*.bc.js\n',
      },
    },
  ],
};
local test_semgrep_js_job = {
  name: 'test semgrep js',
  needs: [
    'build-semgrep-js-ocaml-test',
  ],
  'runs-on': 'ubuntu-22.04',
  container: 'emscripten/emsdk:3.1.46',
  env: {
    HOME: '/root',
  },
  steps: [
    {
      name: 'Restore git checkout cache',
      id: 'restore-git',
      uses: 'actions/cache/restore@v3',
      with: {
        path: '.',
        key: 'semgrep-with-submodules-and-tree-sitter-${{ github.sha }}',
      },
    },
    {
      name: 'Make checkout speedy',
      'if': "${{ steps.restore-git.outputs.cache-hit != 'true' }}",
      run: 'git config --global fetch.parallel 50',
    },
    {
      uses: 'actions/checkout@v3',
      'if': "${{ steps.restore-git.outputs.cache-hit != 'true' }}",
      with: {
        submodules: true,
      },
    },
    {
      name: 'Set up tree-sitter',
      'if': "${{ steps.restore-git.outputs.cache-hit != 'true' }}",
      run: '(cd libs/ocaml-tree-sitter-core && ./configure && ./scripts/install-tree-sitter-lib)\n',
    },
    {
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'semgrep-js-ocaml-test-${{ github.sha }}',
        path: '_build/default/js',
      },
    },
    {
      uses: 'actions/setup-node@v3',
      with: {
        'node-version': '18',
      },
    },
    {
      name: 'Run semgrep js e2e tests',
      run: 'make -C js test\n',
    },
  ],
};
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
    {
      uses: 'actions/checkout@v3',
      with: {
        'persist-credentials': false,
      },
    },
    {
      name: 'Fetch semgrep-cli submodules',
      run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces',
    },
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '${{ matrix.python }}',
        cache: 'pipenv',
      },
    },
    {
      run: 'pip install pipenv==2022.6.7',
    },
    {
      name: 'Download artifacts',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'ocaml-build-artifacts-release',
      },
    },
    {
      name: 'Install artifacts',
      run: 'tar xf ocaml-build-artifacts.tgz\nsudo cp ocaml-build-artifacts/bin/* /usr/bin\n',
    },
    {
      name: 'Install Python dependencies',
      'working-directory': 'cli',
      run: 'pipenv install --dev',
    },
    {
      name: 'Run pytest',
      'working-directory': 'cli',
      run: '# tests should simulate CI environment iff they need one\nunset CI\nunset "${!GITHUB_@}"\n\npipenv run pytest -n auto -vv --snapshot-update --allow-snapshot-deletion\n',
    },
    {
      name: 'Prepare repo for snapshot commit',
      'if': 'failure()',
      run: "# the commit step that follows will fail to fetch the pfff submodule\n# (perhaps because of the github token's permissions)\n# so we disable recursive fetching\ngit config fetch.recurseSubmodules false\n",
    },
    {
      name: 'Configure git creds for push',
      id: 'configure-creds',
      'if': "failure() && github.event_name == 'pull_request' && (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))",
      run: 'echo "machine github.com" >> ~/.netrc\necho "login ${{ github.repository }}" >> ~/.netrc\necho "password ${{ secrets.GITHUB_TOKEN }}" >> ~/.netrc\n',
    },
    {
      name: 'Commit snapshot updates',
      id: 'snapshot-commit',
      'if': "failure() && github.event_name == 'pull_request' && (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))",
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
      'if': "failure() && github.event_name == 'pull_request' && (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))",
      run: 'rm ~/.netrc',
    },
    {
      name: 'Comment about any snapshot updates',
      'if': "failure() && steps.snapshot-commit.outputs.pushed == 'true'",
      run: 'echo ":camera_flash: The pytest shapshots changed in your PR." >> /tmp/message.txt\necho "Please carefully review these changes and make sure they are intended:" >> /tmp/message.txt\necho >> /tmp/message.txt\necho "1. Review the changes at https://github.com/returntocorp/semgrep/commit/${{ steps.snapshot-commit.outputs.commit_long_sha }}" >> /tmp/message.txt\necho "2. Accept the new snapshots with" >> /tmp/message.txt\necho >> /tmp/message.txt\necho "       git fetch origin && git cherry-pick ${{ steps.snapshot-commit.outputs.commit_sha }} && git push" >> /tmp/message.txt\n\ngh pr comment ${{ github.event.pull_request.number }} --body-file /tmp/message.txt\n',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
    },
  ],
};
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
    {
      uses: 'actions/checkout@v3',
      with: {
        'persist-credentials': false,
      },
    },
    {
      name: 'Fetch semgrep-cli submodules',
      run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces tests/semgrep-rules',
    },
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.11',
        cache: 'pipenv',
      },
    },
    {
      run: 'pip install pipenv==2022.6.7',
    },
    {
      name: 'Download artifacts',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'ocaml-build-artifacts-release',
      },
    },
    {
      name: 'Install artifacts',
      run: 'tar xf ocaml-build-artifacts.tgz\nsudo cp ocaml-build-artifacts/bin/* /usr/bin\n',
    },
    {
      name: 'Install semgrep',
      'working-directory': 'cli',
      run: 'export PATH=/github/home/.local/bin:$PATH\npipenv install --dev\n',
    },
    {
      uses: 'actions/cache@v3',
      with: {
        path: '~/.cache/qa-public-repos',
        key: "qa-public-repos-${{ hashFiles('semgrep/tests/qa/*public_repos*') }}-${{ matrix.split }}",
      },
    },
    {
      run: 'mkdir -p ~/.cache/qa-public-repos\ntouch ~/.cache/qa-public-repos/ok\n',
    },
    {
      name: 'Test semgrep',
      'working-directory': 'cli',
      run: 'export PATH=/github/home/.local/bin:$PATH\npipenv run pytest -n auto -vv --tb=short --splits 4 --group ${{ matrix.split }} tests/qa\n',
      env: {
        QA_TESTS_CACHE_PATH: '~/.cache/qa-public-repos',
      },
    },
  ],
};
local benchmarks_lite_job = {
  'runs-on': 'ubuntu-22.04',
  needs: [
    'build-test-core-x86',
  ],
  steps: [
    {
      uses: 'actions/checkout@v3',
      with: {
        'persist-credentials': false,
      },
    },
    {
      name: 'Fetch semgrep-cli submodules',
      run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces',
    },
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.7',
        cache: 'pipenv',
      },
    },
    {
      run: 'pip install pipenv==2022.6.7',
    },
    {
      name: 'Download artifacts',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'ocaml-build-artifacts-release',
      },
    },
    {
      name: 'Install artifacts',
      run: 'tar xf ocaml-build-artifacts.tgz\nsudo cp ocaml-build-artifacts/bin/* /usr/bin\n',
    },
    {
      name: 'Install cli dependencies',
      'working-directory': 'cli',
      run: 'pipenv install --dev',
    },
    {
      name: 'Test dummy benchmarks on latest',
      'working-directory': 'cli',
      run: 'pipenv run semgrep --version\npipenv run semgrep-core -version\npipenv run python3 ../perf/run-benchmarks --dummy\n',
    },
  ],
};

local benchmarks_full_job = {
  'runs-on': 'ubuntu-22.04',
  needs: [
    'build-test-core-x86',
  ],
  steps: [
    {
      uses: 'actions/checkout@v3',
      with: {
        'persist-credentials': false,
      },
    },
    {
      name: 'Fetch semgrep-cli submodules',
      run: 'git submodule update --init --recursive --recommend-shallow cli/src/semgrep/semgrep_interfaces',
    },
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.7',
        cache: 'pipenv',
      },
    },
    {
      run: 'pip install pipenv==2022.6.7',
    },
    {
      name: 'Download artifacts',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'ocaml-build-artifacts-release',
      },
    },
    {
      name: 'Install artifacts',
      run: 'tar xf ocaml-build-artifacts.tgz\nsudo cp ocaml-build-artifacts/bin/* /usr/bin\n',
    },
    {
      name: 'Install cli dependencies',
      'working-directory': 'cli',
      run: 'pipenv install --dev',
    },
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
local build_test_docker_job = {
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  with: {
    'docker-flavor': 'latest=auto\n',
    'docker-tags': 'type=semver,pattern={{version}}\ntype=semver,pattern={{major}}.{{minor}}\ntype=ref,event=pr\ntype=ref,event=branch\ntype=sha,event=branch\ntype=edge\n',
    'artifact-name': 'image-test',
    'repository-name': '${{ github.repository }}',
    file: 'Dockerfile',
    target: 'semgrep-cli',
    'enable-tests': true,
  },
};

local push_docker_job = {
  needs: [
    'build-test-docker',
  ],
  uses: './.github/workflows/push-docker.yaml',
  'if': "github.ref == 'refs/heads/develop' || (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))",
  secrets: 'inherit',
  with: {
    'artifact-name': 'image-test',
    'repository-name': '${{ github.repository }}',
    'dry-run': false,
  },
};

local build_test_docker_nonroot_job = {
  needs: [
    'build-test-docker',
  ],
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  with: {
    'docker-flavor': 'latest=auto\nsuffix=-nonroot,onlatest=true\n',
    'docker-tags': 'type=semver,pattern={{version}}\ntype=semver,pattern={{major}}.{{minor}}\ntype=ref,event=pr\ntype=ref,event=branch\ntype=sha,event=branch\ntype=edge\n',
    'artifact-name': 'image-test-nonroot',
    'repository-name': '${{ github.repository }}',
    file: 'Dockerfile',
    target: 'nonroot',
    'enable-tests': false,
  },
};

local push_docker_nonroot_job = {
  needs: [
    'build-test-docker-nonroot',
  ],
  uses: './.github/workflows/push-docker.yaml',
  'if': "github.ref == 'refs/heads/develop' || (github.actor != 'dependabot[bot]' && !(github.event.pull_request.head.repo.full_name != github.repository))",
  secrets: 'inherit',
  with: {
    'artifact-name': 'image-test-nonroot',
    'repository-name': '${{ github.repository }}',
    'dry-run': false,
  },
};

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

{
  name: 'tests',
  on: {
    workflow_dispatch: null,
    pull_request: {
      'paths-ignore': [
        '**.md',
      ],
    },
    push: {
      branches: [
        'develop',
      ],
      'paths-ignore': [
        '**.md',
      ],
    },
  },
  jobs: {
    'test-core': test_core_job,
    'test-osemgrep': test_osemgrep_job,
    'build-semgrep-js-ocaml-test': build_semgrep_js_job,
    'test-semgrep-js': test_semgrep_js_job,
    'test-cli': test_cli_job,
    'test-qa': test_qa_job,
    'benchmarks-lite': benchmarks_lite_job,
    'benchmarks-full': benchmarks_full_job,
    'build-test-docker': build_test_docker_job,
    'push-docker': push_docker_job,
    'build-test-docker-nonroot': build_test_docker_nonroot_job,
    'push-docker-nonroot': push_docker_nonroot_job,
    'test-semgrep-pro': test_semgrep_pro_job,
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
      with: {
        'upload-artifacts': "${{ (github.ref == 'refs/heads/develop') || startsWith(github.ref, 'refs/heads/release-') || (!github.event.pull_request.head.repo.fork && contains(github.event.pull_request.labels.*.name, 'publish-js')) }}",
      },
    },
  },
}
