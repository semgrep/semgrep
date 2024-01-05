// This workflow performs additional tasks on a PR when someone
// (or start-release.jsonnet) pushes to a vXXX branch. Those tasks are to
//  - push a new :canary docker image. We used to push to :latest, but it
//    is safer to first push to :canary and a few days later to promote
//    :canary to :latest (see promote-canary-to-latest.yml)
//  - create release artifacts on Github. We now just release the source
//    of Semgrep in https://github.com/semgrep/semgrep/releases
//    We used to release Linux and MacOS binaries, but we prefer now
//    users to install Semgrep via Docker, Pypi, or Homebrew.
//  - prepare and upload to PyPi a new semgrep package
//    see https://pypi.org/project/semgrep/
//  - make a PR for Homebrew's semgrep formula to update to the latest semgrep

local semgrep = import 'libs/semgrep.libsonnet';
local actions = import 'libs/actions.libsonnet';
local release_homebrew = import 'release-homebrew.jsonnet';

// ----------------------------------------------------------------------------
// Constants
// ----------------------------------------------------------------------------

local version = "${{ steps.get-version.outputs.VERSION }}";

// this actually produces the tag (e.g., "v1.55.1", and not "1.55.1")
local get_version_step = {
  name: 'Get the version',
  id: 'get-version',
  run: 'echo "VERSION=${GITHUB_REF/refs\/tags\//}" >> $GITHUB_OUTPUT',
};

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------

local release_inputs = {
  inputs: {
    'dry-run': {
      type: 'boolean',
      description: |||
        Run the release in dry-run mode, e.g., without changing external
        state (like pushing to PyPI/Docker/Homebrew)
      |||,
      required: true,
    },
  },
};

local unless_dry_run = {
  if: "${{ ! inputs.dry-run }}"
};

// ----------------------------------------------------------------------------
// Docker jobs
// ----------------------------------------------------------------------------

local build_test_docker_job = {
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  with: {
    // don't add a "latest" tag (we'll promote "canary" to "latest" after
    // testing)
    'docker-flavor': 'latest=false',
    // TODO: those are comments for the docker-tags below. Not sure
    // why but if we put those comments directly in the ||| |||
    // then the job does not work. It was working though when
    // we were adding those comments in the .yml
    //
    // # tag image with "canary"
    // type=raw,value=canary
    // # tag image with full version (ex. "1.2.3")
    // type=semver,pattern={{version}}
    // # tag image with major.minor (ex. "1.2")
    // type=semver,pattern={{major}}.{{minor}}
    'docker-tags': |||
      type=raw,value=canary
      type=semver,pattern={{version}}
      type=semver,pattern={{major}}.{{minor}}
    |||,
    'repository-name': 'returntocorp/semgrep',
    'artifact-name': 'image-release',
    file: 'Dockerfile',
    target: 'semgrep-cli',
    'enable-tests': true,
  },
};

local build_test_docker_nonroot_job = {
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  needs: [
    // We want to run build-test-docker-nonroot *after* build-test-docker
    // so that it reuses the warmed-up docker cache.
    'build-test-docker',
  ],
  with: {
    // # suffix all tags with "-nonroot"
    // suffix=-nonroot
    // # don't add a "latest-nonroot" tag (we'll promote "canary-nonroot" to
    // "latest-nonroot" after testing)
    // latest=false
    'docker-flavor': |||
      suffix=-nonroot
      latest=false
    |||,
    // # tag image with "canary-nonroot"
    // type=raw,value=canary
    // # tag image with full version (ex. "1.2.3-nonroot")
    // type=semver,pattern={{version}}
    // # tag image with major.minor version (ex. "1.2-nonroot")
    // type=semver,pattern={{major}}.{{minor}}
    'docker-tags': |||
      type=raw,value=canary
      type=semver,pattern={{version}}
      type=semver,pattern={{major}}.{{minor}}
    |||,
    'repository-name': 'returntocorp/semgrep',
    'artifact-name': 'image-release-nonroot',
    file: 'Dockerfile',
    target: 'nonroot',
    'enable-tests': false,
  },
};

local push_docker_job(artifact_name) = {
  needs: [
    'wait-for-build-test',
  ],
  uses: './.github/workflows/push-docker.yaml',
  secrets: 'inherit',
  with: {
    'artifact-name': artifact_name,
    'repository-name': 'returntocorp/semgrep',
    'dry-run': "${{ inputs.dry-run == 'true' }}",
  },
};

// ----------------------------------------------------------------------------
// Pypy jobs
// ----------------------------------------------------------------------------

local park_pypi_packages_job = {
  'runs-on': 'ubuntu-latest',
  defaults: {
    run: {
      'working-directory': 'cli/',
    },
  },
  steps: [
    {
      uses: 'actions/checkout@v3',
    },
    actions.setup_python('3.10'),
    {
      run: 'sudo python3 -m pip install pipenv==2022.6.7',
    },
    {
      run: 'pipenv install --dev',
    },
    // There are no semgrep-core here, just the Python code.
    // The wheels are separately added to the pypi package
    // in the upload-wheels job below.
    {
      name: 'Build parked packages',
      run: 'pipenv run python setup.py park',
    },
    {
      name: 'Publish to Pypi',
      uses: 'pypa/gh-action-pypi-publish@release/v1',
      with: {
        user: '__token__',
        password: '${{ secrets.pypi_upload_token }}',
        skip_existing: true,
        packages_dir: 'cli/dist/',
      },
    },
    {
      name: 'Publish to test Pypi',
      uses: 'pypa/gh-action-pypi-publish@release/v1',
      with: {
        repository_url: 'https://test.pypi.org/legacy/',
        user: '__token__',
        password: '${{ secrets.test_pypi_upload_token }}',
        skip_existing: true,
        packages_dir: 'cli/dist/',
      },
    },
  ],
} + unless_dry_run;

local download_step(name) = {
  name: 'Download %s' % name,
  uses: 'actions/download-artifact@v3',
  with: {
    name: name,
    path: name,
  },
};

local upload_wheels_job = {
  name: 'Upload Wheels to PyPI',
  'runs-on': 'ubuntu-latest',
  needs: [
    'wait-for-build-test',
  ],
  steps: [
    download_step('manylinux-x86-wheel'),
    download_step('manylinux-aarch64-wheel'),
    download_step('osx-x86-wheel'),
    download_step('osx-arm64-wheel'),
    {
      run: |||
       unzip ./manylinux-x86-wheel/dist.zip
       unzip ./manylinux-aarch64-wheel/dist.zip "*.whl"
       unzip ./osx-x86-wheel/dist.zip "*.whl"
       unzip ./osx-arm64-wheel/dist.zip "*.whl"
     |||,
    },
    {
      name: 'Publish to Pypi',
      uses: 'pypa/gh-action-pypi-publish@release/v1',
      with: {
        user: '__token__',
        password: '${{ secrets.pypi_upload_token }}',
        skip_existing: true,
      },
    } + unless_dry_run,
  ],
};

// ----------------------------------------------------------------------------
// Github jobs
// ----------------------------------------------------------------------------

local create_release_job = {
  'runs-on': 'ubuntu-latest',
  needs: [
    'wait-for-build-test',
  ],
  steps: [
    get_version_step,
    // wait for the draft release since these may not be ready after the refactor
    // of the start-release.
    {
      name: 'Wait for Draft Release if not Ready',
      id: 'wait-draft-release',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
      run: |||
        while ! gh release --repo returntocorp/semgrep list -L 5 | grep -q "%s"; do
          echo "release not yet ready, sleeping for 5 seconds"
          sleep 5
        done
      ||| % version,
    },
    {
      name: 'Publish Release',
      id: 'publish_release',
      env: {
        GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
      },
      run: 'gh release --repo returntocorp/semgrep edit %s --draft=false' % version,
    },
  ],
} + unless_dry_run;

local create_release_interfaces_job = {
  'runs-on': 'ubuntu-latest',
  needs: [
    'wait-for-build-test',
  ],
  steps: semgrep.github_bot.get_token_steps + [
    get_version_step,
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
        token: semgrep.github_bot.token_ref,
      },
    },
    {
      name: 'Upload Schema Files',
      id: 'upload-semgrep-schema-files',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: 'gh release --repo returntocorp/semgrep-interfaces upload %s cli/src/semgrep/semgrep_interfaces/rule_schema_v1.yaml' % version,
    },
    {
      name: 'Publish Release Semgrep Interfaces',
      id: 'publish_release_semgrep_interfaces',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: 'gh release --repo returntocorp/semgrep-interfaces edit %s --draft=false' % version,
    },
  ],
} + unless_dry_run;

// ----------------------------------------------------------------------------
// Homebrew jobs
// ----------------------------------------------------------------------------

local sleep_before_homebrew_job = {
  // Need to wait for Pypi to propagate information so that
  // 'brew bump-formula-pr' in release_homebrew.jsonnet can access
  // information about the latest semgrep from Pypi
  // TODO: is this still needed? We used to rely on a pipgrip thing,
  // but it's not the case anymore, so maybe we can just sleep 1m
  needs: [
    'upload-wheels',
  ],
  'runs-on': 'ubuntu-latest',
  steps: [
    {
      run: 'sleep 10m',
    } + unless_dry_run,
  ],
};

local homebrew_core_pr_job_base =
  release_homebrew.export.homebrew_core_pr(version);

local homebrew_core_pr_job =
 homebrew_core_pr_job_base + {
  // Needs to run after Pypi released so brew can update Pypi dependency hashes
  needs: [
    'sleep-before-homebrew',
  ],
  steps: [
    {
      name: 'Get the version',
      id: 'get-version',
      run: |||
        TAG=${GITHUB_REF/refs\/tags\//}
        if [ "${{ inputs.dry-run }}" = "true" ]; then
          TAG=v99.99.99
        fi
        echo "VERSION=${TAG#v}" >> $GITHUB_OUTPUT
      |||,
    },
  ] + homebrew_core_pr_job_base.steps,
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'release',
  on: {
    // to trigger manually a release, especially in dry-mode
    workflow_dispatch: release_inputs,
    // see nightly.jsonnet
    workflow_call: release_inputs,
    // see start-release.jsonnet which pushes to a vxxx branch
    push: {
      tags: [
        // Push events to matching v*, i.e. v1.0, v20.15.10
        'v*',
      ],
    },
  },
  jobs: {
    'park-pypi-packages': park_pypi_packages_job,
    'build-test-docker': build_test_docker_job,
    'build-test-docker-nonroot': build_test_docker_nonroot_job,
    // TODO? Not sure why we run those jobs here again; tests.jsonnet already
    // runs those jobs on the release PR. Not sure also why then we don't run
    // build-test-javascript like in tests.jsonnet.
    'build-test-core-x86': {
      uses: './.github/workflows/build-test-core-x86.yml',
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
    'build-test-manylinux-x86': {
      needs: [
        'build-test-core-x86',
      ],
      uses: './.github/workflows/build-test-manylinux-x86.yml',
      secrets: 'inherit',
    },
    'build-test-manylinux-aarch64': {
      needs: [
        // we extract aarch64 from our docker image
        'build-test-docker',
      ],
      uses: './.github/workflows/build-test-manylinux-aarch64.yml',
      secrets: 'inherit',
    },
    'wait-for-build-test': {
      name: 'Wait for Build/Test All Platforms',
      'runs-on': 'ubuntu-22.04',
      needs: [
        'build-test-docker',
        'build-test-docker-nonroot',
        'build-test-manylinux-x86',
        'build-test-manylinux-aarch64',
        'build-test-osx-x86',
        'build-test-osx-arm64',
      ],
      steps: [
        {
          name: 'Continue',
          run: 'echo "All Platforms have been built and tested - proceeding!"',
        },
      ],
    },
    'push-docker': push_docker_job('image-release'),
    'push-docker-nonroot': push_docker_job('image-release-nonroot'),
    'upload-wheels': upload_wheels_job,
    'create-release': create_release_job,
    'create-release-interfaces': create_release_interfaces_job,
    'sleep-before-homebrew': sleep_before_homebrew_job,
    'homebrew-core-pr': homebrew_core_pr_job,
  },
}
