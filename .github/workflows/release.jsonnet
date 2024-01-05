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

local get_version_step = {
  name: 'Get the version',
  id: 'get-version',
  run: 'echo "VERSION=${GITHUB_REF/refs\/tags\//}" >> $GITHUB_OUTPUT',
};

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------
// to be used by the workflow
local release_inputs = {
  inputs: {
    'dry-run': {
      description: |||
        Run the release in dry-run mode, e.g., without changing external
        state (like pushing to PyPI/Docker)
      |||,
      required: true,
      type: 'boolean',
    },
  },
};

local unless_dry_run = {
  if: "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}"
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// TODO? delete this intermediate? do we care about those *test* refs below ?
// In any case to test you can always create a branch, make a PR, and
// then trigger the workflow manually with dry-mode on from the GHA action
// dashboard
local inputs_job = {
  'runs-on': 'ubuntu-22.04',
  outputs: {
    'dry-run': '${{steps.dry-run.outputs.dry-run}}',
  },
  steps: [
    {
      name: 'Evaluate Dry Run',
      id: 'dry-run',
      run: |||
        if [[ "${{ inputs.dry-run }}" == "true" ]] || [[ "${{ github.ref_name }}" == *test* ]]; then
          echo "dry-run=true" >> $GITHUB_OUTPUT
          echo "Setting dry-run to TRUE"
        else
          echo "dry-run=false" >> $GITHUB_OUTPUT
          echo "Setting dry-run to FALSE"
        fi
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// Docker jobs
// ----------------------------------------------------------------------------

// TODO: those are comments for the docker-tags below. Not sure
// why but if we put those comments directly in the ||| |||
// then the job does not work.
//
// # tag image with "canary"
// type=raw,value=canary
// # tag image with full version (ex. "1.2.3")
// type=semver,pattern={{version}}
// # tag image with major.minor (ex. "1.2")
// type=semver,pattern={{major}}.{{minor}}

local build_test_docker_job = {
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  needs: [
    'inputs',
  ],
  with: {
    // don't add a "latest" tag (we'll promote "canary" to "latest" after testing)
    'docker-flavor': 'latest=false',
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

// # suffix all tags with "-nonroot"
// suffix=-nonroot
// # don't add a "latest-nonroot" tag (we'll promote "canary-nonroot" to "latest-nonroot" after testing)
// latest=false

// # tag image with "canary-nonroot"
// type=raw,value=canary
// # tag image with full version (ex. "1.2.3-nonroot")
// type=semver,pattern={{version}}
// # tag image with major.minor version (ex. "1.2-nonroot")
// type=semver,pattern={{major}}.{{minor}}

local build_test_docker_nonroot_job = {
  uses: './.github/workflows/build-test-docker.yaml',
  secrets: 'inherit',
  needs: [
    'inputs',
    // We want to run build-test-docker-nonroot *after*
    // build-test-docker so that it reuses the warmed-up
    // docker cache.
    'build-test-docker',
  ],
  with: {
    'docker-flavor': |||
      suffix=-nonroot
      latest=false
    |||,
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
    'inputs',
  ],
  uses: './.github/workflows/push-docker.yaml',
  secrets: 'inherit',
  with: {
    'artifact-name': artifact_name,
    'repository-name': 'returntocorp/semgrep',
    'dry-run': "${{ needs.inputs.outputs.dry-run == 'true' }}",
  },
};

// ----------------------------------------------------------------------------
// Pypy jobs
// ----------------------------------------------------------------------------

local park_pypi_packages_job = {
  'runs-on': 'ubuntu-latest',
  needs: [
    'inputs',
  ],
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
      'if': "${{ !contains(github.ref,'-test-release') }}",
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
      'if': "${{ contains(github.ref,'-test-release') }}",
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

local upload_wheels_job = {
  name: 'Upload Wheels to PyPI',
  'runs-on': 'ubuntu-latest',
  needs: [
    'wait-for-build-test',
    'inputs',
  ],
  steps: [
    {
      name: 'Download Artifact',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'manylinux-x86-wheel',
        path: 'manylinux-x86-wheel',
      },
    },
    {
      name: 'Download aarch64 Artifact',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'manylinux-aarch64-wheel',
        path: 'manylinux-aarch64-wheel',
      },
    },
    {
      name: 'Download OSX x86 Artifact',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'osx-x86-wheel',
        path: 'osx-x86-wheel',
      },
    },
    {
      name: 'Download OSX ARM64 Artifact',
      uses: 'actions/download-artifact@v3',
      with: {
        name: 'osx-arm64-wheel',
        path: 'osx-arm64-wheel',
      },
    },
    {
      name: 'Unzip x86_64 Wheel',
      run: 'unzip ./manylinux-x86-wheel/dist.zip',
    },
    {
      name: 'Unzip aarch64 Wheel',
      // Don't unzip tar.gz because it already exists from ./manylinux-x86-wheel/dist.zip.
      run: 'unzip ./manylinux-aarch64-wheel/dist.zip "*.whl"',
    },
    {
      name: 'Unzip OSX x86 Wheel',
      // Don't unzip tar.gz because it already exists from ./manylinux-x86-wheel/dist.zip.
      run: 'unzip ./osx-x86-wheel/dist.zip "*.whl"',
    },
    {
      name: 'Unzip OSX ARM64 Wheel',
      // Don't unzip tar.gz because it already exists from ./manylinux-x86-wheel/dist.zip.
      run: 'unzip ./osx-arm64-wheel/dist.zip "*.whl"',
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
    'inputs',
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
    'inputs',
  ],
  steps: [
    get_version_step,
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
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
    'inputs',
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
  release_homebrew.export.homebrew_core_pr(version, unless_dry_run);

local homebrew_core_pr_job =
 homebrew_core_pr_job_base + {
  // Needs to run after Pypi released so brew can update Pypi dependency hashes
  needs: [
    'inputs',
    'sleep-before-homebrew',
  ],
  steps: [
    {
      name: 'Get the version',
      id: 'get-version',
      run: |||
        TAG=${GITHUB_REF/refs\/tags\//}
        if [ "${{ needs.inputs.outputs.dry-run }}" = "true" ]; then
          TAG=v99.99.99
        fi
        echo "Using TAG=${TAG}"
        echo "TAG=${TAG}" >> $GITHUB_OUTPUT
        echo "Using VERSION=${TAG#v}"
        echo "VERSION=${TAG#v}" >> $GITHUB_OUTPUT
      |||,
    },
    {
      name: 'Dry Run Brew PR',
      // This step does some brew oddities (setting a fake version, and
      // setting a revision) to allow the brew PR prep to succeed.
      // The `brew bump-formula-pr` does checks to ensure your PR is legit,
      // but we want to do a phony PR (or at least prep it) for Dry Run only
      env: {
        HOMEBREW_GITHUB_API_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
      },
      // this is run only in dry-mode
      if: "${{ contains(github.ref, '-test-release') || needs.inputs.outputs.dry-run == 'true' }}",
      run: |||
        brew bump-formula-pr --force --no-audit --no-browse --write-only \
          --message="semgrep 99.99.99" \
          --tag="v99.99.99" --revision="${GITHUB_SHA}" semgrep --python-exclude-packages semgrep
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
    // this workflow can be triggered manually, via a call (see nightly.jsonnet)
    // or when something (human or start-release.jsonnet) push to a vxxx branch.
    workflow_dispatch: release_inputs,
    workflow_call: release_inputs,
    push: {
      branches: [
        // Sequence of patterns matched against refs/tags
        '**-test-release',
      ],
      tags: [
        // Push events to matching v*, i.e. v1.0, v20.15.10
        'v*',
      ],
    },
  },
  jobs: {
    inputs: inputs_job,
    'park-pypi-packages': park_pypi_packages_job,
    'build-test-docker': build_test_docker_job,
    'build-test-docker-nonroot': build_test_docker_nonroot_job,
    // similar to tests.jsonnet
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
        'build-test-docker',
      ],
      uses: './.github/workflows/build-test-manylinux-aarch64.yml',
      secrets: 'inherit',
    },
    // no build-test-javascript though here because ??? TODO?
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
