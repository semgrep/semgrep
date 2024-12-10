// Release workflow part 2 (part 1 is start-release.jsonnet).
//
// This workflow performs additional tasks on a PR when someone
// (or start-release.jsonnet) pushes to a vXXX branch. Those tasks are to:
//
//  - build and push a new :canary docker image. We used to push to :latest,
//    but it is safer to first push to :canary and a few days later to promote
//    the :canary image to :latest (see promote-canary-to-latest.jsonnet)
//
//  - create release artifacts on Github. We now just release the source
//    of Semgrep in https://github.com/semgrep/semgrep/releases
//    We used to release Linux and MacOS binaries, but we prefer now
//    users to install Semgrep via Docker, Pypi, or Homebrew.
//
//  - prepare and upload to PyPi a new semgrep package
//    see https://pypi.org/project/semgrep/
//
// We used to also automatically create a PR to Homebrew's repo to
// update the Semgrep "formula" to link to the latest semgrep artifacts
// (https://github.com/Homebrew/homebrew-core/blob/master/Formula/s/semgrep.rb)
// As of January 2024, this is now handled instead by the Homebrew's folk with
// an "autobump" mechanism. This is great because this part of the release was
// failing very often because of frequent changes to Homebrew process.
// Note that we still check in CI that the Semgrep "formula" can still be built,
// but we do it in nightly.jsonnet, not during the release.

local semgrep = import 'libs/semgrep.libsonnet';
local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// Constants
// ----------------------------------------------------------------------------

local version = "${{ steps.get-version.outputs.VERSION }}";

// this actually produces the tag (e.g., "v1.55.1", and not "1.55.1")
local get_version_step = {
  name: 'Get the version',
  id: 'get-version',
  // Note the double escape on this single-line command. If this got updated
  // to a multi-line command, i.e., with |||, then we would only need
  // a single backslash to escape.
  run: 'echo "VERSION=${GITHUB_REF/refs\\/tags\\//}" >> $GITHUB_OUTPUT',
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
  'if': "${{ ! inputs.dry-run }}"
};

// This is ugly, but you can't just use "${{ inputs.dry-run }}" because GHA
// will complain at runtime with "the template is not valid, Unexpected
// value ''".
// This is because even though there is 'inputs:' above with a 'type: boolean',
// this workflow can also be triggered by pushing to a 'vXxx' branch
// (see the on: at the end of this file), and in that case inputs.dry-run
// will not be false but Null, which is then casted as an empty string '',
// which can not be passed to the push-docker.yml workflow which expects
// a proper boolean. Hence the || false boolean to normalize to a boolean.
//
// See https://docs.github.com/en/actions/learn-github-actions/expressions
// for more information.
//
// alt: have a preleminary job to normalize the inputs (spencer was doing
// that before), but then it's a bit heavy as all other workflows
// now must depend on this preliminary job and the dry-run expression
// gets more complicated.
// alt: use strings everywhere (but boolean offers a nice checkbox when used
// in workflow_dispatch).
local dry_run = "${{ inputs.dry-run || false }}";

// ----------------------------------------------------------------------------
// Docker jobs (build and then push)
// ----------------------------------------------------------------------------

local build_test_docker_job = {
  uses: './.github/workflows/build-test-docker.yml',
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
  uses: './.github/workflows/build-test-docker.yml',
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

local push_docker_job(artifact_name, repository_name) = {
  needs: [
    'wait-for-build-test',
  ],
  uses: './.github/workflows/push-docker.yml',
  secrets: 'inherit',
  with: {
    'artifact-name': artifact_name,
    'repository-name': repository_name,
    'dry-run': dry_run,
  },
};

// ----------------------------------------------------------------------------
// Pypy jobs
// ----------------------------------------------------------------------------

// Note that we now have a 50GB quota on pypi thx to a request we made in
// Dec 2023: https://github.com/pypi/support/issues/3464
// Indeed around that time we reached our quota because each release was
// taking 170MB and we had released many versions.
// alt: remove old versions, but Bence didn't like it.

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
    actions.setup_python_step('3.10'),
    {
      run: 'sudo python3 -m ' + actions.pipenv_install_step.run,
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
        password: '${{ secrets.PYPI_UPLOAD_TOKEN }}',
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
        password: '${{ secrets.TEST_PYPI_UPLOAD_TOKEN }}',
        skip_existing: true,
        packages_dir: 'cli/dist/',
      },
    },
  ],
} + unless_dry_run;

local download_step(name) = {
  name: 'Download %s' % name,
  uses: 'actions/download-artifact@v4',
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
        password: '${{ secrets.PYPI_UPLOAD_TOKEN }}',
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
        while ! gh release --repo semgrep/semgrep list -L 5 | grep -q "%s"; do
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
      run: 'gh release --repo semgrep/semgrep edit %s --draft=false' % version,
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
      run: 'gh release --repo semgrep/semgrep-interfaces upload %s cli/src/semgrep/semgrep_interfaces/rule_schema_v1.yaml' % version,
    },
    {
      name: 'Publish Release Semgrep Interfaces',
      id: 'publish_release_semgrep_interfaces',
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      run: 'gh release --repo semgrep/semgrep-interfaces edit %s --draft=false' % version,
    },
  ],
} + unless_dry_run;

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'release',
  on: {
    // to trigger manually a release, especially in dryrun mode
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
  // These extra permissions are needed by some of the jobs, e.g. build-test-docker,
  // and create_release_job.
  permissions: gha.write_permissions,
  jobs: {
    'park-pypi-packages': park_pypi_packages_job,
    'build-test-docker': build_test_docker_job,
    'build-test-docker-nonroot': build_test_docker_nonroot_job,
    // TODO? Not sure why we run those jobs here again; tests.jsonnet already
    // runs those jobs on the release PR.
    // TODO?? Not sure also why we don't run build-test-javascript like in
    // tests.jsonnet then
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
    'push-docker-returntocorp': push_docker_job('image-release', 'returntocorp/semgrep'),
    'push-docker-returntocorp-nonroot': push_docker_job('image-release-nonroot', 'returntocorp/semgrep'),
    'push-docker-semgrep': push_docker_job('image-release', 'semgrep/semgrep'),
    'push-docker-semgrep-nonroot': push_docker_job('image-release-nonroot', 'semgrep/semgrep'),
    'upload-wheels': upload_wheels_job,
    'create-release': create_release_job,
    'create-release-interfaces': create_release_interfaces_job,
  },
}
