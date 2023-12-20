// This workflow performs additional tasks on a PR when someone
// (or start-release.jsonnet) push to a vXXX branch. Those tasks are to
//  - push a new canary docker image
//  - create release artifacts with the Linux and MacOS semgrep packages
//  - update PyPy
//  - update homebrew

// TODO: remove the \n

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------

// to be used by the workflow
local release_input = {
      inputs: {
        'dry-run': {
          description: |||
             Run the release in dry-run mode, e.g., without changing external
             state (like pushing to PyPI/Docker)
           |||,
          required: true,
          type: 'boolean',
	  //TODO? when in workflow_call there was no default set
          default: false,
        },
      }};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------
local inputs_job = {
      name: 'Evaluate Inputs',
      'runs-on': 'ubuntu-22.04',
      outputs: {
        'dry-run': '${{steps.dry-run.outputs.dry-run}}',
      },
      steps: [
        {
          name: 'Evaluate Dry Run',
          id: 'dry-run',
          run: 'if [[ "${{ inputs.dry-run }}" == "true" ]] || [[ "${{ github.ref_name }}" == *test* ]]; then\n  echo "dry-run=true" >> $GITHUB_OUTPUT\n  echo "Setting dry-run to TRUE"\nelse\n  echo "dry-run=false" >> $GITHUB_OUTPUT\n  echo "Setting dry-run to FALSE"\nfi\n',
        },
      ],
};

// ----------------------------------------------------------------------------
// Docker jobs
// ----------------------------------------------------------------------------

local build_test_docker_job = {
      uses: './.github/workflows/build-test-docker.yaml',
      secrets: 'inherit',
      needs: [
        'inputs',
      ],
      with: {
        'docker-flavor': "# don't add a \"latest\" tag (we'll promote \"canary\" to \"latest\" after testing)\nlatest=false\n",
        'docker-tags': '# tag image with "canary"\ntype=raw,value=canary\n# tag image with full version (ex. "1.2.3")\ntype=semver,pattern={{version}}\n# tag image with major.minor (ex. "1.2")\ntype=semver,pattern={{major}}.{{minor}}\n',
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
        'inputs',
        'build-test-docker',
      ],
      with: {
        'docker-flavor': "# suffix all tags with \"-nonroot\"\nsuffix=-nonroot\n# don't add a \"latest-nonroot\" tag (we'll promote \"canary-nonroot\" to \"latest-nonroot\" after testing)\nlatest=false\n",
        'docker-tags': '# tag image with "canary-nonroot"\ntype=raw,value=canary\n# tag image with full version (ex. "1.2.3-nonroot")\ntype=semver,pattern={{version}}\n# tag image with major.minor version (ex. "1.2-nonroot")\ntype=semver,pattern={{major}}.{{minor}}\n',
        'repository-name': 'returntocorp/semgrep',
        'artifact-name': 'image-release-nonroot',
        file: 'Dockerfile',
        target: 'nonroot',
        'enable-tests': false,
      },
};

local push_docker_job = {
      needs: [
        'wait-for-build-test',
        'inputs',
      ],
      uses: './.github/workflows/push-docker.yaml',
      secrets: 'inherit',
      with: {
        'artifact-name': 'image-release',
        'repository-name': 'returntocorp/semgrep',
        'dry-run': "${{ needs.inputs.outputs.dry-run == 'true' }}",
      },
};

local push_docker_nonroot_job = {
      needs: [
        'wait-for-build-test',
        'inputs',
      ],
      uses: './.github/workflows/push-docker.yaml',
      secrets: 'inherit',
      with: {
        'artifact-name': 'image-release-nonroot',
        'repository-name': 'returntocorp/semgrep',
        'dry-run': "${{ needs.inputs.outputs.dry-run == 'true' }}",
      },
};

// ----------------------------------------------------------------------------
// Pypy jobs
// ----------------------------------------------------------------------------

local park_pypi_packages_job = {
      name: 'Park PyPI package names',
      'runs-on': 'ubuntu-latest',
      needs: [
        'inputs',
      ],
      'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
      defaults: {
        run: {
          'working-directory': 'cli/',
        },
      },
      steps: [
        {
          uses: 'actions/checkout@v3',
        },
        {
          uses: 'actions/setup-python@v4',
          with: {
            'python-version': '3.10',
            cache: 'pipenv',
          },
        },
        {
          run: 'sudo python3 -m pip install pipenv==2022.6.7',
        },
        {
          name: 'Install dependencies',
          run: 'pipenv install --dev',
        },
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
};

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
          run: 'unzip ./manylinux-aarch64-wheel/dist.zip "*.whl"',
        },
        {
          name: 'Unzip OSX x86 Wheel',
          run: 'unzip ./osx-x86-wheel/dist.zip "*.whl"',
        },
        {
          name: 'Unzip OSX ARM64 Wheel',
          run: 'unzip ./osx-arm64-wheel/dist.zip "*.whl"',
        },
        {
          name: 'Publish to Pypi',
          uses: 'pypa/gh-action-pypi-publish@release/v1',
          'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
          with: {
            user: '__token__',
            password: '${{ secrets.pypi_upload_token }}',
            skip_existing: true,
          },
        },
      ],
};

// ----------------------------------------------------------------------------
// Github jobs
// ----------------------------------------------------------------------------

local create_release_job = {
      name: 'Create the Github Release',
      'runs-on': 'ubuntu-latest',
      needs: [
        'wait-for-build-test',
        'inputs',
      ],
      'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
      steps: [
        {
          name: 'Get the version',
          id: 'get-version',
          run: 'echo "VERSION=${GITHUB_REF/refs\\/tags\\//}" >> $GITHUB_OUTPUT',
        },
        {
          name: 'Wait for Draft Release if not Ready',
          id: 'wait-draft-release',
          env: {
            GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
          },
          run: 'while ! gh release --repo returntocorp/semgrep list -L 5 | grep -q "${{ steps.get-version.outputs.VERSION }}"; do\n  echo "release not yet ready, sleeping for 5 seconds"\n  sleep 5\ndone\n',
        },
        {
          name: 'Publish Release',
          id: 'publish_release',
          env: {
            GITHUB_TOKEN: '${{ secrets.GITHUB_TOKEN }}',
          },
          run: 'gh release --repo returntocorp/semgrep edit ${{ steps.get-version.outputs.VERSION }} --draft=false',
        },
      ],
};

local create_release_interfaces_job = {
      name: 'Create the Github Release on Semgrep Interfaces',
      'runs-on': 'ubuntu-latest',
      'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
      needs: [
        'wait-for-build-test',
        'inputs',
      ],
      steps: [
        {
          name: 'Get the version',
          id: 'get-version',
          run: 'echo "VERSION=${GITHUB_REF/refs\\/tags\\//}" >> $GITHUB_OUTPUT',
        },
        {
          name: 'Get JWT for semgrep-ci GitHub App',
          id: 'jwt',
          uses: 'docker://public.ecr.aws/y9k7q4m1/devops/cicd:latest',
          env: {
            EXPIRATION: 600,
            ISSUER: '${{ secrets.SEMGREP_CI_APP_ID }}',
            PRIVATE_KEY: '${{ secrets.SEMGREP_CI_APP_KEY }}',
          },
        },
        {
          name: 'Get token for semgrep-ci GitHub App',
          id: 'token',
          run: 'TOKEN="$(curl -X POST \\\n-H "Authorization: Bearer ${{ steps.jwt.outputs.jwt }}" \\\n-H "Accept: application/vnd.github.v3+json" \\\n"https://api.github.com/app/installations/${{ secrets.SEMGREP_CI_APP_INSTALLATION_ID }}/access_tokens" | \\\njq -r .token)"\necho "::add-mask::$TOKEN"\necho "token=$TOKEN" >> $GITHUB_OUTPUT\n',
        },
        {
          name: 'Checkout',
          uses: 'actions/checkout@v3',
          with: {
            submodules: true,
            token: '${{ steps.token.outputs.token }}',
          },
        },
        {
          name: 'Upload Schema Files',
          id: 'upload-semgrep-schema-files',
          env: {
            GITHUB_TOKEN: '${{ steps.token.outputs.token }}',
          },
          run: 'gh release --repo returntocorp/semgrep-interfaces upload ${{ steps.get-version.outputs.VERSION }} cli/src/semgrep/semgrep_interfaces/rule_schema_v1.yaml',
        },
        {
          name: 'Publish Release Semgrep Interfaces',
          id: 'publish_release_semgrep_interfaces',
          env: {
            GITHUB_TOKEN: '${{ steps.token.outputs.token }}',
          },
          run: 'gh release --repo returntocorp/semgrep-interfaces edit ${{ steps.get-version.outputs.VERSION }} --draft=false',
        },
      ],
};

// ----------------------------------------------------------------------------
// Homebrew jobs
// ----------------------------------------------------------------------------
// see also nightly.jsonnet

local sleep_before_homebrew_job = {
      name: 'Sleep 10 min before releasing to homebrew',
      needs: [
        'inputs',
        'upload-wheels',
      ],
      'runs-on': 'ubuntu-latest',
      steps: [
        {
          name: 'Sleep 10 min',
          'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
          run: 'sleep 10m',
        },
      ],
};

local homebrew_core_pr_job = {
      name: 'Update on Homebrew-Core',
      needs: [
        'inputs',
        'sleep-before-homebrew',
      ],
      'runs-on': 'macos-12',
      steps: [
        {
          name: 'Get the version',
          id: 'get-version',
          run: 'TAG=${GITHUB_REF/refs\\/tags\\//}\nif [ "${{ needs.inputs.outputs.dry-run }}" = "true" ]; then\n  TAG=v99.99.99\nfi\necho "Using TAG=${TAG}"\necho "TAG=${TAG}" >> $GITHUB_OUTPUT\necho "Using VERSION=${TAG#v}"\necho "VERSION=${TAG#v}" >> $GITHUB_OUTPUT\n',
        },
        {
          uses: 'actions/setup-python@v4',
          id: 'python-setup',
          with: {
            'python-version': '3.10',
          },
        },
        {
          name: 'Brew update',
          run: 'brew update',
        },
        {
          name: 'Dry Run Brew PR',
          env: {
            HOMEBREW_GITHUB_API_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
          },
          'if': "${{ contains(github.ref, '-test-release') || needs.inputs.outputs.dry-run == 'true' }}",
          run: 'brew bump-formula-pr --force --no-audit --no-browse --write-only \\\n  --message="semgrep 99.99.99" \\\n  --tag="v99.99.99" --revision="${GITHUB_SHA}" semgrep --python-exclude-packages semgrep\n',
        },
        {
          name: 'Open Brew PR',
          'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
          env: {
            HOMEBREW_GITHUB_API_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
          },
          run: 'brew bump-formula-pr --force --no-audit --no-browse --write-only \\\n  --message="semgrep ${{ steps.get-version.outputs.VERSION }}" \\\n  --tag="${{ steps.get-version.outputs.TAG }}" semgrep\n',
        },
        {
          name: 'Prepare Branch',
          env: {
            GITHUB_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
            R2C_HOMEBREW_CORE_FORK_HTTPS_URL: 'https://github.com/semgrep-release/homebrew-core.git',
          },
          run: 'cd "$(brew --repository)/Library/Taps/homebrew/homebrew-core"\ngit status\ngit diff\ngit config user.name ${{ github.actor }}\ngit config user.email ${{ github.actor }}@users.noreply.github.com\ngh auth setup-git\ngit remote add r2c "${R2C_HOMEBREW_CORE_FORK_HTTPS_URL}"\ngit checkout -b bump-semgrep-${{ steps.get-version.outputs.VERSION }}\ngit add Formula/s/semgrep.rb\ngit commit -m "semgrep ${{ steps.get-version.outputs.VERSION }}"\n',
        },
        {
          name: 'Push Branch to Fork',
          env: {
            GITHUB_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
          },
          'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
          run: 'cd "$(brew --repository)/Library/Taps/homebrew/homebrew-core"\ngit push --set-upstream r2c --force "bump-semgrep-${{ steps.get-version.outputs.VERSION }}"\n',
        },
        {
          name: 'Push to Fork',
          env: {
            GITHUB_TOKEN: '${{ secrets.SEMGREP_HOMEBREW_RELEASE_PAT }}',
            R2C_HOMEBREW_CORE_OWNER: 'semgrep-release',
          },
          'if': "${{ !contains(github.ref, '-test-release') && needs.inputs.outputs.dry-run != 'true' }}",
          run: 'gh pr create --repo homebrew/homebrew-core \\\n  --base master --head "${R2C_HOMEBREW_CORE_OWNER}:bump-semgrep-${{ steps.get-version.outputs.VERSION }}" \\\n  --title="semgrep ${{ steps.get-version.outputs.VERSION }}" \\\n  --body "Bump semgrep to version ${{ steps.get-version.outputs.VERSION }}"\n',
        },
      ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'release',
  on: {
    // this workflow can be triggered manually, via a call (see nightly.sonnet)
    // or when something (human or start-release.jsonnet) push to a vxxx branch.
    workflow_dispatch: release_input,
    workflow_call: release_input,
    push: {
      branches: [
        '**-test-release',
      ],
      tags: [
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
    'push-docker': push_docker_job,
    'push-docker-nonroot': push_docker_nonroot_job,
    'upload-wheels': upload_wheels_job,
    'create-release': create_release_job,
    'create-release-interfaces': create_release_interfaces_job,
    'sleep-before-homebrew': sleep_before_homebrew_job,
    'homebrew-core-pr': homebrew_core_pr_job,
  },
}
