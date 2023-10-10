// This workflow builds and tests the semgrep-core binary for macOS arm64
// and generates the arm64-wheel for pypi.
// coupling: if you modify this file, modify also build-test-osx-x86.jsonnet

local osx_x86 = import 'build-test-osx-x86.jsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------
local runs_on = [
  'self-hosted',
  'macOS',
  'ARM64',
  'ghcr.io/cirruslabs/macos-monterey-xcode:latest',
];

local setup_runner_step = {
  name: 'Setup runner directory',
  run: |||
    sudo mkdir /Users/runner
    sudo chown admin:staff /Users/runner
    sudo chmod 750 /Users/runner
  |||,
};

// could be moved to actions.libsonnet
local setup_python_step = {
  uses: 'actions/setup-python@v4',
  with: {
    'python-version': '3.11',
  },
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// alt: we could factorize more with build-test-osx-x86.jsonnet by making
// the xxx_job functions, but let's copy paste a bit for now.
local artifact_name = 'semgrep-osx-arm64-${{ github.sha }}';
local wheel_name = 'osx-arm64-wheel';

local build_core_job = {
  'runs-on': runs_on,
  env: {
    OPAM_SWITCH_NAME: semgrep.opam_switch,
  },
  steps: [
    setup_runner_step,
    setup_python_step,
    actions.checkout_with_submodules(),
    osx_x86.export.cache.cache_opam_step,
    {
      name: 'Install dependencies',
      run: './scripts/osx-setup-for-release.sh "${{ env.OPAM_SWITCH_NAME }}"\n',
    },
    {
      name: 'Compile semgrep',
      run: |||
        opam exec -- make core
        mkdir -p artifacts
        cp ./bin/semgrep-core artifacts
        zip -r artifacts.zip artifacts
      |||,
    },
    {
      uses: 'actions/upload-artifact@v3',
      with: {
        path: 'artifacts.zip',
        name: artifact_name,
      },
    },
  ],
};

local build_wheels_job = {
  'runs-on': runs_on,
  needs: [
    'build-core',
  ],
  steps: [
    setup_runner_step,
    setup_python_step,
    actions.checkout_with_submodules(),
    {
      uses: 'actions/download-artifact@v3',
      with: {
        name: artifact_name,
      },
    },
    // the --plat-name is macosx_11_0_arm64 here!
    {
      run: |||
        unzip artifacts.zip
        cp artifacts/semgrep-core cli/src/semgrep/bin
        ./scripts/build-wheels.sh --plat-name macosx_11_0_arm64
      |||,
    },
    {
      uses: 'actions/upload-artifact@v3',
      with: {
        path: 'cli/dist.zip',
        name: wheel_name,
      },
    },
  ],
};

local test_wheels_job = {
  'runs-on': runs_on,
  needs: [
    'build-wheels',
  ],
  steps: [
    setup_runner_step,
    setup_python_step,
    {
      uses: 'actions/download-artifact@v1',
      with: {
        name: wheel_name,
      },
    },
    {
      run: 'unzip ./osx-arm64-wheel/dist.zip',
    },
    {
      name: 'install package',
      run: 'pip3 install dist/*.whl',
    },
  ] + osx_x86.export.test_semgrep_steps,
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-osx-arm64',
  on: {
    workflow_dispatch: osx_x86.export.cache.use_cache_inputs(required=true),
    workflow_call: osx_x86.export.cache.use_cache_inputs(required=false),
  },
  jobs: {
    'build-core': build_core_job,
    'build-wheels': build_wheels_job,
    'test-wheels': test_wheels_job,
  },
}
