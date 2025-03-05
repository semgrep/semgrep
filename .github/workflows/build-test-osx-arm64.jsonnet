// This workflow builds and tests the semgrep-core binary for macOS arm64
// and generates the arm64-wheel for pypi.
// coupling: if you modify this file, modify also build-test-osx-x86.jsonnet

local osx_x86 = import 'build-test-osx-x86.jsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local gha = import 'libs/gha.libsonnet';

local wheel_name = 'osx-arm64-wheel';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------
local runs_on = 'macos-latest';

// Our self-hosted runner do not come with python pre-installed.
//
// Note that we can't reuse actions.setup_python because it comes with the
// cache: 'pipenv' which then trigger failures when we don't checkout any code
// and there's no code with a Pipfile.lock
local setup_python_step =  {
  uses: 'actions/setup-python@v5',
  with: {
    'python-version': semgrep.python_version,
  }
};


// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// alt: we could factorize more with build-test-osx-x86.jsonnet by making
// the xxx_job functions, but let's copy paste a bit for now.
local artifact_name = 'semgrep-osx-arm64-${{ github.sha }}';

local build_core_job = {
  'runs-on': runs_on,
  steps: actions.checkout_with_submodules() +
  semgrep.build_test_steps() +
  [
    actions.make_artifact_step("./bin/semgrep-core"),
    actions.upload_artifact_step(artifact_name),
  ],
};

local build_wheels_job = {
  'runs-on': runs_on,
  needs: [
    'build-core',
  ],
  steps: actions.checkout_with_submodules() + [
    setup_python_step,
    // needed for ./script/build-wheels.sh below
    actions.download_artifact_step(artifact_name),
    // the --plat-name is macosx_11_0_arm64 here!
    {
      run: |||
        tar xvfz artifacts.tgz
        cp artifacts/semgrep-core cli/src/semgrep/bin
        ./scripts/build-wheels.sh --plat-name macosx_11_0_arm64
      |||,
    },
    {
      uses: 'actions/upload-artifact@v4',
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
    setup_python_step,
    actions.download_artifact_step(wheel_name),
    {
      run: 'unzip dist.zip',
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
  on: gha.on_dispatch_or_call,
  jobs: {
    'build-core': build_core_job,
    'build-wheels': build_wheels_job,
    'test-wheels': test_wheels_job,
  },
}
