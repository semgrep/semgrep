// This workflow builds and tests the semgrep-core binary for macOS arm64
// and generates the arm64-wheel for pypi.
// coupling: if you modify this file, modify also build-test-osx-x86.jsonnet

local osx_x86 = import 'build-test-osx-x86.jsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

local wheel_name = 'osx-arm64-wheel';

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
    sudo mkdir -p /Users/runner
    sudo chown admin:staff /Users/runner
    sudo chmod 750 /Users/runner
  |||,
};

// Our self-hosted runner do not come with python pre-installed.
//
// Note that we can't reuse actions.setup_python because it comes with the
// cache: 'pipenv' which then trigger failures when we don't checkout any code
// and there's no code with a Pipfile.lock
local setup_python_step =  {
  uses: 'actions/setup-python@v4',
  with: {
    'python-version': '3.11',
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
  steps: [
    setup_runner_step,
    setup_python_step,
    actions.checkout_with_submodules(),
    // TODO: like for osx-x86, we should use opam.lock
    semgrep.cache_opam.step(
       key=semgrep.opam_switch + "-${{hashFiles('semgrep.opam')}}")
     + semgrep.cache_opam.if_cache_inputs,
    // exactly the same than in build-test-oxs-x86.jsonnet
    {
      name: 'Install dependencies',
      run: './scripts/osx-setup-for-release.sh "%s"' % semgrep.opam_switch,
    },
    {
      name: 'Compile semgrep',
      run: "opam exec -- make core",
    },
    actions.make_artifact_step("./bin/semgrep-core"),
    actions.upload_artifact_step(artifact_name),
    {
      name: 'Test semgrep-core',
      run: 'opam exec -- make core-test',
    }
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
    // needed for ./script/build-wheels.sh below
    actions.checkout_with_submodules(),
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
    setup_runner_step,
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
  on: {
    workflow_dispatch: semgrep.cache_opam.inputs(required=true),
    workflow_call: semgrep.cache_opam.inputs(required=false),
  },
  jobs: {
    'build-core': build_core_job,
    'build-wheels': build_wheels_job,
    'test-wheels': test_wheels_job,
  },
}
