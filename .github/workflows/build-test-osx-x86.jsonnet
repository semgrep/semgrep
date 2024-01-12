// This workflow builds and tests the semgrep-core binary for macOS X86
// and generates the osx-wheel for pypi.

// coupling: if you modify this file, modify also build-test-osx-arm64.jsonnet

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

// See https://github.com/actions/runner-images/blob/main/images/macos/macos-12-Readme.md
// This already comes with Python installed so no need to
// have a setup_python_step like in build-test-osx-arm64.jsonnet
local runs_on = 'macos-12';

// This is reused in build-test-osx-arm64.jsonnet
local test_semgrep_steps = [
  {
    run: 'semgrep --version',
  },
  {
    name: 'e2e semgrep-core test',
    run: "echo '1 == 1' | semgrep -l python -e '$X == $X' -",
  },
  {
    name: 'test dynamically linked libraries are in /usr/lib/',
    shell: 'bash {0}',
    run: |||
      otool -L $(semgrep --dump-engine-path) | tee otool.txt
      if [ $? -ne 0 ]; then
         echo "Failed to list dynamically linked libraries.";
         exit 1;
      fi
      NON_USR_LIB_DYNAMIC_LIBRARIES=$(tail -n +2 otool.txt | grep -v "^\\s*/usr/lib/")
      if [ $? -eq 0 ]; then
         echo "Error: semgrep-core has been dynamically linked against libraries outside /usr/lib:"
         echo $NON_USR_LIB_DYNAMIC_LIBRARIES
         exit 1;
      fi;
    |||,
  },
];

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local artifact_name = 'semgrep-osx-${{ github.sha }}';
local wheel_name = 'osx-x86-wheel';

local build_core_job = {
  'runs-on': runs_on,
  //TODO: could pass it via an argument to cache_opam_step instead of env?
  steps: [
    actions.checkout_with_submodules(),
    // TODO: we should use opam.lock instead of semgrep.opam at some point
    // so any update to our dependencies would automatically trigger a
    // cache miss and generate a fresh ~/.opam.
    semgrep.cache_opam.step(
         key=semgrep.opam_switch + "-${{hashFiles('semgrep.opam'}}")
      + semgrep.cache_opam.if_cache_inputs,
    {
      name: 'Install dependencies',
      run: './scripts/osx-setup-for-release.sh "%s"' % semgrep.opam_switch,
    },
    {
      name: 'Compile semgrep',
      run: 'opam exec -- make core',
    },
    {
      name: 'Make artifact',
      run: |||
        mkdir artifacts
        cp ./bin/semgrep-core artifacts/
        tar czf artifacts.tgz artifacts
      |||,
    },
    {
      uses: 'actions/upload-artifact@v3',
      with: {
        path: 'artifacts.tgz',
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
    actions.checkout_with_submodules(),
    {
      uses: 'actions/download-artifact@v3',
      with: {
        name: artifact_name,
      },
    },
    {
      run: |||
        tar xvfz artifacts.tgz
        cp artifacts/semgrep-core cli/src/semgrep/bin
        ./scripts/build-wheels.sh --plat-name macosx_10_14_x86_64
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
    {
      uses: 'actions/download-artifact@v1',
      with: {
        name: wheel_name,
      },
    },
    {
      run: 'unzip ./osx-x86-wheel/dist.zip',
    },
    {
      name: 'install package',
      run: 'pip3 install dist/*.whl',
    },
  ] + test_semgrep_steps,
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-osx-x86',
  on: {
    workflow_dispatch: semgrep.cache_opam.inputs(required=true),
    workflow_call: semgrep.cache_opam.inputs(required=false),
  },
  jobs: {
    'build-core': build_core_job,
    'build-wheels': build_wheels_job,
    'test-wheels': test_wheels_job,
  },
  // to be used by other workflows (build-test-osx-arm64.jsonnet)
  export:: {
    test_semgrep_steps: test_semgrep_steps,
  },
}
