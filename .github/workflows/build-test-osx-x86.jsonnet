// This workflow builds and tests the semgrep-core binary for macOS X86
// and generates the osx-wheel for pypi.

// coupling: if you modify this file, modify also build-test-osx-arm64.jsonnet

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

local wheel_name = 'osx-x86-wheel';

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

// See https://github.com/actions/runner-images/blob/main/images/macos/macos-12-Readme.md
// This already comes with Python installed so no need to
// have a setup_python_step like in build-test-osx-arm64.jsonnet
// Macos 13 is the last version that has x86 + osx; macos-14 and above are arm
// only
local runs_on = 'macos-13';

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

local build_core_job = {
  'runs-on': runs_on,
  steps: [
    actions.checkout_with_submodules(),
    // TODO: we should use opam.lock instead of semgrep.opam at some point
    // so any update to our dependencies would automatically trigger a
    // cache miss and generate a fresh ~/.opam.
    semgrep.cache_opam.step(
         key=semgrep.opam_switch + "-${{hashFiles('semgrep.opam')}}")
      + semgrep.cache_opam.if_cache_inputs,
    {
      name: 'Install dependencies',
      run: './scripts/osx-setup-for-release.sh "%s"' % semgrep.opam_switch,
    },
    {
      name: 'Compile semgrep (in case of linking errors, adjust src/main/flags.sh)',
      run: 'opam exec -- make core',
    },
    actions.make_artifact_step("./bin/semgrep-core"),
    actions.upload_artifact_step(artifact_name),
  ],
};

local build_wheels_job = {
  'runs-on': runs_on,
  needs: [
    'build-core',
  ],
  steps: [
    actions.checkout_with_submodules(),
    actions.download_artifact_step(artifact_name),
    {
      run: |||
        tar xvfz artifacts.tgz
        cp artifacts/semgrep-core cli/src/semgrep/bin
        ./scripts/build-wheels.sh --plat-name macosx_10_14_x86_64
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
    actions.download_artifact_step(wheel_name),
    {
      run: 'unzip dist.zip',
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
