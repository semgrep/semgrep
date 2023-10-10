// This workflow builds and tests the semgrep-core binary for macOS arm64
// and generates the arm64-wheel for pypi.
// coupling: if you modify this file, modify also build-test-osx-x86.jsonnet

local actions = import "libs/actions.libsonnet";
local semgrep = import "libs/semgrep.libsonnet";
local osx_x86 = import "build-test-osx-x86.jsonnet";

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// alt: could factorize with build-test-osx-x86.jsonnet by making
// the xxx_job functions.
local artifact_name = 'semgrep-osx-arm64-${{ github.sha }}';
local wheel_name = 'osx-arm64-wheel';
local runs_on = [
    'self-hosted',
    'macOS',
    'ARM64',
    'ghcr.io/cirruslabs/macos-monterey-xcode:latest',
  ];

local build_core_job = {
  name: 'Build the OSX arm64 binaries',
  'runs-on': runs_on,
  env: {
    OPAM_SWITCH_NAME: semgrep.opam_switch,
  },
  steps: [
    {
      name: 'Setup runner directory',
      run: |||
         sudo mkdir /Users/runner
         sudo chown admin:staff /Users/runner
         sudo chmod 750 /Users/runner
      |||,
    },
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.11',
      },
    },
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
    {
      name: 'Setup runner directory',
      run: |||
        sudo mkdir /Users/runner
        sudo chmod 750 /Users/runner
        sudo chown -R admin:staff /Users/runner
      |||,
    },
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.11',
      },
    },
    actions.checkout_with_submodules(),
    {
      uses: 'actions/download-artifact@v3',
      with: {
        name: artifact_name,
      },
    },
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
    {
      name: 'Setup runner directory',
      run: |||
        sudo mkdir /Users/runner
        sudo chmod 750 /Users/runner
        sudo chown -R admin:staff /Users/runner
       |||,
    },
    {
      uses: 'actions/download-artifact@v1',
      with: {
        name: wheel_name,
      },
    },
    {
      uses: 'actions/setup-python@v4',
      with: {
        'python-version': '3.11',
      },
    },
    {
      run: 'unzip ./osx-arm64-wheel/dist.zip',
    },
    {
      name: 'install package',
      run: 'pip3 install dist/*.whl',
    },
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
  ],
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
