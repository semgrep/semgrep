// This workflow builds and tests the semgrep-core binary for macOS X86
// and generates the osx-wheel for pypi.

// coupling: if you modify this file, modify also build-test-osx-arm64.jsonnet

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Helpers (also reused in build-test-osx-arm64.jsonnet
// ----------------------------------------------------------------------------

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
// OPAM caching (TODO: move in semgrep.libsonnet at some point)
// ----------------------------------------------------------------------------

// This workflow uses the actions/cache@v3 GHA extension to cache
// the ~/.opam directory, which is different from what we do for our other
// architecture's build processes.
//
// This workflow runs on GHA-hosted runners and without caching it would run
// very slowly (like 35min instead of 10min with caching). The Linux build process
// uses a special container (returntocorp/ocaml:alpine-xxx) to bring in the
// required dependencies, which makes opam switch create unnecessary and opam
// install almost a noop. The M1 build runs on fast self-hosted runners where
// caching does not seem to be necessary.
//
// TODO? If this experiment goes well, we might want to use this ~/.opam caching
// technique also for M1 for consistency, and maybe even get rid of our
// returntocorp/ocaml:alpine-xxx container to simplify things.
//
// To update to a new version of OCaml, we can modify the `OPAM_SWITCH_NAME` var
// below, which will update the cache key, and lead to a cache miss on the new builds.
//
// TODO? we might want to use opam.lock as a key so any update to our dependencies
// would automatically trigger a cache miss and generate a fresh ~/.opam.
//
// alt:
//  - use a self-hosted runner where we can save the content of ~/.opam between
//    runs and do whatever we want. The problem is that the build is then
//    not "hermetic", and we ran in many issues such as the disk of the self-hosted
//    runner being full, or some stuff being left from other CI runs
//    (such as a semgrep install) entering in conflicts with some of our build steps.
//    This also requires some devops work to create and maintain those pools of
//    self-hosted runners.
//  - use a GHA-hosted runner which is nice because we don't have to do
//    anything, and the build are guaranteed to be hermetic. The only problem originally
//    was that it was slower, and for unknown reasons ocamlc was not working well
//    on those macos-12 GHA runners, but caching the ~/.opam with actions/cache@v3
//    seems to solve the speed issue (and maybe ocamlc works now well under macos-12).
//  - use a technique similar to what we do for Linux with our special container, but
//    can this be done for macos?
//
// See also https://www.notion.so/semgrep/Caching-the-Opam-Environment-5d7e594203884d289acdac53713fb39f for more information.

// to be used with workflow_dispatch and workflow_call in the workflow
local use_cache_inputs(required) = {
  inputs: {
    'use-cache': {
      description: 'Use Opam Cache - uncheck the box to disable use of the opam cache, meaning a long-running but completely from-scratch build.',
      required: required,
      type: 'boolean',
      default: true,
    },
  },
};

// Note that this actions does cache read and cache write.
// See https://docs.github.com/en/actions/using-workflows/caching-dependencies-to-speed-up-workflows for more information on GHA caching.
//
// Note that this works and speedup things because of the way OPAM works
// and osx-setup-for-release.sh is written. Indeed, this script checks
// if the opam switch is already created, and if a package is already
// installed (in ~/.opam), then opam install on this package will do nothing.
// If we use new packages in semgrep.opam, then we currently would still
// hit the cache unfortunately, but we would spend time only for installing
// those new packages (ideally we would want to regenerate the cache by
// using an opam.pock in the cache key).
//
// PRE: the env.OPAM_SWITCH_NAME must be set in the caller
local cache_opam_step = {
  name: 'Cache Opam',
  uses: 'actions/cache@v3',
  // see use_cache_inputs() above
  'if': '${{ inputs.use-cache }}',
  env: {
    SEGMENT_DOWNLOAD_TIMEOUT_MINS: 2,
  },
  with: {
    path: '~/.opam',
    //TODO: we should add the md5sum of opam.lock as part of the key
    key: '${{ runner.os }}-${{ runner.arch }}-${{ env.OPAM_SWITCH_NAME }}-opam-deps-${{ github.run_id }}',
    //TODO: what is restore-keys needed for? Why we don't suffix it with
    // the github.run_id?
    'restore-keys': '${{ runner.os }}-${{ runner.arch }}-${{ env.OPAM_SWITCH_NAME }}-opam-deps\n',
  },
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local artifact_name = 'semgrep-osx-${{ github.sha }}';
local wheel_name = 'osx-x86-wheel';
local runs_on = 'macos-12';

local build_core_job = {
  'runs-on': runs_on,
  //TODO: could pass it via an argument to cache_opam_step instead of env?
  env: {
    // This name is used in the cache key. If we update to a newer version of
    // ocaml, we'll want to change the OPAM_SWITCH_NAME as well to avoid issues
    // with caching.
    OPAM_SWITCH_NAME: semgrep.opam_switch,
  },
  steps: [
    actions.checkout_with_submodules(),
    cache_opam_step,
    {
      name: 'Install dependencies',
      run: './scripts/osx-setup-for-release.sh "${{ env.OPAM_SWITCH_NAME }}"',
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
    workflow_dispatch: use_cache_inputs(required=true),
    workflow_call: use_cache_inputs(required=false),
  },
  jobs: {
    'build-core': build_core_job,
    'build-wheels': build_wheels_job,
    'test-wheels': test_wheels_job,
  },
  // to be used by other workflows (build-test-osx-arm64.jsonnet)
  export:: {
    cache: {
      use_cache_inputs: use_cache_inputs,
      cache_opam_step: cache_opam_step,
    },
    test_semgrep_steps: test_semgrep_steps,
  },
}
