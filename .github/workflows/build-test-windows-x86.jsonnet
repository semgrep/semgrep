local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local windows = import 'libs/windows.libsonnet';

// actually not exported for now to other workflows, but we might,
// and at least can be downloaded from the GHA job page.
local artifact_name = 'semgrep-core-and-dependent-libs-w64-artifact-${{ github.sha }}';

local wheel_name = 'windows-x86-wheel';
// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local build_core_job = {
  // This job is currently disabled because it fails with the ocaml/setup-ocaml@v3
  // github action, which we need for the latest cohttp and for OCaml 5. Currently,
  // `ocamlfind` fails to build when we run this workflow in CI. The ticket for
  // re-enabling the job is https://linear.app/semgrep/issue/SAF-1728/restore-windows-workflow
  'runs-on': windows.runs_on,
  defaults: windows.defaults,
  steps: actions.checkout_with_submodules() + [
           semgrep.opam_setup(semgrep.opam_switch),
           {
             // TODO: Remove this once the stable version of `mingw64-x86_64-openssl`
             // is updated in Cygwin.
             //
             // setup-ocaml@v3 uses a newer version of `mingw64-x86_64-openssl` which
             // isn't marked as "stable"; see:
             // https://github.com/ocaml/setup-ocaml/issues/856#issuecomment-2439978460
             //
             // But, we need an older version of `mingw64-x86_64-openssl` for our
             // build since some of our depexts, for instance, `mingw64-x86_64-curl`
             // would be compiled against the stable (older) version of
             // `mingw64-x86_64-openssl`. So, we install an older version here.
             name: 'Install older openssl in Cygwin',
             run: |||
               PACKAGES='mingw64-x86_64-openssl=1.0.2u+za-1,mingw64-i686-openssl=1.0.2u+za-1'
               CYGWIN_ROOT=$(cygpath -w /)
               $CYGWIN_ROOT/setup-x86_64.exe -P $PACKAGES --quiet-mode -R $CYGWIN_ROOT
             |||,
           },
           {
             name: 'Debug stuff',
             run: |||
               ls
               # to see the bin symlink for example
               ls -l
               set
               # tree-sitter fails to compile without an ar, you can use
               # CC=x86_64-w64-mingw32-gcc but there is no AR=x86_64-w64-mingw32-ar
               which ar
               ar --version
               which ar
               ar --version
               which opam
               # this should be fdopen's opan, so 2.0.10
               opam --version
               opam repo
               # we should be on 4.14.0~mingw
               opam switch
             |||,
           },
         ] +
         windows.install_deps_steps +
         [
           {
             name: 'Build semgrep-core',
             run: |||
               export TREESITTER_INCDIR=$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/include
               export TREESITTER_LIBDIR=$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/lib
               export TREESITTER_BINDIR=$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/bin

               # We have to strip rpath from the tree-sitter projects because there's no
               # equivalent in Windows
               # TODO: investigate removing rpath from the tree-sitter projects
               for filename in $(find ./languages/ ./libs/ocaml-tree-sitter-core/ -name dune); do
                 grep -v rpath $filename > $filename.new
                 mv $filename.new $filename
               done
               opam exec -- dune build _build/install/default/bin/semgrep-core.exe
             |||,
           },
           {
             name: 'Test semgrep-core',
             //TODO: semgrep-core displays also parse errors in the JSON output
             // weird. CRLF windows issue?
             //NOTE: we need to include the tree-sitter DLL into our path to execute
             // semgrep, since windows checks for DLLs in it's path
             // see: https://groups.google.com/g/comp.lang.tcl/c/J48G1yhvFrc?pli=1
             run: |||
               treesitter_bindir="$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/bin"
               export PATH="$treesitter_bindir:$PATH"
               # see pro workflow & semgrep-proprietary/pull/3522
               opam exec -- _build/install/default/bin/semgrep-core.exe -l python -rules tests/windows/rules.yml -json tests/windows/test.py
             |||,
           },
           windows.copy_executable_dlls('$(pwd)/libs/', 'bin/semgrep-core.exe', 'extra-artifacts'),
           actions.make_artifact_step('bin/semgrep-core.exe extra-artifacts/*'),
           actions.upload_artifact_step(artifact_name),
         ],
};

local build_wheels_job = {
  'runs-on': windows.runs_on,
  defaults: windows.defaults,
  needs: [
    'build-core',
  ],
  steps: actions.checkout_with_submodules() + [
    // Why make life harder? Disable cache for now.
    actions.setup_python_step(version=semgrep.default_python_version, cache=false),
    actions.download_artifact_step(artifact_name),
    {
      run: |||
        tar xvfz artifacts.tgz
        cp artifacts/* cli/src/semgrep/bin
        ./scripts/build-wheels.sh --plat-name win_amd64
      |||,
    },
    {
      uses: 'actions/upload-artifact@v4',
      with: {
        path: 'cli/dist.tgz',
        name: wheel_name,
      },
    },
  ],
};

local test_wheels_job = {
  'runs-on': windows.runs_on,
  defaults: windows.defaults,
  needs: [
    'build-wheels',
  ],
  steps: [
    // Why make life harder? Disable cache for now.
    actions.setup_python_step(version=semgrep.default_python_version, cache=false),
    actions.download_artifact_step(wheel_name),
    {
      run: 'tar xzvf dist.tgz',
    },
    // *.whl is fine here because we're building one wheel with the "any"
    // platform compatibility tag
    {
      name: 'install package',
      run: 'pip3 install dist/*.whl',
    },
    {
      name: 'test package',
      run: 'semgrep --version',
    },
    {
      name: 'e2e semgrep-core test',
      // --strict to make sure that any errors cause the CI job to fail
      run: "echo '1 == 1' | semgrep -l python -e '$X == $X' --strict -",
    },
  ],
};

// ----------------------------------------------------------------------------
// The workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-windows-x86',
  on: gha.on_dispatch_or_call,
  jobs: {
    'build-core': build_core_job,
    'build-wheels': build_wheels_job,
    'test-wheels': test_wheels_job,
  },
}
