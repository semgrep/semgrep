// This workflow just checks whether we can build semgrep-core on windows
// using the mingw compiler.
//
// Eventually this will allow us to release (o)semgrep for Windows with
// native support which will be faster and more convenient than what current
// Semgrep users have to do which is to use Docker or the Window Subsystem
// for Linux (WSL).

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job = {
  'runs-on': 'windows-latest',
  defaults: {
    run: {
      // Windows GHA runners default to pwsh (PowerShell). We want to use bash to
      // be consistent with our other workflows.
      shell: 'bash',
    },
  },
  steps: [
    gha.git_longpaths_step,
    gha.speedy_checkout_step,
    actions.checkout_with_submodules(),
    {
      uses: 'ocaml/setup-ocaml@v2',
      with: {
        'ocaml-compiler': '4.14',
	#TODO: do not reindent this, ojsonnet bug with newlines which
	# then prevents setup-ocaml to work correctly
        'opam-repositories': |||
opam-repository-mingw: https://github.com/ocaml-opam/opam-repository-mingw.git#sunset
default: https://github.com/ocaml/opam-repository.git
|||,
        // bogus filename to prevent the action from attempting to install
        // anything (we want deps only)
        'opam-local-packages': 'dont_install_local_packages.opam',
      },
    },
    {
      name: 'Build tree-sitter',
      env: {
        CC: 'x86_64-w64-mingw32-gcc',
      },
      run: |||
        cd libs/ocaml-tree-sitter-core
        ./configure
        ./scripts/download-tree-sitter --lazy
        prefix="$(pwd)/tree-sitter"
        cd downloads/tree-sitter
        make PREFIX="$prefix" CFLAGS="-O3 -Wall -Wextra"
        make PREFIX="$prefix" install
      |||,
    },
    {
      name: 'Install deps',
      run: |||
        export PATH="${CYGWIN_ROOT_BIN}:${PATH}"
        opam depext conf-pkg-config conf-gmp conf-libpcre
        opam install -y ./ ./libs/ocaml-tree-sitter-core --deps-only
      |||,
    },
    {
      name: 'Build semgrep-core',
      run: |||
        export PATH=\"${CYGWIN_ROOT_BIN}:${PATH}\"
        export TREESITTER_INCDIR=$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/include
        export TREESITTER_LIBDIR=$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/lib
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
      run: |||
        echo 'print(\"foo\")' > test.py
        _build/install/default/bin/semgrep-core.exe -e 'print($X)' -lang python -json test.py
      |||,
    },
    {
      name: 'Package semgrep-core',
      // TODO: figure out how to statically link in cygwin / windows
      // (use opam for inspiration which managed to ship a static opam.exe for windows)
      run: |||
        tar czvf ocaml-build-artifacts.tgz _build/install/default/bin/semgrep-core.exe \
         d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libstdc++-6.dll \
         d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgcc_s_seh-1.dll \
         d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libwinpthread-1.dll \
         d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libpcre-1.dll \
         d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgmp-10.dll
      |||,
    },
    {
      uses: 'actions/upload-artifact@v3',
      with: {
        path: 'ocaml-build-artifacts.tgz',
        name: 'ocaml-build-artifacts-release-w64',
      },
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
    job: job,
  },
}
