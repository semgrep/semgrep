// This workflow just checks whether we can build semgrep-core on windows
// using the mingw compiler.
//
// Eventually this will allow us to release (o)semgrep for Windows with
// native support which will be faster and more convenient than what current
// Semgrep users have to do which is to use Docker or the Window Subsystem
// for Linux (WSL).
//
// Note that if you want to build semgrep yourself on a Windows machine,
// you'll need to imitate some of the magic done by setup-ocaml@v2:
//   c:\Windows\system32\fsutil.exe behavior query SymlinkEvaluation
//   c:\Windows\system32\fsutil.exe behavior set SymlinkEvaluation R2L:1 R2R:1
// otherwise the symlinks in semgrep used to link our .atd files would
// confuse 'dune'

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job = {
  'runs-on': 'windows-latest',
  defaults: {
    run: {
      // Windows GHA runners default to pwsh (PowerShell). We want to use bash
      // to be consistent with our other workflows.
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
	#TODO: do not reindent this, ojsonnet/ocaml-yaml/--yaml bug with newlines
	# which then prevents setup-ocaml@v2 to work correctly
        'opam-repositories': |||
opam-repository-mingw: https://github.com/ocaml-opam/opam-repository-mingw.git#sunset
default: https://github.com/ocaml/opam-repository.git
|||,
        // bogus filename to prevent the action from attempting to install
        // anything (we want deps only)
        'opam-local-packages': 'dont_install_local_packages.opam',
      },
    },
    // Why this cache when ocaml/setup-ocaml is already caching things?
    // - setup-ocaml caches the cygwin and downloaded opam packages, but not the
    //   installed opam packages
    // - without the _opam cache we would spend 8-9 minutes every build
    //   running `opam install`
    // Note: we must cache after setup-ocaml, not before, because
    // setup-ocaml would reset the cached _opam
    semgrep.cache_opam.step(
      key=semgrep.opam_switch + "-${{ hashFiles('semgrep.opam') }}",
      // ocaml/setup-ocaml creates the opam switch local to the repository
      // (vs. ~/.opam in our other workflows)
      path='_opam',
      ),
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
    // this should be mostly a noop thx to cache_opam above
    {
      name: 'Install deps',
      run: |||
        export PATH="${CYGWIN_ROOT_BIN}:${PATH}"
        opam depext conf-pkg-config conf-gmp conf-libpcre conf-libcurl
        wget https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg
        chmod +x apt-cyg
        mv apt-cyg /usr/local/bin
        apt-cyg install libcurl-devel
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
      run: |||
        mkdir archive
        cp _build/install/default/bin/semgrep-core.exe archive/

        # TODO: somehow upgrade to the latest flexdll, which should allow us to statically link these libraries
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libstdc++-6.dll archive/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgcc_s_seh-1.dll archive/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libwinpthread-1.dll archive/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libpcre-1.dll archive/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgmp-10.dll archive/

        cd archive
        tar czvf ocaml-build-artifacts.tgz *.exe *.dll
      |||,
    },
    {
      uses: 'actions/upload-artifact@v3',
      with: {
        path: 'archive/ocaml-build-artifacts.tgz',
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
