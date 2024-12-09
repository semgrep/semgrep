local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// actually not exported for now to other workflows, but we might,
// and at least can be downloaded from the GHA job page.
local artifact_name = 'semgrep-core-and-dependent-libs-w64-artifact-${{ github.sha }}';

local wheel_name = 'windows-x86-wheel';
local runs_on = 'windows-latest';
local defaults = {
  run: {
    // Windows GHA runners default to pwsh (PowerShell). We want to use bash
    // to be consistent with our other workflows.
    shell: 'bash',
  },
};

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local build_core_job = {
  // This job is currently disabled because it fails with the ocaml/setup-ocaml@v3
  // github action, which we need for the latest cohttp and for OCaml 5. Currently,
  // `ocamlfind` fails to build when we run this workflow in CI. The ticket for
  // re-enabling the job is https://linear.app/semgrep/issue/SAF-1728/restore-windows-workflow
  'if': 'false',
  'runs-on': runs_on,
  defaults: defaults,
  steps: [
    actions.checkout_with_submodules(),
    {
      uses: 'ocaml/setup-ocaml@v2',
      with: {
        'ocaml-compiler': '4.14',
        // we switch from fdopen's opam mingw repo to the official one
        // otherwise we can't install recent packages like ocamlformat 0.26.2
        // the opam-repository-mingw has the "sunset" branch because it should
        // soon be unecessary once opam 2.2 is released.
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
    { name: 'Debug stuff',
      run: |||
        ls
        # to see the bin symlink for example
        ls -l
        set
        # tree-sitter fails to compile without an ar, you can use
        # CC=x86_64-w64-mingw32-gcc but there is no AR=x86_64-w64-mingw32-ar
        which ar
        ar --version
        # GHA installs cygwin in a special place
        export PATH="${CYGWIN_ROOT_BIN}:${PATH}"
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
    {
      name: 'Build tree-sitter',
      env: {
        CC: 'x86_64-w64-mingw32-gcc',
      },
      // TODO: ideally we should reuse 'make install-deps-for-semgrep-core'
      // but we do a few things differently here for windows (same issue
      // with our HomeBrew formula which has some special tree-sitter
      // installation)
      // TODO: move this in a script, but I got issue executing this script
      // from windows, weird
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
    // TODO: we should also reuse 'make install-deps-for-semgrep-core'
    {
      name: 'Install OPAM deps',
      run: |||
        export PATH="${CYGWIN_ROOT_BIN}:${PATH}"
        make install-deps-WINDOWS-for-semgrep-core
        make install-opam-deps
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
      //TODO: semgrep-core displays also parse errors in the JSON output
      // weird. CRLF windows issue?
      run: |||
        _build/install/default/bin/semgrep-core.exe -l python -rules tests/windows/rules.yml -json tests/windows/test.py
      |||,
    },
    {
      name: 'Package semgrep-core',
      run: |||
        mkdir artifacts
        cp _build/install/default/bin/semgrep-core.exe artifacts/

        # TODO: somehow upgrade to the latest flexdll, which should allow us
        # to statically link these libraries
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libstdc++-6.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgcc_s_seh-1.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libwinpthread-1.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libpcre-1.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgmp-10.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libcurl-4.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libpcre2-8-0.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libeay32.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libidn2-0.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libnghttp2-14.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libssh2-1.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/ssleay32.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libzstd-1.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/iconv.dll artifacts/
        cp d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libintl-8.dll artifacts/

        tar czvf artifacts.tgz artifacts
     |||,
    },
    actions.upload_artifact_step(artifact_name),
  ],
};

local build_wheels_job = {
  'runs-on': runs_on,
  defaults: defaults,
  needs: [
    'build-core',
  ],
  steps: [
    actions.checkout_with_submodules(),
    actions.download_artifact_step(artifact_name),
    {
      env: {
        "SEMGREP_FORCE_INSTALL": 1
      },
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
  ]
};

local test_wheels_job = {
  'runs-on': runs_on,
  defaults: defaults,
  needs: [
    'build-wheels',
  ],
  steps: [
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
      run: 'echo \'1 == 1\' | semgrep -l python -e \'$X == $X\' -'
    },
  ],
};

// ----------------------------------------------------------------------------
// The workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-windows-x86',
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
