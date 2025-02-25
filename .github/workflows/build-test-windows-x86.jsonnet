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
// TODO: We can remove this and switch to semgrep.opam_switch once we move to
// OCaml 5 everywhere.
local opam_switch = '5.2.1';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local build_core_job = {
  // This job is currently disabled because it fails with the ocaml/setup-ocaml@v3
  // github action, which we need for the latest cohttp and for OCaml 5. Currently,
  // `ocamlfind` fails to build when we run this workflow in CI. The ticket for
  // re-enabling the job is https://linear.app/semgrep/issue/SAF-1728/restore-windows-workflow
  'runs-on': runs_on,
  defaults: defaults,
  steps: [
    actions.checkout_with_submodules(),
    // Why this cache when ocaml/setup-ocaml is already caching things?
    // - setup-ocaml caches the cygwin and downloaded opam packages, but not the
    //   installed opam packages
    // - without the _opam cache we would spend 8-9 minutes every build
    //   running `opam install`
    // Note: we must cache after setup-ocaml, not before, because
    // setup-ocaml would reset the cached _opam
    semgrep.cache_opam.step(
      key=semgrep.opam_switch + "-${{ hashFiles('semgrep-pro.opam', 'OSS/semgrep.opam') }}",
    ),
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
      |||
    },
    {
      // TODO: We can remove this once these flexdll PRs are merged and a new
      // version of flexdll is released:
      // - https://github.com/ocaml/flexdll/pull/151
      // - https://github.com/ocaml/flexdll/pull/152

      // Currently, flexlink only uses response files with MSVC and LIGHTLD
      // compilers. With the MINGW64 compiler, we get an "argument list too
      // long error". We use a patched version of flexlink that uses response
      // files with MINGW64.

      // flexlink also calls cygpath to normalize a bunch of paths, and our
      // build has too many search paths which causes an "argument list too
      // long" error. We use a patched flexlink which passes these arguments
      // in a file to cygpath.
      name: 'Install flexlink patched to use response files and cygpath -file arg',
      run: |||
          git clone -b argument-list-too-long https://github.com/punchagan/flexdll.git
          cd flexdll/
          opam exec -- make all MSVC_DETECT=0 CHAINS="mingw64"
          cp flexlink.exe ../_opam/bin/
      |||
    },
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
        # NOTE: ocurl's ./configure fails with an error finding curl/curl.h.
        # Setting PKG_CONFIG_PATH to $(x86_64-w64-mingw32-gcc
        # -print-sysroot)/mingw/include would set UNIX paths for CFLAG and
        # LDFLAG, but that doesn't work. Setting Windows PATHs for them gets
        # the ocurl build to work. To avoid setting these PATHs for all the
        # package builds, we first try to install all the dependencies, and
        # then install ocurl and later other dependencies that depend on ocurl.
        make install-opam-deps || true
        export CYGWIN_SYS_ROOT="$(x86_64-w64-mingw32-gcc --print-sysroot)"
        CFLAGS="-I$(cygpath -w $CYGWIN_SYS_ROOT/mingw/include)" LDFLAGS="-L$(cygpath -w $CYGWIN_SYS_ROOT/mingw/lib)" opam install -y ocurl.0.9.1
        make install-opam-deps
      |||,
    },
    {
      name: 'Build semgrep-core',
      run: |||
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
