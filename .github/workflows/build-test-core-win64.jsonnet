// This workflow builds and test semgrep-core. It also generates an
// ocaml-build-artifacts.tgz file which is used in many other jobs
// such as test-cli in tests.jsonnet or build-wheels-manylinux in
// build-test-manylinux-x86.jsonnet

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// exported for other workflows
local artifact_name = 'ocaml-build-artifacts-release-w64';
local artifact_path = '/tmp/ocaml-build-artifacts.tgz';

local checkout_dir = 'checkout';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job(artifact=artifact_name) =
  {
    'runs-on': 'windows-latest',
    defaults: {
      run: {
        shell: 'bash',
      }
    },
    steps: [
      gha.git_longpaths,
      gha.speedy_checkout_step,
      {
        // TODO: update actions.checkout_with_submodules to accept a custom path
        uses: 'actions/checkout@v3',
        with: {
          submodules: true,
          path: checkout_dir,
        },
      },
      gha.git_safedir,
      {
        uses: "ocaml/setup-ocaml@v2",
        with: {
          'ocaml-compiler': '4.14',
          'opam-pin': false,
          'opam-repositories': 'opam-repository-mingw: https://github.com/ocaml-opam/opam-repository-mingw.git#sunset\ndefault: https://github.com/ocaml/opam-repository.git'
        }
      },
      {
        id: 'build-tree-sitter',
        name: 'Build tree-sitter',
        'working-directory': checkout_dir,
        run: |||
          export CC=x86_64-w64-mingw32-gcc
          cd libs/ocaml-tree-sitter-core
          ./configure
          ./scripts/download-tree-sitter --lazy
          prefix="$(pwd)/tree-sitter"
          cd downloads/tree-sitter
          make PREFIX="$prefix" CFLAGS="-O3 -Wall -Wextra"
          make PREFIX="$prefix" install
        |||
      },
      {
        id: 'install-deps',
        name: 'Install deps',
        'working-directory': checkout_dir,
        run: |||
          opam depext conf-pkg-config conf-gmp conf-libpcre
          opam install -y . ./libs/ocaml-tree-sitter-core --deps-only
        |||
      }
      {
        name: 'Build semgrep-core',
        'working-directory': checkout_dir,
        env: {
          ARTIFACT_PATH: artifact_path
        },
        run: |||
          export PATH="${CYGWIN_ROOT_BIN}:${PATH}"
          export TREESITTER_INCDIR=$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/include
          export TREESITTER_LIBDIR=$(pwd)/libs/ocaml-tree-sitter-core/tree-sitter/lib

          # rpath is not supported in cygwin
          for filename in $(find ./languages/ -name dune); do
            grep -v rpath $filename > $filename.new
            mv $filename.new $filename
          done
          for filename in $(find ./libs/ocaml-tree-sitter-core/ -name dune); do
            grep -v rpath $filename > $filename.new
            mv $filename.new $filename
          done

          opam exec -- dune build src/main

          tar czvf ${ARTIFACT_PATH} _build/default/src/main/Main.exe \
            d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libstdc++-6.dll \
            d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgcc_s_seh-1.dll \
            d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libwinpthread-1.dll \
            d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libpcre-1.dll \
            d:/cygwin/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgmp-10.dll
        |||,
      },
      {
        name: "Test semgrep-core",
        'working-directory': checkout_dir,
        run: |||
          echo 'print("foo")' > test.py
          _build/default/src/main/Main.exe -e 'print($X)' -lang python -json test.py
        |||
      },
      {
        uses: 'actions/upload-artifact@v3',
        with: {
          path: artifact_path,
          name: artifact,
        },
      },
    ],
  };

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'build-test-core-win64',
  // This is called from tests.jsonnet and release.jsonnet
  // TODO: just make this job a func so no need to use GHA inherit/call
  on: gha.on_dispatch_or_call,
  jobs: {
    job: job(),
  },
  // to be reused by other workflows
  export:: {
    artifact_name: artifact_name,
    // used by build-test-core-x86-ocaml5.jsonnet
    job: job,
  },
}
