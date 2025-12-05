// Factorize windows-specific settings.

local actions = import 'actions.libsonnet';
local gha = import 'gha.libsonnet';

local git_config = {
  // On Windows, cloning the repository may fail with a "filename too long
  // error", if longpaths is not set. Also, we want to disable autocrlf and set
  // eol to lf to avoid issues with line endings.
  name: 'Configure git to allow long paths, disable autocrlf, and set eol to lf',
  run: |||
    git config --global core.longpaths true
    git config --global core.autocrlf false
    git config --global core.eol lf
  |||,
};


local copy_executable_dlls(path_to_libs, executable, target_dir) =
  {
    name: 'Copy %s DLLs to %s/' % [executable, target_dir],
    // cygcheck lists the library (DLL) dependencies of the binary. We only
    // copy the DLLs from the x86_64-w64-mingw32/sys-root/ directory, where the
    // DLLs installed from the opam depexts are located. The other DLLs that we
    // depend on are Windows System DLLs or other DLLs which should already be
    // available to be able to run Python.
    //
    // NOTE: although not a depext, we recently begun seeing tree-sitter.dll be
    // not included/not found after a bumping the ocaml-tree-sitter-core lib.
    // Since they aren't managed by opam, we have to include them when
    // we copy these depext DLLs.
    // see: pull/3790
    run: |||
      mkdir -p %(dst)s
      SYS_ROOT_BIN="$(x86_64-w64-mingw32-gcc -print-sysroot)/mingw/bin"
      # path to tree-sitter.dll
      TREESITTER_BIN=%(lib_path)s
      DLL_PATHS=$SYS_ROOT_BIN:$TREESITTER_BIN
      dlls=$(PATH=$DLL_PATHS:$PATH cygcheck "%(exe)s" | grep '\(x86_64-w64-mingw32\|ocaml-tree-sitter-core\)' | sed 's/^[[:space:]]*//' | sort -u)
      for dll in $dlls; do
        echo "Copying $dll to %(dst)s/"
        cp -p "$dll" "%(dst)s"
      done
    ||| % { dst: target_dir, lib_path: path_to_libs + 'ocaml-tree-sitter-core/tree-sitter/bin', exe: executable },
  };


local install_deps_steps = [
  {
    name: 'Install OPAM deps',
    run: |||
      # NOTE: ocurl's ./configure fails with an error finding curl/curl.h.
      # Setting PKG_CONFIG_PATH to
      #    $(x86_64-w64-mingw32-gcc -print-sysroot)/mingw/include
      # would set Unix paths for CFLAGS and LDFLAGS, but that doesn't
      # work. Setting Windows paths for them gets the ocurl build to
      # work. To avoid setting these paths for all the package builds,
      # we first try to install all the dependencies, and then install
      # ocurl and later other dependencies that depend on ocurl.
      make -C OSS install-opam-deps || true
      export CYGWIN_SYS_ROOT="$(x86_64-w64-mingw32-gcc --print-sysroot)"
      CFLAGS="-I$(cygpath -w $CYGWIN_SYS_ROOT/mingw/include)" LDFLAGS="-L$(cygpath -w $CYGWIN_SYS_ROOT/mingw/lib)" opam install -y ocurl.0.9.1
      make install-deps
    |||,
  },
];

local optimize_temp =
  {
    name: 'Optimize temp directory on Windows runners',
    'if': "startsWith(runner.os, 'Windows')",
    run: |||
      mkdir "D:\\Temp"
      echo "TEMP=D:\\Temp" >> $env:GITHUB_ENV
      echo "TMP=D:\\Temp" >> $env:GITHUB_ENV
    |||,
  }
;
// ----------------------------------------------------------------------------
// Entry point
// ----------------------------------------------------------------------------

{
  copy_executable_dlls: copy_executable_dlls,
  git_config: git_config,
  install_deps_steps: install_deps_steps,
  optimize_temp: optimize_temp,
}
