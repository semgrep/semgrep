// Factorize Semgrep-specific settings.

{
  secrets: {
    // this token is stored in the GHA secrets settings
    SEMGREP_APP_TOKEN: '${{ secrets.SEMGREP_APP_TOKEN }}',
  },
  // used in the build-test-osx-xxx jobs but ideally we should get rid
  // of it and rely on opam.lock for caching issues
  opam_switch: '4.14.0',

  ocaml_alpine_container: {
    'runs-on': 'ubuntu-latest',
    container: 'returntocorp/ocaml:alpine-2023-06-16',
    // We need this hack because GHA tampers with the HOME in container
    // and this does not play well with 'opam' installed in /root
    env: {
      HOME: '/root',
    },
  },

  ocaml5_alpine_container: {
    'runs-on': 'ubuntu-latest',
    container: 'returntocorp/ocaml:alpine5.0-2023-09-29',
    env: {
      HOME: '/root',
    },
  },

}
