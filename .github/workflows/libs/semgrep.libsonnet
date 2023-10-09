// Factorize Semgrep-specific settings.

{
  secrets: {
    SEMGREP_APP_TOKEN: '${{ secrets.SEMGREP_APP_TOKEN }}',
  },
  ocaml_alpine_container: {
    'runs-on': 'ubuntu-latest',
    container: 'returntocorp/ocaml:alpine5.0-2023-09-29',
    // We need this hack because GHA tampers with the HOME in container
    // and this does not play well with 'opam' installed in /root
    env: {
      HOME: '/root',
    },
  },
}
