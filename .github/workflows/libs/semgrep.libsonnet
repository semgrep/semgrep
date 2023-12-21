// Factorize Semgrep-specific settings.

// ----------------------------------------------------------------------------
// Helpers to be able to use 'gh' (and trigger PRs) from a workflow
// ----------------------------------------------------------------------------
// 'gh' is one of the github CLIs (another one is 'hub') and it allows
// to make PRs (and other github stuff) from scripts (and from workflows).

// From infra:
// "We use the semgrep-ci bot as the auth. The custom (internally-developed)
// docker image below is used to get a JWT, which is then used by git to
// fetch the code. Using the built-in secrets.GITHUB_TOKEN won't allow for
// downstream jobs to fire.
// See https://docs.github.com/en/enterprise-cloud@latest/actions/using-workflows/triggering-a-workflow#triggering-a-workflow-from-a-workflow
// for more information"
//
// TODO: where is stored/configured/built this semgrep-ci github App?
// TODO: where is configured this docker://public.ecr.aws/... image below?
// TODO: How is it built?
// TODO: if a token is rotated, do we need to update this docker link?

local github_bot = {
  get_jwt_step: {
    name: 'Get JWT for semgrep-ci GitHub App',
    id: 'jwt',
    uses: 'docker://public.ecr.aws/y9k7q4m1/devops/cicd:latest',
    env: {
      // This is the shortest expiration setting. It ensures that if an
      // attacker got a hold of these credentials after the job runs,
      // they're expired.
      // TODO: how an attacker can access this credential?
      EXPIRATION: 600,  // in seconds
      ISSUER: '${{ secrets.SEMGREP_CI_APP_ID }}',
      PRIVATE_KEY: '${{ secrets.SEMGREP_CI_APP_KEY }}',
    },
  },
  // We are using the standard github-recommended method for short-live
  // authentification.
  // See https://docs.github.com/en/developers/apps/building-github-apps/authenticating-with-github-apps#authenticating-as-a-github-app
  get_token_step: {
    name: 'Get token for semgrep-ci GitHub App',
    id: 'token',
    run: |||
      TOKEN="$(curl -X POST \
      -H "Authorization: Bearer ${{ steps.jwt.outputs.jwt }}" \
      -H "Accept: application/vnd.github.v3+json" \
      "https://api.github.com/app/installations/${{ secrets.SEMGREP_CI_APP_INSTALLATION_ID }}/access_tokens" | \
      jq -r .token)"
      echo "::add-mask::$TOKEN"
      echo "token=$TOKEN" >> $GITHUB_OUTPUT
    |||,
  },
  // Token computed in get_token_step to be used by the user of this
  // github_bot.
  token_ref: '${{ steps.token.outputs.token }}',
};

// ----------------------------------------------------------------------------
// Entry point
// ----------------------------------------------------------------------------

{
  secrets: {
    // this token is stored in the GHA secrets settings
    SEMGREP_APP_TOKEN: '${{ secrets.SEMGREP_APP_TOKEN }}',
    // for e2e-semgrep-ci.jsonnet
    E2E_APP_TOKEN: '${{ secrets.SEMGREP_E2E_APP_TOKEN }}',
  },
  // used in the build-test-osx-xxx jobs but ideally we should get rid
  // of it and rely on opam.lock for caching issues
  opam_switch: '4.14.0',

  ocaml_alpine_container: {
    'runs-on': 'ubuntu-latest',
    container: 'returntocorp/ocaml:alpine-2023-11-07',
    // We need this hack because GHA tampers with the HOME in container
    // and this does not play well with 'opam' installed in /root
    env: {
      HOME: '/root',
    },
  },


  // ocaml-layer builds an image based on Alpine and another one based on
  // Ubuntu.
  // Alpine is necessary in practice for static linking (especially for C++
  // libraries). Ubuntu is an alternative Linux distribution people may be
  // more familiar with. It's been cheap to maintain both so far but we could
  // decide to keep just one if it makes things simpler.
  ocaml_ubuntu_container: {
    'runs-on': 'ubuntu-latest',
    container: 'returntocorp/ocaml:ubuntu-2023-11-07',
    env: {
      HOME: '/root',
    },
  },

  ocaml5_alpine_container: {
    'runs-on': 'ubuntu-latest',
    container: 'returntocorp/ocaml:alpine5.1-2023-11-07',
    env: {
      HOME: '/root',
    },
  },

  github_bot: github_bot,
}
