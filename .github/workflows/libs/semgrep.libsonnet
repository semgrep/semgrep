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
  get_token_steps: [
   {
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
  {
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
  }],
  // Token computed in get_token_steps to be used in the caller
  token_ref: '${{ steps.token.outputs.token }}',
};

// ----------------------------------------------------------------------------
// OPAM caching
// ----------------------------------------------------------------------------

// The step below uses the actions/cache@v3 GHA extension to cache
// the ~/.opam directory which speedups a lot the "install opam dependencies"
// step, especially in workflows where we can't use ocaml-layer.
// See also actions.libsonnet for other GHA caching helpers.
// Note that actions/setup-ocaml@v2 is using a similar technique.
//
// For example, on GHA-hosted macos runners, without caching it would run
// very slowly like 35min instead of 10min with caching.
// The M1 build runs on fast self-hosted runners where caching does not seem
// to be necessary.
// In Linux, we use a special container (returntocorp/ocaml:alpine-xxx) to
// bring in the required dependencies, which makes 'opam switch create'
// and 'opam install deps' unnecessary and almost a noop.
// Still, we could potentially get rid of ocaml-layer and replace it with
// this more general caching mechanism (or switch to setup-ocaml@v2).
//
// alt:
//  - use a self-hosted runner where we can save the content of ~/.opam between
//    runs and do whatever we want. The problem is that the build is then
//    not "hermetic", and we ran in many issues such as the disk of the
//    self-hosted runner being full, or some stuff being left from other CI
//    runs (such as a semgrep install) entering in conflicts with some of our
//    build steps. This also requires some devops work to create and maintain
//    those pools of self-hosted runners.
//  - use a GHA-hosted runner which is nice because we don't have to do
//    anything, and the build are guaranteed to be hermetic. The only problem
//    originally was that it was slower, and for unknown reasons ocamlc was
//    not working well on those macos-12 GHA runners, but caching the ~/.opam
//    with actions/cache@v3 seems to solve the speed issue (and maybe ocamlc
//    works now well under macos-12).
//  - use a technique similar to what we do for Linux with our special
//    container, but can this be done for macos?
//  - use setup-ocaml@v2 which internally uses a GHA cache too
//
// See also https://www.notion.so/semgrep/Caching-the-Opam-Environment-5d7e594203884d289acdac53713fb39f
// for more information.

// Note that this action does cache read and cache write.
// See https://docs.github.com/en/actions/using-workflows/caching-dependencies-to-speed-up-workflows
// for more information on GHA caching.
//
// Note that this works and speedup things because of the way OPAM works
// and osx-setup-for-release.sh is written. Indeed, this script checks
// if the opam switch is already created, and if a package is already
// installed (in ~/.opam), then opam install on this package will do nothing.
//
// See https://github.com/organizations/semgrep/settings/actions/caches
// (requires admin access to github org) to see the GHA cache settings
// and https://github.com/semgrep/semgrep/actions/caches?query=sort%3Asize-desc
// to see the actual cache files created and used.

local cache_opam = {
  step(key, path="~/.opam"): {
    name: 'Set GHA cache for OPAM in ' + path,
    uses: 'actions/cache@v3',
    env: {
      SEGMENT_DOWNLOAD_TIMEOUT_MINS: 2,
    },
    with: {
      path: path,
      key: '${{ runner.os }}-${{ runner.arch }}-opam-deps-%s' % key,
    },
   },
   // to be used with workflow_dispatch and workflow_call in the workflow
  inputs(required): {
    inputs: {
    'use-cache': {
      description: 'Use Opam Cache - uncheck the box to disable use of the opam cache, meaning a long-running but completely from-scratch build.',
      required: required,
      type: 'boolean',
      default: true,
    },
  }
  },
  if_cache_inputs: {
    'if': '${{ inputs.use-cache}}'
  },
};

// ----------------------------------------------------------------------------
// Containers
// ----------------------------------------------------------------------------

local containers = {
  ocaml_alpine: {
    // used in the build-test-osx-xxx jobs but ideally we should get rid
    // of it and rely on opam.lock for caching issues
    opam_switch: '4.14.0',
    job: {
      'runs-on': 'ubuntu-latest',
      container: 'returntocorp/ocaml:alpine-2024-01-18',
      // We need this hack because GHA tampers with the HOME in container
      // and this does not play well with 'opam' installed in /root
      env: {
        HOME: '/root',
      },
     },
  },
  // ocaml-layer builds an image based on Alpine and another one based on
  // Ubuntu.
  // Alpine is necessary in practice for static linking (especially for C++
  // libraries). Ubuntu is an alternative Linux distribution people may be
  // more familiar with. It's been cheap to maintain both so far but we could
  // decide to keep just one if it makes things simpler.
  ocaml_ubuntu: {
    opam_switch: '4.14.0',
    job: {
      'runs-on': 'ubuntu-latest',
      container: 'returntocorp/ocaml:ubuntu-2024-01-18',
      env: {
        HOME: '/root',
      },
      },
   },
};

// ----------------------------------------------------------------------------
// Slack helpers
// ----------------------------------------------------------------------------

//TODO: use instead the more direct:
//        if: failure()
//        uses: slackapi/slack-github-action@v1.23.0
//        with:
//          channel-id: "C05TW5S2EFJ" # team-frameworks-and-services
//          slack-message: "The `${{ github.workflow }}` workflow has failed! Please take a look: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}"
//        env:
//           SLACK_BOT_TOKEN: ${{ secrets.R2C_SLACK_TOKEN }}
// (but this need R2C_SLACK_TOKEN which was not added to the public semgrep repo)

local slack = {
  // This will post on Slack on #logs-semgrep-release
  curl_notify_nightly_homebrew(commit, url): |||
        curl --request POST \
        --url  ${{ secrets.HOMEBREW_NIGHTLY_NOTIFICATIONS_URL }} \
        --header 'content-type: application/json' \
        --data '{
          "commit_sha": "%s",
          "workflow_url": "%s"
        }'
   ||| %  [ commit, url],
 notify_failure_nightly_job(commit, url): {
  'runs-on': 'ubuntu-20.04',
  'if': 'failure()',
  steps: [
    {
      run: slack.curl_notify_nightly_homebrew(commit, url),
    },
   ],
  },

  // This will post on Slack on #???
  // TODO: change the target, I have no idea where those notifications go
  curl_notify_e2e_semgrep_ci(docker_tag, message): |||
    curl --request POST \
        --url  ${{ secrets.SEMGREP_CI_E2E_NOTIFICATIONS_URL }} \
        --header 'content-type: application/json' \
        --data '{
          "workflow_run_url": "https://github.com/${{github.repository}}/actions/runs/${{github.run_id}} for more details!",
          "docker_tag": %s,
          "message": "%s"
         }'
   ||| % [docker_tag, message],

 notify_failure_e2e_semgrep_ci_job(docker_tag, message): {
  'runs-on': 'ubuntu-20.04',
  'if': 'failure()',
  steps: [
    {
      run: slack.curl_notify_e2e_semgrep_ci(docker_tag, message),
    },
   ],
  },

  // this will post on Slack on #team-semgrep-core
  // TODO: this channel is archived so need update NOTIFICATIONS_URL
  curl_notify_start_release(version, message): |||
      curl --request POST \
       --url  ${{ secrets.NOTIFICATIONS_URL }} \
       --header 'content-type: application/json' \
       --data '{
         "version": "%s",
         "message": "%s"
       }'
    ||| % [version, message],

  notify_failure_start_release_job(version, message): {
   'runs-on': 'ubuntu-20.04',
   'if': 'failure()',
    steps: [
      {
        run: slack.curl_notify_start_release(version, message),
      },
     ],
    },
  // This will post on Slack on #semgrep-cli-release from a
  // 'gha-notification' user.
  // The actual URL secret is stored in 1password in our Engineering vault
  // (look for slack webhook) and configured to post to
  // #semgrep-cli-release in ???
  curl_notify(message): |||
      curl --request POST \
       --url  ${{ secrets.NOTIFICATIONS_URL }} \
       --header 'content-type: application/json' \
       --data '{
         "text": "%s"
       }'
    ||| % message,

  notify_failure_job(message): {
   'runs-on': 'ubuntu-20.04',
   'if': 'failure()',
    steps: [
      {
        run: slack.curl_notify(message),
      },
     ],
    },
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

  aws_credentials_step(role, session_name): {
      name: 'Configure AWS credentials for %s' % role,
      uses: 'aws-actions/configure-aws-credentials@v4',
      with: {
        // This seems to be semgrep specific magic number
        'role-to-assume': 'arn:aws:iam::338683922796:role/%s' % role,
        'role-duration-seconds': 900,
        'role-session-name': session_name,
        'aws-region': 'us-west-2',
      },
    },
  // works well with actions.upload_artifact_step
  make_artifact_step(path): {
      name: 'Make artifact for %s' % path,
      run: |||
          mkdir artifacts
          cp %s artifacts/
          tar czf artifacts.tgz artifacts
        ||| % path,
  },

  // default one
  // coupling: with containers above
  opam_switch: '4.14.0',

  containers: containers,
  github_bot: github_bot,
  cache_opam: cache_opam,
  slack: slack,
}
