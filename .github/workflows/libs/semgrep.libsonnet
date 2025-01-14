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

local actions = import 'actions.libsonnet';
local gha = import 'gha.libsonnet';

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
// the ~/.opam directory which speedups a lot the "Install opam dependencies"
// steps in our workflows, especially the one where we can't use ocaml-layer.
// See also actions.libsonnet for other GHA caching helpers.
// Note that actions/setup-ocaml@v2 is using a similar technique.
//
// For example, on GHA-hosted macos runners, without caching the osx workflow
// would run very for 35min instead of 10min with caching.
// The M1 build runs on fast self-hosted runners where caching does not seem
// to be necessary.
// In Linux, we use a special container (returntocorp/ocaml:alpine-xxx) to
// bring in the required dependencies, which makes 'opam switch create'
// and 'opam install deps' unnecessary and almost a noop.
// Still, we are gradually getting rid of ocaml-layer and replace it with
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
//    ocaml-layer container, but can this be done for macos?
//  - use setup-ocaml@v2 which internally uses a GHA cache too, but this
//    cache just the downloaded package; it still install/compiles the packages
//    each time.
//
// See also https://www.notion.so/semgrep/Caching-the-Opam-Environment-5d7e594203884d289acdac53713fb39f
// for more information.

// Note that this action does cache read and cache write.
// See https://docs.github.com/en/actions/using-workflows/caching-dependencies-to-speed-up-workflows
// for more information on GHA caching.
//
// See also https://github.com/organizations/semgrep/settings/actions/caches
// (requires admin access to github org) to see the GHA cache settings
// and https://github.com/semgrep/semgrep/actions/caches?query=sort%3Asize-desc
// to see the actual cache files created and used.
//
// Note that from the doc:
// "Workflow runs cannot restore caches created for child branches or sibling
//  branches. For example, a cache created for the child feature-b branch would
//  not be accessible to a workflow run triggered on the parent main branch.
//  Similarly, a cache created for the feature-a branch with the base main
//  would not be accessible to its sibling feature-c branch with the base main."
// This explains why you can see multiple cache entries with the exact same
// cache key name; it's because they are from different branches.

// Note that this caching works and speedup things because of the way OPAM works
// and osx-setup-for-release.sh is written. Indeed, this script checks
// if the opam switch is already created, and if a package is already
// installed (in ~/.opam), then opam install on this package will do nothing.

// Sometimes the cache key is not precise enough and some external changes
// do not trigger cache invalidation but should. For example, we use
// hashFiles('semgrep.opam') in many workflows for the cache key, but
// semgrep.opam is not a lock file and some updates in the opam repo might
// trigger the recompilation/installation of packages which would slow
// down the workflow, even in the presence of the GHA cache, because this
// cache is not up to date with the opam repo. Same for changes such as
// an upgrade from opam 2.1 to 2.2 which is not captured in the cache key
// but which should invalidate the cache.
// This bump_cache is one way to cope with the limitations of our cache keys.
// Moreover, GHA itself does not have a big "delete all cache" button like
// in depot.dev so this bump_cache can act as one too.
local bump_cache = 1;

local cache_opam = {
  step(key, path="~/.opam"): {
    name: 'Set GHA cache for OPAM in ' + path,
    uses: 'actions/cache@v3',
    env: {
      SEGMENT_DOWNLOAD_TIMEOUT_MINS: 2,
    },
    with: {
      path: path,
      key: '${{ runner.os }}-${{ runner.arch }}-v%d-opam-%s' % [bump_cache, key],
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


// Escape all the inner double quotes of a string so that
// it could be embedded in a JSON string.
local escapeStringJson = function(str)
  std.lstripChars(
    std.rstripChars(
      std.escapeStringJson(str),
      '"'),
    '"');

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
  // This will post on Slack on the #semgrep-cli-release channel from a
  // 'gha-notification' user.
  // The actual URL secret is stored in 1password in our Engineering vault
  // (look for "slack webhook") and configured by #team-techops to post to
  // #semgrep-cli-release at
  // https://semgrepinc.slack.com/apps/A0F7XDUAZ-incoming-webhooks?tab=settings&next_id=0

  // Double escape quotes because they are nested in two layers of double quotes. Which still allows string interpolation at the bash level.
  curl_notify(message): |||
      curl --request POST \
       --url  ${{ secrets.NOTIFICATIONS_URL }} \
       --header 'content-type: application/json' \
       --data "{
         \"text\": \"%s\"
       }"
  ||| % escapeStringJson(escapeStringJson(message)),

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


// default one
// coupling: with containers above
local opam_switch = '4.14.0';

// this must be done after the checkout as opam installs itself
// locally in the project folder (/home/runner/work/semgrep/semgrep/_opam)
// coupling: default is above opam_switch
local opam_setup = function(opam_switch="4.14.0") {
      uses: 'ocaml/setup-ocaml@v3',
      with: {
        'ocaml-compiler': opam_switch,
	'opam-pin': false,
      },
    };

// We can't use ubuntu-latest (currently 24.04) just yet until
// https://github.com/ocaml/setup-ocaml/issues/872
// is fixed.
local stable_ubuntu_version_for_setup_ocaml = 'ubuntu-22.04';

local osemgrep_test_steps_after_checkout = [
  gha.git_safedir,
  {
    name: 'Build semgrep-core',
    run: |||
      eval $(opam env)
      make install-deps-ALPINE
      make install-deps
      make core
    |||,
  },
  {
    name: 'Install osemgrep',
    run: |||
      eval $(opam env)
      make copy-core-for-cli
    |||,
  },
   // For '--ignore-installed distlib' below see
   // https://stackoverflow.com/questions/63515454/why-does-pip3-install-pipenv-give-error-error-cannot-uninstall-distlib
  //
  {
    name: 'Install Python dependencies',
    run: |||
      apk add --no-cache python3
      pip install --no-cache-dir --ignore-installed distlib pipenv==%s
      (cd cli; pipenv install --dev)
    ||| % actions.pipenv_version,
  },
  {
    name: 'Run pytest for osemgrep known passing tests',
    'working-directory': 'cli',
    run: |||
      git config --global --add safe.directory "$(pwd)"
      make osempass
    |||,
  },
];

local setup_nix_step = [
  {
    name: "Set up Nix",
    uses: "DeterminateSystems/nix-installer-action@main",
    with: {
      // need to pin until
      // https://github.com/DeterminateSystems/nix-installer-action/issues/133
      // is fixed
      "source-tag": "v0.32.3",
        // pysemgrep and osemgrep have networking tests that rely on the
        // actual internet (i.e. semgrep.dev). When sandbox=false nix builds
        // everything fine, but all networking tests fail. So we set sandbox
        // to false here so networking tests succeed
        //
        // TODO: disable networking tests for nix? that would be the nix way
        // of doing things

        // extra substituters and public keys use https://app.cachix.org/cache/semgrep
        // to cache the build dependencies!
        "extra-conf": "sandbox = false",
    },
  },
  // This will automatically install cachix and upload to cachix
  {
      name: "Install Cachix",
      uses: "cachix/cachix-action@v14",
      with: {
          name: "semgrep",
          authToken: "${{ secrets.CACHIX_AUTH_TOKEN }}",
      },
  }
];

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
        // This seems to be a semgrep specific magic number
        'role-to-assume': 'arn:aws:iam::338683922796:role/%s' % role,
        'role-duration-seconds': 900,
        'role-session-name': session_name,
        'aws-region': 'us-west-2',
      },
    },
  // See https://depot.dev/orgs/9ks3jwp44z/projects/fhmxj6w9z8/settings
  depot_project_id: 'fhmxj6w9z8',
  opam_switch: opam_switch,
  opam_setup: opam_setup,
  // coupling: cli/setup.py, the matrix in run-cli-tests.libsonnet,
  // build-test-manylinux-x86.jsonnet in pro, tests.jsonnet in OSS
  // TODO? could switch to higher like 3.11
  default_python_version: '3.9',
  containers: containers,

  github_bot: github_bot,
  cache_opam: cache_opam,
  slack: slack,

  stable_ubuntu_version_for_setup_ocaml: stable_ubuntu_version_for_setup_ocaml,

  // Reusable sequences of test steps
  osemgrep_test_steps_after_checkout: osemgrep_test_steps_after_checkout,
  setup_nix_step: setup_nix_step,
}
