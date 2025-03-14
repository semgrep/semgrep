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
// Containers
// ----------------------------------------------------------------------------

// default one
// coupling: with containers above
local opam_switch = '5.2.1';
// also default but needed by another nameso we can use it as a function default arg
local opam_switch_default = opam_switch;
local containers = {
  ocaml_alpine: {
    // used in the build-test-osx-xxx jobs but ideally we should get rid
    // of it and rely on opam.lock for caching issues
    opam_switch: opam_switch,
    job(steps): {
      'runs-on': 'ubuntu-latest',
      // coupling: if you change this you must change the dockerfile alpine
      // version
      container: 'alpine:3.21',
      // We need this hack because GHA tampers with the HOME in container
      // and this does not play well with 'opam' installed in /root
      env: {
        HOME: '/root',
      },
      steps:
        [
          {
            name: 'setup alpine',
            // needed for ocaml deps
            run: 'apk add --no-cache git git-lfs bash curl'
          },
        ] + steps,
     },
  },
  // ocaml-layer builds an image based on Alpine and another one based on
  // Ubuntu.
  // Alpine is necessary in practice for static linking (especially for C++
  // libraries). Ubuntu is an alternative Linux distribution people may be
  // more familiar with. It's been cheap to maintain both so far but we could
  // decide to keep just one if it makes things simpler.
  ocaml_ubuntu: {
    opam_switch: opam_switch,
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


// This is the version of the cache we use below. If you need to invalidate it
// for some reason then bump this.
local opam_cache_version = "v1";

// this must be done after the checkout as opam installs itself
// locally in the project folder (/home/runner/work/semgrep/semgrep/_opam)
// TODO upstream the changes in austin's custom setup-ocaml action,
// or move the project to the semgrep org
// coupling: default is above opam_switch
local opam_setup = function(opam_switch=opam_switch_default, cache_deps=["semgrep.opam"]) {
      uses: 'semgrep/setup-ocaml@latest',
      with: {
        'ocaml-compiler': opam_switch,
	      'opam-pin': false,
        # Save the cache post run instead of after installing the compiler
        'save-opam-post-run': true,
        'cache-prefix': '%s-${{hashFiles(\'%s\')}}' % [opam_cache_version, std.join('\', \'', cache_deps)],
      },
    };

// We can't use ubuntu-latest (currently 24.04) just yet until
// https://github.com/ocaml/setup-ocaml/issues/872
// is fixed.
local stable_ubuntu_version_for_setup_ocaml = 'ubuntu-22.04';

local osemgrep_test_steps_after_checkout = [
  {
    name: 'Build semgrep-core',
    run: |||
      eval $(opam env)
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
      apk add --no-cache python3 py3-pip
      pip install --no-cache-dir --ignore-installed --break-system-packages distlib pipenv==%s
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
    uses: "DeterminateSystems/nix-installer-action@v16",
    with: {
      // pin for more stability
      "source-tag": "v0.34.0",
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


local build_test_steps(opam_switch=opam_switch_default, cache_deps=['semgrep.opam'], name='semgrep-core', time=false) = [
    opam_setup(opam_switch, cache_deps=cache_deps),
    {
      name: 'Install dependencies',
      run: "opam exec -- make install-deps",
    },
    {
      name: 'Build %s' % name,
      run: "opam exec -- make",
    },
] + (if time then [
    {
      name: 'Test %s (and time it)' % name,
      run: |||
        # Small hack to get deps to work fine, will remove in follow up pr
        apk add --no-cache python3 py3-pip
        pip install --no-cache-dir --ignore-installed --break-system-packages check-jsonschema
        eval $(opam env)
        START=`date +%s`
        opam exec -- make test
        opam exec -- make core-test-e2e

        END=`date +%s`
        TEST_RUN_TIME=$((END-START))
        curl --fail -L -X POST "https://dashboard.semgrep.dev/api/metric/semgrep.core.test-run-time-seconds.num" -d "$TEST_RUN_TIME"
      |||,
    },
    {
      name: 'Report Number of Tests Stats',
      'if': "github.ref == 'refs/heads/develop'",
      run: './scripts/report_test_metrics.sh',
    }
] else [
    {
      name: 'Test %s' % name,
      run: 'opam exec -- make test',
    }
  ]);

local wheel_name(arch) = '%s-wheel'% arch;

local build_wheel_steps(arch, platform) = [
    actions.setup_python_step(cache='pip'),
    {
      run: |||
        tar xvfz artifacts.tgz
        cp artifacts/semgrep-core cli/src/semgrep/bin
        ./scripts/build-wheels.sh --plat-name %s
      ||| % platform,
    },
    actions.upload_artifact_step(wheel_name(arch), path='cli/dist.zip'),
];

local test_wheel_steps(arch) = [
    // caching is hard and why complicate things
    actions.setup_python_step(cache=false),
    actions.download_artifact_step(wheel_name(arch)),
    {
      run: 'unzip dist.zip',
    },
    {
      name: 'install package',
      run: 'pip3 install dist/*.whl',
    },
    {
      run: 'semgrep --version',
    },
    {
      name: 'e2e semgrep-core test',
      run: "echo '1 == 1' | semgrep -l python -e '$X == $X' -",
    },

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
  build_test_steps: build_test_steps,
  build_wheel_steps: build_wheel_steps,
  test_wheel_steps: test_wheel_steps,
  // coupling: cli/setup.py, the matrix in run-cli-tests.libsonnet,
  // build-test-manylinux-x86.jsonnet in pro, tests.jsonnet in OSS
  // TODO? could switch to higher like 3.11
  default_python_version: '3.9',
  python_version: '3.12',
  containers: containers,

  github_bot: github_bot,
  slack: slack,

  stable_ubuntu_version_for_setup_ocaml: stable_ubuntu_version_for_setup_ocaml,

  // Reusable sequences of test steps
  osemgrep_test_steps_after_checkout: osemgrep_test_steps_after_checkout,
  setup_nix_step: setup_nix_step,
}
