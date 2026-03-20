// Factorize Semgrep-specific settings.

// ----------------------------------------------------------------------------
// Helpers to be able to use 'gh' (and trigger PRs) from a workflow
// ----------------------------------------------------------------------------

local uses = import './uses.libsonnet';
local actions = import 'actions.libsonnet';
local gha = import 'gha.libsonnet';

local github_bot = {
  get_token_steps: [
    {
      name: 'Get JWT for semgrep-ci GitHub App',
      id: 'jwt',
      uses: 'docker://public.ecr.aws/y9k7q4m1/devops/cicd:latest',
      env: {
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
      env: {
        SEMGREP_CI_APP_INSTALLATION_ID: '${{ secrets.SEMGREP_CI_APP_INSTALLATION_ID }}',
        JWT: '${{ steps.jwt.outputs.jwt }}',
      },
      run: |||
        TOKEN="$(curl -X POST \
        -H "Authorization: Bearer $JWT" \
        -H "Accept: application/vnd.github.v3+json" \
        "https://api.github.com/app/installations/${SEMGREP_CI_APP_INSTALLATION_ID}/access_tokens" | \
        jq -r .token)"
        echo "::add-mask::$TOKEN"
        echo "token=$TOKEN" >> $GITHUB_OUTPUT
      |||,
    },
  ],
  // Token computed in get_token_steps to be used in the caller
  token_ref: '${{ steps.token.outputs.token }}',
};

// ----------------------------------------------------------------------------
// Containers
// ----------------------------------------------------------------------------

// default one
local opam_switch = 'ocaml-variants.5.3.0+options,ocaml-option-flambda';
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
      container: 'alpine:3.23',
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
            run: 'apk add --no-cache git git-lfs bash curl build-base make bubblewrap rsync',
          },
        ] + steps,
    },
  },
};


// Escape all the inner double quotes of a string so that
// it could be embedded in a JSON string.
local escapeStringJson = function(str)
  std.lstripChars(
    std.rstripChars(
      std.escapeStringJson(str),
      '"'
    ),
    '"'
  );

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

// Double escape quotes because they are nested in two layers of double quotes. Which still allows string interpolation at the bash level.
local curl_notify(message) = |||
  curl --request POST \
   --url  "$NOTIFICATIONS_URL"  \
   --header 'content-type: application/json' \
   --data "{
     \"text\": \"%s\"
   }"
||| % escapeStringJson(escapeStringJson(message));

local slack = {
  // This will post on Slack on the #semgrep-cli-release channel from a
  // 'gha-notification' user.
  // The actual URL secret is stored in 1password in our Engineering vault
  // (look for "slack webhook") and configured by #team-techops to post to
  // #semgrep-cli-release at
  // https://semgrepinc.slack.com/apps/A0F7XDUAZ-incoming-webhooks?tab=settings&next_id=0

  notify_success_job(message, env={}):
    (if env == {} then {} else { env: env }) +
    {
      'runs-on': 'ubuntu-22.04',
      'if': 'success()',
      steps: [
        {
          env: { NOTIFICATIONS_URL: '${{ secrets.NOTIFICATIONS_URL }}' },
          run: curl_notify(message),
        },
      ],
    },
  notify_failure_job(message, env={}):
    (if env == {} then {} else { env: env }) +
    {
      'runs-on': 'ubuntu-22.04',
      'if': 'failure()',
      steps: [
        {
          env: { NOTIFICATIONS_URL: '${{ secrets.NOTIFICATIONS_URL}}' },
          run: curl_notify(message),
        },
      ],
    },
};


// This is the version of the cache we use below. If you need to invalidate it
// for some reason then bump this.
//
// NOTE: When the compiler fork SHA is bumped, it's very common that you will
// observe failures in our pipelines that use setup-ocaml's cache.
//
// coupling: if you modify the compiler pin sha you will need to bump this
// prefix (or similar) to invalidate the cache
local opam_cache_version = 'v5';

// this must be done after the checkout as opam installs itself
// locally in the project folder (/home/runner/work/semgrep/semgrep/_opam)
// TODO upstream the changes in austin's custom setup-ocaml action,
// or move the project to the semgrep org
// coupling: default is above opam_switch
local opam_setup = function(opam_switch=opam_switch_default) {
  uses: uses.semgrep.setup_ocaml,
  with: {
    'ocaml-compiler': opam_switch,
    'opam-pin': false,
    // Save the cache post run instead of after installing the compiler
    'save-opam-post-run': true,
    'cache-prefix': opam_cache_version,
  },
};

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
  {
    name: 'Install Python dependencies',
    run: |||
      apk add --no-cache python3 py3-pip
      pip install --no-cache-dir --ignore-installed --break-system-packages distlib uv==%s
      (cd cli; uv sync)
    ||| % actions.default_uv_version,
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
    name: 'Set up Nix',
    uses: uses.DeterminateSystems.nix_installer_action,
    with: {
      // pin for more stability
      // this is just the version of
      // https://github.com/DeterminateSystems/nix-installer NOT the nix version
      // itself
      'source-tag': 'v3.4.2',
      // pysemgrep and osemgrep have networking tests that rely on the
      // actual internet (i.e. semgrep.dev). When sandbox=false nix builds
      // everything fine, but all networking tests fail. So we set sandbox
      // to false here so networking tests succeed
      //
      // TODO: disable networking tests for nix? that would be the nix way
      // of doing things

      // extra substituters and public keys use https://app.cachix.org/cache/semgrep
      // to cache the build dependencies!
      'extra-conf': 'sandbox = false',
    },
  },
  {
    name: 'Print nix version',
    run: 'nix --version',
  },
  // This will automatically install cachix and upload to cachix
  {
    name: 'Install Cachix',
    uses: uses.cachix.cachix_action,
    'continue-on-error': true,
    with: {
      name: 'semgrep',
      authToken: '${{ secrets.CACHIX_AUTH_TOKEN }}',
    },
  },
];


local build_test_steps(opam_switch=opam_switch_default, name='semgrep-core', time=false) = [
  opam_setup(opam_switch),
  {
    name: 'Install dependencies',
    run: 'opam exec -- make install-deps',
  },
  {
    name: 'Build %s' % name,
    run: 'opam exec -- make',
  },
] + (if time then [
       {
         name: 'Test %s (and time it)' % name,
         run: |||
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
       },
     ] else [
       {
         name: 'Test %s' % name,
         run: 'opam exec -- make test',
       },
     ]);

local is_windows_arch(arch) = std.findSubstr('windows', arch) != [];
local bin_ext(arch) = if is_windows_arch(arch) then '.exe' else '';
local archive_ext(arch) = if is_windows_arch(arch) then '.tgz' else '.zip';
local wheel_name(arch, pro=false) = 'wheel-%s%s' % [arch, if pro then '-pro' else ''];

//TODO always want to include semgrep pro ...
local build_wheel_steps(arch, copy_semgrep_pro=false) =
  [
    actions.setup_python_step(),
    {
      name: 'Untar artifacts',
      run: |||
        tar xvfz artifacts.tgz
      |||,
    },
  ] +
  (if !copy_semgrep_pro then [{
     name: 'Remove pro binary',
     run: '(rm artifacts/semgrep-core-proprietary%s && rm artifacts/pro-installed-by.txt) || true' % bin_ext(arch),
   }] else []) +
  [
    {
      name: 'Copy artifacts to wheel',
      run: 'cp -LR artifacts/* cli/src/semgrep/bin',
    },
    {
      name: 'Clean up old artifacts',
      run: 'rm -rf artifacts artifacts.tgz',
    },
    {
      name: 'Build wheel',
      run: './scripts/build-wheels.sh',
    },
    actions.make_artifact_step('cli/dist%s' % archive_ext(arch)),
    actions.upload_artifact_step(wheel_name(arch, pro=copy_semgrep_pro)),
  ];

local unpack_wheel_steps = [

  {
    name: 'Unpack artifact',
    run: 'tar xzvf artifacts.tgz',
  },
  {
    name: 'Unpack wheel',
    run: 'tar --wildcards -xzf ./artifacts/dist.tgz "*.whl" || unzip ./artifacts/dist.zip "*.whl"',
  },
];

// Only retags the SMS image, we have to do this via ecr
local retag_sms_docker_image_step(version, tag, dry_run=false) = {
  name: 'Retag SMS docker image from %s to %s' % [version, tag],
  env: {
    GITHUB_TOKEN: github_bot.token_ref,
    SEMGREP_VERSION: version,
    SEMGREP_TAG: tag,
    DRY_RUN: dry_run,
  },
  // TODO Factor out gh workflow run XYZ
  run: |||
    # append dry-run to the tag if needed
    if [ "$DRY_RUN" == "true" ]; then
      SEMGREP_TAG="${SEMGREP_TAG}-dry-run"
    fi
    echo "Tagging SMS docker image semgrep-app/zcs-agent from $SEMGREP_VERSION to $SEMGREP_TAG"
    gh workflow run tag-sms-image.yml --repo semgrep/semgrep-app --raw-field version="$SEMGREP_VERSION" --raw-field tag="$SEMGREP_TAG"
  |||,
};

local trigger_build_sms_docker_image_step(tag, use_nightly_repo='false') = {
  name: 'Trigger build SMS docker image from %s' % tag,
  env: {
    GITHUB_TOKEN: github_bot.token_ref,
    SEMGREP_TAG: tag,
  },
  // TODO Factor out gh workflow run XYZ with above
  run: 'gh workflow run build-sms-image.yml --repo semgrep/semgrep-app --raw-field use_nightly_repo=%s --raw-field version="$SEMGREP_TAG"' % use_nightly_repo,
};

local test_wheel_steps(arch, copy_semgrep_pro=false) = [
  // caching is hard and why complicate things
  actions.setup_python_step(cache=false),
  actions.download_artifact_step(wheel_name(arch, pro=copy_semgrep_pro)),
] + unpack_wheel_steps + [
  {
    name: 'install package',
    run: 'uv venv && uv pip install dist/*.whl',
  },
  {
    run: 'uv run semgrep --version',
  },
  {
    name: 'e2e semgrep-core test',
    run: "echo '1 == 1' | uv run semgrep -l python -e '$X == $X' --strict -",
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
    uses: uses.aws_actions.configure_aws_credentials,
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
  unpack_wheel_steps: unpack_wheel_steps,
  retag_sms_docker_image_step: retag_sms_docker_image_step,
  trigger_build_sms_docker_image_step: trigger_build_sms_docker_image_step,
  wheel_name: wheel_name,
  // coupling: cli/setup.py, the matrix in run-cli-tests.libsonnet,
  // coupling: if you change this field, also specify a specific patch version
  // for the default_python_patch_version field below!
  // build-test-manylinux-x86.jsonnet in pro, tests.jsonnet in OSS
  // TODO? could switch to higher like 3.11
  default_python_version: '3.10',
  // this is the patch version, the last part of the version string
  // this is important when we need to know the specific version
  // to install, e.g. `3.10.5`, which we will create from this field
  // and the `default_python_version` field
  default_python_patch_version: '5',
  python_version: '3.12',
  containers: containers,

  github_bot: github_bot,
  slack: slack,

  // Reusable sequences of test steps
  osemgrep_test_steps_after_checkout: osemgrep_test_steps_after_checkout,
  setup_nix_step: setup_nix_step,
}
