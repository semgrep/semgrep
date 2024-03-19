// This workflow checks whether the current semgrep PR can break the
// compilation and tests of semgrep-pro if semgrep-pro was using this
// semgrep branch as a submodule. It also checks whether this PR
// break Pro rules (rules from the semgrep-rules-proprietary repo).

local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// exported for other workflows
local artifact_name = 'semgrep-core-pro-x86-artifact';

// We're using our classic alpine-based container here. At some point
// we were using setup-ocaml@v2, because we had trouble getting 'gh'
// under our ocaml-layer based alpine, but this got fixed, and we
// need now an alpine-based container to statically build
// the semgrep-core-pro artifact which is used in other workflow and
// we don't want those other workflow to have to install tree-sitter
// runtime libraries, so simpler to build a static binary.
//
// alt: use setup-ocaml@v2, but need to be careful when moving around dirs
//      as opam installs itself in /home/runner/work/semgrep/semgrep/_opam
//      and opam can work only when run from this directory
//      morever, we can't build a static binary which is annoying for the
//      other workflow importing the exported semgrep-core-pro binary
// alt: use our r2c ubuntu-ocaml but get 'git ls-files exit 128' error under
//      GHA that I could not reproduce locally in docker or in circleCI
//      Update 2024-03-05: this is likely a safe.directory setting.
// alt: use circleCI but then even with a GH token and with gh I could not
//      clone semgrep-pro
local container = semgrep.containers.ocaml_alpine;

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job = container.job {
  steps: [
    {
      name: 'Install required alpine packages',
      run: |||
        # Needed by github bot to parse json results from github's endpoint.
        apk add jq
        # Needed for gh commands.
        apk add github-cli
        # Needed for large files in semgrep-proprietary.
        apk add git-lfs
      |||,
    },
  ] + semgrep.github_bot.get_token_steps + [
    gha.speedy_checkout_step,
    actions.checkout_with_submodules(),
    gha.git_safedir,
    semgrep.cache_opam.step(
      key=container.opam_switch + "-${{hashFiles('semgrep.opam')}}"),
    {
      name: 'Install semgrep dependencies',
      run: |||
        eval $(opam env)
        make install-deps-ALPINE-for-semgrep-core
        make install-deps-for-semgrep-core
        make install-deps
      |||,
    },
    {
      # Needed for access to private repo.
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      name: 'Checkout semgrep-pro',
      // We are in /home/runner/work/semgrep/semgrep at this point
      // and we must keep it that way otherwise GHA will complain
      // in post-cleanup if this directory does not exist anymore.
      run: |||
        cd ..
        gh repo clone semgrep/semgrep-proprietary
        cd semgrep-proprietary
        git submodule update --init
      |||,
    },
    {
      name: 'Adjust semgrep-pro to use the semgrep in this PR',
      run: |||
        cd ../semgrep-proprietary
        rm -rf semgrep
        ln -s ../semgrep
      |||,
    },
    {
      name: 'Install semgrep-pro dependencies',
      run: |||
        cd ../semgrep-proprietary
        eval $(opam env)
        make install-deps-ALPINE
        make install-deps
      |||,
    },

    {
      name: 'Compile semgrep-pro',
      run: |||
        cd ../semgrep-proprietary
        eval $(opam env)
        make
      |||,
    },
    // alt: use semgrep.make_artifact_step but here we store 2 binaries
    {
      name: 'Make artifact',
      run: |||
        mkdir artifacts
        cp ../semgrep-proprietary/bin/semgrep-core artifacts/
        cp ../semgrep-proprietary/bin/semgrep-core-proprietary artifacts/
        tar czf artifacts.tgz artifacts/
      |||,
    },
    actions.upload_artifact_step(artifact_name),
    {
      name: 'Test semgrep-pro',
      run: |||
        cd ../semgrep-proprietary
        eval $(opam env)
        make test
      |||,
    },

    // We are in /home/runner/work/semgrep/semgrep at this point.
    {
      env: {
        GITHUB_TOKEN: semgrep.github_bot.token_ref,
      },
      name: 'Checkout Pro rules',
      run: |||
        cd ..
        gh repo clone semgrep/semgrep-rules-proprietary
        cd semgrep-rules-proprietary
        git submodule update --init
      |||,
    },

    {
      name: 'Test Pro rules',
      run: |||
        cd ../semgrep-rules-proprietary/paid
        # This rule is missing a target file
        rm -f kotlin/ktor/active-debug-code/ktor-development-mode-yaml.yaml
        # This is much faster than `pysemgrep --test` and it's also stricter.
        # TODO: Replace with `osemgrep-pro test` when that is ready.
        ../../semgrep-proprietary/bin/semgrep-core-proprietary -test_rules .
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'check-semgrep-pro',
  // on_classic so that it's triggered on PRs
  // on_dispatch_or_call so its artifacts can be used by another workflow
  on: gha.on_classic + gha.on_dispatch_or_call,
  jobs: {
    job: job,
  },
  // to be reused by other workflows
  export:: {
    artifact_name: artifact_name,
  },
}
