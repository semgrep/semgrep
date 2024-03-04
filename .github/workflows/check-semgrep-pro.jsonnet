// This workflow checks whether the current semgrep PR can break the
// compilation and tests of semgrep-pro if semgrep-pro was using this
// semgrep branch as a submodule. It also checks whether this PR
// break Pro rules (rules from the semgrep-rules-proprietary repo).

local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// exported for other workflows
local artifact_name = 'pro-ocaml-build-artifacts-release';

local container = semgrep.containers.ocaml_alpine;

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job = container.job {
  // The ocaml_alpine container we are using here has opam already
  // installed, as well as an opam switch already created, and a big set of
  // packages already installed. Thus, the
  // 'make install-deps-ALPINE-for-semgrep-core' below is very fast and almost a
  // noop.
  // As of now, we are unable to use setup-ocaml@v2 because it uses apt-get
  // which is not available on alpine.
  // alt: use our r2c ubuntu-ocaml but get 'git ls-files exit 128' error under
  //      GHA that I could not reproduce locally in docker or in circleCI
  //      Update 2024-03-05: this is likely a safe.directory setting.
  // alt: use circleCI but then even with a GH token and with gh I could not
  //      clone semgrep-pro
  // alt: use setup-ocaml@v2, but need to be careful when moving around dirs
  //      as opam installs itself in /home/runner/work/semgrep/semgrep/_opam
  //      and opam can work only when run from this directory
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

    {
      name: 'Make artifact',
      run: |||
        mkdir -p ocaml-build-artifacts/bin
        cp ../semgrep-proprietary/bin/semgrep-core ocaml-build-artifacts/bin/
        cp ../semgrep-proprietary/bin/semgrep-core-proprietary ocaml-build-artifacts/bin/
        tar czf ocaml-build-artifacts.tgz ocaml-build-artifacts
      |||,
    },

    {
      uses: 'actions/upload-artifact@v3',
      with: {
        path: 'ocaml-build-artifacts.tgz',
        name: artifact_name,
      },
    },

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
