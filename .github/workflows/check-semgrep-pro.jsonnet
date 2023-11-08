// This workflow checks whether the current semgrep PR can break the
// compilation of semgrep-pro if semgrep-pro was using this semgrep
// branch as a submodule.

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local check_compile_semgrep_pro_job = {
  'runs-on': 'ubuntu-latest',
  // Switching to Ubuntu here because Alpine does not provide easily 'gh'
  // which is needed to checkout semgrep-pro from semgrep GHA.
  // alt: use our r2c alpine-ocaml and procedure to download gh binary tarball
  // alt: use our r2c ubuntu-ocaml but get 'git ls-files exit 128' error under
  //      GHA that I could not reproduce locally in docker or in circleCI
  // alt: use circleCI but then even with a GH token and with gh I could not
  //      clone semgrep-pro
  // alt: use setup-ocaml@v2, but need to be careful when moving around dirs
  //      as opam installs itself in /home/runner/work/semgrep/semgrep/_opam
  //      and opam can work only when run from this directory
  steps: [
    actions.checkout_with_submodules(),
    // this must be done after the checkout as opam installs itself
    // locally in the project folder (/home/runner/work/semgrep/semgrep/_opam)
    {
      name: 'Setup OCaml and opam',
      uses: 'ocaml/setup-ocaml@v2',
      with: {
        'ocaml-compiler': '4.14.x',
      },
    },
    // alt: call 'sudo make install-deps-UBUNTU-for-semgrep-core'
    // but looks like opam and setup-ocaml@ can automatically install
    // depext dependencies.
    {
      name: 'Install semgrep dependencies',
      run: |||
        eval $(opam env)
        make setup
      |||,
    },
    // Let's use gh and our github_bot token to access a private repo
    {
      run: 'sudo apt-get install gh',
    },
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
    {
      env: semgrep.github_bot.github_token,
      name: 'Checkout semgrep-pro',
      // We are in /home/runner/work/semgrep/semgrep at this point
      // and we must keep it that way otherwise GHA will complain
      // in post-cleanup if this directory does not exist anymore.
      run: |||
        cd ..
        gh repo clone semgrep/semgrep-proprietary
        cd semgrep-proprietary
        make setup
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
      name: 'Compile semgrep-pro',
      run: |||
        cd ../semgrep-proprietary
        eval $(opam env)
        make
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'check-semgrep-pro',
  on: {
    workflow_dispatch: null,
    pull_request: null,
    push: {
      branches: [
        'develop',
      ],
    },
  },
  jobs: {
    'check-compile-semgrep-pro': check_compile_semgrep_pro_job,
  },
}
