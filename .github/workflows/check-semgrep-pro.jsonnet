// This workflow checks whether the current semgrep PR can break the
// compilation and tests of semgrep-pro if semgrep-pro was using this
// semgrep branch as a submodule.

local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job = {
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
        make install-deps-for-semgrep-core
        make install-deps
      |||,
    },
    // Let's use gh and our github_bot token to access a private repo
    {
      run: 'sudo apt-get install gh',
    },
    semgrep.github_bot.get_jwt_step,
    semgrep.github_bot.get_token_step,
    {
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
    // setup-ocaml@ installs opam in a local folder per project,
    // not in a global ~/.opam/, so here we reuse the same _opam
    // in the semgrep-pro otherwise opam commands would fail
    // with 'no opam switch set'
    {
      name: 'Ugly hack for setup-ocaml',
      run: |||
        cd ../semgrep-proprietary
        ln -s ../semgrep/_opam
      |||,
    },
    {
      name: 'Install semgrep-pro dependencies',
      run: |||
        cd ../semgrep-proprietary
        eval $(opam env)
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
      name: 'Test semgrep-pro',
      run: |||
        cd ../semgrep-proprietary
        eval $(opam env)
        make test
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'check-semgrep-pro',
  on: gha.on_classic,
  jobs: {
    job: job,
  },
}
