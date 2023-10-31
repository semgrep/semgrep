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
  // Switching to ubuntu here because alpine does not offer easily 'gh'
  // which is needed to checkout semgrep-pro from semgrep GHA.
  // alt: use our r2c alpine-ocaml and procedure to download gh binary tarball
  // alt: use our r2c ubuntu-ocaml but get 'git ls-files exit 128' error under
  //      GHA that I could not reproduce locally or in circleCI
  // alt: use circleCI but then even with token I can't clone semgrep-pro
  // alt: use setup-ocaml@v2, but seems fragile when you move
  //  around folders

    steps: [
    // actions.checkout_with_submodules(),
    {
      uses: 'actions/checkout@v3',
      with: {
        submodules: true,
      },
    },
      {
	name: 'Setup OCaml and opam',
	uses: 'ocaml/setup-ocaml@v2',
	with: {
	  'ocaml-compiler': '4.14.x',
	},
      },
      # old: make -C semgrep install-deps-ALPINE-for-semgrep-core
      # but we're on ubuntu here and most packages are already installed
      {
        name: 'Install dependencies',
        run: |||
	  eval $(opam env)
	  pwd
	  ls
	  opam switch
	  set
          make install-deps-for-semgrep-core
          make install-deps
        |||,
      },

      {
        run: 'apt-get install gh'
      },
      //semgrep.github_bot.get_jwt_step,
      //semgrep.github_bot.get_token_step,
      //{
      //  env: semgrep.github_bot.github_token,
      //  name: 'Checkout semgrep-pro',
      //  run: |||
      //	  cd ..
      //    gh repo clone returntocorp/semgrep-proprietary
      //	  mv semgrep semgrep-proprietary/
      //	  # GHA post cleanup requires /home/runner/work/semgrep/semgrep to still exist
      //	  mv semgrep-proprietary semgrep
      //  |||,
      //  },
      //{
      //  name: 'Install pro dependencies',
      //  run: |||
      //    eval $(opam env)
      //    make install-deps
      //  |||,
      //},

      //{
      //  name: 'compile semgrep-pro',
      //  run: |||
      //    eval $(opam env)
      //    make
      //  |||,
      //  },
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
