// This workflow checks whether the current PR can break
// the compilation of semgrep-pro if semgrep-pro was using
// this semgrep as a submodule

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local check_compile_semgrep_pro_job = {
  'runs-on': 'ubuntu-latest',
  // Switching to ubuntu here because alpine does not offer gh
  // which is needed to checkout semgrep-pro from semgrep GHA.
  // alt: use alpine and procedure to download gh binary tarball
  // alt: use setup-ocaml@v2, but seems fragile when you move
  //  around folders
    container: 'returntocorp/ocaml:alpine-2023-10-17',
    // We need this hack because GHA tampers with the HOME in container
    // and this does not play well with 'opam' installed in /root
    env: {
      HOME: '/root',
    },
    steps: [
      actions.checkout_with_submodules(),
//      {
//        run: 'apt-get install gh'
//      },
      # old: make -C semgrep install-deps-ALPINE-for-semgrep-core
      # but we're on ubuntu here and most packages are already installed
      {
        name: 'Test',
        run: |||
	  pwd
	  ls
	  opam switch
          make install-deps-ALPINE-for-semgrep-core
          make install-deps-for-semgrep-core
          make install-deps
        |||,
      },

      semgrep.github_bot.get_jwt_step,
      semgrep.github_bot.get_token_step,
      {
        env: semgrep.github_bot.github_token,
        name: 'Checkout semgrep-pro',
        run: |||
	  cd ..
          gh repo clone returntocorp/semgrep-proprietary
	  mv semgrep semgrep-proprietary/
	  # GHA post cleanup requires /home/runner/work/semgrep/semgrep to still exist
	  mv semgrep-proprietary semgrep
        |||,
        },
      {
        name: 'Install pro dependencies',
        run: |||
          eval $(opam env)
          make install-deps
        |||,
      },

      {
        name: 'compile semgrep-pro',
        run: |||
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
