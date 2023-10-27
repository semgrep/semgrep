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
    steps: [
      semgrep.github_bot.get_jwt_step,
      semgrep.github_bot.get_token_step,
      actions.checkout_with_submodules(),
/* TODO
      {
        name: 'Setup OCaml and opam',
        uses: 'ocaml/setup-ocaml@v2',
        with: {
          'ocaml-compiler': '5.1.x',
        },
      },
      # old: make -C semgrep install-deps-ALPINE-for-semgrep-core
      # but we're on ubuntu here, not alpine, and it seems like setup-ocaml
      # is able to infer the dependencies to install by inspecting
      # the semgrep-server.opam file!
      #
      # TODO: use a actions/cache@ with the semgrep-server.opam.locked
      # as a key? note that setup-ocaml is already using some caching
      # mechanism, so it's maybe not needed.
      {
        name: 'Install dependencies',
        run: |||
          eval $(opam env)
          make -C semgrep install-deps-for-semgrep-core
          make install-deps
        |||,
      },
*/
      {
        env: semgrep.github_bot.github_token,
        name: 'checkout semgrep-pro',
        run: |||
          pwd
          ls
          git clone git@github.com:returntocorp/semgrep-proprietary.git
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
