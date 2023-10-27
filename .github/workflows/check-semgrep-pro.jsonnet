// This workflow checks whether the current PR can break
// the compilation of semgrep-pro if semgrep-pro was using
// this semgrep as a submodule

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local check_compile_semgrep_pro_job =
  semgrep.ocaml_alpine_container {
    steps: [
      actions.checkout_with_submodules(),
      {
        name: 'checkout semgrep-pro',
        run: |||
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
