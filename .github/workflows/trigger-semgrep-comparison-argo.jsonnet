// This workflow triggers the `semgrep-comparison` Argo Workflow, which will
// create a GitHub Check on this pull request (or specified branch if running
// manually).
// The `semgrep-comparison` workflow compares two Semgrep versions for timing
// differences.

local util = import 'libs/util.libsonnet';

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'trigger-semgrep-comparison-argo',
  on: {
    workflow_call: {},
    workflow_dispatch: {
      inputs: {
        branch: {
          required: true,
          type: 'string',
          description: 'The branch to compare against develop',
        },
      },
    },
  },
  jobs: {
    'setup-docker-tag': util.setup_docker_tag(),
    'get-sha': util.get_sha(),
    'trigger-semgrep-comparison-argo-workflow': util.trigger_argo_workflow(
      'https://argoworkflows-dev2.corp.semgrep.dev/api/v1/events/security-research/semgrep-compare',
      [
        { name: 'ruleset', value: 'p/default-v2' },
        { name: 'container_image_base', value: "${{ needs.setup-docker-tag.outputs.docker-tag == 'develop' && 'latest' || 'develop' }}" },
        { name: 'container_image_development', value: '${{ needs.setup-docker-tag.outputs.docker-tag }}' },
        // up to 17 minutes because the reaper task that watches this timeout runs every 5 minutes.
        { name: 'timeout_in_minutes', value: '12' },
        // for PR comments
        { name: 'issue_number', value: '${{ github.event.pull_request.number }}' },
      ],
    ),
  },
}
