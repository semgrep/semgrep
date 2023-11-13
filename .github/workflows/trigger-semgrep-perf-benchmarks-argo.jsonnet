local util = import 'libs/util.libsonnet';
// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'trigger-semgrep-perf-benchmarks-argo',
  on: {
    workflow_dispatch: {
      inputs: {
        branch: {
          required: true,
          type: 'string',
          description: 'The branch to attach this check run to',
        },
      },
    },
    workflow_call: {},
  },
  jobs: {
    'setup-docker-tag': util.setup_docker_tag(),
    'get-sha': util.get_sha(),
    'trigger-semgrep-perf-benchmarks-argo': util.trigger_argo_workflow(
      "https://argoworkflows-dev2.corp.r2c.dev/api/v1/events/security-research/semgrep-perf-benchmarks",
      [
        {"name": "image_tag", "value": "${{ needs.setup-docker-tag.outputs.docker-tag }}"},
      ],
    ),
  }
}
