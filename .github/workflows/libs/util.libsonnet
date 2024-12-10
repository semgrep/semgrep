// Factorize utility jobs or steps for repeated use across multiple workflows

{
  setup_docker_tag: function() {
    name: 'Reconstruct Docker tag created by docker/metadata-action',
    'runs-on': 'ubuntu-22.04',
    outputs: {
      'docker-tag': '${{ steps.setup-docker-tag.outputs.docker-tag }}'
    },
    steps: [{
      id: 'setup-docker-tag',
      run: |||
        echo "Github event is ${{ github.event_name }}"
        if [ "${{ github.repository }}" = "semgrep/semgrep-proprietary" ]; then
          PRO_PREFIX="pro-"
        else
          PRO_PREFIX=""
        fi
        if [ "${{ github.event_name }}" = "pull_request" ]; then
          echo "docker-tag=${PRO_PREFIX}pr-${{ github.event.pull_request.number }}" >> "$GITHUB_OUTPUT"
          echo "Setting docker tag to current pull request number"
        else
          echo "docker-tag=${PRO_PREFIX}develop" >> "$GITHUB_OUTPUT"
          echo "Setting dry-run to develop"
        fi
      |||,
    }],
  },
  get_sha: function() {
    name: 'Get SHA of branch head for attaching Argo Workflow checks',
    'runs-on': 'ubuntu-22.04',
    outputs: {
      sha: '${{ steps.get-sha.outputs.sha }}',
    },
    steps: [
      {
        uses: 'actions/checkout@v3',
      },
      {
        id: 'get-sha',
        env: {
          BRANCH: '${{ github.event.inputs.branch }}',
        },
        run: |||
          echo "Branch: $BRANCH"
          if [ "${{ github.event_name }}" = "pull_request" ]; then
            echo "sha=${{ github.event.pull_request.head.sha }}" >> "$GITHUB_OUTPUT"
          elif [ "${{ github.event_name }}" = "workflow_dispatch" ]; then
            git fetch origin $BRANCH
            SHA=$(git rev-parse origin/$BRANCH)
            echo "sha=$SHA" >> "$GITHUB_OUTPUT"
          fi
        |||,
      },
    ],
  },
  // workflow_inputs is a JSON object that looks like [{ "name": "name_1", "value": "value_1" }, ...]
  // "value_1" should be a string that can include GHA expression for the value, e.g.,
  // "semgrep/semgrep:${{ needs.setup-docker-tag.outputs.docker-tag }}".
  // This will be converted into env vars to avoid accidental script injections
  // and reconstructed as the JSON body in the POST request to Argo.
  trigger_argo_workflow: function(trigger_url, workflow_inputs) {
    'runs-on': 'ubuntu-22.04',
    needs: ['setup-docker-tag', 'get-sha'],
    steps: [{
      id: 'trigger',
      env: {
        REPOSITORY: '${{ github.repository }}',
        SHA: '${{ needs.get-sha.outputs.sha }}',
        TOKEN: '${{ secrets.ARGO_WORKFLOWS_TOKEN }}',
      } + {
        [std.asciiUpper(e.name)]: e.value for e in workflow_inputs
      },
      run: |||
        echo "Repository: $REPOSITORY"
        echo "SHA: $SHA"
        curl --fail-with-body -X POST %s \
          -H "Authorization: Bearer $TOKEN" \
          -d %s
      ||| % [ trigger_url, std.escapeStringJson(std.toString(
        {
          repository: '$REPOSITORY',
          sha: '$SHA',
        } + {
          [e.name]: "$" + std.asciiUpper(e.name) for e in workflow_inputs
        }
      ))],
    }]
  }
}
