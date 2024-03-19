// Workflow to check whether changes in the PR are not breaking
// the interaction with the semgrep-core-proprietary binary in 'develop'
// in the semgrep-proprietary repository.
//
// TODO? now that the semgrep-core-proprietary is part of the semgrep docker
// image, do we still need this check?

local gha = import 'libs/gha.libsonnet';
local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The inputs
// ----------------------------------------------------------------------------
local inputs = {
  inputs: {
    'artifact-name': {
      type: 'string',
      description: 'Name of the docker image tarball',
      required: true,
    },
    'repository-name': {
      type: 'string',
      description: 'The repository/name of the docker image',
      required: true,
    },
  },
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------
local setup_docker_tag_job = {
  name: 'Set up Docker tag based on if this is a pull request',
  'runs-on': 'ubuntu-22.04',
  outputs: {
    'docker-tag': '${{ steps.setup-docker-tag.outputs.docker-tag }}',
  },
  steps: [
    {
      name: 'Setup Docker Tag',
      id: 'setup-docker-tag',
      run: |||
        echo "Github event is ${{ github.event_name }}"
        if [ "${{ github.event_name }}" = "pull_request" ]; then
          echo "docker-tag=pr-${{ github.event.pull_request.number }}" >> "$GITHUB_OUTPUT"
          echo "Setting docker tag to current pull request number"
        else
          echo "docker-tag=develop" >> "$GITHUB_OUTPUT"
          echo "Setting dry-run to develop"
        fi
      |||,
    },
  ],
};

local test_semgrep_pro_job = {
  'runs-on': 'ubuntu-22.04',
  permissions: {
    'id-token': 'write',
    contents: 'read',
  },
  needs: 'setup-docker-tag',
  env: {
    SEMGREP_APP_TOKEN: '${{ secrets.SEMGREP_APP_TOKEN }}',
  },
  steps: [
    actions.checkout_with_submodules(),
    {
      uses: 'docker/setup-buildx-action@v2',
    },
    {
      uses: 'actions/download-artifact@v3',
      with: {
        name: '${{ inputs.artifact-name }}-arch-amd64',
        path: '/tmp',
      },
    },
    {
      name: 'Load image',
      run: 'docker load --input /tmp/image.tar',
    },
    semgrep.aws_credentials_step(
       role='returntocorp-semgrep-deploy-role',
       session_name='semgrep-deploy'
     ),
    // This is the `develop` binary, so this is truly the most recent version
    // of `semgrep-core-proprietary` from that repository's `develop` branch.
    {
      name: 'Download Semgrep Pro `develop` binary',
      run: 'aws s3 cp s3://web-assets.r2c.dev/assets/semgrep-core-proprietary-manylinux-develop ./semgrep-core-proprietary',
    },
    {
      name: 'Run Semgrep Pro Engine!',
      // old: we used to also pass '--entrypoint=bash' below, but bash is not
      // anymore in the semgrep docker image. Moreover, there was no need
      // for --entrypoint, which is used to override an existing entrypoint,
      // because the semgrep Dockerfile does not have an ENTRYPOINT.
      run: 'docker run --rm -v "$(pwd):/root" -e SEMGREP_APP_TOKEN=${{ secrets.SEMGREP_APP_TOKEN }} "${{ inputs.repository-name }}:${{ needs.setup-docker-tag.outputs.docker-tag }}" /root/scripts/test-pro.sh',
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'test-semgrep-pro',
  on: {
    workflow_dispatch: inputs,
    workflow_call: inputs,
  },
  jobs: {
    'setup-docker-tag': setup_docker_tag_job,
    'test-semgrep-pro': test_semgrep_pro_job,
  },
}
