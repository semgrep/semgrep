// Build and test our semgrep docker image.

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------
local inputs = {
      inputs: {
        'docker-flavor': {
          required: true,
          type: 'string',
          description: 'A multi-line string in the format accepted by docker metadata tag action for the flavor of the image. See https://github.com/docker/metadata-action#flavor-input for more information',
        },
        'docker-tags': {
          required: true,
          type: 'string',
          description: 'A multi-line string in the format accepted by docker metadata tag action for the tags to apply to the image. See https://github.com/docker/metadata-action#tags-input for more information',
        },
        'artifact-name': {
          required: true,
          type: 'string',
          description: 'Name (key) to use when uploading the docker image tarball as a artifact',
        },
        'repository-name': {
          required: true,
          type: 'string',
          description: 'The repository/name of the docker image to push, e.g., returntocorp/semgrep',
          default: 'returntocorp/semgrep',
        },
        file: {
          required: true,
          type: 'string',
          description: 'Dockerfile to build',
          default: 'Dockerfile',
        },
        target: {
          required: true,
          type: 'string',
          description: 'Dockerfile target to build',
        },
        'enable-tests': {
          required: true,
          type: 'boolean',
          description: 'Whether or not to run validation on the built image',
        },
      },
};

// ----------------------------------------------------------------------------
// The Job
// ----------------------------------------------------------------------------

local job = {
      'runs-on': 'ubuntu-latest',
      permissions: {
        contents: 'read',
        'id-token': 'write',
      },
      strategy: {
        matrix: {
          architecture: [
            'amd64',
            'arm64',
          ],
        },
      },
      steps: [
        {
          'if': "${{ matrix.architecture != 'amd64' }}",
          uses: 'docker/setup-qemu-action@v3',
        },
        {
          uses: 'docker/setup-buildx-action@v2',
        },
        {
          id: 'meta',
          name: 'Set tags and labels',
          uses: 'docker/metadata-action@v5',
          with: {
            images: '${{ inputs.repository-name }}',
            flavor: '${{ inputs.docker-flavor }}',
            tags: '${{ inputs.docker-tags }}',
          },
        },
        {
          uses: 'depot/setup-action@v1',
        },
        {
          name: 'Build image',
          id: 'build-image',
          uses: 'depot/build-push-action@v1.9.0',
          with: {
            project: 'fhmxj6w9z8',
            platforms: 'linux/${{ matrix.architecture }}',
            outputs: 'type=docker,dest=/tmp/image.tar',
            tags: '${{ steps.meta.outputs.tags }}',
            labels: '${{ steps.meta.outputs.labels }}',
            file: '${{ inputs.file }}',
            target: '${{ inputs.target }}',
            'buildx-fallback': true,
            secrets: 'SEMGREP_APP_TOKEN=${{ secrets.SEMGREP_APP_TOKEN }}',
          },
        },
        {
          name: 'Load image',
          'if': '${{ inputs.enable-tests }}',
          run: 'docker load --input /tmp/image.tar',
        },
        {
          uses: 'actions/checkout@v3',
          'if': '${{ inputs.enable-tests }}',
        },
        {
          name: 'Test Image',
          'if': '${{ inputs.enable-tests }}',
          run: './scripts/validate-docker-build.sh ${{ steps.build-image.outputs.imageid }} linux/${{ matrix.architecture }}',
        },
        {
          uses: 'actions/upload-artifact@v3',
          with: {
            name: '${{ inputs.artifact-name }}-arch-${{ matrix.architecture }}',
            path: '/tmp/image.tar',
          },
        },
      ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-docker',
  on: {
    workflow_dispatch: inputs,
    workflow_call: {
      inputs: {
        'docker-flavor': {
          required: true,
          type: 'string',
          description: 'A multi-line string in the format accepted by docker metadata tag action for the flavor of the image. See https://github.com/docker/metadata-action#flavor-input for more information',
        },
        'docker-tags': {
          required: true,
          type: 'string',
          description: 'A multi-line string in the format accepted by docker metadata tag action for the tags to apply to the image. See https://github.com/docker/metadata-action#tags-input for more information',
        },
        'artifact-name': {
          required: true,
          type: 'string',
          description: 'Name (key) to use when uploading the docker image tarball as a artifact',
        },
        'repository-name': {
          required: true,
          type: 'string',
          description: 'The repository/name of the docker image to push, e.g., returntocorp/semgrep',
        },
        file: {
          required: true,
          type: 'string',
          description: 'Dockerfile to build',
        },
        target: {
          required: true,
          type: 'string',
          description: 'Dockerfile target to build',
        },
        'enable-tests': {
          required: true,
          type: 'boolean',
          description: 'Whether or not to run validation on the built image',
        },
      },
    },
  },
  jobs: {
    'job': job,
  },
}
