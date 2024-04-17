// Build and validate our (multi-arch) semgrep docker image defined in our
// Dockerfile and save it as a GHA artifact.
//
// Note that the actual push to https://hub.docker.com/r/returntocorp/semgrep
// is done in the push-docker.jsonnet workflow, not here.

local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------
local inputs(default) = {
  inputs: {
    // See https://github.com/docker/metadata-action#flavor-input
    'docker-flavor': {
      type: 'string',
      description: 'Multi-line string for the metadata tag action for the flavor of the image. ',
      required: true,
    },
    // See https://github.com/docker/metadata-action#tags-input
    'docker-tags': {
      type: 'string',
      description: 'Multi-line string for the metadata tag action for the tags to apply to the image. ',
      required: true,
    },
    'artifact-name': {
      type: 'string',
      description: 'Name (key) to use when uploading the docker image tarball as a artifact',
      required: true,
    },
    'repository-name': {
      type: 'string',
      description: 'The repository/name of the docker image to push, e.g., returntocorp/semgrep',
      required: true,

    } + if default then {default: 'returntocorp/semgrep'} else {},
    file: {
      type: 'string',
      description: 'Dockerfile to build',
      required: true,
    } + if default then {default: 'Dockerfile' } else {},
    target: {
      type: 'string',
      description: 'Dockerfile target to build',
      required: true,
    },
    'enable-tests': {
      type: 'boolean',
      description: 'Whether or not to run validation on the built image',
      required: true,
    },
  },
};

// ----------------------------------------------------------------------------
// The Job
// ----------------------------------------------------------------------------

local job = {
  'runs-on': 'ubuntu-latest',
  permissions: gha.read_permissions,
  strategy: {
    matrix: {
      // multi-arch!! https://docs.docker.com/build/building/multi-platform/
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
    // We're now using depot.dev to build our docker image which is
    // more efficient, especially for arm64.
    {
      uses: 'depot/setup-action@v1',
    },
    {
      name: 'Build image',
      id: 'build-image',
      uses: 'depot/build-push-action@v1.9.0',
      with: {
        project: semgrep.depot_project_id,
        platforms: 'linux/${{ matrix.architecture }}',
        outputs: 'type=docker,dest=/tmp/image.tar',
        tags: '${{ steps.meta.outputs.tags }}',
        labels: '${{ steps.meta.outputs.labels }}',
        file: '${{ inputs.file }}',
        target: '${{ inputs.target }}',
        // This flag means that if for whatever reason depot fails to
        // build the docker image on their fast native arm64 runners, it
        // will fallback to docker-buildx which uses emulation (which is
        // really slow). So if this job suddently takes more than 15min,
        // it's probably because there is a problem somewhere and the
        // fallback is activated. A common solution is to reset the depot.dev
        // cache (especially useful when depot.dev gets confused by changes
        // in submodules in a PR) by clicking "Reset cache" at the bottom of
        // https://depot.dev/orgs/9ks3jwp44z/projects/fhmxj6w9z8/settings
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
    workflow_dispatch: inputs(default=true),
    workflow_call: inputs(default=false),
  },
  jobs: {
    job: job,
  },
}
