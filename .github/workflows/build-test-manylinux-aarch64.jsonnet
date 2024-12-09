// Build aarch64 Python wheels (ab)using our multi-arch docker build.

local gha = import "libs/gha.libsonnet";
local semgrep = import "libs/semgrep.libsonnet";

local wheel_name = 'manylinux-aarch64-wheel';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

// TODO? why not separate in build_wheels and test_wheels like for the other?
local build_wheels_job = {
  'runs-on': 'ubuntu-latest',
  // TODO? why we suddenly need that?
  permissions: gha.read_permissions,
  // TODO? factorize more with build-test-docker.jsonnet?
  steps: [
    {
      uses: 'docker/setup-qemu-action@v3',
    },
    {
      uses: 'docker/setup-buildx-action@v2',
    },
    {
      uses: 'depot/setup-action@v1',
    },
    {
      name: 'Build and test python wheel',
      id: 'build-semgrep-wheel',
      uses: 'depot/build-push-action@v1.9.0',
      with: {
        project: semgrep.depot_project_id,
        platforms: 'linux/arm64',
        outputs: 'type=docker,dest=/tmp/image.tar',
        target: 'semgrep-wheel',
        'buildx-fallback': true,
      },
    },
    {
      name: 'Extract wheel from docker image',
      run: |||
        # load the docker image containing the semgrep python wheel
        docker load --input /tmp/image.tar

        # create a new docker container using the image we just loaded
        # note: `docker create` simply prepares the container filesystem -- nothing is executed!
        CONTAINER_ID=$(docker create ${{ steps.build-semgrep-wheel.outputs.imageid }})

        # use `docker export` to extract the python wheel zipfile out of the container
        docker export $CONTAINER_ID | tar xv semgrep/cli/dist.zip

        # clean up after ourselves
        docker rm $CONTAINER_ID

        # note: this was originally accomplished by running `cat` from within the container and redirecting stdout to a file:
        #
        # docker run --platform linux/arm64 --rm ${{ steps.build-semgrep-wheel.outputs.imageid }} cat cli/dist.zip > /tmp/dist.zip
        #
        # we ended up hitting an edge case where the final ~100kb doesn't always get written to disk, which results in a corrupted zip file.
        # our theory is that the pipe between the container and the host is being closed prematurely.
      |||,
    },
    {
      uses: 'actions/upload-artifact@v4',
      with: {
        path: 'semgrep/cli/dist.zip',
        name: wheel_name,
      },
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'build-test-manylinux-aarch64',
  on: gha.on_dispatch_or_call,
  jobs: {
    'build-wheels': build_wheels_job,
  },
}
