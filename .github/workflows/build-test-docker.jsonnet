// Build and validate our (multi-arch) semgrep docker image defined in our
// Dockerfile and save it as a GHA artifact.

local docker = import 'libs/docker.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';
local uses = import 'libs/uses.libsonnet';

local build_job = docker.build_and_run_gha_job(
  name='build-test-docker',
  description='Build Core',
  target='semgrep-cli',
  write_permission=false,
);

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'build-test-docker',
  on: docker.on_docker_workflow('build-test-docker'),
  jobs: {
    'build-test-docker': build_job,
  },
}
