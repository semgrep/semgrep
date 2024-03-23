// This workflow generates the manylinux-wheel for pypi.
//
// We rely on https://github.com/pypa/manylinux which helps in
// handling the many different Linux distributions out there for x86
// (for arm64 see the build-test-manylinux-aarch64.jsonnet instead).
// From the manylinux website:
//
//   Building manylinux-compatible wheels is not trivial; as a general
//   rule, binaries built on one Linux distro will only work on other
//   Linux distros that are the same age or newer. Therefore, if we want
//   to make binaries that run on most Linux distros, we have to use an
//   old enough distro.
//   Rather than forcing you to install an old distro yourself, install
//   Python, etc., we provide Docker images where we've done the work
//   for you. The images are uploaded to quay.io and are tagged for
//   repeatable builds.
//
// quay.io is a container registry, similar to hub.docker.com .
// It seems a bit more fragile so in case of problems check
// https://isdown.app/integrations/quay-io
// We use it because the manylinux project is using it.

local gha = import "libs/gha.libsonnet";
local actions = import "libs/actions.libsonnet";
local core_x86 = import "build-test-core-x86.jsonnet";

local wheel_name = 'manylinux-x86-wheel';

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local build_wheels_job = {
  'runs-on': 'ubuntu-latest',
  // This is a 4 years old container, built from a 4 years old file
  // https://github.com/semgrep/sgrep-build-docker/blob/master/Dockerfile
  // TODO: switch to a standard container (use quay.io/manylinux_2014_x86_64 ?)
  container: 'returntocorp/sgrep-build:ubuntu-18.04',
  steps: [
    actions.checkout_with_submodules(),
    {
      run: 'apt-get update && apt install -y zip musl-tools software-properties-common python3-pip',
    },
    {
      run: |||
        add-apt-repository ppa:deadsnakes/ppa
        apt install -y python3.8
        update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.6 1
        update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.8 2
        update-alternatives --config python3
      |||
    },
    actions.download_artifact_step(core_x86.export.artifact_name),
    {
      run: |||
        tar xf artifacts.tgz
        cp artifacts/semgrep-core cli/src/semgrep/bin
        ./scripts/build-wheels.sh
      |||,
    },
    {
      uses: 'actions/upload-artifact@v3',
      with: {
        name: wheel_name,
        path: 'cli/dist.zip',
      },
    },
  ],
};

local test_wheels_job = {
  'runs-on': 'ubuntu-latest',
  container: 'quay.io/pypa/manylinux2014_x86_64',
  needs: [
    'build-wheels',
  ],
  steps: [
    actions.download_artifact_step(wheel_name),
    {
      run: 'unzip dist.zip',
    },
    // *.whl is fine here because we're building one wheel with the "any"
    // platform compatibility tag
    {
      name: 'install package',
      run: '/opt/python/cp38-cp38/bin/pip install dist/*.whl',
    },
    // TODO? could reuse build-test-osx-x86.test_semgrep_steps
    // only diff is PATH adjustments
    {
      name: 'test package',
      run: |||
        export PATH=/opt/python/cp38-cp38/bin:$PATH
        semgrep --version
      |||,
    },
    {
      name: 'e2e semgrep-core test',
      run: |||
        export PATH=/opt/python/cp38-cp38/bin:$PATH
        echo '1 == 1' | semgrep -l python -e '$X == $X' -
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  name: 'build-test-manylinux-x86',
  on: gha.on_dispatch_or_call,
  jobs: {
    'build-wheels': build_wheels_job,
    'test-wheels': test_wheels_job,
  },
}
