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

// TODO, switch this to use a docker image like we do in pro, so it's easier to run+reproduce
// from proprietary, and more aligned to what we do there for CI

local core_x86 = import 'build-test-core-x86.jsonnet';
local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

local wheel_name = 'manylinux-x86-wheel';
// The '2_28' is the minimum version of GLIBC supported by the image, we need
// 2.28 since GHA runners use node20, which has 2.28 as a dependancy.
local manylinux_container = 'quay.io/pypa/manylinux_2_28_x86_64';

local default_specific_python_version = semgrep.default_python_version + '.' + semgrep.default_python_patch_version;

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local build_wheels_job = {
  'runs-on': 'ubuntu-latest',
  container: manylinux_container,
  steps: actions.checkout_with_submodules() + [
    // coupling: if you modify the python version, update the cp310-cp310 further below
    {
      run: |||
        # history:
        # we used to `yum install python3.9`
        # but now that we are on python 3.10, there is no python3.10 in `yum`
        # so we need to download and install it ourselves

        yum install -y wget zip python3-pip
        wget https://www.python.org/ftp/python/%(python_version)s/Python-%(python_version)s.tgz
        tar xzf Python-%(python_version)s.tgz
        cd Python-%(python_version)s
        ./configure --with-system-ffi --with-computed-gotos --enable-loadable-sqlite-extensions

        make -j ${nproc}
        make altinstall
      ||| % { python_version: default_specific_python_version },
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
      uses: 'actions/upload-artifact@v4',
      with: {
        name: wheel_name,
        path: 'cli/dist.zip',
      },
    },
  ],
};

local test_wheels_job = {
  'runs-on': 'ubuntu-latest',
  container: manylinux_container,
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
      run: '/opt/python/cp310-cp310/bin/pip install dist/*.whl',
    },
    // TODO? could reuse build-test-osx-x86.test_semgrep_steps
    // only diff is PATH adjustments
    {
      name: 'test package',
      run: |||
        export PATH=/opt/python/cp310-cp310/bin:$PATH
        semgrep --version
      |||,
    },
    {
      name: 'e2e semgrep-core test',
      run: |||
        export PATH=/opt/python/cp310-cp310/bin:$PATH
        echo '1 == 1' | semgrep -l python -e '$X == $X' -
      |||,
    },
  ],
};

local test_wheels_venv_job = {
  'runs-on': 'ubuntu-latest',
  container: manylinux_container,
  needs: [
    'build-wheels',
  ],
  steps: [
    actions.download_artifact_step(wheel_name),
    {
      run: 'unzip dist.zip',
    },
    {
      name: 'create venv',
      run: '/opt/python/cp310-cp310/bin/python3 -m venv env',
    },
    // *.whl is fine here because we're building one wheel with the "any"
    // platform compatibility tag
    {
      name: 'install package',
      run: 'env/bin/pip install dist/*.whl',
    },
    // TODO? could reuse build-test-osx-x86.test_semgrep_steps
    // only diff is PATH adjustments
    {
      name: 'test package',
      run: |||
        env/bin/semgrep --version
      |||,
    },
    {
      name: 'e2e semgrep-core test',
      run: |||
        echo '1 == 1' | env/bin/semgrep -l python -e '$X == $X' -
      |||,
    },
  ],
};

local test_wheels_wsl_job = {
  'runs-on': 'windows-2025',
  needs: [
    'build-wheels',
  ],
  steps: [
    // Why make life harder? Disable cache for now.
    actions.setup_python_step(version=semgrep.default_python_version, cache=false),
    actions.download_artifact_step(wheel_name),
    {
      run: 'unzip dist.zip',
    },
    {
      uses: 'Vampire/setup-wsl@v6',
      with: {
        distribution: 'Ubuntu-22.04',
        update: true,
        'additional-packages': |||
          python3
          python3-venv
          python3-pip
        |||,
      },
    },
    {
      name: 'install package',
      shell: 'wsl-bash {0}',
      run: 'python3 -m pip install dist/*.whl',
    },
    {
      name: 'test package',
      shell: 'wsl-bash {0}',
      run: |||
        semgrep --version
      |||,
    },
    {
      name: 'e2e semgrep-core test',
      shell: 'wsl-bash {0}',
      run: |||
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
    'test-wheels-venv': test_wheels_venv_job,
    'test-wheels-wsl': test_wheels_wsl_job,
  },
}
