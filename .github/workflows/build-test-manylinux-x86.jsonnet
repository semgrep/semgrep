// This workflow generates the manylinux-wheel for pypi.

{
  name: 'build-test-manylinux-x86',
  on: {
    workflow_dispatch: null,
    workflow_call: null,
  },
  jobs: {
    'build-wheels-manylinux': {
      'runs-on': 'ubuntu-latest',
      // pad: What is this sgrep-xxx image?
      container: 'returntocorp/sgrep-build:ubuntu-16.04',
      steps: [
        {
          uses: 'actions/checkout@v3',
          with: {
            submodules: true,
          },
        },
        // pad: Why do we have this weird setup python step?
        {
          name: 'Setup Python',
          run: |||
            rm /usr/bin/python
            ln `which python3.7` /usr/bin/python3
          |||,
        },
        {
          run: 'apt-get update && apt install -y zip musl-tools',
        },
        {
          uses: 'actions/download-artifact@v3',
          with: {
            name: 'ocaml-build-artifacts-release',
          },
        },
        {
          run: |||
            tar xf ocaml-build-artifacts.tgz
            cp ocaml-build-artifacts/bin/semgrep-core cli/src/semgrep/bin
            ./scripts/build-wheels.sh
          |||,
        },
        {
          uses: 'actions/upload-artifact@v3',
          with: {
            name: 'manylinux-x86-wheel',
            path: 'cli/dist.zip',
          },
        },
      ],
    },
    'test-wheels-manylinux': {
      'runs-on': 'ubuntu-latest',
      container: 'quay.io/pypa/manylinux2014_x86_64',
      needs: [
        'build-wheels-manylinux',
      ],
      steps: [
        {
          uses: 'actions/download-artifact@v1',
          with: {
            name: 'manylinux-x86-wheel',
          },
        },
        {
          run: 'unzip ./manylinux-x86-wheel/dist.zip',
        },
        // *.whl is fine here because we're building one wheel with the "any"
        // platform compatibility tag
        {
          name: 'install package',
          run: '/opt/python/cp37-cp37m/bin/pip install dist/*.whl',
        },
        {
          name: 'test package',
          run: |||
             export PATH=/opt/python/cp37-cp37m/bin:$PATH
             semgrep --version
           |||,
        },
        {
          name: 'e2e semgrep-core test',
          run: |||
             export PATH=/opt/python/cp37-cp37m/bin:$PATH
             echo '1 == 1' | semgrep -l python -e '$X == $X' -
           |||,
        },
      ],
    },
  },
}
