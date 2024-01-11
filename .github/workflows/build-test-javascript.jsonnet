// Build, test, and upload the javascript code for Turbo mode.

local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

local artifact_name = 'semgrep-js-artifacts-${{ github.sha }}';

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------

// to be used by the workflow
local upload_artifacts_input = {
  inputs: {
    'upload-artifacts': {
      type: 'boolean',
      default: false,
      description: 'Whether or not to upload JS artifacts to S3',
    },
  },
};

// ----------------------------------------------------------------------------
// Helpers (Cache)
// ----------------------------------------------------------------------------

// TODO? move in semgrep.libsonnet? or actions.libsonnet?

local cache_key = 'semgrep-with-submodules-and-tree-sitter-${{ github.sha }}';

local save_in_cache = {
  name: 'Cache git checkout',
  id: 'cache-git',
  uses: 'actions/cache/save@v3',
  with: {
    path: '.',
    key: cache_key,
  },
};

local restore_from_cache = {
  name: 'Restore git checkout cache',
  id: 'restore-git',
  uses: 'actions/cache/restore@v3',
  with: {
    path: '.',
    key: cache_key,
  },
};

local guard_cache_hit = {
  'if': "${{ steps.restore-git.outputs.cache-hit != 'true' }}",
};

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

local build_artifact_name = 'semgrep-js-ocaml-build-${{ github.sha }}';

local build_job =
  semgrep.ocaml_alpine_container
  {
    'runs-on': 'ubuntu-latest-16-core',
    steps: [
      gha.speedy_checkout_step,
      actions.checkout_with_submodules(),
      gha.git_safedir,
      // TODO: we should just call 'make install-deps-for-semgrep-core'
      {
        name: 'Set up tree-sitter',
        run: '(cd libs/ocaml-tree-sitter-core && ./configure && ./scripts/install-tree-sitter-lib)',
      },
      // saving the checkout for test_job below to save time
      save_in_cache,
      {
        name: 'Build semgrep',
        run: |||
          eval $(opam env)
          make install-deps-ALPINE-for-semgrep-core
          make install-deps-for-semgrep-core
          make build-semgrep-jsoo
        |||,
      },
      {
        uses: 'actions/upload-artifact@v3',
        with: {
          'retention-days': 1,
          path: |||
            _build/default/js/**/*.bc.js
          |||,
          name: build_artifact_name,
        },
      },
    ],
  };

local test_job = {
  'runs-on': 'ubuntu-latest-16-core',
  container: 'emscripten/emsdk:3.1.46',
  needs: [
    'build',
  ],
  env: {
    HOME: '/root',
  },
  steps: [
    restore_from_cache,
    gha.speedy_checkout_step + guard_cache_hit,
    gha.git_safedir,
    actions.checkout_with_submodules() + guard_cache_hit,
    {
      name: 'Set up tree-sitter',
      run: '(cd libs/ocaml-tree-sitter-core && ./configure && ./scripts/install-tree-sitter-lib)',
    } + guard_cache_hit,
    {
      uses: 'actions/download-artifact@v3',
      with: {
        name: build_artifact_name,
        path: '_build/default/js',
      },
    },
    {
      uses: 'actions/setup-node@v3',
      with: {
        'node-version': '18',
      },
    },
    {
      name: 'Build JS artifacts',
      run: "make -C js -j $(nproc) build",
    },
    {
      name: 'Test JS artifacts',
      run: |||
          # Allow 'git rev-parse --show-toplevel' even though the owner of the
          # semgrep folder is different than the owner of its contents.
          # Needed by OCaml test code to determine the project root.
          git config --global --add safe.directory /__w/semgrep/semgrep
          make -C js -j $(nproc) test
      |||
    },
    // xvfb is a virtual x11 display, so we can run headless tests for LSP.js
    // libatk-bridge2.0-0 libgtk-3-0 libgbm1 are dependencies for vscode's test runner
    {
      name: 'Setup APT for xvfb',
      run: |||
          sudo apt-get update
          sudo apt-get install -y libatk-bridge2.0-0 libgtk-3-0 libgbm1
      |||,
    },
    {
      name: 'Test LSP.js',
      uses: 'coactions/setup-xvfb@v1',
      with: {
        run: "make -C js/language_server test",
      }
    },
    {
      name: 'Package JS artifacts',
      run:|||
        tar cvzf semgrep-js-artifacts.tar.gz \
          js/engine/dist/index.cjs \
          js/engine/dist/index.mjs \
          js/languages/*/dist/index.cjs \
          js/languages/*/dist/index.mjs \
          js/languages/*/dist/semgrep-parser.wasm \
          js/language_server/dist/*
      |||,
    },
    {
      name: 'Upload JS artifacts',
      uses: 'actions/upload-artifact@v3',
      with: {
        path: 'semgrep-js-artifacts.tar.gz',
        'retention-days': 2,
        name: artifact_name,
      },
    }
  ],
};

local upload_job = {
  'runs-on': 'ubuntu-latest',
  needs: [
    'test',
  ],
  'if': '${{ inputs.upload-artifacts }}',
  permissions: gha.write_permissions,
  steps: [
    {
      name: 'Configure AWS credentials',
      uses: 'aws-actions/configure-aws-credentials@v4',
      with: {
        'role-to-assume': 'arn:aws:iam::338683922796:role/semgrep-oss-js-artifacts-deploy-role',
        'role-duration-seconds': 900,
        'role-session-name': 'semgrep-s3-access',
        'aws-region': 'us-west-2',
      },
    },
    {
      uses: 'actions/download-artifact@v3',
      with: {
        name: artifact_name,
        path: '/tmp/semgrep',
      },
    },
    {
      name: 'Upload to S3',
      run: |||
        cd /tmp/semgrep
        tar xvzf semgrep-js-artifacts.tar.gz
        branch_name=${GITHUB_HEAD_REF:-${GITHUB_REF#refs/heads/}}
        urlencoded_branch_name=$(printf %s $branch_name | jq -sRr @uri)

        cache_control=""
        if [[ "${branch_name}" =~ "^release-[0-9.]+$" ]]; then
          # If this is a release:
          # - public: The response can be stored in a shared cache
          # - max-age=31536000: Cache for up to 1 year
          # - immutable: The response will not be updated while fresh
          cache_control="public,max-age=31536000,immutable"
        else
          # Otherwise:
          # - public: This response can be stored in a shared cache
          # - max-age=300: Cache for up to 5 mins
          cache_control="public,max-age=300"
        fi

        aws s3 cp --recursive --cache-control "${cache_control}" /tmp/semgrep/js/ "s3://semgrep-app-static-assets/static/turbo/${urlencoded_branch_name}/"
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'build-test-javascript',
  on: {
    workflow_dispatch: upload_artifacts_input,
    workflow_call: upload_artifacts_input,
  },
  jobs: {
    build: build_job,
    test: test_job,
    upload: upload_job,
  },
}
