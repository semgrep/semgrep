// This workflow builds and tests semgrep via nix!
local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
local job = gha.os_matrix(
  oss=['ubuntu-latest', 'macos-latest'],
  steps=[
    gha.speedy_checkout_step,
    actions.checkout_with_submodules(),
    {
        name: "Set up Nix",
        uses: "DeterminateSystems/nix-installer-action@main",
        with: {
            "extra-conf": "sandbox = false"
        }
    },
    {
        name: "Cache Nix",
        uses: "DeterminateSystems/magic-nix-cache-action@main"
    },
    {
        name: "Check Flake Validity",
        uses: "DeterminateSystems/flake-checker-action@main"
    },
    {
        name: "Build and Check Flake",
        run: "make nix-check-verbose"
    }
    /* TODO - add this in so we can cache everything on every push

    {
        name: "Install Cachix",
        uses: "cachix/cachix-action@v14",
        with: {
        name: "semgrep",
        },
        authToken: "${{ secrets.CACHIX_AUTH_TOKEN }}",
    },
    {
        name: "Cache Shells and Binaries",
        run: "make nix-cache"
    }

    */
]);
{
  name: 'build-test-nix',
  // This is called from tests.jsonnet and release.jsonnet
  // TODO: just make this job a func so no need to use GHA inherit/call
  on: gha.on_classic,
  jobs: {
    job: job,
  },
}
