local actions = import 'libs/actions.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
local job = gha.os_matrix(steps=[
  gha.speedy_checkout_step,
  actions.checkout_with_submodules(),
  {
    name: "Set up Nix",
    uses: "DeterminateSystems/nix-installer-action@main"
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
    run: "make nix-check"
  }
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
