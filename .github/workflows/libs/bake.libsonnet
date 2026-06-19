// Helper for building a whole Docker pipeline with a single `depot bake`.
//
// Where docker.libsonnet's `job()` builds ONE Dockerfile target per GHA job
// (one `depot/build-push-action` invocation each), this helper builds a whole
// bake GROUP in one job: a single `depot/bake-action` runs `depot bake <group>`
// which builds every target in the group (and the stages they depend on) in one
// request, with shared layer cache. The DAG itself lives in `docker-bake.hcl` at
// the repo root.
//
// coupling: target names, group names, and the `type=local,dest=/tmp/bake-out/<target>`
// output paths are defined in docker-bake.hcl; the `artifacts` argument below
// must agree with those dest paths.

local actions = import 'actions.libsonnet';
local gha = import 'gha.libsonnet';
local uses = import 'uses.libsonnet';

// The artifacts we publish are per-arch. x86 reuses the amd64 binaries (same
// content, historical artifact name). coupling: docker.libsonnet uses the same
// mapping for its own artifact uploads.
local archs = ['amd64', 'arm64', 'x86'];
local arch_to_docker_arch = {
  amd64: 'amd64',
  arm64: 'arm64',
  x86: 'amd64',
};

// Where docker-bake.hcl writes `type=local` outputs on the runner.
local bake_out(target) = '/tmp/bake-out/%s' % target;

// Upload one `type=local` bake target's exported files as the three per-arch
// GHA artifacts (<name>-linux-amd64/arm64/x86), mirroring docker.libsonnet.
local upload_artifact_steps(artifact) =
  std.flattenArrays(std.map(function(arch) [
    actions.make_artifact_step('%s/linux_%s/*' % [bake_out(artifact.target), arch_to_docker_arch[arch]]),
    actions.upload_artifact_step('%s-linux-%s' % [artifact.name, arch]),
  ], archs));

// The git/CI context passed to docker-bake.hcl as bake variables (env vars of
// the same name). coupling: variable names in docker-bake.hcl.
local bake_env = {
  REF: gha.ref_expr,
  BRANCH: '${{ github.ref_name }}',
  PR_NUMBER: '${{ github.event.pull_request.number }}',
  EVENT_NAME: '${{ github.event_name }}',
  VCS_REF_HEAD_NAME: '${{ github.head_ref || github.ref_name }}',
  VCS_REF_HEAD_REVISION: gha.ref_expr,
};

// Expose a pushed image's manifest digest as `outputs.digest` (consumed e.g. by
// pro-core-validation and the build-and-test argo-comparison jobs).
//
// depot/bake-action exposes no `metadata` output, so we resolve the digest from
// the registry by inspecting the always-present `sha-<REF>` tag that the build
// just pushed. coupling: the `sha-<REF>` tag and `digest_image` repo are defined
// in docker-bake.hcl's `tags` function; `.Manifest.Digest` matches what the old
// depot/build-push-action `outputs.digest` returned (the manifest-list digest).
local digest_step(digest_image) = {
  id: 'digest',
  name: 'Resolve %s image digest' % digest_image,
  env: {
    IMAGE: digest_image,
    REF: gha.ref_expr,
  },
  run: |||
    digest=$(docker buildx imagetools inspect --format '{{printf "%s" .Manifest.Digest}}' "${IMAGE}:sha-${REF}")
    echo "digest=${digest}" >> "$GITHUB_OUTPUT"
  |||,
};

{
  archs: archs,

  // Build a bake group in a single job.
  //
  // group:         bake group to build (e.g. 'alpine', 'manylinux').
  // checkout_steps: function(ref) returning the checkout steps.
  // push:          group pushes images to Docker Hub (needs docker login).
  // push_ecr:      group pushes images to Amazon ECR (needs ECR login).
  // digest_image: registry repo (e.g. 'semgrep/semgrep-nightly') whose pushed
  //               `sha-<REF>` tag digest to expose as outputs.digest
  //               (null = no digest output).
  // artifacts:     list of { target, name } — each `type=local` bake target to
  //                upload as <name>-linux-<arch> GHA artifacts.
  // needs:         prerequisite jobs.
  // large:         use a bigger runner.
  // extra_env:     extra bake variables (env vars) merged into bake_env, e.g.
  //                to override a `variable` declared in docker-bake.hcl.
  job(
    group,
    checkout_steps,
    push=false,
    push_ecr=false,
    digest_image=null,
    artifacts=[],
    needs=[],
    large=false,
    extra_env={},
  ):
    (if needs != [] then { needs: needs } else {}) +
    {
      'runs-on': (if large then 'depot-ubuntu-24.04-8' else 'depot-ubuntu-24.04'),
      permissions: gha.read_permissions,
      [if digest_image != null then 'outputs']: {
        digest: '${{ steps.digest.outputs.digest }}',
      },
      steps:
        checkout_steps(ref=gha.ref_expr) +
        (if push then [actions.docker_login_step] else []) +
        (if push || push_ecr then actions.ecr_login_steps else []) +
        [
          { uses: uses.depot.setup_action },
          {
            id: 'bake',
            name: 'Build %s pipeline with depot bake' % group,
            uses: uses.depot.bake_action,
            env: bake_env + extra_env,
            with: {
              // Reads the Depot project id from depot.json in the repo root.
              files: './docker-bake.hcl',
              targets: group,
              // Outputs (push / local export / cacheonly) are declared
              // per-target in docker-bake.hcl, so we deliberately do NOT set the
              // action's push/load flags (they would override every target).
            },
          },
        ] +
        (if digest_image != null then [digest_step(digest_image)] else []) +
        std.flattenArrays(std.map(upload_artifact_steps, artifacts)),
    },
}
