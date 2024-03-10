// Workflow to manually promote the returntocorp/semgrep:canary docker image
// to returntocorp/semgrep:latest that most customers are using.
// See https://www.notion.so/semgrep/returntocorp-semgrep-canary-docker-canary-45e4927fb1f847429f6064a96bf9dffd
// for more context.
//
// Promoting an image via Docker tags is typically done via:
//    $ docker pull source-image
//    $ docker tag source-image target-image
//    $ docker push target-image
//
// Sadly, this falls apart when dealing with multi-arch docker images. This is
// because a multi-arch image (e.g. returntocorp/semgrep:latest) doesn't actually
// point to a specific docker image manifest, but rather, a "manifest list" which
// is effectively a mapping of OS architectures to actual docker image manifests.
//
// When `docker pull` encounters a manifest list, it implicitly grabs the
// arch-specific image for the machine that's docker is running on, which means
// that the subsequent tag and push will never push the multi-arch image.
//
// Instead, we use `docker buildx imagetools create` which only operates on
// manifest lists. This allows us to point to a manifest list successfully.
//
// For more info:
// - manifest lists: https://docs.docker.com/registry/spec/manifest-v2-2/
// - imagetools create: https://docs.docker.com/engine/reference/commandline/buildx_imagetools_create/

local actions = import 'libs/actions.libsonnet';

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------
local inputs = {
  inputs: {
    docker_image: {
      type: 'choice',
      options: [
        // testing image
        'returntocorp/semgrep-test',
        'returntocorp/semgrep',
      ],
      description: 'Semgrep docker image to promote',
      required: true,
    },
    debug: {
      type: 'boolean',
      description: 'Check to enable verbose logging of bash commands',
      required: true,
      default: false,
    },
    update_also_semgrep_semgrep: {
      type: 'boolean',
      description: 'Check to also promote semgrep/semgrep:canary to :latest',
      required: true,
      default: false,
    },
    update_also_nonroot: {
      type: 'boolean',
      description: 'Check to also promote returntocorp/semgrep:canary-nonroot to :latest-nonroot',
      required: true,
      default: false,
    },
    confirmed: {
      description: 'Are you sure you want to do this? This is a sensitive operation',
      type: 'boolean',
      required: true,
      default: false,
    },
  },
};

// ----------------------------------------------------------------------------
// The Job
// ----------------------------------------------------------------------------

//ex: promote("returntocorp/semgrep", "-nonroot")
local promote_step(image, suffix='') = {
  name: "Promoting %(img)s:canary%(suffix)s to %(img)s:latest%(suffix)s" %
     { img: image, suffix: suffix },
  run: |||
    if [[ "${{ inputs.debug }}" == "true" ]]; then
      echo "Enabling debug logging..."
      set -x
    fi

    canary_digest=$(docker buildx imagetools inspect --format '{{printf "%%s" .Manifest.Digest}}' %(image)s:canary%(suffix)s || echo "")
    latest_digest=$(docker buildx imagetools inspect --format '{{printf "%%s" .Manifest.Digest}}' %(image)s:latest%(suffix)s || echo "(not set)")

    if [[ "${canary_digest}" == "" ]]; then
      echo "Error: %(image)s:canary%(suffix)s did not resolve to a manifest list"
      echo "If this is urgent, you can manually login to our Docker Hub account and then run these commands to promote an arch-specific image:"
      echo "docker pull %(image)s:canary%(suffix)s"
      echo "docker tag %(image)s:canary%(suffix)s %(image)s:latest%(suffix)s"
      echo "docker push %(image)s:latest%(suffix)s"
      exit 1
    fi

    echo "Promoting %(image)s:canary%(suffix)s (${canary_digest}) to %(image)s:latest%(suffix)s (was ${latest_digest})"
    echo ""

    if [[ "${{ inputs.confirmed }}" == "true" ]]; then
      docker buildx imagetools create -t %(image)s:latest%(suffix)s %(image)s:canary%(suffix)s
    else
      echo "(dry run)"
      docker buildx imagetools create --dry-run -t %(image)s:latest%(suffix)s %(image)s:canary%(suffix)s
      echo "(dry run)"
    fi
  ||| % { image: image, suffix: suffix },
}
;

local job = {
  'runs-on': 'ubuntu-22.04',
  steps: [
    actions.docker_login_step,
    promote_step('${{ inputs.docker_image }}', ''),
    promote_step('${{ inputs.docker_image }}', '-nonroot') + {
      'if': '${{ inputs.update_also_nonroot }}',
    },
    promote_step('semgrep/semgrep', '') + {
      'if': '${{ inputs.update_also_semgrep_semgrep }}',
    },
    promote_step('semgrep/semgrep', '-nonroot') + {
      'if': '${{ inputs.update_also_semgrep_semgrep && inputs.update_also_nonroot }}',
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'promote-canary-to-latest',
  on: {
    workflow_dispatch: inputs,
  },
  jobs: {
    job: job,
  },
}
