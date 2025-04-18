// Workflow to build and push arbitrary images for each PR.
// This also saves the built binaries as artifacts for other GHA workflows.
//
// A lot of this file is copy-pasted from:
// - build-test-linux-arm64.jsonnet which has good configurations
//   to build successfully on Pro.
// - OSS/.../build-test-docker.jsonnet which has good configurations
//   to add tags and labels to the docker image properly, and also
//   to build for multiple platforms.

local actions = import 'actions.libsonnet';
local gha = import 'gha.libsonnet';

// GHA expression that evaluates to the input ref or, if that is empty, the sha
// associated with the trigger for this action.
local ref_expr = "${{ inputs.ref != '' && inputs.ref || github.sha }}";

// TODO: Generalize and also make build-test-linux-arm64.jsonnet share the
//       same job. Maybe also use it in the release script.

local default_platforms = 'linux/amd64,linux/arm64';
local archs = ['amd64', 'arm64', 'x86'];
// Needed to find the binaries when making the artifact
local arch_to_docker_arch = {
  amd64: 'amd64',
  arm64: 'arm64',
  x86: 'amd64',
};
// utils

// Useful for copying files out of a docker container
//
// NOTE: This always copies EVERY FILE from the container, so usually you want
// to copy needed files into a scratch image then output from that
local copy_from_docker_step(target, output_dir, file='Dockerfile', platforms=default_platforms) = {

  name: 'Copy all files from %s to GHA runner machine' % target,
  uses: 'depot/build-push-action@v1.9.0',
  with: {
    project: '${{ secrets.DEPOT_PROJECT_ID }}',
    context: '.',
    platforms: platforms,
    file: file,
    target: target,
    outputs: 'type=local,dest=/tmp/%s' % output_dir,
  },
};

local validate(
  job,
  script,
  needs,
  checkout_steps=actions.checkout(),
      ) =
  {
    'runs-on': 'ubuntu-latest',
    needs: needs,
    steps: checkout_steps +
           [
             {
               name: 'Test %s image' % job,
               env: {
                 IMAGEID: 'semgrep/semgrep@${{ needs.%s.outputs.digest }}' % job,
               },
               // only test amd64 for now. I suspect this is fine for now
               run: '%s "$IMAGEID" linux/amd64' % script,
             },
           ],
  };

local retag_step(image, tag, ref, confirmed=true, debug=false, dry_run=false) = {
  name: 'Retag %(image)s:%(ref)s to %(image)s:%(tag)s' % { image: image, ref: ref, tag: tag },
  env:
    {
      docker_image: image,
      image_ref: ref,
      docker_tag: tag,
      confirmed: confirmed,
      debug: debug,
      dry_run: dry_run,
    },
  run: |||
    if [[ "${debug}" == "true" ]]; then
      echo "Enabling debug logging..."
      set -x
    fi
    if [[ "${dry_run}" == "true" ]]; then
      docker_tag="${docker_tag}-dry-run"
    fi

    source_image="${docker_image}:${image_ref}"
    target_image="${docker_image}:${docker_tag}"

    old_digest=$(docker buildx imagetools inspect --format '{{printf "%s" .Manifest.Digest}}' ${target_image} || echo "(not found)")
    new_digest=$(docker buildx imagetools inspect --format '{{printf "%s" .Manifest.Digest}}' ${source_image} || echo "(not found)")

    echo ""

    if [[ "${new_digest}" == "" ]]; then
      echo "Error: ${source_image} did not resolve to a manifest list"
      echo "If this is urgent, you can manually login to our Docker Hub account and then run these commands to point to an arch-specific image:"
      echo "docker pull ${source_image}"
      echo "docker tag ${source_image} ${target_image}"
      echo "docker push ${target_image}"
      exit 1
    fi

    echo "Resolved ${source_image} to digest: ${new_digest}"
    echo ""

    echo "Will update ${target_image} from ${old_digest} to ${new_digest}"
    echo ""
    if [[ "${confirmed}" == "true" ]]; then
      docker buildx imagetools create -t ${target_image} ${source_image}
    else
      echo "(dry run)"
      docker buildx imagetools create --dry-run -t ${target_image} ${source_image}
      echo "(dry run)"
    fi
  |||,
};

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job(
  name,  // Name of docker image if uploaded (i.e. semgrep/NAME)
  target,  // Target docker layer (i.e. build, semgrep-cli, etc.)
  prefix='oss-',  // prefix for image tags
  suffix='',  // suffix for image tags
  artifact_name=null,  // name of artifact to copy from docker image
  needs=[],  // prereq GHA steps
  script='OSS/scripts/validate-docker-build.sh',  // script to verify docker image ok
  checkout_steps=actions.checkout_with_submodules,
  push=false,  // push to registry
  file='Dockerfile',
  build_args='',  // if you use ARG FOO=... in a dockerfile, set it here as 'FOO1=FOO,FOO2=FOO,...'
  platforms=default_platforms
      ) =
  (if needs != [] then { needs: needs } else {}) +
  {
    'runs-on': 'ubuntu-latest',
    outputs: {
      digest: '${{ steps.build-%s-docker-image.outputs.digest }}' % name,
    },
    permissions: gha.read_permissions,
    steps: checkout_steps(ref=ref_expr) + [
             // Documentation for this step is here:
             // https://github.com/docker/metadata-action
             //
             // In summary, this step prepares the right tags (and docker registries)
             // for us to push to in the build-pro-docker-image below.
             {
               id: 'prepare-metadata',
               name: 'Set tags and labels',
               uses: 'docker/metadata-action@v5',
               with: {
                 // The image will be pushed to semgrep/semgrep and Amazon ECR
                 // but with the tags being prefixed with "pro-"
                 //
                 // semgrep/semgrep pushes to the semgrep/semgrep registry on docker hub.
                 // *.ecr.*.amazonaws.com pushes to Amazon ECR which is
                 // our private image repository.
                 //
                 // Currently we're pushing to both, but ideally we probably want to
                 // keep Pro-related features private.
                 //
                 // We're pushing to docker hub because it's the simpler thing. But docker
                 // hub images also have issues. We have some argo workflows which pull from
                 // docker hub through a read-only repository in Amazon ECR with a feature
                 // called pull-through cache. Whenever we request for an image that's not
                 // in ECR, Amazon automatically pulls that image from docker hub and caches
                 // it. The problem is that if we make more pushes to the same docker hub tag
                 // then ECR does not detect this, resulting in our argo workflow using an
                 // outdated docker image.
                 //
                 // Because of this, we push directly to ECR in a non-pull-through registry
                 // called semgrep/semgrep-proprietary. To get more information about this
                 // registry and the AWS roles that can access it (aka the roles that appear
                 // in the configure-aws-credentials step below), login to the company's
                 // AWS portal at go/aws and go to our ECR registry at go/ecr. Search
                 // for the registry semgrep/semgrep-proprietary and from there, the URL and
                 // the role to assume that is used in this workflow will be present.
                 images: |||
                   returntocorp/%(name)s
                   semgrep/%(name)s
                   338683922796.dkr.ecr.us-west-2.amazonaws.com/semgrep/semgrep-proprietary
                 ||| % { name: name },
                 // latest=true the tag will be PREFIX-latest-SUFFIX. Since we always
                 // promote from canary to latest manually, we never want to do this
                 //
                 // suffix=SUFFIX will suffix always suffix a tag set by `tags` with said
                 // suffix
                 //
                 // prefix works the same as suffix but -- prefix!
                 flavor: |||
                   latest=false
                   prefix=%(prefix)s
                   suffix=%(suffix)s
                 ||| % { prefix: prefix, suffix: suffix },
                 // Previously used type=sha for pro-sha-* but it queries github for the
                 // sha associated with the event instead of using the actual sha that we
                 // checked out!
                 //
                 // TODO: When we trigger this manually, the pro-<branch> tag is applied
                 // based on the branch that the workflow is on, not the sha provided (if
                 // any). We should set this tag only if no sha is provided.
                 //
                 // if run on push to a PR, tag image with the PR number (ex. "123")
                 // NOTE: we need to set prefix here again if we want "-pr-" to be
                 // included in the tag
                 // type=ref,event=pr,prefix=%(prefix)-pr-
                 //
                 // if run on push to branch, tag image with the branch name (ex. "main")
                 // type=ref,event=branch
                 //
                 // if not a PR or branch push, tag image with the sha (ex. "abcdef123456")
                 // type=raw,prefix=%s-sha-,value=%s
                 //
                 // if pushed to a GH tag, tag image with full version (ex. "1.2.3")
                 // type=semver,pattern={{version}}
                 //
                 // if pushed to a GH tag, tag image with major.minor (ex. "1.2")
                 // type=semver,pattern={{major}}.{{minor}}
                 tags: |||
                   type=ref,event=pr,prefix=%(prefix)spr-
                   type=ref,event=branch
                   type=raw,value=sha-%(ref_expr)s
                   type=semver,pattern={{version}}
                   type=semver,pattern={{major}}.{{minor}}
                 ||| % { prefix: prefix, ref_expr: ref_expr },
               },
             },
             {
               uses: 'depot/setup-action@v1',
             },
           ] +
           (if !push then [] else [
              // This is needed so we can push images to docker hub successfully.
              actions.docker_login_step,

              // The configure-aws-credentials and login-ecr steps are needed so
              // we can push to Amazon ECR successfully.
              {
                id: 'configure-aws-credentials',
                name: 'Configure AWS credentials',
                uses: 'aws-actions/configure-aws-credentials@v4',
                // These fields are different than what's in semgrep_pro.libjsonnet
                // because they're used for different purposes.
                // Here is the role used to access ECR, while in semgrep_pro.libjsonnet
                // is the role to upload pro binary buckets to a specific S3 location.
                with: {
                  'role-to-assume': 'arn:aws:iam::338683922796:role/semgrep-semgrep-proprietary-deploy-role',
                  'role-duration-seconds': 900,
                  'role-session-name': 'semgrep-proprietary-build-test-docker-gha',
                  'aws-region': 'us-west-2',
                },
              },
              {
                id: 'login-ecr',
                name: 'Login to Amazon ECR',
                uses: 'aws-actions/amazon-ecr-login@v2',
              },
            ]) +
           // The tags should be setup correctly from the prepare-metadata step
           // above, so this step knows that it needs to push to both
           // docker hub and Amazon ECR.
           //
           // To test the docker image from docker hub, you can do something like
           //   docker run -it --rm \
           //     -v $(pwd):/src \
           //     semgrep/semgrep:pro-pr-XXXX
           //     semgrep scan /src
           // at a directory with some source code.
           //
           // To test the docker image from Amazon ECR, you can do something similar
           // but also requires logging in to AwS and ECR.
           //
           //   aws sso login --sso-session semgrep
           //
           //   aws ecr get-login-password --region us-west-2 | \
           //     docker login --username AWS --password-stdin \
           //     338683922796.dkr.ecr.us-west-2.amazonaws.com
           //
           //
           //   docker run -it --rm \
           //     -v $(pwd):/src \
           //     semgrep/semgrep:pro-pr-XXXX
           //     338683922796.dkr.ecr.us-west-2.amazonaws.com/semgrep/semgrep-proprietary:pro-pr-XXXX
           //     semgrep scan /src
           //
           // where you can look up available images at go/ecr and navigate to the
           // semgrep/semgrep-proprietary registry.
           [
             {
               id: 'build-%s-docker-image' % name,
               name: 'Build docker image in Depot%s' % (if push then ' and push to Docker Hub' else ''),
               uses: 'depot/build-push-action@v1.9.0',
               with: {
                 project: '${{ secrets.DEPOT_PROJECT_ID }}',

                 // By default, build-push-action (depot/ and docker/) git clone the
                 // repo and remove the .git, which is a sane default for most
                 // Dockerfile, but in our case wee call 'git lfs fetch' so
                 // we need the .git, so the context below just says to
                 // copy the files from the current dir, hence the need also for
                 // the pro.checkout_step above.
                 // alt: use build-args: "BUILDKIT_CONTEXT_KEEP_GIT_DIR=1"
                 // TODO: we do not call `git lfs fetch` anymore in the Dockerfile
                 // so do we still need this line?
                 context: '.',
                 platforms: platforms,

                 // tags and labels populated from the 'meta' step above
                 tags: '${{ steps.prepare-metadata.outputs.tags }}',
                 labels: '${{ steps.prepare-metadata.outputs.labels }}',

                 // The file used to build the image. Depot seems to default
                 // to whatever's in the top-level git directory, but putting
                 // it in here to be explicit.
                 file: file,

                 target: target,

                 // This flag controls if for whatever reason depot fails to
                 // build the docker image on their fast native arm64 runners, whether
                 // depot will fallback to docker-buildx which uses emulation (which is
                 // really slow). So if this job suddently takes more than 15min,
                 // it's probably because there is a problem somewhere and the
                 // fallback is activated. A common solution is to reset the depot.dev
                 // cache (especially useful when depot.dev gets confused by changes
                 // in submodules in a PR) by clicking "Reset cache" at the bottom of
                 // https://depot.dev/orgs/9ks3jwp44z/projects/t321zh0146/settings
                 // We used to set this to true, just in case Depot had some bugs
                 // but this would fallback for any error, not just Depot error,
                 // and then you need to wait 1h30min to actually see the error
                 // so better to set this to false
                 'buildx-fallback': false,

                 // Also maybe push to Docker hub and ec2
                 push: push,
               } + (if build_args != '' then {
                      'build-args': build_args,
                    } else {}),
             },
           ] +
           (if artifact_name != null then (
              [
                copy_from_docker_step(target='%s-binaries' % target, output_dir='binaries', platforms=platforms),
              ] + std.flattenArrays(
                std.map(function(arch)
                  [
                    actions.make_artifact_step('/tmp/binaries/linux_%s/*' % arch_to_docker_arch[arch]),
                    actions.upload_artifact_step('%s-linux-%s' % [artifact_name, arch]),
                  ], archs)
              )

            ) else []),
  };

local inputs = {
  ref: {
    description: 'Git ref to checkout. Defaults to github.sha',
    required: false,
    type: 'string',
    default: '',
  },
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  job: job,
  // Called from other workflows (e.g. build-and-test.jsonnet)
  on_docker_workflow: function(output_digest_job_name='job') {
    workflow_dispatch: {
      inputs: inputs,
    },
    workflow_call: {
      inputs: inputs,
      outputs: {
        digest: {
          description: 'Digest of the built Docker image',
          value: '${{ jobs.%s.outputs.digest }}' % output_digest_job_name,
        },
      },
    },
  },
  copy_from_docker_step: copy_from_docker_step,
  validate: validate,
  retag_step: retag_step,
}
