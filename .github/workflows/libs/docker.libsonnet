// Workflow to build and push arbitrary images for each PR.
// This also saves the built binaries as artifacts for other GHA workflows.
//
// A lot of this file is copy-pasted from:
// - build-test-linux-arm64.jsonnet which has good configurations
//   to build successfully on Pro.
// - OSS/.../build-test-docker.jsonnet which has good configurations
//   to add tags and labels to the docker image properly, and also
//   to build for multiple platforms.

local uses = import './uses.libsonnet';
local actions = import 'actions.libsonnet';
local gha = import 'gha.libsonnet';

// TODO: Generalize and also make build-test-linux-arm64.jsonnet share the
//       same job. Maybe also use it in the release script.

local default_platforms = 'linux/amd64,linux/arm64';
local archs = ['amd64', 'arm64', 'x86'];
local ecr_repo = '338683922796.dkr.ecr.us-west-2.amazonaws.com/semgrep/semgrep-proprietary';

local digest_output_template = '${{ steps.build-%s-docker-image.outputs.digest }}';

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
local copy_from_docker_step(target, output_dir, file='Dockerfile', platforms=default_platforms, build_args='') = {

  name: 'Copy all files from %s to GHA runner machine' % target,
  uses: uses.depot.build_push_action,
  with: {
    // Expect the checkout root for this build context to include a depot.json
    // with the Depot project ID for the repo/workspace being built.
    context: '.',
    platforms: platforms,
    file: file,
    target: target,
    outputs: 'type=local,dest=/tmp/%s' % output_dir,
    // These need to be the same as for the actual build so as to ensure that caching triggers
    'build-args': |||
      VCS_REF_HEAD_NAME=${{ github.head_ref || github.ref_name }}
      VCS_REF_HEAD_REVISION=%(ref_expr)s%(build_args)s
      GITHUB_SERVER_URL=${{ github.server_url }}
      GITHUB_REPOSITORY=${{ github.repository }}
      GITHUB_SHA=${{ github.sha }}
      GITHUB_RUN_ID=${{ github.run_id }}
      GITHUB_RUN_ATTEMPT=${{ github.run_attempt }}
      GITHUB_HEAD_REF=${{ github.head_ref }}
      GITHUB_REF=${{ github.ref }}
      GITHUB_WORKFLOW=${{ github.workflow }}
      GITHUB_RUN_NUMBER=${{ github.run_number }}
      GITHUB_JOB=${{ github.job }}
      GITHUB_WORKSPACE=${{ github.workspace }}
    ||| % { ref_expr: gha.ref_expr, build_args: if build_args == '' then '' else '\n' + build_args },
  },
};

local validate(
  job,
  script,
  needs,
  checkout_steps=actions.checkout(),
  image='semgrep',
      ) =
  {
    'runs-on': 'ubuntu-latest',
    needs: needs,
    steps: checkout_steps +
           [
             {
               name: 'Test %s image' % job,
               env: {
                 IMAGEID: 'semgrep/%s@${{ needs.%s.outputs.digest }}' % [image, job],
               },
               // only test amd64 for now. I suspect this is fine for now
               run: '%s "$IMAGEID" linux/amd64' % script,
             },
           ],
  };

local retag_step(source_image, target_image, tag, ref, debug=false, dry_run) = {
  name: 'Retag %(source_image)s:%(ref)s to %(target_image)s:%(tag)s' % { source_image: source_image, target_image: target_image, ref: ref, tag: tag },
  env:
    {
      source_image: source_image,
      source_ref: ref,
      target_image: target_image,
      target_tag: tag,
      debug: debug,
      dry_run: dry_run,
    },
  run: |||
    if [[ "${debug}" == "true" ]]; then
      echo "Enabling debug logging..."
      set -x
    fi

    source_image="${source_image}:${source_ref}"
    target_image="${target_image}:${target_tag}"

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
    if [[ "${dry_run}" == "false" ]]; then
      docker buildx imagetools create -t ${target_image} ${source_image}
    else
      echo "(dry run)"
      docker buildx imagetools create --dry-run -t ${target_image} ${source_image}
      echo "(dry run)"
    fi
  |||,
};

local github_sha = gha.ref_expr;

// pull and run a docker image, optionally copying some files from it
local pull_and_run_image_steps(image, target_dir=null, output_dir=null, env='') = [
  {
    name: 'Make env file for docker run',
    env: {
      ENV_VARS: env,
    },
    run: 'echo "${ENV_VARS}" > /tmp/docker-env-file.env',
  },
  {
    id: 'run-docker-image',
    name: 'Run docker image',
    env: {
      IMAGE: image,
    },
    run: 'docker run --env-file /tmp/docker-env-file.env --name run_step ${IMAGE}',
  },
] + (if output_dir != null && target_dir != null then [
       {
         name: 'Copy files from docker image',
         // only if prev step failed
         'if': "failure() && steps.run-docker-image.outcome == 'failure'",
         env: {
           TARGET_DIR: target_dir,
           OUTPUT_DIR: output_dir,
         },
         run: 'docker cp run_step:${TARGET_DIR} /tmp/${OUTPUT_DIR}',
       },
     ] else []);

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local build_steps(
  name,  // Name of docker image if uploaded (i.e. semgrep/NAME)
  description='',  // User-friendly name to appear in the GitHub Actions step name
  target,  // Target docker layer (i.e. build, semgrep-cli, etc.)
  prefix='',  // prefix for image tags
  suffix='',  // suffix for image tags
  push=false,  // push to registries including PUBLIC ONES! Do NOT push our code to public plz
  push_ecr=false,  // push only to ecr
  ecr_repo_override=null,  // override the default ECR repo (e.g. for pushing to a separate repo)
  file='Dockerfile',
  build_args='',  // if you use ARG FOO=... in a dockerfile, set it here as 'FOO1=FOO,FOO2=FOO,...'
  platforms=default_platforms,  // arm or x86 or both
  load=false,  // load image after (useful if you want to run it) NOTE: you probably want only want the platform to be the one of the runner
      ) =
  local effective_ecr_repo = if ecr_repo_override != null then ecr_repo_override else ecr_repo;
  [
    // Documentation for this step is here:
    // https://github.com/docker/metadata-action
    //
    // In summary, this step prepares the right tags (and docker registries)
    // for us to push to in the build-pro-docker-image below.
    {
      id: '%s-prepare-metadata' % target,
      name: 'Set tags and labels',
      uses: uses.docker.metadata_action,
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
        images: (if push then |||
                   returntocorp/%(name)s
                   semgrep/%(name)s
                 ||| % { name: name } else '') + (if push || push_ecr then |||
                                                    %(ecr_repo)s
                                                  ||| % { ecr_repo: effective_ecr_repo } else ''),
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
        ||| % { prefix: prefix, ref_expr: gha.ref_expr },
      },
    },
    // The tags should be setup correctly from the prepare-metadata step
    // above, so this step knows that it needs to push to both
    // docker hub and Amazon ECR.
    //
    // To test the docker image from docker hub, you can do something like
    //   docker run -it --rm \
    //     -v $(pwd):/src \
    //     semgrep/semgrep:pr-XXXX
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
    //     semgrep/semgrep:pr-XXXX
    //     338683922796.dkr.ecr.us-west-2.amazonaws.com/semgrep/semgrep-proprietary:pr-XXXX
    //     semgrep scan /src
    //
    // where you can look up available images at go/ecr and navigate to the
    // semgrep/semgrep-proprietary registry.
    {
      id: 'build-%s-docker-image' % target,
      // step name = "WHAT: HOW"
      name: description
            + (if description == '' then '' else ': ')
            + (
              'Build docker image in Depot%s'
              % ((if push then ' and push to Docker Hub/ECR' else '') +
                 (if push_ecr then ' and push to Amazon ECR' else ''))
            ),
      uses: uses.depot.build_push_action,
      with: {
        // Expect the checkout root for this build context to include a
        // depot.json with the Depot project ID for the repo/workspace being
        // built.
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
        tags: '${{ steps.%s-prepare-metadata.outputs.tags }}' % target,
        labels: '${{ steps.%s-prepare-metadata.outputs.labels }}' % target,

        // The file used to build the image. Depot seems to default
        // to whatever's in the top-level git directory, but putting
        // it in here to be explicit.
        file: file,

        target: target,

        // Set the build args for the docker image
        // VCS_* just specify what commit/branch this was built on
        // These have no effect if ARG isn't used in the Dockerfile
        'build-args': |||
          VCS_REF_HEAD_NAME=${{ github.head_ref || github.ref_name }}
          VCS_REF_HEAD_REVISION=%(ref_expr)s%(build_args)s
        ||| % { ref_expr: gha.ref_expr, build_args: if build_args == '' then '' else '\n' + build_args }
        ,
        // This flag controls if for whatever reason depot fails to
        // build the docker image on their fast native arm64 runners, whether
        // depot will fallback to docker-buildx which uses emulation (which is
        // really slow). So if this job suddently takes more than 15min,
        // it's probably because there is a problem somewhere and the
        // fallback is activated. A common solution is to reset the active
        // repo's depot.dev cache using the "Reset cache" button at the
        // bottom of the project settings page, especially if depot.dev gets
        // confused by changes in submodules in a PR.
        // We used to set this to true, just in case Depot had some bugs
        // but this would fallback for any error, not just Depot error,
        // and then you need to wait 1h30min to actually see the error
        // so better to set this to false
        'buildx-fallback': false,

        // Also maybe push to Docker hub and/or ec2
        push: push || push_ecr,

        // Whether to load the built image into the GHA runner
        load: load,
      },
    },
  ];

local job(
  name,  // Name of docker image if uploaded (i.e. semgrep/NAME)
  description='',  // User-friendly name to appear in the GitHub Actions
  target,  // Target docker layer (i.e. build, semgrep-cli, etc.)
  prefix='',  // prefix for image tags
  suffix='',  // suffix for image tags
  artifact_name=null,  // name of artifact to copy from docker image
  needs=[],  // prereq GHA steps
  checkout_steps=actions.checkout_with_submodules,
  push=false,  // push to registries including PUBLIC ONES! Do NOT push our code to public plz
  push_ecr=false,  // push only to ecr
  ecr_repo_override=null,  // override the default ECR repo (e.g. for pushing to a separate repo)
  file='Dockerfile',
  build_args='',  // if you use ARG FOO=... in a dockerfile, set it here as 'FOO1=FOO,FOO2=FOO,...'
  platforms=default_platforms,
  post_steps=[],
  load=false,
  large=false,
      ) =
  (if needs != [] then { needs: needs } else {}) +
  {
    'runs-on': (if large then 'depot-ubuntu-24.04-8' else 'depot-ubuntu-24.04'),
    outputs: {
      digest: digest_output_template % target,
    },
    permissions: gha.read_permissions,
    steps: checkout_steps(ref=gha.ref_expr) +
           (
             if push then [
               // This is needed so we can push images to docker hub successfully.
               actions.docker_login_step,
             ] else []
           ) + (if push_ecr || push then actions.ecr_login_steps else []) +
           [

             {
               uses: uses.depot.setup_action,
             },
           ] +
           build_steps(
             name=name,
             description=description,
             target=target,
             prefix=prefix,
             suffix=suffix,
             push=push,
             push_ecr=push_ecr,
             ecr_repo_override=ecr_repo_override,
             file=file,
             build_args=build_args,
             platforms=platforms,
             load=load,
           )
           +
           (if artifact_name != null then (
              [
                copy_from_docker_step(target='%s-binaries' % target, output_dir='binaries', platforms=platforms, build_args=build_args),
              ] + std.flattenArrays(
                std.map(function(arch)
                  [
                    actions.make_artifact_step('/tmp/binaries/linux_%s/*' % arch_to_docker_arch[arch]),
                    actions.upload_artifact_step('%s-linux-%s' % [artifact_name, arch]),
                  ], archs)
              )

            ) else []) + post_steps,
  };

local build_and_run_gha_job(
  name,
  description,
  target,
  needs=[],
  checkout_steps=actions.checkout_with_submodules,
  build_args='',
  target_dir=null,
  output_dir=null,
  platforms=default_platforms,
  env='',
  write_permission=true,
      ) = job(
  name=name,
  target=target,
  description='Build %s' % description,
  needs=needs,
  checkout_steps=checkout_steps,
  push=false,
  push_ecr=false,
  build_args=build_args,
  platforms=platforms,
  // if you're doing some testing lets give you a larger machine
  large=true,
  post_steps=pull_and_run_image_steps(
    'sha-%(ref_expr)s' % { ref_expr: gha.ref_expr },
    target_dir=target_dir,
    output_dir=output_dir,
    env=env,
  ),
  load=true
) + (if write_permission then {
       permissions: gha.pull_request_permissions,
     } else {});

local inputs = {
  ref: gha.ref_input,
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------

{
  job: job,
  build_steps: build_steps,
  // Called from other workflows (e.g. build-and-test.jsonnet)
  on_docker_workflow: function(output_digest_job_name='job', extra_inputs={}) {
    workflow_dispatch: {
      inputs: inputs + extra_inputs,
    },
    workflow_call: {
      inputs: inputs + extra_inputs,
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
  build_and_run_gha_job: build_and_run_gha_job,
  ecr_repo: ecr_repo,
}
