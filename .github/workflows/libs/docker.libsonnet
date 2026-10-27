// Shared Docker build/run helpers and binary artifact extraction.
// Registry authentication and publishing policy belong to callers.

local uses = import './uses.libsonnet';
local actions = import 'actions.libsonnet';
local gha = import 'gha.libsonnet';

local default_platforms = 'linux/amd64,linux/arm64';
local archs = ['amd64', 'arm64'];

local digest_output_template = '${{ steps.build-%s-docker-image.outputs.digest }}';

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
  description='',  // User-friendly name to appear in the GitHub Actions step name
  target,  // Target docker layer (i.e. build, semgrep-cli, etc.)
  prefix='',  // prefix for image tags
  suffix='',  // suffix for image tags
  file='Dockerfile',
  build_args='',  // if you use ARG FOO=... in a dockerfile, set it here as 'FOO1=FOO,FOO2=FOO,...'
  platforms=default_platforms,  // arm or x86 or both
  load=false,  // load image after (useful if you want to run it) NOTE: you probably want only want the platform to be the one of the runner
      ) =
  [
    // Documentation for this step is here:
    // https://github.com/docker/metadata-action
    //
    // Prepare the tags and labels for the build step.
    {
      id: '%s-prepare-metadata' % target,
      name: 'Set tags and labels',
      uses: uses.docker.metadata_action,
      with: {
        images: '',
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
        // Use the checked-out SHA rather than the event's SHA.
        //
        // TODO: When we trigger this manually, the branch tag is applied
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
    {
      id: 'build-%s-docker-image' % target,
      // step name = "WHAT: HOW"
      name: description
            + (if description == '' then '' else ': ')
            + 'Build docker image in Depot',
      uses: uses.depot.build_push_action,
      with: {
        // Expect the checkout root for this build context to include a
        // depot.json with the Depot project ID for the repo/workspace being
        // built.
        // Use the caller's checkout as the build context.
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

        push: false,

        // Whether to load the built image into the GHA runner
        load: load,
      },
    },
  ];

local job(
  description='',  // User-friendly name to appear in the GitHub Actions
  target,  // Target docker layer (i.e. build, semgrep-cli, etc.)
  prefix='',  // prefix for image tags
  suffix='',  // suffix for image tags
  artifact_name=null,  // name of artifact to copy from docker image
  needs=[],  // prereq GHA steps
  checkout_steps=actions.checkout_with_submodules,
  pre_build_steps=[],  // steps between checkout and Depot setup
  build_steps_fn=build_steps,  // function with the build_steps signature
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
           pre_build_steps +
           [

             {
               uses: uses.depot.setup_action,
             },
           ] +
           build_steps_fn(
             description=description,
             target=target,
             prefix=prefix,
             suffix=suffix,
             file=file,
             build_args=build_args,
             platforms=platforms,
             load=load,
           )
           +
           (if artifact_name != null then (
              [
                copy_from_docker_step(target='%s-binaries' % target, output_dir='binaries', file=file, platforms=platforms, build_args=build_args),
              ] + std.flattenArrays(
                std.map(function(arch)
                  [
                    actions.upload_artifact_step('%s-linux-%s' % [artifact_name, arch], '/tmp/binaries/linux_%s/*' % arch),
                  ], archs)
              )

            ) else []) + post_steps,
  };

local build_and_run_gha_job(
  description,
  target,
  needs=[],
  checkout_steps=actions.checkout_with_submodules,
  build_args='',
  target_dir=null,
  output_dir=null,
  platforms=default_platforms,
  env='',
      ) = job(
  target=target,
  description='Build %s' % description,
  needs=needs,
  checkout_steps=checkout_steps,
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
);

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
  build_and_run_gha_job: build_and_run_gha_job,
  pull_and_run_image_steps: pull_and_run_image_steps,
}
