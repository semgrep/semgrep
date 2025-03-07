// Workflow to build and push arbitrary images for each PR.
// This also saves the built binaries as artifacts for other GHA workflows.
//
// A lot of this file is copy-pasted from:
// - build-test-linux-arm64.jsonnet which has good configurations
//   to build successfully on Pro.
// - OSS/.../build-test-docker.jsonnet which has good configurations
//   to add tags and labels to the docker image properly, and also
//   to build for multiple platforms.

local gha = import 'gha.libsonnet';
local actions = import 'actions.libsonnet';

// GHA expression that evaluates to the input ref or, if that is empty, the sha
// associated with the trigger for this action.
local ref_expr = "${{ inputs.ref != '' && inputs.ref || github.sha }}";

// TODO: Generalize and also make build-test-linux-arm64.jsonnet share the
//       same job. Maybe also use it in the release script.

local platforms = 'linux/amd64,linux/arm64';
local archs = ['amd64', 'arm64'];
// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local copy_from_docker(artifact_name, target, arch, file='Dockerfile') = [
    {
        id: 'copy-%s-%s-binaries' % [artifact_name, arch],
        name: 'Copy %s binaries from the Docker image to GHA runner machine' % artifact_name,
        uses: 'depot/build-push-action@v1.9.0',
        with: {
        project: '${{ secrets.DEPOT_PROJECT_ID }}',
        context: '.',
        platforms: platforms,
        file: file,
        target: '%s-binaries' % target,
        outputs: 'type=local,dest=/tmp/binaries',
        },
    },
    actions.make_artifact_step('/tmp/binaries/linux_%s/%s' % [arch, artifact_name]),
    actions.upload_artifact_step('%s-linux-%s' % [artifact_name, arch])
];

local job(
    name,
    target,
    prefix='oss',
    artifact_name=null,
    checkout_steps=actions.checkout_with_submodules,
    latest=false,
    push=false,
    file='Dockerfile') = {
  'runs-on': 'ubuntu-latest',
  outputs: {
    'digest': '${{ steps.build-%s-docker-image.outputs.digest }}' % name
  },
  permissions: gha.read_permissions,
  steps:checkout_steps(ref = ref_expr) + [
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
          semgrep/%s
          338683922796.dkr.ecr.us-west-2.amazonaws.com/semgrep/semgrep-proprietary
        ||| % name,
        flavor: |||
          latest=%s
        ||| % latest,
        // Previously used type=sha for pro-sha-* but it queries github for the
        // sha associated with the event instead of using the actual sha that we
        // checked out!
        //
        // TODO: When we trigger this manually, the pro-<branch> tag is applied
        // based on the branch that the workflow is on, not the sha provided (if
        // any). We should set this tag only if no sha is provided.
        tags: |||
          type=ref,event=pr,prefix=%s-pr-
          type=ref,event=branch,prefix=%s-
          type=raw,prefix=%s-sha-,value=%s
        ||| % [prefix, prefix, prefix, ref_expr],
      },
    },
    {
      uses: 'docker/setup-buildx-action@v3',
    },
    {
      uses: 'depot/setup-action@v1',
    },

    // This is needed so we can push images to docker hub successfully.
    actions.docker_login_step,

    // The configure-aws-credentials and login-ecr steps are needed so
    // we can push to Amazon ECR successfully.
    { id: 'configure-aws-credentials',
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
      }
    },
    { id: 'login-ecr',
      name: 'Login to Amazon ECR',
      uses: 'aws-actions/amazon-ecr-login@v2',
    },

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
    {
      id: 'build-%s-docker-image' % name,
      name: 'Build docker image in Depot %s' % (if push then 'and push to Docker Hub' else ''),
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
      },
    },
    ] +
    (if artifact_name != null then (
        copy_from_docker(artifact_name, target, "amd64", file) +
        copy_from_docker(artifact_name, target, "arm64", file)
    ) else []),
};

local inputs = {
  ref: {
    description: 'Git ref to checkout. Defaults to github.sha',
    required: false,
    type: 'string',
    default: '',
  }
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
        }
      }
    }
  },
}
