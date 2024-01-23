// workflow used to check the latest version of Semgrep Pro, ensuring the
// version manifests are up to date before releasing.

local semgrep = import 'libs/semgrep.libsonnet';
local gha = import 'libs/gha.libsonnet';

// ----------------------------------------------------------------------------
// The inputs
// ----------------------------------------------------------------------------
local inputs = {
  inputs: {
    'bucket-name': {
      type: 'string',
      description: 'Bucket name (not the ARN) to pull the manifest from.',
      required: true,
    },
    'manifest-key': {
      type: 'string',
      description: 'Name (key) of the manifest JSON file in S3',
      required: true,
    },
    'semgrep-version': {
      type: 'string',
      description: 'The version of Semgrep OSS to add to the manifest',
      required: true,
    },
    'dry-run': {
      type: 'boolean',
      description: 'Dry-Run flag determines whether or not the check failing means that the job will actually fail.',
      required: true,
    },
  },
};

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------
local job = {
  'runs-on': 'ubuntu-22.04',
  permissions: gha.write_permissions,
  steps: [
    semgrep.aws_credentials_step(
     role='returntocorp-semgrep-deploy-role',
     session_name='semgrep-s3-access'
    ),
    {
      name: 'Download binaries from S3',
      run: 'aws s3 cp "s3://${{ inputs.bucket-name }}/${{ inputs.manifest-key }}" versions-manifest.json',
    },
    {
      name: 'Check Version',
      'if': '${{ ! inputs.dry-run }}',
      run: |||
        VERSION_EXISTS=$(jq --arg semgrep_version ${{ inputs.semgrep-version }} '.versions | has($semgrep_version)' versions-manifest.json)

        if [ "${VERSION_EXISTS}" = "true" ]; then
            echo "This vesion has a corresponding Semgrep Pro version - continuing."
        else
            echo "This version of semgrep does not yet have a corresponding version of Semgrep Pro."
            echo "Please run a release of Semgrep Pro, and pass in Semgrep Version = ${{ inputs.semgrep-version }}"
            exit 1
        fi
      |||,
    },
    {
      name: 'Check Version - Dry Run',
      'if': '${{ inputs.dry-run }}',
      run: |||
        VERSION_EXISTS=$(jq --arg semgrep_version ${{ inputs.semgrep-version }} '.versions | has($semgrep_version)' versions-manifest.json)

        if [ "${VERSION_EXISTS}" = "true" ]; then
            echo "This vesion has a corresponding Semgrep Pro version - continuing."
        else
            echo "This version of semgrep does not yet have a corresponding version of Semgrep Pro."
            echo "Please run a release of Semgrep Pro, and pass in Semgrep Version = ${{ inputs.semgrep-version }} before running a release"
            echo "Job was not blocked because dry-run was true."
        fi
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'check-semgrep-pro-version',
  on: {
    workflow_dispatch: inputs,
    workflow_call: inputs,
  },
  jobs: {
    job: job,
  },
}
