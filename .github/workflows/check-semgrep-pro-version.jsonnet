// workflow used to check the latest version of Semgrep Pro, ensuring the version
// manifests are up to date before releasing.

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
  name: 'Check Semgrep Pro Version Manifest',
  permissions: {
    'id-token': 'write',
    contents: 'write',
  },
  steps: [
    {
      name: 'Configure AWS credentials',
      uses: 'aws-actions/configure-aws-credentials@v4',
      with: {
        'role-to-assume': 'arn:aws:iam::338683922796:role/returntocorp-semgrep-deploy-role',
        'role-duration-seconds': 900,
        'role-session-name': 'semgrep-s3-access',
        'aws-region': 'us-west-2',
      },
    },
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
