// Push docker images to https://hub.docker.com/r/semgrep/semgrep
// (and before that https://hub.docker.com/r/returntocorp/semgrep).
//
// This is actually a workflow called from other workflows, so
// see release.jsonnet and tests.jsonnet for example of use.
//
// Note that we now support multi-arch docker images, one for
// linux/amd64 and a linux/arm64 (which is useful for macOS actually).
// See https://docs.docker.com/build/building/multi-platform/,
// but that means this workflow is more complicated than just
// a 'docker push'. Instead, we rely on regctl
// https://github.com/regclient/regclient/tree/main
//
// See also promote-canary-to-latest.jsonnet for more info.

local actions = import 'libs/actions.libsonnet';

// ----------------------------------------------------------------------------
// Input
// ----------------------------------------------------------------------------
local inputs = {
  inputs: {
    'artifact-name': {
      type: 'string',
      description: 'Name (key) to use when uploading the docker image tarball as an artifact',
      required: true,
    },
    'repository-name': {
      type: 'string',
      description: 'The repository/name of the docker image to push, e.g., semgrep/semgrep',
      required: true,
    },
    'dry-run': {
      type: 'boolean',
      description: 'Whether a dry-run (e.g., print tags to push) should be peformed. Actually push images if false.',
      required: true,
    },
  },
};

local unless_dry_run = {
  'if': "${{ ! inputs.dry-run }}"
};

// ----------------------------------------------------------------------------
// The Job
// ----------------------------------------------------------------------------
local job = {
  'runs-on': 'ubuntu-22.04',
  steps: [
    {
      uses: 'iarekylew00t/regctl-installer@v1',
    },
    {
      uses: 'actions/download-artifact@v4',
      with: {
        path: '/tmp/artifacts',
      },
    },
    {
      name: 'Merge images',
      env: {
        MERGED_IMAGE_PATH: '/tmp/merged-image',
      },
      //TODO: link to a blog post explaining those magic incantations?
      run: |||
        mkdir -p ${MERGED_IMAGE_PATH}/blobs/sha256

        # Find docker image artifacts
        image_filenames=(/tmp/artifacts/${{ inputs.artifact-name }}-arch-*/image.tar)
        if [[ ${#image_filenames[@]} == 0 ]]; then
          echo "No artifacts found matching ${{ inputs.artifact-name }}-arch-*"
          exit 1
        else
          echo "Found images: ${image_filenames[@]}"
        fi

        # Create an empty manifest list. This will ultimately be the union of
        # all the arch-specific image manifests we intend to publish.
        cat << "EOF" > ${MERGED_IMAGE_PATH}/index.json
        {
          "schemaVersion": 2,
          "mediaType": "application/vnd.docker.distribution.manifest.list.v2+json",
          "manifests": []
        }
        EOF
        echo "Wrote empty manifest list to ${MERGED_IMAGE_PATH}/index.json"

        # Merge the contents (blobs and manifest) of each docker image into one
        # big multi-arch image
        for image_filename in ${image_filenames[@]}; do
          echo "Merging contents of ${image_filename}"
          cd $(dirname $image_filename)
          tar xvf image.tar

          # Copy image's blobs into merged blobs folder (overwrite is fine
          # since this is content-addressable storage)
          cp -rvf blobs/sha256/* ${MERGED_IMAGE_PATH}/blobs/sha256/

          # Merge image's manifest list into our merged manifest list
          echo "Adding image manifest(s) to manifest list:"
          jq ".manifests" index.json
          jq -s '.[0].manifests+=.[1].manifests|.[0]' ${MERGED_IMAGE_PATH}/index.json index.json > ${MERGED_IMAGE_PATH}/index.tmp
          mv ${MERGED_IMAGE_PATH}/index.tmp ${MERGED_IMAGE_PATH}/index.json
        done
      |||,
    },
    actions.docker_login_step + unless_dry_run,
    {
      name: 'Push merged image',
      env: {
        MERGED_IMAGE_PATH: '/tmp/merged-image',
      },
      run: |||
        echo "Uploading image blobs..."
        for blob_filename in ${MERGED_IMAGE_PATH}/blobs/sha256/*; do
          blob_digest="sha256:$(basename $blob_filename)"
          echo "-> ${blob_digest}"
          if ! regctl blob head ${{ inputs.repository-name }} ${blob_digest}; then
            cat $blob_filename | regctl blob put ${{ inputs.repository-name }} --digest ${blob_digest}
          fi
        done

        echo "Uploading image manifests..."
        for manifest_digest in $(cat ${MERGED_IMAGE_PATH}/index.json | jq -r '.manifests | unique_by(.digest) | .[].digest' | cut -d: -f2); do
          echo "-> ${manifest_digest}"
          cat ${MERGED_IMAGE_PATH}/blobs/sha256/${manifest_digest} | regctl manifest put ${{ inputs.repository-name }} --by-digest
        done

        echo "Pushing image tags..."
        for tag in $(cat ${MERGED_IMAGE_PATH}/index.json | jq -r '.manifests | unique_by(.annotations."org.opencontainers.image.ref.name") | .[].annotations."org.opencontainers.image.ref.name"'); do
          echo "-> ${{ inputs.repository-name }}:${tag}"
          index_digest=$(cat ${MERGED_IMAGE_PATH}/index.json | regctl manifest put ${{ inputs.repository-name }}:${tag} -t application/vnd.docker.distribution.manifest.list.v2+json)
        done

        echo "Done!"
      |||,
    } + unless_dry_run,
  ],
};

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'push-docker',
  on: {
    workflow_dispatch: inputs,
    workflow_call: inputs,
  },
  jobs: {
    job: job,
  },
}
