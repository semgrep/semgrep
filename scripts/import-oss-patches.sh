#!/usr/bin/env bash
# Import source commits into a destination directory using its clean filters.
# Run from a clean, LFS-expanded checkout; source commit objects must be available.
set -euo pipefail

directory=${1:?Usage: import-oss-patches.sh DIRECTORY COMMIT...}
shift

apply_args=(--allow-empty)
if [[ "$directory" != . ]]; then
    apply_args+=(--directory="$directory")
fi

if [[ -n "$(git status --porcelain)" ]]; then
    echo "error: importing commits requires a clean checkout" >&2
    exit 1
fi

# Validate every source commit before importing any of them.
for commit in "$@"; do
    git cat-file -e "$commit^{commit}"
    if git diff-tree --root --no-commit-id --no-renames --raw -r "$commit" |
        grep -E '^:(160000 |[0-7]{6} 160000 )' > /dev/null; then
        echo "error: submodule changes are not supported (commit $commit)" >&2
        exit 1
    fi
done

for commit in "$@"; do
    # Match the source diff against expanded contents, then let git add encode
    # the result using the destination's LFS rules.
    git diff-tree --root --no-commit-id --no-ext-diff --no-textconv \
        --binary --full-index -p "$commit" |
        git -c filter.lfs.process= -c filter.lfs.clean=cat \
            -c filter.lfs.required=false apply "${apply_args[@]}"
    # Force only source-tracked paths, preserving ignored additions without
    # including unrelated ignored files. Disable rename detection to list both ends.
    git diff-tree --root --no-commit-id --no-renames --name-only -r -z "$commit" |
        while IFS= read -r -d '' path; do
            git --literal-pathspecs add -f -A -- "$directory/$path"
        done
    git commit --allow-empty --allow-empty-message --cleanup=verbatim -C "$commit"
    echo "Imported $commit"
done
