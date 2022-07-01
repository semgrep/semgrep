#!/usr/bin/env python3
import hashlib
import json
import os
import sys
import urllib.request
from typing import Any
from typing import Callable
from typing import Dict
from typing import List


def asset_for(
    release: Dict[str, Any], f: Callable[[Dict[str, Any]], Any]
) -> List[Dict[str, Any]]:
    return [asset for asset in release["assets"] if f(asset)]


def check_ubuntu_binary(release: Dict[str, Any], version: str) -> List[str]:
    errs = []
    # TODO: should we be making a 16.04 asset?
    for ubuntu_version in ["16.04"]:  # , '18.04']:
        expected_name = f"semgrep-{version}-ubuntu-{ubuntu_version}.tgz"
        ubuntu_assets = asset_for(release, lambda a: a["name"] == expected_name)
        if len(ubuntu_assets) != 1:
            asset_names = [asset["name"] for asset in release["assets"]]
            errs.append(
                f"No asset for {ubuntu_version} (expected name: {expected_name}. Assets: {asset_names}"
            )
            continue
    return errs


def check_osx_binary(release: Dict[str, Any], version: str) -> List[str]:
    errs = []
    expected_name = f"semgrep-{version}-osx.zip"
    osx_assets = asset_for(release, lambda a: a["name"] == expected_name)
    if len(osx_assets) != 1:
        asset_names = [asset["name"] for asset in release["assets"]]
        errs.append(
            f"No asset for osx (expected name: {expected_name}. Assets: {asset_names}"
        )
    return errs


def validate_checksums(release: Dict[str, Any], version: str) -> List[str]:
    errs: List[str] = []
    all_assets = asset_for(release, lambda asset: "sha256" not in asset["name"])
    for asset in all_assets:
        checksum = asset_for(release, lambda a: a["name"] == asset["name"] + ".sha256")
        if not checksum:
            errs.append(f'No check sum for {asset["name"]}')
        checksum_data = checksum[0]
        errs += validate_checksum(
            asset["name"],
            asset["browser_download_url"],
            checksum_data["browser_download_url"],
        )

    return errs


def validate_checksum(name: str, actual_url: str, checksum_url: str) -> List[str]:
    actual_data = urllib.request.urlopen(actual_url).read()
    checksum_data = (
        urllib.request.urlopen(checksum_url).read().decode("utf-8").split()[0].strip()
    )
    digest = hashlib.sha256(actual_data).hexdigest()
    if digest != checksum_data:
        checksum_string = f"Checksum for {name} was invalid {digest} != {checksum_data}"
        return [checksum_string]
    return []


CHECKS = [
    check_ubuntu_binary,
    check_osx_binary,
    validate_checksums,
]

if __name__ == "__main__":
    # Environment variables
    # 'GITHUB_REF': 'refs/tags/vGenInstall-10',
    # 'GITHUB_REPOSITORY': 'rcoh/semgrep', 'GITHUB_
    if len(sys.argv) != 2:
        print("USAGE: python3 validate-release.py 0.X.0")
        exit(1)

    release_tag = f"v{sys.argv[1]}"

    version = release_tag
    print(f"Testing {release_tag}")
    repo = os.environ.get("GITHUB_REPOSITORY", "rcoh/semgrep")

    release_json_url = f"https://api.github.com/repos/{repo}/releases"
    releases = json.load(urllib.request.urlopen(release_json_url))
    matching_releases = [r for r in releases if r["tag_name"] == release_tag]
    print([r["tag_name"] for r in releases])
    if not matching_releases:
        print(f"No matching release found for {release_tag}")
        sys.exit(1)

    release = matching_releases[0]

    errs: List[str] = []
    for check in CHECKS:
        errs += check(release, version)

    if errs:
        print("Several problems with the release were found:")
        print("\n".join(errs))
        sys.exit(1)
    else:
        print("Release looks good!")
