#!/usr/bin/env python3
from __future__ import print_function

import hashlib
import json
import sys
import urllib.request
from typing import Any
from typing import Callable
from typing import Dict
from typing import List


def check_release_notes(release: Dict[str, Any], version: str) -> List[str]:
    if release["prerelease"]:
        print("Not checking release notes on a pre-release")
        return []
    else:
        errs = []
        if not "Changed" in release["body"]:
            errs.append('Missing "Changed" section in release notes')
        if not "Added" in release["body"]:
            errs.append('Missing "Added" section in release notes')
        return errs


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
        return [f"Checksum for {name} was invalid {digest} != {checksum_data}"]
    return []


CHECKS = [
    check_release_notes,
    check_ubuntu_binary,
    check_osx_binary,
    validate_checksums,
]

if __name__ == "__main__":
    import os

    print(os.environ)
    with open("version") as f:
        version = f.read()
    # release_obj = urllib.request.urlopen
    # with open("release.json") as f:
    #    release = json.load(f)

    # version = release["tag_name"]

    # errs: List[str] = []
    # for check in CHECKS:
    #    errs += check(release, version)

    # if errs:
    #    print("Several problems with the release were found:")
    #    print("\n".join(errs))
    #    sys.exit(1)
    # else:
    #    print("Release looks good!")
