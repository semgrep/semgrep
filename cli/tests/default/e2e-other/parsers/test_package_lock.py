from pathlib import Path

import pytest

from semdep.parsers.package_lock import parse_package_name
from semdep.parsers.package_lock import parse_packages_field
from semdep.parsers.util import JSON
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity


@pytest.mark.quick
def test_package_lock_v2_parser_ignores_root():
    lockfile_path = Path("test/fixtures/package-lock-v2/package-lock.json")
    only_root_dep = {
        "": JSON(
            line_number=6,
            value={
                "dependencies": JSON(
                    line_number=7,
                    value={"bootstrap": JSON(line_number=8, value="^5.3.3")},
                )
            },
        )
    }
    parsed_deps = parse_packages_field(lockfile_path, only_root_dep, manifest_path=None)
    assert parsed_deps == []


@pytest.mark.quick
@pytest.mark.parametrize(
    "raw_package_path, package_name",
    [
        ("node_modules/@popperjs/core", "@popperjs/core"),
        ("@npmcli/arborist@latest", "@npmcli/arborist@latest"),
        ("grunt-csscomb", "grunt-csscomb"),
        ("node_modules/mongodb-js/saslprep/@types/whatwg-url", "@types/whatwg-url"),
    ],
)
def test_package_lock_v2_parser_parses_package_name(raw_package_path, package_name):
    assert parse_package_name(raw_package_path) == package_name


@pytest.mark.quick
@pytest.mark.parametrize(
    "raw_package_path, package_name",
    [
        ("lib/@popperjs/core", "@popperjs/core"),
        ("lib/legacy-bootstrap", "legacy-bootstrap"),
    ],
)
def test_package_lock_v2_parser_parses_package_outside_node_modules(
    raw_package_path, package_name
):
    assert parse_package_name(raw_package_path) == package_name


@pytest.mark.quick
def test_package_lock_v2_parser_produces_correct_deps():
    lockfile_path = Path("test/fixtures/package-lock-v2/package-lock.json")
    v3_deps = {
        "": JSON(
            line_number=6,
            value={
                "dependencies": JSON(
                    line_number=7,
                    value={"bootstrap": JSON(line_number=8, value="^5.3.3")},
                )
            },
        ),
        "node_modules/@popperjs/core": JSON(
            line_number=11,
            value={
                "version": JSON(line_number=12, value="2.11.8"),
                "resolved": JSON(
                    line_number=13,
                    value="https://registry.npmjs.org/@popperjs/core/-/core-2.11.8.tgz",
                ),
                "integrity": JSON(
                    line_number=14,
                    value="sha512-P1st0aksCrn9sGZhp8GMYwBnQsbvAWsZAX44oXNNvLHGqAOcoVxmjZiohstwQ7SqKnbR47akdNi+uleWD8+g6A==",
                ),
                "peer": JSON(line_number=15, value=True),
                "funding": JSON(
                    line_number=16,
                    value={
                        "type": JSON(line_number=17, value="opencollective"),
                        "url": JSON(
                            line_number=18, value="https://opencollective.com/popperjs"
                        ),
                    },
                ),
            },
        ),
        "libs/legacy-bootstrap": JSON(
            line_number=21,
            value={
                "version": JSON(line_number=22, value="3.0.1"),
                "resolved": JSON(
                    line_number=23,
                    value="https://registry.npmjs.org/bootstrap/-/bootstrap-5.3.3.tgz",
                ),
                "integrity": JSON(
                    line_number=24,
                    value="sha512-8HLCdWgyoMguSO9o+aH+iuZ+aht+mzW0u3HIMzVu7Srrpv7EBBxTnrFlSCskwdY1+EOFQSm7uMJhNQHkdPcmjg==",
                ),
                "funding": JSON(
                    line_number=25,
                    value=[
                        JSON(
                            line_number=26,
                            value={
                                "type": JSON(line_number=27, value="github"),
                                "url": JSON(
                                    line_number=28,
                                    value="https://github.com/sponsors/twbs",
                                ),
                            },
                        ),
                        JSON(
                            line_number=30,
                            value={
                                "type": JSON(line_number=31, value="opencollective"),
                                "url": JSON(
                                    line_number=32,
                                    value="https://opencollective.com/bootstrap",
                                ),
                            },
                        ),
                    ],
                ),
                "peerDependencies": JSON(
                    line_number=35,
                    value={"@popperjs/core": JSON(line_number=36, value="^2.11.8")},
                ),
            },
        ),
    }

    parsed_deps = parse_packages_field(lockfile_path, v3_deps, manifest_path=None)
    assert parsed_deps == [
        FoundDependency(
            package="@popperjs/core",
            version="2.11.8",
            ecosystem=Ecosystem(value=Npm()),
            allowed_hashes={
                "sha512": [
                    "3f5b2dd1a92c0ab9fdb06661a7c18c63006742c6ef016b19017e38a1734dbcb1c6a8039ca15c668d98a886cb7043b4aa2a76d1e3b6a474d8beba57960fcfa0e8"
                ]
            },
            transitivity=Transitivity(value=Transitive()),
            resolved_url="https://registry.npmjs.org/@popperjs/core/-/core-2.11.8.tgz",
            line_number=11,
            children=None,
            git_ref=None,
            lockfile_path=Fpath(str(lockfile_path)),
            manifest_path=None,
        ),
        FoundDependency(
            package="legacy-bootstrap",
            version="3.0.1",
            ecosystem=Ecosystem(value=Npm()),
            allowed_hashes={
                "sha512": [
                    "f072c2756832a0c82e48ef68f9a1fe8ae67e6a1b7e9b35b4bb71c833356eed2aeba6fec4041c539eb165482b24c1d635f843854129bbb8c2613501e474f7268e"
                ]
            },
            transitivity=Transitivity(value=Transitive()),
            resolved_url="https://registry.npmjs.org/bootstrap/-/bootstrap-5.3.3.tgz",
            line_number=21,
            children=None,
            git_ref=None,
            lockfile_path=Fpath(str(lockfile_path)),
            manifest_path=None,
        ),
    ]
