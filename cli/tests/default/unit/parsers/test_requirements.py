#
# Copyright (c) 2026 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
from pathlib import Path

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.requirements import parse_requirements


@pytest.mark.quick
@pytest.mark.parametrize(
    "line, expected_package, expected_version",
    [
        # exact pins are reported bare
        ("django==4.2.0", "django", "4.2.0"),
        ("Django == 4.2.0", "django", "4.2.0"),
        ("django\t==\t4.2.0", "django", "4.2.0"),
        ("foo==1.0,!=1.1", "foo", "1.0"),
        ("foo>=0.9,==1.0", "foo", "1.0"),
        (
            "first-with-hash==3.2.3 \\\n    --hash=sha256:abc",
            "first-with-hash",
            "3.2.3",
        ),
        (
            "third-with-hash==2019.3 --hash=sha256:abc --hash=sha256:def",
            "third-with-hash",
            "2019.3",
        ),
        # unpinned requirements are reported as their canonical specifier
        ("requests>=2.0", "requests", ">=2.0"),
        ("requests >= 2.0", "requests", ">=2.0"),
        ("requests>=2.0  # via foo", "requests", ">=2.0"),
        ("requests>=2.0 --hash=sha256:abc", "requests", ">=2.0"),
        ("requests>2.0", "requests", ">2.0"),
        ("requests<3.0", "requests", "<3.0"),
        ("requests<=3.0", "requests", "<=3.0"),
        ("numpy~=1.26", "numpy", "~=1.26"),
        ("coverage!=3.5", "coverage", "!=3.5"),
        ("django==2.2.*", "django", "==2.2.*"),
        ("foo===1.0", "foo", "===1.0"),
        ("foo>=1,<2", "foo", ">=1,<2"),
        ("foo >= 1, < 2", "foo", ">=1,<2"),
        ("foo >=1 ,<2", "foo", ">=1,<2"),
        ("foo >= 1.0 , < 2.0", "foo", ">=1.0,<2.0"),
        ("eqeq-star [security] == 2.8.*, <= 2.8.1", "eqeq-star", "==2.8.*,<=2.8.1"),
        ("pkg \\\n    ==1.0", "pkg", "1.0"),
        ("pkg>=1, \\\n    <2", "pkg", ">=1,<2"),
        # PEP 508 allows the specifier list in parentheses
        ("requests (>=2.0)", "requests", ">=2.0"),
        ("requests (==1.0)", "requests", "1.0"),
        ("requests ( >=1, <2 )", "requests", ">=1,<2"),
        ('pkg[extra] (>=1) ; python_version<"3.9"', "pkg", ">=1"),
        # with no whitespace before the parentheses
        ("requests(>=2.0)", "requests", ">=2.0"),
        ("requests(==2.0)", "requests", "2.0"),
        ("requests(>=1,<2)", "requests", ">=1,<2"),
        ("pkg[extra](>=1)", "pkg", ">=1"),
        # bare names are reported with an empty version
        ("flask", "flask", ""),
        ("Flask", "flask", ""),
        ("flask\t", "flask", ""),
        # extras and markers are stripped
        ("pkg[extra]>=1", "pkg", ">=1"),
        ('pkg>=1; python_version<"3.9"', "pkg", ">=1"),
        ('pkg>=1 ; python_version<"3.9"', "pkg", ">=1"),
        ('pkg; python_version<"3.9"', "pkg", ""),
    ],
)
def test_parse_requirements_declared_versions(
    tmp_path: Path, line: str, expected_package: str, expected_version: str
) -> None:
    lockfile = tmp_path / "requirements.txt"
    lockfile.write_text(f"{line}\n")

    deps, errors = parse_requirements(lockfile, None)

    assert errors == []
    assert [(d.package, d.version, d.line_number) for d in deps] == [
        (expected_package, expected_version, 1)
    ]


@pytest.mark.quick
@pytest.mark.parametrize(
    "line",
    [
        "pkg=1.0",
        "pkg ==",
        "pkg (>=1.0",
        "pkg >== 1.0",
        "pkg==1.0 some trailing garbage",
    ],
)
def test_parse_requirements_reports_malformed_specifiers(
    tmp_path: Path, line: str
) -> None:
    lockfile = tmp_path / "requirements.txt"
    lockfile.write_text(f"{line}\n")

    deps, errors = parse_requirements(lockfile, None)

    assert deps == []
    assert [(e.line, e.reason) for e in errors] == [
        (1, "Could not parse the version specifier for 'pkg'")
    ]


@pytest.mark.quick
@pytest.mark.parametrize(
    "line, leading_token",
    [
        ("requests$foo>=1", "requests$foo"),
        ("requests)>=1", "requests)"),
        ("my%20pkg==1.0", "my%20pkg"),
        ("pkg-", "pkg-"),
    ],
)
def test_parse_requirements_reports_malformed_package_names(
    tmp_path: Path, line: str, leading_token: str
) -> None:
    lockfile = tmp_path / "requirements.txt"
    lockfile.write_text(f"{line}\n")

    deps, errors = parse_requirements(lockfile, None)

    assert deps == []
    assert [(e.line, e.reason) for e in errors] == [
        (1, f"'{leading_token}' is not a valid package name")
    ]


@pytest.mark.quick
@pytest.mark.parametrize(
    "line",
    [
        "at-url @ https://example.com/at-url.zip",
        "at-url-no-space@https://example.com/at-url.zip",
        "pkg[extra] @ https://example.com/pkg.zip",
        "./downloads/numpy-1.9.2-cp34-none-win32.whl",
        "numpy-1.9.2-cp34-none-win32.whl",
        "pkg-1.0.tar.gz",
        "pkg-1.0.tar.xz",
        "pkg-1.0.tbz",
        "pkg-1.0.TAR.GZ",
        "http://example.com/wxPython_Phoenix-3.0.3.dev1820+49a8884-cp34-none-win_amd64.whl",
        "git+https://github.com/user/repo.git",
        "git+https://github.com/user/repo.git#egg=repo",
        "git+ssh://git@github.com/user/repo.git",
        "file:///tmp/pkg",
        "../pkg",
        "C:\\dist\\pkg.whl",
        "-r other-requirements.txt",
        "-e .",
    ],
)
def test_parse_requirements_skips_non_registry_lines(tmp_path: Path, line: str) -> None:
    lockfile = tmp_path / "requirements.txt"
    lockfile.write_text(f"{line}\n")

    deps, errors = parse_requirements(lockfile, None)

    assert deps == []
    assert errors == []


@pytest.mark.quick
def test_parse_requirements_keeps_pinned_and_unpinned_lines(tmp_path: Path) -> None:
    lockfile = tmp_path / "requirements.txt"
    lockfile.write_text(
        "# comment\n"
        "flask\n"
        "boto3==1.35.0\n"
        "pydantic>=2.0.0\n"
        "-r other-requirements.txt\n"
        "at-url @ https://example.com/at-url.zip\n"
    )

    deps, errors = parse_requirements(lockfile, None)

    assert errors == []
    assert [(d.package, d.version, d.line_number) for d in deps] == [
        ("flask", "", 2),
        ("boto3", "1.35.0", 3),
        ("pydantic", ">=2.0.0", 4),
    ]


@pytest.mark.quick
def test_parse_requirements_with_manifest_reports_unpinned_lines_as_direct(
    tmp_path: Path,
) -> None:
    manifest = tmp_path / "requirements.in"
    manifest.write_text("requests\nboto3\n")
    lockfile = tmp_path / "requirements.txt"
    lockfile.write_text(
        "requests>=2.0\n"
        "urllib3\n"
        "pydantic>=2.0,<3.0\n"
        "boto3==1.35.0\n"
        "six==1.16.0\n"
    )

    deps, errors = parse_requirements(lockfile, manifest)

    # a range or bare name is hand-written and direct even when the manifest
    # does not name it; the manifest still decides for exact pins
    assert errors == []
    assert [(d.package, d.version, d.transitivity) for d in deps] == [
        ("requests", ">=2.0", out.DependencyKind(out.Direct())),
        ("urllib3", "", out.DependencyKind(out.Direct())),
        ("pydantic", ">=2.0,<3.0", out.DependencyKind(out.Direct())),
        ("boto3", "1.35.0", out.DependencyKind(out.Direct())),
        ("six", "1.16.0", out.DependencyKind(out.Transitive())),
    ]


@pytest.mark.quick
def test_parse_requirements_without_manifest_reports_unpinned_lines_as_direct(
    tmp_path: Path,
) -> None:
    lockfile = tmp_path / "requirements.txt"
    lockfile.write_text(
        "requests>=2.0\n"
        "flask\n"
        "django~=4.2\n"
        "numpy==1.26.*\n"
        "boto3==1.35.0\n"
        "pydantic===2.0\n"
    )

    deps, errors = parse_requirements(lockfile, None)

    # a range or bare name was written by hand, so it is direct; an exact pin
    # could be resolver output, so it stays unknown
    assert errors == []
    assert [(d.package, d.version, d.transitivity) for d in deps] == [
        ("requests", ">=2.0", out.DependencyKind(out.Direct())),
        ("flask", "", out.DependencyKind(out.Direct())),
        ("django", "~=4.2", out.DependencyKind(out.Direct())),
        ("numpy", "==1.26.*", out.DependencyKind(out.Direct())),
        ("boto3", "1.35.0", out.DependencyKind(out.Unknown())),
        ("pydantic", "===2.0", out.DependencyKind(out.Direct())),
    ]
