from datetime import datetime

import pytest

from semdep.golang_version import cmp_core
from semdep.golang_version import cmp_golang_versions
from semdep.golang_version import cmp_pseudo
from semdep.golang_version import compare_golang_specifier
from semdep.golang_version import GolangPseudoVersion
from semdep.golang_version import GolangVersionCore
from semdep.golang_version import parse_golang_core
from semdep.golang_version import parse_golang_pseudo_datetime
from semdep.golang_version import parse_golang_version
from semdep.golang_version import ParsedGolangVersion


@pytest.mark.quick
@pytest.mark.parametrize(
    "versions,results",
    [
        (("0.0.0-20220225172249-27dd8689420f", "<0.13.0"), True),
        (("0.0.0-20220225172249-27dd8689420f", ">0.13.0"), False),
        (
            ("0.0.0-20220225172249-27dd8689420f", "<0.0.0-20210428140749-89ef3d95e781"),
            False,
        ),
        (("0.0.0-20220225172249-27dd8689420f", ">=0"), True),
        (
            (
                "0.0.0-20220225172249-27dd8689420f",
                ">=0.0.0-20220524220425-1d687d428aca",
            ),
            False,
        ),
    ],
)
def test_golang_version_comparison(versions: tuple[str, str], results: bool):
    package_version, spec_version = versions
    assert compare_golang_specifier(spec_version, package_version) == results


@pytest.mark.quick
@pytest.mark.parametrize(
    "core,core_components",
    [
        ("1.17.0", (1, 17, 0)),
        ("2.3.17", (2, 3, 17)),
        ("0", (0, 0, 0)),
    ],
)
def test_parse_golang_core(core: str, core_components: tuple[int, int, int]):
    major, minor, patch = core_components
    parsed_core = GolangVersionCore(major=major, minor=minor, patch=patch)
    assert parse_golang_core(core) == parsed_core


@pytest.mark.quick
def test_parse_pseudo_datetime():
    raw_timestamp = "20220225172249"
    parsed_timestamp = parse_golang_pseudo_datetime(raw_timestamp)
    assert parsed_timestamp == datetime(2022, 2, 25, 17, 22, 49)


@pytest.mark.quick
@pytest.mark.parametrize(
    "raw_version,components",
    [
        (
            "0.0.0-20220225172249-27dd8689420f",
            (0, 0, 0, datetime(2022, 2, 25, 17, 22, 49), "27dd8689420f"),
        ),
        ("0.17.0", (0, 17, 0, None, None)),
        (
            "0.1.1-0.20221104162952-702349b0e862",
            (0, 1, 1, datetime(2022, 11, 4, 16, 29, 52), "702349b0e862"),
        ),
    ],
)
def test_parse_golang_version(
    raw_version: str, components: tuple[int, int, int, datetime | None, str | None]
):
    major, minor, patch, timestamp, commit_hash = components
    parsed_golang_core = GolangVersionCore(major=major, minor=minor, patch=patch)
    parsed_golang_pseudo = None
    if timestamp and commit_hash:
        parsed_golang_pseudo = GolangPseudoVersion(
            timestamp=timestamp, commit_hash=commit_hash
        )
    parsed_golang_version = ParsedGolangVersion(
        core=parsed_golang_core, pseudo=parsed_golang_pseudo
    )
    assert parse_golang_version(raw_version) == parsed_golang_version


@pytest.mark.quick
@pytest.mark.parametrize(
    "core_versions,diff",
    [
        ((GolangVersionCore(2, 17, 0), GolangVersionCore(1, 17, 0)), 1),
        ((GolangVersionCore(1, 13, 0), GolangVersionCore(1, 15, 0)), -2),
        ((GolangVersionCore(1, 17, 23), GolangVersionCore(1, 17, 20)), 3),
        ((GolangVersionCore(0, 0, 0), GolangVersionCore(2, 17, 3)), -2),
        ((GolangVersionCore(1, 17, 0), GolangVersionCore(1, 17, 0)), 0),
    ],
)
def test_cmp_core(
    core_versions: tuple[GolangVersionCore, GolangVersionCore], diff: int
):
    parsed_version_core, specifier_version_core = core_versions
    assert cmp_core(parsed_version_core, specifier_version_core) == diff


@pytest.mark.quick
@pytest.mark.parametrize(
    "pseudos,diff",
    [
        (
            (
                GolangPseudoVersion(datetime(2022, 2, 25, 17, 22, 49), "27dd8689420f"),
                GolangPseudoVersion(datetime(2022, 11, 4, 16, 29, 52), "702349b0e862"),
            ),
            -1,
        ),
        (
            (
                GolangPseudoVersion(datetime(2022, 11, 25, 17, 22, 49), "27dd8689420f"),
                GolangPseudoVersion(datetime(2022, 11, 4, 16, 29, 52), "702349b0e862"),
            ),
            1,
        ),
        (
            (
                GolangPseudoVersion(datetime(2022, 2, 25, 17, 22, 49), "27dd8689420f"),
                GolangPseudoVersion(datetime(2022, 2, 25, 17, 22, 49), "702349b0e862"),
            ),
            0,
        ),
    ],
)
def test_cmp_pseudo(
    pseudos: tuple[GolangPseudoVersion, GolangPseudoVersion], diff: int
):
    parsed_version_pseudo, specifier_version_pseudo = pseudos
    assert cmp_pseudo(parsed_version_pseudo, specifier_version_pseudo) == diff


@pytest.mark.quick
@pytest.mark.parametrize(
    "parsed_version_components,specifier_version_components,diff",
    [
        (
            (0, 1, 17, datetime(2022, 2, 25, 17, 22, 49), "27dd8689420f"),
            (0, 2, 15, datetime(2022, 11, 25, 17, 22, 49), "702349b0e862"),
            -1,
        ),
        (
            (0, 1, 17, datetime(2022, 11, 25, 17, 22, 49), "27dd8689420f"),
            (0, 1, 17, datetime(2022, 3, 8, 17, 22, 49), "702349b0e862"),
            1,
        ),
        (
            (0, 1, 17, datetime(2022, 2, 25, 17, 22, 49), "27dd8689420f"),
            (0, 1, 17, None, None),
            1,
        ),
        (
            (0, 1, 17, None, None),
            (0, 1, 17, datetime(2022, 11, 25, 17, 22, 49), "702349b0e862"),
            -1,
        ),
    ],
)
def test_cmp_golang_versions(
    parsed_version_components: tuple[int, int, int, datetime | None, str | None],
    specifier_version_components: tuple[int, int, int, datetime | None, str | None],
    diff: int,
):
    (
        pv_major,
        pv_minor,
        pv_patch,
        pv_timestamp,
        pv_commit_hash,
    ) = parsed_version_components
    parsed_version_golang_core = GolangVersionCore(
        major=pv_major, minor=pv_minor, patch=pv_patch
    )
    parsed_version_golang_pseudo = None
    if pv_timestamp and pv_commit_hash:
        parsed_version_golang_pseudo = GolangPseudoVersion(
            timestamp=pv_timestamp, commit_hash=pv_commit_hash
        )
    parsed_version = ParsedGolangVersion(
        parsed_version_golang_core, parsed_version_golang_pseudo
    )

    (
        sv_major,
        sv_minor,
        sv_patch,
        sv_timestamp,
        sv_commit_hash,
    ) = specifier_version_components
    specifier_version_golang_core = GolangVersionCore(
        major=sv_major, minor=sv_minor, patch=sv_patch
    )
    specifier_version_golang_pseudo = None
    if sv_timestamp and sv_commit_hash:
        specifier_version_golang_pseudo = GolangPseudoVersion(
            timestamp=sv_timestamp, commit_hash=sv_commit_hash
        )
    specifier_version = ParsedGolangVersion(
        specifier_version_golang_core, specifier_version_golang_pseudo
    )

    assert cmp_golang_versions(parsed_version, specifier_version) == diff
