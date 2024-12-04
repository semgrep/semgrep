from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.resolve_dependency_source import _handle_lockfile_source
from semgrep.subproject import ManifestLockfileDependencySource


@pytest.mark.quick
@patch("semgrep.resolve_dependency_source.PARSERS_BY_LOCKFILE_KIND")
def test_handle_missing_parser_for_lockfile(mock_parsers_dict) -> None:
    """
    Test that _handle_lockfile_source returns the correct values when a parser is missing for the lockfile kind.
    """

    # Pretend a parser is missing for the lockfile kind
    mock_parsers_dict.__getitem__.return_value = None

    dep_source = ManifestLockfileDependencySource(
        manifest=out.Manifest(
            out.ManifestKind(value=out.PyprojectToml()),
            out.Fpath("pyproject.toml"),
        ),
        lockfile=out.Lockfile(
            out.LockfileKind(value=out.UvLock()),
            out.Fpath("uv.lock"),
        ),
    )

    result = _handle_lockfile_source(dep_source, False, False)

    assert result[0] is None
    assert result[1] == []
    assert result[2] == []


@pytest.mark.quick
@patch("semgrep.resolve_dependency_source.ECOSYSTEM_BY_LOCKFILE_KIND")
def test_handle_missing_ecosystem_for_lockfile(mock_ecosystems_dict) -> None:
    """
    Test that _handle_lockfile_source returns the correct values when an ecosystem is missing for the lockfile kind.
    """

    # Pretend an ecosystem is missing for the lockfile kind
    mock_ecosystems_dict.__getitem__.return_value = None

    dep_source = ManifestLockfileDependencySource(
        manifest=out.Manifest(
            out.ManifestKind(value=out.ConanFilePy()),
            out.Fpath("conanfile.py"),
        ),
        lockfile=out.Lockfile(
            out.LockfileKind(value=out.ConanLock()),
            out.Fpath("conan.lock"),
        ),
    )

    result = _handle_lockfile_source(dep_source, False, False)

    assert result[0] is None
    assert result[1] == []
    assert result[2] == []
