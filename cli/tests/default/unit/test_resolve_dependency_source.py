from pathlib import PosixPath
from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyParser
from semgrep.resolve_dependency_source import _handle_lockfile_source
from semgrep.subproject import DependencyResolutionConfig


@pytest.mark.quick
@patch("semgrep.resolve_dependency_source.PARSERS_BY_LOCKFILE_KIND")
def test_handle_missing_parser_for_lockfile(mock_parsers_dict) -> None:
    """
    Test that _handle_lockfile_source returns the correct values when a parser is missing for the lockfile kind.
    """

    # Pretend a parser is missing for the lockfile kind
    mock_parsers_dict.__getitem__.return_value = None

    dep_source = out.ManifestLockfileDependencySource(
        (
            out.Manifest(
                out.ManifestKind(value=out.PyprojectToml()),
                out.Fpath("pyproject.toml"),
            ),
            out.Lockfile(
                out.LockfileKind(value=out.UvLock()),
                out.Fpath("uv.lock"),
            ),
        ),
    )

    result = _handle_lockfile_source(
        dep_source, DependencyResolutionConfig(False, False, False, False)
    )

    assert isinstance(result[0], out.UnresolvedReason)
    assert result[0].value == out.UnresolvedUnsupported()
    assert result[1] == []
    assert result[2] == []


@pytest.mark.quick
@patch("semgrep.resolve_dependency_source.PARSERS_BY_LOCKFILE_KIND")
def test_dependency_parser_exception(mock_parsers_dict) -> None:
    """
    Test that _handle_lockfile_source returns the correct values when a parser is raises an exception
    """

    def bad_parse(lockfile, manfiest):
        raise KeyError("Oh No")

    # Pretend a parser is missing for the lockfile kind
    mock_parsers_dict.__getitem__.return_value = DependencyParser(bad_parse)

    dep_source = out.ManifestLockfileDependencySource(
        (
            out.Manifest(
                out.ManifestKind(value=out.PyprojectToml()),
                out.Fpath("pyproject.toml"),
            ),
            out.Lockfile(
                out.LockfileKind(value=out.PoetryLock()),
                out.Fpath("poetry.lock"),
            ),
        ),
    )

    result = _handle_lockfile_source(
        dep_source, DependencyResolutionConfig(False, False, False, False)
    )

    assert result[0] == (out.ResolutionMethod(out.LockfileParsing()), [])
    assert len(result[1]) == 1
    assert str(result[1][0]) == str(
        out.ScaResolutionError(
            type_=out.ResolutionErrorKind(
                value=out.ParseDependenciesFailed(value=str(KeyError("Oh No")))
            ),
            dependency_source_file=out.Fpath("poetry.lock"),
        )
    )
    assert result[2] == [PosixPath("poetry.lock")]
