#
# Copyright (c) 2024-2025 Semgrep Inc.
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
from dataclasses import replace
from pathlib import PosixPath
from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyParser
from semdep.parsers.util import LockfileParseFailure
from semgrep.resolve_dependency_source import _handle_lockfile_source
from semgrep.resolve_dependency_source import _handle_manifest_only_source
from semgrep.subproject import DependencyResolutionConfig


@pytest.mark.quick
@patch("semgrep.resolve_dependency_source.PARSERS_BY_LOCKFILE_KIND")
def test_handle_missing_parser_for_lockfile(mock_parsers_dict) -> None:
    """
    Test that _handle_lockfile_source returns the correct values when a parser is missing for the lockfile kind.
    """

    # Pretend a parser is missing for the lockfile kind
    mock_parsers_dict.__getitem__.return_value = None
    mock_parsers_dict.get.return_value = None

    dep_source = out.ManifestLockfile(
        (
            out.Manifest(
                out.ManifestKind(value=out.PyprojectToml()),
                out.Fpath("pyproject.toml"),
            ),
            out.Lockfile(
                out.LockfileKind(value=out.GradleLockfile()),
                out.Fpath("uv.lock"),
            ),
        ),
    )

    result = _handle_lockfile_source(
        dep_source, DependencyResolutionConfig(False, False, False, False)
    )

    assert isinstance(result.deps, out.UnresolvedReason)
    assert result.deps.value == out.UnresolvedUnsupported()
    assert result.errors == []
    assert result.targets == []


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
    mock_parsers_dict.get.return_value = DependencyParser(bad_parse)

    dep_source = out.ManifestLockfile(
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

    assert isinstance(result.deps, out.UnresolvedReason)
    assert result.deps.value == out.UnresolvedFailed()
    assert len(result.errors) == 1
    assert str(result.errors[0]) == str(
        out.ScaResolutionError(
            type_=out.ResolutionErrorKind(
                value=out.ParseDependenciesFailed(value=str(KeyError("Oh No")))
            ),
            dependency_source_file=out.Fpath("poetry.lock"),
        )
    )
    assert result.targets == []


POETRY_SOURCE = out.ManifestLockfile(
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


def _poetry_error(cls, path, reason):
    return cls(
        path=out.Fpath(path),
        parser=out.ScaParserName(out.PPoetryLock()),
        reason=reason,
    )


@pytest.mark.quick
@pytest.mark.parametrize(
    "error",
    [
        _poetry_error(out.DependencyParserError, "pyproject.toml", "bad manifest"),
        _poetry_error(
            out.DependencyParserError,
            "poetry.lock",
            "poetry.lock is unexpectedly empty.",
        ),
        _poetry_error(out.DependencyParserError, "poetry.lock", "skipping entry"),
    ],
    ids=["manifest-error", "empty-lockfile", "skipped-entry"],
)
@patch("semgrep.resolve_dependency_source.PARSERS_BY_LOCKFILE_KIND")
def test_non_fatal_error_keeps_subproject_resolved(mock_parsers_dict, error) -> None:
    parser = DependencyParser(lambda lockfile_path, manifest_path: ([], [error]))
    mock_parsers_dict.get.return_value = parser

    result = _handle_lockfile_source(
        POETRY_SOURCE, DependencyResolutionConfig(False, False, False, False)
    )

    assert result.deps == (out.ResolutionMethod(out.LockfileParsing()), [])
    assert result.errors == [error]
    assert result.targets == [PosixPath("poetry.lock")]


@pytest.mark.quick
@patch("semgrep.resolve_dependency_source.PARSERS_BY_LOCKFILE_KIND")
def test_lockfile_parse_failure_marks_subproject_unresolved(mock_parsers_dict) -> None:
    error = _poetry_error(LockfileParseFailure, "poetry.lock", "invalid TOML")
    parser = DependencyParser(lambda lockfile_path, manifest_path: ([], [error]))
    mock_parsers_dict.get.return_value = parser

    result = _handle_lockfile_source(
        POETRY_SOURCE, DependencyResolutionConfig(False, False, False, False)
    )

    assert result.deps == out.UnresolvedReason(out.UnresolvedFailed())
    assert result.errors == [error]
    assert result.targets == []


@pytest.mark.quick
@pytest.mark.parametrize("enabled", [None, False, True])
@pytest.mark.parametrize("persistent", [False, True])
def test_gradle_module_attribution_rpc_transport(mocker, enabled, persistent):
    """The rollout setting reaches either RPC transport, including baseline scans."""
    config = DependencyResolutionConfig(False, False, False, False)
    if enabled is not None:
        config = replace(config, gradle_module_attribution=enabled)
    config = replace(config, is_baseline_scan=True)
    source = out.ManifestOnly(
        out.Manifest(out.ManifestKind(out.BuildGradle()), out.Fpath("build.gradle"))
    )
    session = mocker.Mock() if persistent else None
    call = session.call if session else mocker.patch("semgrep.rpc_call.rpc_call")
    call.return_value = None

    _handle_manifest_only_source(source, config, rpc_session=session)

    call.assert_called_once()
    params = call.call_args.args[0].value.value
    assert params.to_json().get("gradle_module_attribution", False) is bool(enabled)
    assert params.dependency_sources == [out.DependencySource(source)]
