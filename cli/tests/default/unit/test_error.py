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
"""Unit tests for semgrep.error module."""
from pathlib import Path

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import DependencyResolutionSemgrepError
from semgrep.error import sanitize_resolution_error_kind
from semgrep.error import sanitize_resolution_output
from semgrep.error import SemgrepCoreError


def _make_core_error(error_type: out.ErrorType) -> out.CoreError:
    """Helper to create a CoreError with the given error type."""
    return out.CoreError(
        error_type=error_type,
        severity=out.ErrorSeverity(out.Error_()),
        location=out.Location(
            path=out.Fpath("test.py"),
            start=out.Position(line=1, col=1, offset=0),
            end=out.Position(line=1, col=1, offset=0),
        ),
        message="Test error message",
        details=None,
    )


def _make_semgrep_core_error(error_type: out.ErrorType) -> SemgrepCoreError:
    """Helper to create a SemgrepCoreError with the given error type."""
    core_error = _make_core_error(error_type)
    return SemgrepCoreError(
        code=2,  # FATAL_EXIT_CODE
        level=out.ErrorSeverity(out.Error_()),
        spans=None,
        core=core_error,
    )


class TestIsScanFailure:
    """Tests for SemgrepCoreError.is_scan_failure() method."""

    @pytest.mark.quick
    @pytest.mark.parametrize(
        "error_type",
        [
            out.ErrorType(out.Timeout()),
            out.ErrorType(out.OutOfMemory()),
            out.ErrorType(out.StackOverflow()),
            out.ErrorType(out.FixpointTimeout()),
            out.ErrorType(out.TimeoutDuringInterfile()),
            out.ErrorType(out.OutOfMemoryDuringInterfile()),
        ],
        ids=[
            "Timeout",
            "OutOfMemory",
            "StackOverflow",
            "FixpointTimeout",
            "TimeoutDuringInterfile",
            "OutOfMemoryDuringInterfile",
        ],
    )
    def test_scan_failure_error_types(self, error_type: out.ErrorType):
        """Test that scan failure error types return True."""
        error = _make_semgrep_core_error(error_type)
        assert error.is_scan_failure() is True

    @pytest.mark.quick
    @pytest.mark.parametrize(
        "error_type",
        [
            out.ErrorType(out.LexicalError()),
            out.ErrorType(out.ParseError()),
            out.ErrorType(out.RuleParseError()),
            out.ErrorType(out.PatternParseError([])),
            out.ErrorType(out.MissingPlugin()),
        ],
        ids=[
            "LexicalError",
            "ParseError",
            "RuleParseError",
            "PatternParseError",
            "MissingPlugin",
        ],
    )
    def test_non_scan_failure_error_types(self, error_type: out.ErrorType):
        """Test that non-scan-failure error types return False."""
        error = _make_semgrep_core_error(error_type)
        assert error.is_scan_failure() is False


class TestIsTimeout:
    """Tests for SemgrepCoreError.is_timeout() method."""

    @pytest.mark.quick
    def test_timeout_returns_true(self):
        """Test that Timeout error type returns True."""
        error = _make_semgrep_core_error(out.ErrorType(out.Timeout()))
        assert error.is_timeout() is True

    @pytest.mark.quick
    @pytest.mark.parametrize(
        "error_type",
        [
            out.ErrorType(out.OutOfMemory()),
            out.ErrorType(out.FixpointTimeout()),
            out.ErrorType(out.TimeoutDuringInterfile()),
        ],
        ids=["OutOfMemory", "FixpointTimeout", "TimeoutDuringInterfile"],
    )
    def test_other_types_return_false(self, error_type: out.ErrorType):
        """Test that non-Timeout error types return False."""
        error = _make_semgrep_core_error(error_type)
        assert error.is_timeout() is False


class TestCiScanResultsSkippedPaths:
    """Tests for skipped_paths field serialization in CiScanResults."""

    @pytest.mark.quick
    def test_skipped_paths_serialization_round_trip(self):
        """Test that skipped_paths survives JSON serialization round-trip."""
        # Create CiScanResults with non-empty skipped_paths
        skipped = [out.Fpath("/path/to/file1.py"), out.Fpath("/path/to/file2.py")]
        ci_results = out.CiScanResults(
            findings=[],
            ignores=[],
            token=None,
            searched_paths=[],
            renamed_paths=[],
            skipped_paths=skipped,
            rule_ids=[],
        )

        # Serialize to JSON dict
        json_dict = ci_results.to_json()

        # Verify skipped_paths is present in the JSON output
        assert "skipped_paths" in json_dict
        assert json_dict["skipped_paths"] == ["/path/to/file1.py", "/path/to/file2.py"]

        # Deserialize back and verify
        restored = out.CiScanResults.from_json(json_dict)
        assert restored.skipped_paths is not None
        assert len(restored.skipped_paths) == 2
        assert restored.skipped_paths[0].value == "/path/to/file1.py"
        assert restored.skipped_paths[1].value == "/path/to/file2.py"

    @pytest.mark.quick
    def test_skipped_paths_defaults_to_empty_list(self):
        """Test that skipped_paths defaults to an empty list in JSON."""
        ci_results = out.CiScanResults(
            findings=[],
            ignores=[],
            token=None,
            searched_paths=[],
            renamed_paths=[],
            skipped_paths=[],
            rule_ids=[],
        )

        # Serialize to JSON dict
        json_dict = ci_results.to_json()

        # Verify skipped_paths is present as an empty list
        assert json_dict.get("skipped_paths") == []


SECRET = "hunter2"
REGISTRY = f"https://ci-user:{SECRET}@registry.example.com/simple"


def _cmd_failed(command: str, message: str) -> out.ResolutionErrorKind:
    return out.ResolutionErrorKind(
        out.ResolutionCmdFailed_(out.ResolutionCmdFailed(command, message))
    )


def _as_cmd_failed(kind: out.ResolutionErrorKind) -> out.ResolutionCmdFailed:
    assert isinstance(kind.value, out.ResolutionCmdFailed_)
    return kind.value.value


class TestSanitizeResolutionErrorKind:
    @pytest.mark.quick
    def test_redacts_credentials_in_command_and_output(self):
        kind = _cmd_failed(
            f"pip install --index-url {REGISTRY} .",
            f"ERROR: could not reach {REGISTRY}/pkg",
        )
        sanitized = sanitize_resolution_error_kind(kind).to_json()
        assert SECRET not in str(sanitized)
        assert "registry.example.com" in str(sanitized)

    @pytest.mark.quick
    def test_truncates_registry_url(self):
        kind = out.ResolutionErrorKind(
            out.ResourceInaccessible_(
                out.ResourceInaccessible(
                    "mvn", "https://registry.example.com/" + "a" * 50000, "401"
                )
            )
        )
        sanitized_kind = sanitize_resolution_error_kind(kind).value
        assert isinstance(sanitized_kind, out.ResourceInaccessible_)
        sanitized = sanitized_kind.value
        assert sanitized.registry_url is not None
        assert sanitized.registry_url.startswith("https://registry.example.com/")
        assert len(sanitized.registry_url) < 2100

    @pytest.mark.quick
    def test_redacts_registry_url_of_inaccessible_resource(self):
        kind = out.ResolutionErrorKind(
            out.ResourceInaccessible_(
                out.ResourceInaccessible("mvn", REGISTRY, "401 Unauthorized")
            )
        )
        assert SECRET not in str(sanitize_resolution_error_kind(kind).to_json())

    @pytest.mark.quick
    def test_redacts_missing_requirement_and_parse_failure(self):
        for kind in (
            out.ResolutionErrorKind(out.MissingRequirement(f"set {REGISTRY}")),
            out.ResolutionErrorKind(out.ParseDependenciesFailed(f"saw {REGISTRY}")),
        ):
            assert SECRET not in str(sanitize_resolution_error_kind(kind).to_json())

    @pytest.mark.quick
    def test_keeps_start_of_command_and_end_of_output(self):
        kind = _cmd_failed("START " + "c" * 5000, "o" * 5000 + " THE-REASON")
        sanitized = _as_cmd_failed(sanitize_resolution_error_kind(kind))
        assert sanitized.command.startswith("START ")
        assert len(sanitized.command) < 2100
        assert sanitized.message.endswith("THE-REASON")
        assert len(sanitized.message) < 2100

    @pytest.mark.quick
    def test_redacts_before_truncating(self):
        # A credential that straddles the cut must not leave a piece behind:
        # here the command is cut after the first 3 characters of the secret.
        prefix = f"https://ci-user:{SECRET[:3]}"
        padding = "x" * (2000 - len(prefix))
        kind = _cmd_failed(padding + REGISTRY + "/tail", "")
        sanitized = _as_cmd_failed(sanitize_resolution_error_kind(kind)).command
        assert len(sanitized) <= 2003
        assert SECRET[:3] not in sanitized.replace("example", "")

    @pytest.mark.quick
    def test_other_kinds_are_unchanged(self):
        kind = out.ResolutionErrorKind(out.UnsupportedManifest())
        assert sanitize_resolution_error_kind(kind) == kind


class TestDependencyResolutionCliError:
    @pytest.mark.quick
    def test_message_is_short_and_does_not_repeat_the_command(self):
        command = "gradlew " + "--flag " * 300
        error = DependencyResolutionSemgrepError(
            type_=sanitize_resolution_error_kind(
                _cmd_failed(command, "x" * 5000 + "\nUnable to locate a Java Runtime.")
            ),
            dependency_source_file=Path("build.gradle"),
        )
        cli_error = error.to_CliError()
        message = cli_error.message
        assert message is not None
        assert message.startswith(
            "Failed to resolve dependencies for build.gradle. Resolution Command Failed: "
        )
        assert message.endswith("Unable to locate a Java Runtime.")
        assert "--flag" not in message
        assert len(message) < 500
        # the details are still there, once, in the error type
        assert "--flag" in str(cli_error.to_json()["type"])

    @pytest.mark.quick
    def test_message_without_output(self):
        error = DependencyResolutionSemgrepError(
            type_=out.ResolutionErrorKind(out.UnsupportedManifest()),
            dependency_source_file=Path("x/pom.xml"),
        )
        assert (
            error.to_CliError().message
            == "Failed to resolve dependencies for x/pom.xml. Unsupported Manifest"
        )


class TestLockfileParseError:
    @staticmethod
    def _error(detail):
        return DependencyResolutionSemgrepError(
            type_=out.ResolutionErrorKind(
                out.ScaParseError(out.ScaParserName(out.PPnpmLock()))
            ),
            dependency_source_file=Path("pnpm-lock.yaml"),
            detail=detail,
        )

    @pytest.mark.quick
    def test_message_has_the_reason(self):
        error = self._error("no lockfileVersion found in lockfile")
        assert error.to_CliError().message == (
            "Failed to resolve dependencies for pnpm-lock.yaml. "
            "Lockfile Parse Error: no lockfileVersion found in lockfile"
        )
        assert "Lockfile Parse Error (no lockfileVersion found" in str(error)

    @pytest.mark.quick
    def test_message_without_reason(self):
        assert self._error(None).to_CliError().message == (
            "Failed to resolve dependencies for pnpm-lock.yaml. Lockfile Parse Error"
        )


class TestSanitizeResolutionOutput:
    @pytest.mark.quick
    def test_redacts_then_keeps_the_end(self):
        s = sanitize_resolution_output("o" * 5000 + f" {REGISTRY}/x THE-REASON")
        assert SECRET not in s
        assert s.endswith("THE-REASON")
        assert len(s) < 2100
