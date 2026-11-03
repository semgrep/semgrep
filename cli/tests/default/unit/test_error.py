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
import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
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


# Mirrors the engine output pasted in
# https://github.com/semgrep/semgrep/issues/11885: the engine prefixes the
# real cause with the generic file-an-issue preamble.
PRO_ONLY_OPERATOR_MESSAGE = (
    f"{PLEASE_FILE_ISSUE_TEXT}\n\n"
    "metavariable-name:module(s) operator is only supported in the Pro engine"
)


def _make_rule_core_error(error_type: out.ErrorType, message: str) -> SemgrepCoreError:
    """Helper to create a SemgrepCoreError for a rule running on a target."""
    core_error = out.CoreError(
        error_type=error_type,
        severity=out.ErrorSeverity(out.Error_()),
        location=out.Location(
            path=out.Fpath("eslint.config.js"),
            start=out.Position(line=1, col=1, offset=0),
            end=out.Position(line=1, col=1, offset=0),
        ),
        message=message,
        details=None,
        rule_id=out.RuleId(
            "javascript.crypto-js.cryptojs-weak-algorithm.cryptojs-weak-algorithm"
        ),
    )
    return SemgrepCoreError(
        code=2,  # FATAL_EXIT_CODE
        level=out.ErrorSeverity(out.Error_()),
        spans=None,
        core=core_error,
    )


class TestProOnlyOperatorError:
    """
    Regression tests for https://github.com/semgrep/semgrep/issues/11885.

    The OSS engine reports rules using Pro-only operators as a generic
    MatchingError ("Internal matching error") whose message begs the user to
    file a GitHub issue -- even though the message itself states the known
    limitation. These must be labeled as what they are instead.
    """

    @pytest.mark.quick
    def test_pro_only_operator_is_reclassified(self):
        error = _make_rule_core_error(
            out.ErrorType(out.MatchingError()), PRO_ONLY_OPERATOR_MESSAGE
        )
        assert error.is_pro_only_operator_error is True

        rendered = str(error)
        assert rendered.startswith("Pro-only operator ")
        assert "when running" in rendered
        assert "cryptojs-weak-algorithm" in rendered
        assert "Internal matching error" not in rendered
        # The file-an-issue preamble must not be shown for a known limitation.
        assert PLEASE_FILE_ISSUE_TEXT not in rendered
        assert "https://github.com/semgrep/semgrep" not in rendered
        # The actual cause is preserved.
        assert (
            "metavariable-name:module(s) operator is only supported in the Pro engine"
            in rendered
        )

    @pytest.mark.quick
    def test_pro_only_operator_without_preamble_is_reclassified(self):
        cause = "taint-sink:foo operator is only supported in the Pro engine"
        error = _make_rule_core_error(out.ErrorType(out.MatchingError()), cause)
        assert error.is_pro_only_operator_error is True

        rendered = str(error)
        assert "Internal matching error" not in rendered
        assert cause in rendered

    @pytest.mark.quick
    def test_genuine_matching_error_is_unchanged(self):
        # A real engine-side failure must keep its label and its
        # file-an-issue invitation (cf. the metavariable-pattern e2e snapshot).
        message = (
            f"{PLEASE_FILE_ISSUE_TEXT}\n\n"
            "rule some.rule: metavariable-pattern failed when parsing "
            "$SHELL's content as Bash"
        )
        error = _make_rule_core_error(out.ErrorType(out.MatchingError()), message)
        assert error.is_pro_only_operator_error is False

        rendered = str(error)
        assert "Internal matching error" in rendered
        assert PLEASE_FILE_ISSUE_TEXT in rendered

    @pytest.mark.quick
    def test_pro_message_with_other_error_type_is_unchanged(self):
        # Conservative: only reclassify when the engine also reports the
        # generic matching-error type.
        error = _make_rule_core_error(
            out.ErrorType(out.FatalError()), PRO_ONLY_OPERATOR_MESSAGE
        )
        assert error.is_pro_only_operator_error is False
        assert "Internal matching error" not in str(error)
        assert "Fatal error" in str(error)

    @pytest.mark.quick
    def test_cli_error_message_uses_new_label(self):
        # Pins the --json errors[].message path, which goes through str().
        error = _make_rule_core_error(
            out.ErrorType(out.MatchingError()), PRO_ONLY_OPERATOR_MESSAGE
        )
        cli_error = error.to_CliError()
        assert "Pro-only operator" in cli_error.message
        assert "Internal matching error" not in cli_error.message
        assert PLEASE_FILE_ISSUE_TEXT not in cli_error.message


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
