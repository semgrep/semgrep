#
# Copyright (c) 2025 Semgrep Inc.
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
import os
import tempfile
from pathlib import Path

import pytest

from semgrep.mcp.server import safe_join


@pytest.mark.quick
def test_safe_join_valid_paths():
    """Test safe_join with valid paths that should be allowed"""
    base_dir = tempfile.mkdtemp(prefix="semgrep_scan_")

    # Test basic path joining
    assert safe_join(base_dir, "file.txt") == os.path.realpath(
        os.path.join(base_dir, "file.txt")
    )

    # Test with subdirectories
    assert safe_join(base_dir, "subdir/file.txt") == os.path.realpath(
        os.path.join(base_dir, "subdir/file.txt")
    )

    # Test with current directory references
    assert safe_join(base_dir, "./file.txt") == os.path.realpath(
        os.path.join(base_dir, "file.txt")
    )

    # Test with multiple subdirectories
    assert safe_join(base_dir, "sub1/sub2/file.txt") == os.path.realpath(
        os.path.join(base_dir, "sub1/sub2/file.txt")
    )


@pytest.mark.quick
def test_safe_join_path_traversal_attempts():
    """Test safe_join blocks path traversal attempts"""
    base_dir = tempfile.mkdtemp(prefix="semgrep_scan_")

    # Test simple parent directory traversal
    with pytest.raises(ValueError, match="Untrusted path escapes the base directory!"):
        safe_join(base_dir, "../file.txt")

    # Test nested parent directory traversal
    with pytest.raises(ValueError, match="Untrusted path escapes the base directory!"):
        safe_join(base_dir, "subdir/../../file.txt")

    # Test absolute path attempt
    with pytest.raises(ValueError, match="Untrusted path must be relative"):
        safe_join(base_dir, "/etc/passwd")

    # Test complex traversal with current directory references
    with pytest.raises(ValueError, match="Untrusted path escapes the base directory!"):
        safe_join(base_dir, "./subdir/../../../file.txt")


@pytest.mark.quick
def test_safe_join_edge_cases():
    """Test safe_join with edge cases"""
    base_dir = tempfile.mkdtemp(prefix="semgrep_scan_")

    # Test empty path
    assert safe_join(base_dir, "") == os.path.realpath(base_dir)

    # Test current directory
    assert safe_join(base_dir, ".") == os.path.realpath(base_dir)

    # Test path with only slashes
    assert safe_join(base_dir, "///") == os.path.realpath(base_dir)

    # Test path with spaces and special characters
    assert safe_join(base_dir, "my file with spaces.txt") == os.path.realpath(
        os.path.join(base_dir, "my file with spaces.txt")
    )

    # Test path with unicode characters
    assert safe_join(base_dir, "üñîçødé_fïlé.txt") == os.path.realpath(
        os.path.join(base_dir, "üñîçødé_fïlé.txt")
    )


@pytest.mark.quick
def test_safe_join_with_normalized_base():
    """Test safe_join handles base directory normalization correctly"""
    # Test with non-normalized base path
    base_dir = tempfile.mkdtemp(prefix="semgrep_scan_")

    # Should normalize the base path
    assert safe_join(base_dir, "file.txt") == os.path.realpath(
        os.path.join(base_dir, "file.txt")
    )

    # Should still prevent traversal with normalized base
    with pytest.raises(ValueError, match="Untrusted path escapes the base directory!"):
        safe_join(base_dir, "../file.txt")


@pytest.mark.quick
@pytest.mark.skipif(os.name != "nt", reason="Windows-specific path anchoring")
def test_safe_join_blocks_windows_root_and_drive_relative_paths():
    """Paths anchored to the drive root are not absolute for pathlib on Windows"""
    base_dir = tempfile.mkdtemp(prefix="semgrep_scan_")

    for path in ("/Windows/System32", "\\Windows\\Temp", "D:file.txt"):
        with pytest.raises(ValueError, match="Untrusted path must be relative"):
            safe_join(base_dir, path)


@pytest.mark.quick
def test_safe_join_never_escapes_base_dir():
    """An accepted path must resolve inside the base directory"""
    base_dir = tempfile.mkdtemp(prefix="semgrep_scan_")
    base_path = Path(base_dir).resolve()

    for path in (
        "file.txt",
        "sub/file.txt",
        "./file.txt",
        "..",
        "../file.txt",
        "sub/../../file.txt",
        "/etc/passwd",
        "/Windows/System32",
        "\\Windows\\Temp",
        "C:Windows",
        "D:file.txt",
        "C:/Windows",
    ):
        try:
            result = safe_join(base_dir, path)
        except ValueError:
            continue
        assert Path(result).resolve().is_relative_to(base_path)
