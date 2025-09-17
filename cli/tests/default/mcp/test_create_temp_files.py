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
import shutil

import pytest

from semgrep.mcp.server import CodeFile
from semgrep.mcp.server import create_temp_files_from_code_content
from semgrep.mcp.server import McpError


@pytest.mark.quick
def test_create_temp_files_from_code_content():
    """Test that create_temp_files_from_code_content correctly creates temp files with content"""
    # Define test code files
    code_files = [
        CodeFile(path="test_file.py", content="print('Hello, world!')"),
        CodeFile(
            path="nested/path/test_file.js", content="console.log('Hello, world!');"
        ),
        CodeFile(path="special chars/file with spaces.txt", content="Hello, world!"),
    ]

    # Call the function
    temp_dir = None
    try:
        temp_dir = create_temp_files_from_code_content(code_files)

        # Check if temp directory was created
        assert os.path.exists(temp_dir)
        assert os.path.isdir(temp_dir)

        # Check if files were created with correct content
        for code_file in code_files:
            file_path = os.path.join(temp_dir, code_file.path)
            assert os.path.exists(file_path)
            with open(file_path) as f:
                content = f.read()
                assert content == code_file.content

        # Check that nested directories were created
        assert os.path.exists(os.path.join(temp_dir, "nested/path"))
        assert os.path.exists(os.path.join(temp_dir, "special chars"))

    finally:
        # Clean up
        if temp_dir and os.path.exists(temp_dir):
            shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.mark.quick
def test_create_temp_files_from_code_content_empty_list():
    """Test that create_temp_files_from_code_content handles empty file list"""
    code_files: list[CodeFile] = []

    temp_dir = None
    try:
        temp_dir = create_temp_files_from_code_content(code_files)

        # Check if temp directory was created
        assert os.path.exists(temp_dir)
        assert os.path.isdir(temp_dir)

        # Directory should be empty (except for potential system files like .DS_Store)
        # Just check that no files were created from our empty list
        entries = os.listdir(temp_dir)
        assert all(
            not os.path.isfile(os.path.join(temp_dir, entry)) or entry.startswith(".")
            for entry in entries
        )

    finally:
        # Clean up
        if temp_dir and os.path.exists(temp_dir):
            shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.mark.quick
def test_create_temp_files_from_code_content_empty_filename():
    """Test that create_temp_files_from_code_content handles empty filenames"""
    code_files = [
        CodeFile(path="", content="This content should be skipped"),
        CodeFile(path="valid_file.txt", content="This is valid content"),
    ]

    temp_dir = None
    try:
        temp_dir = create_temp_files_from_code_content(code_files)

        # Check if temp directory was created
        assert os.path.exists(temp_dir)
        assert os.path.isdir(temp_dir)

        # The empty filename should be skipped - we can't directly check for a file with empty name
        # because os.path.join(temp_dir, "") just returns temp_dir
        # Instead, we'll check that only the valid file exists in the directory
        files = [
            f
            for f in os.listdir(temp_dir)
            if os.path.isfile(os.path.join(temp_dir, f)) and not f.startswith(".")
        ]
        assert len(files) == 1
        assert "valid_file.txt" in files

        # The valid file should be created
        valid_file_path = os.path.join(temp_dir, "valid_file.txt")
        assert os.path.exists(valid_file_path)
        with open(valid_file_path) as f:
            content = f.read()
            assert content == "This is valid content"

    finally:
        # Clean up
        if temp_dir and os.path.exists(temp_dir):
            shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.mark.quick
def test_create_temp_files_from_code_content_path_traversal():
    """Test that create_temp_files_from_code_content prevents path traversal"""
    # Define test code files with path traversal attempts
    code_files = [
        CodeFile(path="../attempt_to_write_outside.txt", content="This should fail"),
        CodeFile(path="subdir/../../../etc/passwd", content="This should fail too"),
        CodeFile(path="/absolute/path/file.txt", content="This should fail as well"),
    ]

    # The function should raise a ValueError for path traversal attempts
    with pytest.raises(McpError):
        create_temp_files_from_code_content(code_files)
