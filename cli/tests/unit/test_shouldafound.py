from __future__ import annotations

from pathlib import Path
from textwrap import dedent

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands import shouldafound
from semgrep.error import SemgrepError

FILE_CONTENT = dedent(
    """
    package main
    import "fmt"

    func main() {
        fmt.Println("hello world")
        foo(1,2)
    }

    func foo(a int, b int) int {
        return a + b
    }
    """
)


@pytest.mark.quick()
def test_read_all_lines(mocker):
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=FILE_CONTENT))
    output = shouldafound._read_lines(Path("path/to/fake.go"), None, None)
    assert output == FILE_CONTENT


@pytest.mark.quick()
def test_read_selected_lines(mocker):
    desired_content = dedent(
        """
        func main() {
            fmt.Println("hello world")
            foo(1,2)
        }
        """
    )
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=FILE_CONTENT))
    output = shouldafound._read_lines(Path("path/to/fake.go"), 4, 8)
    assert output == desired_content


@pytest.mark.quick()
def test_read_single_line(mocker):
    # Format as single line, with 4 space indent, and a newline at the end (for playground compatibility)
    desired_content = """    fmt.Println("hello world")\n"""
    mocker.patch.object(Path, "open", mocker.mock_open(read_data=FILE_CONTENT))
    output = shouldafound._read_lines(Path("path/to/fake.go"), 6, None)
    assert output == desired_content


@pytest.mark.quick()
def test_read_args(tmp_path, mocker):
    api_content = {"playground_link": "https://foo.bar.semgrep.dev/playground/asdf"}

    request_mock = mocker.Mock(return_value=api_content)

    email = "myemail@foo.com"
    message = "some vuln here"
    path = "path/to/bad/file.go"

    mocker.patch.object(Path, "open", mocker.mock_open(read_data=FILE_CONTENT))
    mocker.patch.object(shouldafound, "_make_shouldafound_request", request_mock)
    runner = CliRunner(env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")})
    result = runner.invoke(
        cli,
        ["shouldafound", "-m", f"{message}", "--email", f"{email}", "-y", path],
    )
    assert api_content["playground_link"] in result.output

    request_mock.assert_called_with(
        {"email": email, "lines": mocker.ANY, "message": message, "path": path}
    )


@pytest.mark.quick()
def test_read_line_args(tmp_path, mocker):
    desired_content = dedent(
        """
        func main() {
            fmt.Println("hello world")
            foo(1,2)
        }
        """
    )

    api_content = {"playground_link": "https://foo.bar.semgrep.dev/playground/asdf"}

    request_mock = mocker.Mock(return_value=api_content)

    email = "myemail@foo.com"
    message = "some vuln here"
    path = "path/to/bad/file.go"

    mocker.patch.object(Path, "open", mocker.mock_open(read_data=FILE_CONTENT))
    mocker.patch.object(shouldafound, "_make_shouldafound_request", request_mock)
    runner = CliRunner(env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")})
    result = runner.invoke(
        cli,
        [
            "shouldafound",
            "-m",
            f"{message}",
            "--email",
            f"{email}",
            "--start",
            "4",
            "--end",
            "8",
            "-y",
            path,
        ],
    )
    assert api_content["playground_link"] in result.output
    assert result.exit_code == 0

    request_mock.assert_called_with(
        {"email": email, "lines": desired_content, "message": message, "path": path}
    )


@pytest.mark.quick()
def test_handle_api_error(tmp_path, mocker):
    runner = CliRunner(env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")})

    mocker.patch.object(Path, "open", mocker.mock_open(read_data=FILE_CONTENT))

    mocker.patch.object(
        shouldafound, "_make_shouldafound_request", side_effect=SemgrepError
    )
    email = "myemail@foo.com"
    message = "some vuln here"
    path = "path/to/bad/file.go"
    code = runner.invoke(
        cli,
        [
            "shouldafound",
            "-m",
            f"{message}",
            "--email",
            f"{email}",
            "--start",
            "4",
            "--end",
            "8",
            "-y",
            path,
        ],
    ).exit_code
    assert code == 2
