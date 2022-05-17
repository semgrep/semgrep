from pathlib import Path
from textwrap import dedent
from unittest import mock

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands import shouldafound
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME


@pytest.mark.quick
def test_read_all_lines():
    file_content = dedent(
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

    with mock.patch.object(Path, "open", mock.mock_open(read_data=file_content)):
        output = shouldafound._read_lines(Path("path/to/fake.go"), None, None)
        assert output == file_content


@pytest.mark.quick
def test_read_selected_lines():
    file_content = dedent(
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
    desired_content = dedent(
        """
        func main() {
            fmt.Println("hello world")
            foo(1,2)
        }
        """
    )
    with mock.patch.object(Path, "open", mock.mock_open(read_data=file_content)):
        output = shouldafound._read_lines(Path("path/to/fake.go"), 4, 8)
        assert output == desired_content


@pytest.mark.quick
def test_read_single_line():
    file_content = dedent(
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

    # Format as single line, with 4 space indent, and a newline at the end (for playground compatibility)
    desired_content = """    fmt.Println("hello world")\n"""
    with mock.patch.object(Path, "open", mock.mock_open(read_data=file_content)):
        output = shouldafound._read_lines(Path("path/to/fake.go"), 6, None)
        assert output == desired_content


@pytest.mark.quick
def test_read_args(tmp_path):
    file_content = dedent(
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

    api_content = {"playground_link": "https://foo.bar.semgrep.dev/playground/asdf"}

    request_mock = mock.Mock(return_value=api_content)

    email = "myemail@foo.com"
    message = "some vuln here"
    path = "path/to/bad/file.go"

    print("testing")

    with mock.patch.object(Path, "open", mock.mock_open(read_data=file_content)):
        with mock.patch.object(
            shouldafound, "_make_shouldafound_request", request_mock
        ):
            runner = CliRunner(
                env={
                    SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
                }
            )
            result = runner.invoke(
                cli, ["shouldafound", f"-m {message}", f"--email {email}", path]
            )

    request_mock.assert_called_with(
        {"email": email, "lines": mock.ANY, "message": message, "path": path}
    )


# test click handling
