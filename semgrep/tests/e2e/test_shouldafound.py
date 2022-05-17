from pathlib import Path
from textwrap import dedent
from unittest import mock

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands import shouldafound
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME


@pytest.mark.quick
def test_shouldafound_no_args(tmp_path, snapshot):
    """
    Test for shouldafound usage output
    """
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
        }
    )
    result = runner.invoke(cli, ["shouldafound"])
    print(result.output)
    # snapshot.assert_match(result.output, "shouldafound.txt")


@pytest.mark.quick
def test_shouldafound_no_confirmation(tmp_path, snapshot):
    """
    Test that the -y flag allows seamless submission
    """
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
        }
    )

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

    output = ""

    with mock.patch.object(Path, "open", mock.mock_open(read_data=file_content)):
        with mock.patch.object(
            shouldafound,
            "_make_shouldafound_request",
            mock.Mock(return_value=api_content),
        ):
            output = runner.invoke(
                cli,
                ["shouldafound", "path/to/vuln.go", "-m", "some vuln", "-y"],
                env={},
            ).output

    snapshot.assert_match(output, "shouldafound.txt")
