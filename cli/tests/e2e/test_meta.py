import pytest

from semgrep.meta import get_url_from_sstp_url


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
def test_git_url_parser():
    tests = [
        # This used to cause the URL parser to crash.
        (
            "https://test@dev.azure.com/test/TestName/_git/Core.Thing",
            "https://dev.azure.com/test/TestName/Core.Thing",
        ),
        (
            "https://foobar.visualstudio.com/Data%20Classification/_git/Data%20Classification",
            "https://foobar.visualstudio.com/Data%20Classification/Data%20Classification",
        ),
        # This one has a "subgroup" structure, which we should be able to parse.
        (
            "https://gitlab.com/example/group2/group3/test-case.git",
            "https://gitlab.com/example/group2/group3/test-case",
        ),
        (
            "https://gitlab.com/example/test-case.git",
            "https://gitlab.com/example/test-case",
        ),
    ]

    for url, expected in tests:
        assert get_url_from_sstp_url(url) == expected
