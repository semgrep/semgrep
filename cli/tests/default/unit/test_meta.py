import pytest

from semgrep.external.git_url_parser import Parser
from semgrep.meta import get_url_from_sstp_url


@pytest.mark.quick
def test_git_url_parser():
    # Note these tests do not indicate correct behavior as much as
    # just document the current behavior of a hacky piece of code.
    tests = [
        (
            "ssh://user@host.xz:20/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": "20",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://user@host.xz/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz:20/path/to/repo.git/",
            {
                "protocol": "ssh",
                "resource": "host.xz",
                "port": "20",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz/path/to/repo.git/",
            {
                "protocol": "ssh",
                "resource": "host.xz",
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://user@host.xz/~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz/~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "resource": "host.xz",
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://user@host.xz/~/path/to/repo.git",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "owner": "~/path/to",
                "name": "repo",
            },
        ),
        (
            "ssh://host.xz/~/path/to/repo.git",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "~/path/to",
                "name": "repo",
            },
        ),
        (
            "user@host.xz:/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "host.xz:/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "user@host.xz:~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": None,
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "host.xz:~user/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        (
            "user@host.xz:path/to/repo.git",
            {
                "protocol": "ssh",
                "user": "user",
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "host.xz:path/to/repo.git",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "rsync://host.xz/path/to/repo.git/",
            {
                "protocol": "rsync",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        # Git transport Protocol
        (
            "git://host.xz/path/to/repo.git/",
            {
                "protocol": "git",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "git://host.xz/~user/path/to/repo.git/",
            {
                "protocol": "git",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "~user/path/to",
                "name": "repo",
            },
        ),
        # HTTP
        (
            "http://host.xz/path/to/repo.git/",
            {
                "protocol": "http",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "https://host.xz/path/to/repo.git/",
            {
                "protocol": "https",
                "user": None,
                "resource": "host.xz",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        # Local file system paths
        (
            "/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "",
                "port": None,
                "owner": "path/to",
                "name": "repo",
            },
        ),
        (
            "path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "path",
                "port": None,
                "owner": "to",
                "name": "repo",
            },
        ),
        (
            "~/path/to/repo.git",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "",
                "port": None,
                "owner": "~/path/to",
                "name": "repo",
            },
        ),
        # Note URLs with the file scheme are completely busted.
        (
            "file:///path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "path",
                "port": None,
                "owner": "to",
                "name": "repo",
            },
        ),
        (
            "file://~/path/to/repo.git/",
            {
                "protocol": "ssh",
                "user": None,
                "resource": "path",
                "port": None,
                "owner": "to",
                "name": "repo",
            },
        ),
        (
            "https://gitlab.net/foo.bar/a-b/a-b-c-d",
            {
                "protocol": "https",
                "user": None,
                "resource": "gitlab.net",
                "port": None,
                "owner": "foo.bar/a-b",
                "name": "a-b-c-d",
            },
        ),
    ]
    for url, expected in tests:
        actual = {}
        # TODO: mypy complains about .items not defined on object
        for key, _value in expected.items():  # type: ignore[attr-defined]
            actual[key] = getattr(Parser(url).parse(), key)
        assert (url, actual) == (url, expected)


@pytest.mark.quick
def test_get_url_from_sstp_url():
    tests = [
        # This used to cause the URL parser to crash.
        (
            "https://test@dev.azure.com/test/TestName/_git/Core.Thing",
            "https://dev.azure.com/test/TestName/_git/Core.Thing",
        ),
        (
            "https://foobar.visualstudio.com/Data%20Classification/_git/Data%20Classification",
            "https://foobar.visualstudio.com/Data%20Classification/_git/Data%20Classification",
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
        (
            "git@code1.somecompany.internal:somecompany-eval/owasp-juice-shop",
            "https://code1.somecompany.internal/somecompany-eval/owasp-juice-shop",
        ),
        (
            "git@code2.somecompany.internal:somecompany-eval/owasp-juice-shop.git",
            "https://code2.somecompany.internal/somecompany-eval/owasp-juice-shop",
        ),
        (
            "git@github.com:somecompany-eval/owasp-juice-shop",
            "https://github.com/somecompany-eval/owasp-juice-shop",
        ),
        (
            "git@code3.somecompany.internal:eval/owasp-juice-shop.git",
            "https://code3.somecompany.internal/eval/owasp-juice-shop",
        ),
    ]

    for url, expected in tests:
        assert get_url_from_sstp_url(url) == expected
