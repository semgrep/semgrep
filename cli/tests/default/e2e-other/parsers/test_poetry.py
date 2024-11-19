import textwrap

import pytest

from semdep.parsers import poetry


@pytest.mark.quick
@pytest.mark.parametrize(
    "original, expected",
    [
        ("[foo, bar]", ""),
        (
            "[\nfoo,\nbar\n]",
            "",
        ),
    ],
)
def test_poetry_list_value_parser(original: str, expected: str) -> None:
    assert poetry.list_value.parse(original) == expected


@pytest.mark.quick
def test_poetry_object_value_parser() -> None:
    original = """{version = "*", optional = true, markers = "python_full_version <= \"3.11.0a6\" and extra == \"toml\""}"""
    expected = ""
    assert poetry.object_value.parse(original) == expected


@pytest.mark.quick
@pytest.mark.parametrize(
    "original, expected",
    [
        ('"foo"\n', ("foo", "\n")),
        ('"foo[bar]"\n', ("foo[bar]", "\n")),
    ],
)
def test_quoted_value_parser(original, expected) -> None:
    assert poetry.quoted_value.parse_partial(original) == expected


@pytest.mark.quick
def test_multi_line_quoted_value_parser() -> None:
    original = '''"""
        foobar
    """\n'''
    expected = ""
    assert poetry.multi_line_quoted_value.parse(original) == expected


@pytest.mark.quick
@pytest.mark.parametrize(
    "original, expected",
    [
        ("foo = bar\n", (("foo", "bar"), "\n")),
        ("foo = [bar, baz]\n", (("foo", ""), "\n")),
    ],
)
def test_key_value_parser(original, expected) -> None:
    assert poetry.key_value.parse_partial(original) == expected


@pytest.mark.quick
def test_manifest_deps_parser() -> None:
    original = textwrap.dedent(
        """        [tool.poetry.dependencies]
        python = "^3.9"
        toml = "^0.10.2"
        requests = "^2.26.0"
    """
    )
    expected = (["python", "toml", "requests"], "\n")
    assert poetry.manifest_deps.parse_partial(original) == expected


@pytest.mark.quick
def test_manifest_sections_extra_parser() -> None:
    original = textwrap.dedent(
        """        [tool.black]
        line-length = 120
        include = '\.pyi?$'
        exclude = '''
        (
        /(\.eggs|\.git|\.hg|\.mypy_cache|\.nox|\.tox|\.venv|_build|buck-out|build|dist)/
        | .*/pippy/.*
        | .*_pb2.py
        | .*_pb2_grpc.py
        )
        '''
    """
    )
    expected = (None, "")
    assert poetry.manifest_sections_extra.parse_partial(original) == expected


@pytest.mark.quick
def test_poetry_dep_extra_parser() -> None:
    original = textwrap.dedent(
        """        [package.extras]
        dev = ["coverage", "django", "flake8", "isort", "pillow", "sqlalchemy", "mongoengine", "wheel (>=0.32.0)", "tox", "zest.releaser"]
        doc = ["sphinx", "sphinx-rtd-theme", "sphinxcontrib-spelling"]
    """
    )
    expected = (None, "\n")
    assert poetry.poetry_dep_extra.parse_partial(original) == expected


@pytest.mark.quick
def test_poetry_source_extra_parser() -> None:
    original = textwrap.dedent(
        """        [[tool.poetry.source]]
        name = "semgrep"
        url = "https://artifact.semgrep.com/"
        secondary = False
    """
    )
    expected = (None, "\n")
    assert poetry.poetry_source_extra.parse_partial(original) == expected


@pytest.mark.quick
def test_manifest_parser() -> None:
    original = textwrap.dedent(
        """        [tool.poetry.dependencies]
        python = "^3.9"
        toml = "^0.10.2"
        requests = "^2.26.0"

        [tool.poetry.dev-dependencies]
        pytest = "^6.2.4"
        pytest-cov = "^2.12.1"
        pytest-mock = "^3.6.1"
        pytest-xdist = "^2.3.0"

        [tool.black]
        line-length = 120
        include = '\.pyi?$'
        exclude = '''
        (
        /(\.eggs|\.git|\.hg|\.mypy_cache|\.nox|\.tox|\.venv|_build|buck-out|build|dist)/
        | .*/pippy/.*
        | .*_pb2.py
        | .*_pb2_grpc.py
        )

        [package.extras]
        dev = ["coverage", "django", "flake8", "isort", "pillow", "sqlalchemy", "mongoengine", "wheel (>=0.32.0)", "tox", "zest.releaser"]
        doc = ["sphinx", "sphinx-rtd-theme", "sphinxcontrib-spelling"]
    """
    )

    assert poetry.manifest.parse_partial(original)[0] == {"toml", "python", "requests"}
