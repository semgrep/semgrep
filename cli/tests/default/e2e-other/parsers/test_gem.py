#
# Copyright (c) 2024 Semgrep Inc.
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
import pytest

from pathlib import Path

from semdep.external.parsy import any_char
from semdep.external.parsy import string
from semdep.parsers import gem


@pytest.mark.quick
@pytest.mark.parametrize(
    "original, output",
    [
        ("  spf-query\n", ["spf-query"]),
        ("  faker!\n", ["faker"]),
        ("  minitest (= 5.15.0)\n", ["minitest"]),
        ("  simplecov (= 0.17.1, < 0.18)\n", ["simplecov"]),
    ],
)
def test_gem_direct_dep_block(original: str, output: str):
    result = (
        gem.manifest_package.sep_by(string("\n")) << any_char.many()
    ).parse_partial(original)
    assert result[0] == output
    assert not result[1]


@pytest.mark.quick
def test_gemfile_platform_suffix_stripped(tmp_path: Path):
    """Bundler writes precompiled native gems with a platform suffix inside
    the parentheses, e.g. `nokogiri (1.13.1-x86_64-linux)`. The suffix must
    not leak into FoundDependency.version, or Gem range matching breaks
    (see #12103)."""
    lockfile = tmp_path / "Gemfile.lock"
    lockfile.write_text(
        "GEM\n"
        "  remote: https://rubygems.org/\n"
        "  specs:\n"
        "    nokogiri (1.13.1-x86_64-linux)\n"
        "      racc (~> 1.4)\n"
        "    racc (1.7.3)\n"
        "    ffi (1.17.1-x64-mingw-ucrt)\n"
        "    jruby-openssl (0.14.0-java)\n"
        "\n"
        "PLATFORMS\n"
        "  x86_64-linux\n"
        "\n"
        "DEPENDENCIES\n"
        "  nokogiri\n"
        "\n"
        "BUNDLED WITH\n"
        "   2.5.6\n"
    )
    deps, errors = gem.parse_gemfile(lockfile, None)
    assert not errors
    assert {dep.package: dep.version for dep in deps} == {
        "nokogiri": "1.13.1",
        "racc": "1.7.3",
        "ffi": "1.17.1",
        "jruby-openssl": "0.14.0",
    }
