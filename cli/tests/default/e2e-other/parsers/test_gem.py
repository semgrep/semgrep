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
