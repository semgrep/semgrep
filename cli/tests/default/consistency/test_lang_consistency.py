#
# Copyright (c) 2021-2024 Semgrep Inc.
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

from semgrep.semgrep_types import LANGUAGE


@pytest.mark.quick
def test_no_duplicate_keys() -> None:
    """
    Ensures one-to-one assumption of mapping from keys to language in lang.json
    """
    keys = set()
    for d in LANGUAGE.definition_by_id.values():
        for k in d.keys:
            if k in keys:
                raise Exception(f"Duplicate language key {k}")
            keys.add(k)
