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
