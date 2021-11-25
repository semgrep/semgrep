from semgrep.semgrep_types import LANGUAGE


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


def test_no_duplicate_reverse_exts() -> None:
    """
    Ensures one-to-one assumption of mapping from reverse file extensions to language in lang.json
    """
    exts = set()
    for d in LANGUAGE.definition_by_id.values():
        for e in d.reverse_exts:
            if e in exts:
                raise Exception(f"Duplicate language key {e}")
            exts.add(e)
