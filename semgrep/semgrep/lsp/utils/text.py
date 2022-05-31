from semgrep.lsp.types import Range


def text_ranges_overlap(range1: Range, range2: Range) -> bool:
    if (
        range1["start"]["line"] <= range2["end"]["line"]
        and range1["end"]["line"] >= range2["start"]["line"]
        and range1["start"]["character"] <= range2["end"]["character"]
        and range1["end"]["character"] >= range2["start"]["character"]
    ):
        return True
    else:
        return False
