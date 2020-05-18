from semgrep.pattern_lints import EquivalentPatterns
from semgrep.pattern_lints import pattern_to_json
from semgrep.pattern_lints import patterns_are_equivalent

pattern_assign = pattern_to_json(
    """$X == $X
...
$X = callfunc()
""",
    lang="python",
)

pattern_assign_whitespace = pattern_to_json(
    """$X ==     $X
...

$X = callfunc()
""",
    lang="python",
)

pattern_return = pattern_to_json(
    """$X == $X
...
return callfunc()
""",
    lang="python",
)

pattern_different = pattern_to_json(
    """$X == $X
...
return catfunc()
""",
    lang="python",
)


def test_trivial_equivalence():
    assert (
        patterns_are_equivalent(patt1_json=pattern_assign, patt2_json=pattern_assign)
        == EquivalentPatterns.ExactMatch
    )


def test_whitespace_equivalence():
    assert (
        patterns_are_equivalent(
            patt1_json=pattern_assign, patt2_json=pattern_assign_whitespace
        )
        == EquivalentPatterns.ExactMatch
    )


def test_return_assignment_equivalence() -> None:
    assert (
        patterns_are_equivalent(patt1_json=pattern_assign, patt2_json=pattern_return)
        == EquivalentPatterns.ReturnPairedWithAssignment
    )
    assert (
        patterns_are_equivalent(
            patt1_json=pattern_assign, patt2_json=pattern_different,
        )
        == EquivalentPatterns.Different
    )
