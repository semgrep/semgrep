from semgrep.pattern_lints import patterns_are_equivalent

pattern_assign = """$X == $X
...
$X = callfunc()
"""

pattern_return = """$X == $X
...
return callfunc()
"""


def test_trivial_equivalence():
    assert patterns_are_equivalent(
        language="python", patt1=pattern_assign, patt2=pattern_assign
    )


def test_whitespace_equivalence():
    assert patterns_are_equivalent(
        language="python", patt1=pattern_assign.replace(" ", ""), patt2=pattern_assign
    )


def test_return_assignment_equivalence():
    assert patterns_are_equivalent(
        language="python", patt1=pattern_assign, patt2=pattern_return
    )
    assert not patterns_are_equivalent(
        language="python",
        patt1=pattern_assign,
        patt2=pattern_return.replace("callfun", "catfunc"),
    )
