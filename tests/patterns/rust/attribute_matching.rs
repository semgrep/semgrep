// MATCH:
#[test]
#[more(complicated, attribute)]
fn precise() {
}

// No subset of the group should match 
fn subset0() {
}

#[test]
fn subset1() {
}

#[more(complicated, attribute)]
fn subset2() {
}

// One tag being a non-match should not match.
#[not_test]
#[more(complicated, attribute)]
fn one_tag_diff1() {
}

#[test]
#[more(sophisticated, attribute)]
fn one_tag_diff2() {
}

#[test]
#[more(complicated, syntax)]
fn one_tag_diff3() {
}

#[test]
#[another(complicated, attribute)]
fn one_tag_diff3() {
}

// Patterns should match supersets of the pattern attributes.
// MATCH:
#[test]
#[more(complicated, attribute)]
#[anotherAttribute]
fn superset() {
}

// Patterns should match supersets of the pattern attributes.
// MATCH:
#[more(complicated, attribute)]
#[anotherAttribute]
#[test]
fn superset_unordered() {
}
