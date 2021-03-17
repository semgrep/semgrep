// using a pattern-not-inside instead of a pattern-not in the rule
// will lead to not finding the match below.
let func = new Function('var x = "static strings are okay";');
func();

//ruleid: match (only if use pattern-not)
func = new Function(xxx);
func();
