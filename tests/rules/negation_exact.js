// using a pattern-not-inside instead of a pattern-not in the rule
// will lead to not finding the match below.
let func = new Function('var x = "static strings are okay";');
func();

// match (only if use pattern-not)
//ruleid: new-function-detected
func = new Function(xxx);
func();
