/**
 * Only report `eval` when we provide it with non-constant parameters.
 */

/**
 * Negative matches
 */

// ok
eval('var x = "static strings are okay";');

// ok
const constVar = "function staticStrings() { return 'static strings are okay';}";
eval(constVar);

// ok - const within another const
eval(`${constVar}`);

// ok - concatenating with another const okay
const secondConstVar = 'this is a const variable';
eval(constVar + secondConstVar);

/**
 * Positive Matches
 */

let dynamic = "function dynamicStrings() { return 'dynamic strings are not'; }"

// ruleid:eval-detected
eval(dynamic + 'possibly malicious code');

// ruleid:eval-detected
eval(`${dynamic} possibly malicious code`);

// ruleid:eval-detected
eval(dynamic.concat(''));

function evalSomething(something) {
    // ruleid:eval-detected
    eval(something);
}
