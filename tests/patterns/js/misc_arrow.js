// arrows are normalized in regular Lambda, so an arrow pattern
// will match also old-style anynonous function.
//ERROR: match
foo(function (a) { console.log("foo"); });

//ERROR: match
foo( (a) => { console.log("foo"); });

//ERROR: match, does not matter whether pattern use extra paren or not
foo( a => console.log("foo"));
