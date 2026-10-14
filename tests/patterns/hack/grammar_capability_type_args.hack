<?hh
// ERROR: match
function scoped()[policied<Foo>]: void {}
function other()[policied<Bar>]: void {}
function plain()[io]: void {}
