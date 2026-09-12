// import is allowed also as an identifier, but then how
// disambiguate  'import * ...'? Could be the start of
// a namespace import or a multiplication at the toplevel.
// TODO

if(Math.sin(1) > 1000)
	import("missing");

it("should run", function() {});
