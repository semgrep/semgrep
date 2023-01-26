// This used to match here with focus-metavariable, but
// I don't think it was a good idea
import {bar as baz} from "foo";

// ruleid: resolved-metavariables 
var res1 = baz()

// ruleid: resolved-metavariables 
var res2 = baz()

// ruleid: resolved-metavariables 
var res3 = baz()

// ruleid: resolved-metavariables 
var res4 = baz()

// ruleid: resolved-metavariables 
var res5 = baz()

// those tests were used to double check that when we build
// artificial names using a single token (from an ImportedEntity)
// that we don't use then this single token to get the
// content of the metavariable but instead pretty print
// the code.

// OK:
var res6 = baz()

// OK:
var res7 = baz()

// OK:
var res8 = baz()
