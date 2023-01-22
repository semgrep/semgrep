// ruleid: resolved-metavariables 
import {bar as baz} from "foo";

// ruleid: resolved-metavariables 
var res1 = baz()

// ruleid: resolved-metavariables 
var res2 = baz()

// ruleid: resolved-metavariables 
var res3 = baz()

// ruleid: resolved-metavariables 
var res4 = baz()

// OK:
var res5 = baz()

// OK:
var res6 = baz()

// OK:
var res7 = baz()

// OK:
var res8 = baz()