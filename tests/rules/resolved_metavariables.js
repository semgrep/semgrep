// MATCH:
import {bar as baz} from "foo";

// MATCH:
var res1 = baz()

// MATCH:
var res2 = baz()

// MATCH:
var res3 = baz()

// MATCH:
var res4 = baz()

var res5 = baz()

var res6 = baz()

var res7 = baz()