// MATCH:
import Foo, { y, x } from "bar";
// MATCH:
import Foo, { x, y } from "bar";
// MATCH:
import Foo, { x } from "bar";
