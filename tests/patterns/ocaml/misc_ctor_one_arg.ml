(* ERROR: match *)
let a = Foo 1

(* ERROR: match *)
let b = Foo (1+2)

(* this is not matched, even though $X could be matched to the
 * Tuple (1,2), because we treat specially constructor calls.
 * Those are parsed as Constructor(Foo, [1; 2]) not as
 * Constructor(Foo, Tuple(1; 2)) which fix things for
 * range and autofix.
 *)
let c = Foo (1, 2)

