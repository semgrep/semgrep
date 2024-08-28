(*
 * Important note about exception:
 * from marshal.mli in the OCaml stdlib:
 *  "Values of extensible variant types, for example exceptions (of
 *  extensible type [exn]), returned by the unmarshaller should not be
 *  pattern-matched over through [match ... with] or [try ... with],
 *  because unmarshalling does not preserve the information required for
 *  matching their constructors. Structural equalities with other
 *  extensible variant values does not work either.  Most other uses such
 *  as Printexc.to_string, will still work as expected."
 *)

val invoke_in_child_process : ('a -> 'b) -> 'a -> unit -> 'b
