(*s: objet.mli *)
(* TypeClass via objects. Cf also now interfaces.ml *)
class virtual objet : object ('o)
  method invariant : unit -> unit
  (* method check: unit -> unit *)

  method of_string : string -> unit
  method to_string : unit -> string
  method debug : unit -> unit

  (* ugly (but convenient): those methods allow to extend an interface without
   * changing its interface. For instance in oassocbtree I want to
   * provide a method to commit, but doing so will mean break the interface
   * of oassoc. But if provide the commit code via a misc_op_hook, then
   * I will not break the interface.
   *)
  method misc_op_hook : unit -> 'o
  method misc_op_hook2 : unit
end

(*e: objet.mli *)
