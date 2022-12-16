open Common

(* Type classes via objects. See also now interfaces.ml
 *
 * todo? get more inspiration from Java to put fundamental interfaces
 * here ? such as cloneable, equaable, showable, debugable, etc
*)

class virtual objet =
  object(_o:'o)
    method invariant: unit -> unit = fun () ->
      raise Todo
    (* method check: unit -> unit = fun () ->
       assert(o#invariant());
    *)

    method of_string: string -> unit =
      raise Todo
    method to_string: unit -> string =
      raise Todo
    method debug: unit -> unit =
      raise Todo

    method misc_op_hook: unit -> 'o =
      raise Todo

    method misc_op_hook2: unit =
      ()
  end
