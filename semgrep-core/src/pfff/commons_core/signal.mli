(*
   Utilities to deal with signal numbers.
*)

(* Return the name and description of a signal, if known. *)
val get_info : int -> (string * string) option

(* Return the name of a signal, if known. *)
val get_name : int -> string option

(* Return the name of a signal, if known. *)
val get_descr : int -> string option
