(*
   Infer a good name for a pattern (regexp) when possible, such as
   for case-insensitive keywords such as /[Ii][Ff]/.
*)

(* Return a lowercase, alphanumeric name from a regexp in JavaScript syntax. *)
val infer : string -> string option
