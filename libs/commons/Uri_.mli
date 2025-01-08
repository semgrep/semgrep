(* like Uri.of_string but instead of silently returning an
 * empty uri in case of error, we return None here.
 *)
val of_string_opt : string -> Uri.t option

(* rely on Uri.pp *)
val show : Uri.t -> string
