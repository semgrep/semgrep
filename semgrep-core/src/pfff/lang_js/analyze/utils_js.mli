
(* print utils *)
val string_of_any : Ast_js.any -> string

(* Example: load file task
 * if file exists, unmarshal data in the file and return it
 * otherwise, run task to generate data, store it in the file, and return it
*)
val load : Common.filename -> (unit -> 'a) -> 'a
