(*
   An S-expression pretty-printer that doesn't try to be clever.
*)

open Printf
open Sexplib.Sexp

let is_alnum s =
  s <> "" &&
  try
    String.iter (function
      | 'A'..'Z'
      | 'a'..'z'
      | '0'..'9'
      | '_' -> ()
      | _ -> raise Exit
    ) s;
    true
  with Exit -> false

let extend_indent s =
  let len = String.length s in
  if len mod 4 = 0 then
    s ^ "| "
  else
    s ^ "  "

(*
   This shows variants, tuples, lists, and options all in the same way,
   which isn't ideal.
*)
let to_buffer buf x =
  let rec print indent x =
    match x with
    | Atom s ->
        if is_alnum s then
          bprintf buf "%s%s\n" indent s
        else
          bprintf buf "%s%S\n" indent s
    | List l ->
        List.iter (print (extend_indent indent)) l
  in
  print "" x

let to_string x =
  let buf = Buffer.create 1024 in
  to_buffer buf x;
  Buffer.contents buf

let to_stdout x =
  print_string (to_string x);
  flush stdout
