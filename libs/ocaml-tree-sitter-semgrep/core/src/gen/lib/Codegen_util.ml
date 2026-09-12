(*
   Various reusable utilities involved in code generation.
*)

open Printf

let translate_ident =
  let map =
    Protect_ident.create ~reserved_dst:Protect_ident.ocaml_reserved ()
  in
  fun ident -> Protect_ident.add_translation map ident

(* Capitalize an identifier to be used in OCaml code for variants and such.
   It checks that the argument starts with an ASCII letter. *)
let translate_ident_uppercase s =
  let res = String.capitalize_ascii s |> translate_ident in
  if String.length res > 0 &&
     match res.[0] with
     | 'A'..'Z' -> true
     | _ -> false
  then
    res
  else
    invalid_arg (
      sprintf "translate_ident_uppercase: cannot capitalize identifier %S"
        s
    )

let interleave sep l =
  let rec loop = function
    | [] -> []
    | x :: xs -> sep :: x :: loop xs
  in
  match l with
  | x :: xs -> x :: loop xs
  | [] -> []

(* Like List.fold_right, with additional argument indicating the position
   in the list. *)
let fold_righti f xs init_acc =
  let rec fold pos = function
    | [] -> init_acc
    | x :: xs ->
        f pos x (fold (pos + 1) xs)
  in
  fold 0 xs

(* Create the list [0; 1; ...; n-1] *)
let enum n =
  Array.init n (fun i -> i) |> Array.to_list

let format_binding ~is_rec ~is_local pos binding =
  let open Indent.Types in
  let is_first = (pos = 0) in
  let let_ =
    if is_first then (
      if is_rec then
        "let rec"
      else
        "let"
    )
    else (
      if is_rec then
        "and"
      else
        "let"
    )
  in
  let individual_in =
    if is_local && not is_rec then
      [Line "in"]
    else
      []
  in
  (match binding with
   | Line first_line :: rest ->
       Line (sprintf "%s %s" let_ first_line) :: rest
   | _ ->
       Line let_ :: binding
  ) @ individual_in

(* Insert the correct 'let', 'let rec', 'and', 'in' from a list of OCaml
   bindings.
   The first item must be a line without the 'let'.
*)
let format_bindings ~is_rec ~is_local bindings =
  let open Indent.Types in
  match bindings with
  | [] -> []
  | bindings ->
      let final_in, spacing =
        if is_local && is_rec then
          [Line "in"], []
        else
          [], [Line ""]
      in
      [
        Inline (List.mapi (format_binding ~is_rec ~is_local) bindings
                |> interleave spacing
                |> List.flatten);
        Inline final_in;
      ]

(* Tuareg-mode for emacs gets confused by comments like "*)", breaking syntax
   highlighting.
   "(*"

   This inserts a line-break after the slash.
*)
let make_editor_friendly_comment =
  let regexp = Str.regexp "\\*)" in
  fun s ->
    Str.global_substitute regexp (fun _ -> "*\\\n  )") s

let has_escape_characters s =
  try
    String.iter (function
      | '"'
      | '\'' -> raise Exit
      | _ -> ()
    ) s;
    false
  with Exit -> true

(*
   Prevent code injections from tree-sitter token nodes with funny names.

   It's like sprintf "%S", but tries to not double-quote things that don't
   need it.
*)
let comment s =
  (if has_escape_characters s then
     sprintf "%S" s
   else
     s
  )
  |> make_editor_friendly_comment
