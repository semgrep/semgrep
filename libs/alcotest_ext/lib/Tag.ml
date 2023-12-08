(*
   Test tags
*)

open Printf

type t = (* private *) string

let compare = String.compare
let equal = String.equal
let show x = x
let to_string x = x

(*
   The tag syntax is a dot-separated identifier similar to pytest markers.
   coupling: update the error message below when changing this syntax
*)
let tag_syntax = {|\A[a-z_][a-z_0-9]*(?:[.][a-z_][a-z_0-9]*)*\z|}

let has_valid_tag_syntax =
  (* We use the same regexp library as Alcotest to facilitate
     future integration efforts. *)
  let re = Re.Pcre.regexp tag_syntax in
  fun tag -> Re.execp re tag

let check_tag_syntax tag =
  if not (has_valid_tag_syntax tag) then
    invalid_arg
      (sprintf
         "Alcotest_ext.declare_tag: invalid syntax for test tag %S.\n\
          It must be a dot-separated sequence of one or more lowercase \
          alphanumeric\n\
          identifiers e.g. \"foo_bar.v2.todo\" . It must match the following \
          regexp:\n\
         \  %s" tag tag_syntax)

(* no duplicates are allowed *)
let declared_tags : (t, unit) Hashtbl.t = Hashtbl.create 100

let declare tag =
  check_tag_syntax tag;
  if Hashtbl.mem declared_tags tag then
    invalid_arg
      (sprintf
         "Alcotest_ext.declare_tag: tag %S was declared multiple times.\n\
          Each tag must be declared exactly once to avoid accidental conflicts."
         tag)
  else Hashtbl.add declared_tags tag ();
  tag

let list () =
  Hashtbl.fold (fun tag () acc -> tag :: acc) declared_tags []
  |> List.sort String.compare
