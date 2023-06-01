(*
   Type of the AST for an aliengrep pattern.

   This will be converted into a PCRE regexp for matching a target file.
*)

open Printf

type t = node list

and node =
  | Ellipsis
  | Long_ellipsis
  | Metavar of string (* identifier "FOO" only without "$" *)
  | Metavar_ellipsis of string (* same *)
  | Long_metavar_ellipsis of string (* same *)
  | Bracket of char * t * char
  | Word of string
  | Newline
  | Other of string
[@@deriving show]

let check ast =
  let metavariables = ref [] in
  let add name mv =
    match List.assoc_opt name !metavariables with
    | None -> metavariables := (name, mv) :: !metavariables
    | Some mv2 ->
        if mv2 <> mv then
          failwith
            (sprintf
               "error in aliengrep pattern. Inconsistent use of the \
                metavariable %S in %s"
               name (show ast))
  in
  let rec check_node = function
    | Ellipsis
    | Long_ellipsis ->
        ()
    | (Metavar name | Metavar_ellipsis name | Long_metavar_ellipsis name) as
      kind ->
        add name kind
    | Bracket (_open, seq, _close) -> check_seq seq
    | Word _str -> ()
    | Newline -> ()
    | Other _str -> ()
  and check_seq seq = List.iter check_node seq in
  check_seq ast
