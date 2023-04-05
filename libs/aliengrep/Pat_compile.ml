(*
   Compile a pattern into a regexp.
*)

open Printf

type metavariable_kind =
  | Metavariable
  | Metavariable_ellipsis (* regular or long *)

(* metavariable kind, bare name *)
type metavariable = metavariable_kind * string

type t = {
  pcre_pattern : string;
  pcre : Pcre.regexp;
  (* Array of PCRE capturing groups. Each capturing group has a metavariable
     name. *)
  metavariable_groups : metavariable array;
}

(*
   to do:
   - ellipses: lazy repeat
   - metavariables: capturing groups
   - reoccurring metavariables: backreferences
   - recursive structure: reference to root pattern
*)
let to_regexp (ast : Pat_AST.t) =
  (* Pattern definitions

     (?(DEFINE) xxx) is a conditional pattern whose condition is always false.
     This allows us to define a named pattern without using it at the same
     time.

     (?<foo> xxx) gives the name 'foo' to the pattern xxx (and uses it).
     (?(DEFINE)(?<foo> xxx)) gives the name 'foo' to the pattern xxx without
     using it.

     \<foo> is a reference to the pattern named foo.
  *)
  let def_any_word =
    sprintf {|(?(DEFINE)(?<word>[%s]++))|} "TODO"
  in
  let def_any_bracket =
    sprintf {|(?(DEFINE)(?<bracket>%s))|} "TODO"
  in
  let def_whitespace = {|(?(DEFINE)(?<ws>[%s]*+|} "TODO" in
  let def_any_node = {|(?(DEFINE)(?<node>\k<word>|\k<bracket>|\k<ws>|.))|} in
  let def_any_seq =
    (* lazy repeat *)
    {|(?(DEFINE)(?<seq>\k<ws>(?:\k<node>\k<ws>)*?))|}
  in
  let definitions = [
    def_any_word;
    def_any_bracket;
    def_whitespace;
    def_any_node;
    def_any_seq;
  ] in
  let new_capturing_group =
    let n = ref 0 in
    (* start numbering groups from 1 *)
    fun () -> incr n; !n
  in
  let capturing_groups = ref [] in
  let get_capturing_group_array () =
    List.rev !capturing_groups |> Array.of_list
  in
  let capture metavariable pat =
    let num = new_capturing_group () in
    capturing_groups := metavariable :: !capturing_groups;
    num, sprintf {|(%s)|} pat
  in
  let ellipsis = {|\k<seq>|} in

  let entrypoint =
    let buf = Buffer.create 200 in
    let rec of_node node =
      match node with
      | ... -> ...
    and of_seq xs =
      List.iter of_node xs
    in
    of_seq ast;
    Buffer.contents buf
  in
  let root =
    sprintf {|%s%s"|}
      (String.concat "" definitions)
      entrypoint
  in
  root, get_capturing_group_array ()

let compile_ast pattern_ast =
  let pcre_pattern, metavariable_groups = to_regexp pattern_ast in
  {
    pcre_pattern;
    pcre = SPcre.regexp pcre_pattern;
    metavariable_groups;
  }
