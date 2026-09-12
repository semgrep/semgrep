(*
   Transform rules as needed to avoid cyclic type abbreviations,
   in the sense of the -rectypes flag of the OCaml compiler.

   For example, the following fails to typecheck without '-rectypes':

     type weird_list = int * weird_list option

   Context: Very occasionally, one may find a rule like this one found
   in the tree-sitter C++ grammar:

     new_declarator: $ => prec.right(seq(
       '[',
       field('length', $._expression),
       ']',
       optional($.new_declarator)
     )),

   This results in the following OCaml type definition:

     and new_declarator = (
         Token.t (* "[" *)
       * expression
       * Token.t (* "]" *)
       * new_declarator option
     )

   which fails with the following error:

     File "lib/CST.ml", lines 673-678, characters 0-1:
     673 | and new_declarator = (
     674 |     Token.t (* "[" *)
     675 |   * expression
     676 |   * Token.t (* "]" *)
     677 |   * new_declarator option
     678 | )
     Error: The type abbreviation new_declarator is cyclic

   A solution is to introduce a variant constructor or a field when
   necessary. This module takes care of detecting such cases so that
   we could turn the rule body into a single-choice alternative, e.g.

     and new_declarator = [
       `Rectype of (
           Token.t (* "[" *)
         * expression
         * Token.t (* "]" *)
         * new_declarator option
       )
     ]
*)

open CST_grammar

let is_rectype get_rule (x : rule) =
  let start_name = x.name in
  let visited = Hashtbl.create 100 in
  let mark_visited name = Hashtbl.add visited name () in
  let was_visited name = Hashtbl.mem visited name in

  (*
     Visit the rules referenced in the rule body until either:
     - we hit a Choice, to be translated to polymorphic variants, breaking
       the cycle,
     - or we hit a reference to the start name, indicating that we closed
       a cycle without coming across a Choice.
  *)
  let rec visit rule_body =
    match rule_body with
    | Symbol name
    | Alias (name, _) ->
        if name = start_name then
          true
        else if was_visited name then
          false
        else
          visit_rule name
    | Token _
    | Blank -> false

    | Repeat x
    | Repeat1 x
    | Optional x -> visit x

    | Choice _ -> false

    | Seq l -> List.exists visit l

  and visit_rule name =
    match get_rule name with
    | None -> false
    | Some rule ->
        mark_visited name;
        visit rule.body
  in
  visit_rule start_name

(*
   We operate on a set of rules already known to be mutually-recursive.
   (should be a little faster than visiting all the rules referenced in
   a rule body)
*)
let rectypes rule_group =
  let get_rule =
    let tbl = Hashtbl.create 100 in
    List.iter (fun (x : rule) ->
      Hashtbl.add tbl x.name x
    ) rule_group;
    Hashtbl.find_opt tbl
  in
  List.map (fun (x : rule) ->
    (x, is_rectype get_rule x)
  ) rule_group

(*
   Detect and wrap rules so as to not generate cyclic type abbreviations.
*)
let prevent_cyclic_type_abbreviations grammar =
  let rules =
    List.map (fun rule_group ->
      rectypes rule_group
      |> List.map (fun (x, rectype) ->
        if rectype then
          { x with body = Choice [("Rectype", x.body)] }
        else
          x
      )
    ) grammar.rules
  in
  { grammar with rules }
