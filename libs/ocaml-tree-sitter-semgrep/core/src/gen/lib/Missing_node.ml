(*
   Ensure that the tree-sitter gives us a node in the CST for every node
   in the grammar. Patterns (regexps) and token() constructs have no name
   and tree-sitter will produce no node in the CST unless a name is
   somehow assigned. We use the alias() construct to assign names to grammar
   nodes that don't have one.

   See:
   - for patterns: https://github.com/tree-sitter/tree-sitter/issues/1151
   - for token(): https://github.com/tree-sitter/tree-sitter/issues/1156
*)

open Printf
open Tree_sitter_grammar

type token_node_name =
  | Literal of string
  | Name of string

let get_token_node_name (token_contents : Rule_body.t) : token_node_name option =
  let rec get (x : Rule_body.t) =
    match x with
    | Symbol name -> Some (Name name)
    | Literal name -> Some (Literal name)
    | Blank -> None
    | Pattern _ -> None

    | Immediate_token _ -> None
    | Token _ -> None
    | Repeat _ -> None
    | Repeat1 _ -> None
    | Choice _ -> None
    | Seq _ -> None
    | Prec (_type, _prec, x) -> get x
    | Alias alias -> get alias.content
    | Field (_field_name, x) -> get x
    | Reserved reserved -> get reserved.content
  in
  get token_contents

let extract_alias_rules_from_body add_rule (body : Rule_body.t) =
  let rec extract (x : Rule_body.t) =
    match x with
    | Symbol _
    | Literal _
    | Blank -> x

    | Pattern _
    | Immediate_token _
    | Token _ as x ->
        let preferred_name = Type_name.name_ts_rule_body x in
        let name = add_rule preferred_name x in
        Alias {
          value = name;
          named = true;
          content = x;
          must_be_preserved = true;
        }

    | Repeat x -> Repeat (extract x)
    | Repeat1 x -> Repeat1 (extract x)
    | Choice xs -> Choice (List.map extract xs)
    | Seq xs -> Seq (List.map extract xs)
    | Prec (type_, prec, x) -> Prec (type_, prec, extract x)
    | Alias alias -> Alias { alias with content = extract alias.content }
    | Field (field_name, x) -> Field (field_name, extract x)
    | Reserved reserved -> Reserved { reserved with content = extract reserved.content }
  in
  match body with
  | Pattern _
  | Immediate_token _
  | Token _ as x ->
      (* already at the root of a rule body, will have a name. *)
      x
  | x -> extract x

module Rule_body_map = Map.Make (Rule_body)

let extract_rules make_unique rules =
  let new_rules = Hashtbl.create 100 in
  let new_rule_bodies = ref Rule_body_map.empty in
  let add_rule preferred_name rule_body =
    (* Avoid introducing two rules x and x_ for the same rule body if said
       body occurs multiple times in the grammar. Instead, share the same
       name and rule. *)
    match Rule_body_map.find_opt rule_body !new_rule_bodies with
    | Some name -> name
    | None ->
        let name = make_unique preferred_name in
        Hashtbl.replace new_rules name rule_body;
        new_rule_bodies := Rule_body_map.add rule_body name !new_rule_bodies;
        name
  in
  let rewritten_rules =
    List.map (fun (name, body) ->
      let body = extract_alias_rules_from_body add_rule body in
      (name, body)
    ) rules
  in
  let new_rules =
    Hashtbl.fold (fun name body acc -> (name, body) :: acc) new_rules []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  rewritten_rules @ new_rules

(*
   Create rules for constructs that are known to produce a missing node.
*)
let work_around_missing_nodes grammar =
  let rules = grammar.rules in
  let make_unique =
    let scope = Fresh.create_scope () in
    fun preferred_name -> Fresh.create_name scope preferred_name
  in
  (* Register the rule names. They should be unique already. *)
  List.iter (fun (name, _body) ->
    let unique_name = make_unique name in
    if unique_name <> name then
      failwith (
        sprintf "Grammar defines multiple rules with the same name: %s"
          name
      )
  ) rules;
  (* Then create new rules. Their preferred names are automatically
     derived and may collide. *)
  let new_rules = extract_rules make_unique rules in
  { grammar with rules = new_rules }
