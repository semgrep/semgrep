(*
   Conversion and simplification from type specified in Tree_sitter.atd.
*)

open CST_grammar

(*
   Traverse the grammar starting from all the entrypoints, which
   are the main entrypoint (first rule in the tree-sitter grammar)
   and the extras. Return the set of visited rule names.
*)
let detect_used ~entrypoints rules =
  let rule_tbl = Hashtbl.create 100 in
  List.iter (fun (name, x) -> Hashtbl.add rule_tbl name x) rules;
  let get_rule name =
    (* could be an external rule (from the 'externals' field), which
       doesn't need to be visited. *)
    Hashtbl.find_opt rule_tbl name
  in
  let visited = Hashtbl.create 100 in
  let mark_visited name = Hashtbl.replace visited name () in
  let was_visited name = Hashtbl.mem visited name in
  let rec scan (x : Tree_sitter_grammar.Rule_body.t) =
    match x with
    | Symbol name ->
        if not (was_visited name) then
          visit name
    | Literal _
    | Pattern _
    | Blank -> ()
    | Immediate_token x
    | Token x
    | Repeat x
    | Repeat1 x -> scan x
    | Choice l
    | Seq l -> List.iter scan l
    | Prec (_, _, x)
    | Field (_, x) -> scan x
    | Alias { value = name; content; _ } ->
        if not (was_visited name) then
          visit name;
        scan content
    | Reserved reserved -> scan reserved.content
  and visit name =
    mark_visited name;
    match get_rule name with
    | None -> ()
    | Some x -> scan x
  in
  List.iter visit entrypoints;
  was_visited

let name_of_body opt_rule_name (body : Tree_sitter_grammar.Rule_body.t) =
  match opt_rule_name with
  | Some rule_name -> Some rule_name
  | None ->
      match body with
      | Literal cst -> Some cst
      | Pattern { value; _ } -> Some value
      | _ -> None

(*
   A constant string gets the name of the rule if applicable,
   otherwise its name is its value.

   grammar.js:

     percent: $ => '%'            // name is 'percent'
     percents: $ => repeat1('%')  // name of the repeated element is '%'
*)
let translate_constant opt_rule_name cst =
  let name, is_inlined =
    match opt_rule_name with
    | Some rule_name -> rule_name, false
    | None -> cst, true
  in
  Token {
    name;
    is_inlined;
    description = Constant cst
  }

(*
   Unlike string constants, patterns without a name are omitted from
   tree-sitter's output. This is not desirable. Normally, we apply a pass
   on the grammar (see Simplify_grammar.ml) to make sure patterns
   have a name and show up in the parse tree.
*)
let translate_pattern opt_rule_name pat =
  match opt_rule_name with
  | Some name -> Token { name; is_inlined = false; description = Pattern pat }
  | None -> Blank

(*
   We discard the rules describing how to parse a token since we don't
   need them.
*)
let translate_token opt_rule_name token_contents =
  match name_of_body opt_rule_name token_contents with
  | Some name -> Token { name; is_inlined = false; description = Token }
  | None ->
      match Missing_node.get_token_node_name token_contents with
      | Some (Literal cst) ->
          translate_constant opt_rule_name cst
      | Some (Name name) ->
          Token { name; is_inlined = false; description = Token }
      | None -> Blank

(*
   Remove constructs that are not relevant to us, such as precedence levels.
*)
let rec strip (x : Tree_sitter_grammar.Rule_body.t) : Tree_sitter_grammar.Rule_body.t =
  match x with
  | Symbol _
  | Literal _
  | Pattern _
  | Blank as x -> x
  | Immediate_token body -> Immediate_token (strip body)
  | Token body -> Token (strip body)
  | Repeat x -> Repeat (strip x)
  | Repeat1 x -> Repeat1 (strip x)
  | Choice l -> Choice (List.map strip l)
  | Seq l -> Seq (List.map strip l)
  | Prec (_, _, x) -> strip x
  | Field (_, x) -> strip x
  | Alias alias -> Alias { alias with content = strip alias.content }
  | Reserved reserved -> Reserved { reserved with content = strip reserved.content }

(*
   Simple translation without normalization. Get rid of PREC_*

   find_alias takes a rule name and an alias name, and returns the
   globally-unique alias ID.

   The optional rule_name is the name given to this value, if there's one.
   e.g. in grammar.js:

     number: $ => /[0-9]+/

   For the above, we'd call:

     translate ~rule_name:"number" (Pattern "[0-9]+")

   The parsing result would be a node with field type:"number" instead of
   type:"[0-9]+" in an anonymous context.
*)
let translate ~rule_name (x : Tree_sitter_grammar.Rule_body.t) =
  let rec translate ?rule_name (x : Tree_sitter_grammar.Rule_body.t) =
    match x with
    | Symbol ident -> Symbol ident
    | Literal cst
    | Immediate_token (Literal cst) -> translate_constant rule_name cst
    | Pattern { value; _ }
    | Immediate_token (Pattern { value; _ }) -> translate_pattern rule_name value
    | Immediate_token body
    | Token body -> translate_token rule_name body
    | Blank -> Blank
    | Repeat x -> Repeat (translate x)
    | Repeat1 x -> Repeat1 (translate x)
    | Choice [x; Blank] -> Optional (translate x)
    | Choice l -> Choice (translate_choice rule_name l)
    | Seq l -> Seq (List.map translate l)
    | Alias x ->
        assert x.must_be_preserved;
        Alias (x.value, translate x.content)
    | Prec _ -> assert false
    | Field _ -> assert false
    | Reserved reserved -> translate reserved.content

  and translate_choice opt_rule_name cases =
    let translated_cases = List.map translate cases |> Util_list.deduplicate in
    Type_name.assign_case_names ?rule_name:opt_rule_name translated_cases
  in
  translate ~rule_name (strip x)

(*
   Algorithm: convert the nodes of tree from unnormalized to normalized,
   starting from the leaves. This ensures that the argument of flatten_seq
   is already fully normalized.

   We no longer flatten choices because they're rarely nested anyway,
   and it would require re-generating case names so as to avoid ambiguities.
*)
let rec normalize x =
  match x with
  | Symbol _
  | Token _
  | Blank
  | Alias _ as x -> x
  | Repeat x -> Repeat (normalize x)
  | Repeat1 x -> Repeat1 (normalize x)
  | Choice l ->
      Choice (List.map (fun (name, body) -> (name, normalize body)) l)
  | Optional x -> Optional (normalize x)
  | Seq l -> Seq (List.map normalize l |> flatten_seq)

and flatten_seq normalized_list =
  normalized_list
  |> List.map (function
    | Seq l -> l
    | other -> [other]
  )
  |> List.flatten

let make_external_rules externals =
  List.filter_map (fun (rule_body : Tree_sitter_grammar.Rule_body.t) ->
    match rule_body with
    | Symbol name ->
        let body =
          Token { name; is_inlined = false; description = External }
        in
        Some (name, body)
    | Pattern _
    | Literal _ -> None (* no need for a rule *)
    | _ -> failwith "found member of 'externals' that's not a Symbol or Literal"
  ) externals

let translate_rules rules =
  List.map (fun (rule_name, body) ->
    let body = translate ~rule_name body |> normalize in
    (rule_name, body)
  ) rules

let tsort_rules rules =
  let sorted = Topo_sort.sort rules in
  List.map (fun group ->
    let rec_group =
      match group with
      | [] -> assert false
      | [_] -> false
      | _ -> true
    in
    List.map (fun (is_rec, rule) ->
      let is_rec = rec_group || is_rec in
      { rule with is_rec }
    ) group
  ) sorted

(*
   Extras can be rule names, strings or patterns.
   Here we only keep rule names. We need them to identify tree nodes that
   are extras and should be handled independently from the grammar.
*)
let filter_extras bodies =
  List.filter_map (fun (x : Tree_sitter_grammar.Rule_body.t) ->
    match x with
    | Symbol name -> Some name
    | Literal _ -> None
    | _ -> None
  ) bodies

let of_tree_sitter (x : Tree_sitter_grammar.grammar) : t =
  let entrypoint =
    (* assuming the grammar's entrypoint is the first rule in grammar.json *)
    match x.rules with
    | (name, _) :: _ -> name
    | _ -> "program"
  in
  let extras = filter_extras x.extras in
  let is_used = detect_used ~entrypoints:(entrypoint :: extras) x.rules in
  let grammar_rules = translate_rules x.rules in
  let all_rules =
    make_external_rules x.externals @ grammar_rules
    |> List.map (fun (name, body) ->
      let is_inlined_rule = not (is_used name) in
      {
        name;
        body;
        is_rec = true; (* set correctly by tsort below *)
        is_inlined_rule = is_inlined_rule;
        is_inlined_type = false;
      }
    )
  in
  let sorted_rules = tsort_rules all_rules in
  {
    name = x.name;
    entrypoint;
    rules = sorted_rules;
    extras;
  }
  |> Rectypes.prevent_cyclic_type_abbreviations
