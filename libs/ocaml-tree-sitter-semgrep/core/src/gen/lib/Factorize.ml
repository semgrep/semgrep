(*
   As part of the simplify-grammar program, identify duplicated anonymous
   rules and give them a name.
*)

open Printf
open CST_grammar

(*
   Translate a name into something else if that name is already taken.
*)
let make_name_translator rules =
  let reserved = List.map fst rules in
  let scope =
    match Fresh.init_scope reserved with
    | Ok x -> x
    | Error duplicates ->
        failwith ("Duplicate rule names: " ^ String.concat " " duplicates)
  in
  fun name ->
    Fresh.create_name scope name

(*
   Determine the size of a grammar node for the purpose of determinining
   if it's worth deduplicating.

   A parent's size MUST be strictly higher than the size of its child,
   so as to guarantee bottom-up scanning. (scroll down for algorithm
   description)

   'resolve' maps a node to itself or to its replacement if there's one.
*)
let compute_size resolve node =
  let rec size node =
    match node with
    | Symbol _ -> 0
    | Token _ -> 0
    | Blank -> 0
    | Repeat x
    | Repeat1 x
    | Optional x -> 1 + size (resolve x)
    | Choice xs ->
        1 + List.fold_left (fun sum (_, x) -> sum + 1 + size (resolve x)) 0 xs
    | Seq xs ->
        1 + List.fold_left (fun sum x -> sum + size (resolve x)) 0 xs
    | Alias _ -> 0
  in
  size (resolve node)

let compute_original_size node =
  compute_size (fun x -> x) node

let recompute_size replaced_nodes node =
  let resolve node =
    match Hashtbl.find_opt replaced_nodes node with
    | Some name -> Symbol name
    | None -> node
  in
  compute_size resolve node

let sort_candidates ~min_uses ~min_size orig_rules =
  let node_counts = Hashtbl.create 100 in
  let rec add node =
    let count =
      match Hashtbl.find_opt node_counts node with
      | None -> 0
      | Some n -> n
    in
    Hashtbl.replace node_counts node (count + 1);
    match node with
    | Symbol _
    | Token _
    | Blank
    | Alias _ -> ()
    | Repeat x
    | Repeat1 x
    | Optional x -> add x
    | Choice xs -> List.iter (fun (_, x) -> add x) xs
    | Seq xs -> List.iter add xs
  in
  List.iter (fun (_name, body) -> add body) orig_rules;

  let candidates =
    Hashtbl.fold (fun body count acc ->
      if count >= min_uses then
        let size = compute_original_size body in
        if size >= min_size then
          (size, body) :: acc
        else
          acc
      else
        acc
    ) node_counts []
  in
  List.sort ((fun (a, _) (b, _) -> compare (a : int) b)) candidates

let replace_nodes node_names root_node =
  let rec replace node =
    match Hashtbl.find_opt node_names node with
    | Some name when node != root_node (* physical equality *) -> Symbol name
    | _ ->
        match node with
        | Symbol _
        | Token _
        | Blank
        | Alias _ -> node
        | Repeat x -> Repeat (replace x)
        | Repeat1 x -> Repeat1 (replace x)
        | Optional x -> Optional (replace x)
        | Choice xs ->
            Choice (List.map (fun (name, x) -> (name, replace x)) xs)
        | Seq xs -> Seq (List.map replace xs)
  in
  replace root_node

(*
   1. Group identical nodes in a table, count members in each group.
   2. Sort all nodes by increasing size, ensuring that we can iterate
      in a bottom-up order, such that if B is a descendant of A, B
      is handled first.
   3. Iterate over the nodes, bottom-up. Any duplicate nodes of sufficient
      size is assigned a name and registered as a new named rule.
      Computing the size of a node takes into account substitutions in
      descendant nodes.
   4. Perform top-down substitutions of all the bodies of the named rules,
      including old and new rules.
*)
let factorize_rules
    ?(create_names = true)
    ?(min_uses = 2)
    ?(min_size = 3)
    grammar =
  let orig_rules =
    grammar.rules
    |> List.flatten
    |> List.map (fun {name; body; _} -> (name, body))
  in
  let make_name_unique = make_name_translator orig_rules in
  let cur_rules = Hashtbl.create 100 in
  List.iter (fun (name, node) ->
    if Hashtbl.mem cur_rules name then
      invalid_arg
        (sprintf "Factorize_grammar.deduplicate: duplicate rule named %S"
           name);
    Hashtbl.add cur_rules name node;
  ) orig_rules;
  let sorted_candidates = sort_candidates ~min_uses ~min_size orig_rules in
  let node_names = Hashtbl.create 100 in
  let new_rules = Hashtbl.create 100 in
  List.iter (fun (name, node) ->
    Hashtbl.add node_names node name;
    Hashtbl.add new_rules name node
  ) orig_rules;
  if create_names then
    List.iter (fun (_orig_size, node) ->
      let size = recompute_size node_names node in
      if size >= min_size
      && not (Hashtbl.mem node_names node)
      then
        let name =
          sprintf "anon_%s_%s"
            (Type_name.name_rule_body node) (Type_name.hash_rule_body node)
          |> make_name_unique in
        assert (not (Hashtbl.mem new_rules name));
        Hashtbl.add node_names node name;
        Hashtbl.add new_rules name node
      else
        ()
    ) sorted_candidates;
  let all_rules =
    Hashtbl.fold (fun name node acc -> (name, node) :: acc) new_rules []
  in
  let get_orig_rule = CST_grammar.make_rule_lookup grammar in
  let rules =
    List.map (fun (name, orig_node) ->
      let is_inlined_rule, is_inlined_type =
        match get_orig_rule name with
        | None -> false, false
        | Some x -> x.is_inlined_rule, x.is_inlined_type
      in
      let body = replace_nodes node_names orig_node in
      { name;
        body;
        is_rec = true; (* will be set correctly by tsort below *)
        is_inlined_rule;
        is_inlined_type }
    ) all_rules
    |> CST_grammar_conv.tsort_rules
  in
  { grammar with rules }
