(*
   Transform the grammar's original type definitions so as to inline
   some of them. This is meant to make the OCaml types more readable.
*)

open Printf
open CST_grammar

(*
   Iterate over rule names occurring in a rule body.
*)
let rec iter_ident f body =
  match body with
  | Symbol ident -> f ident
  | Token _ -> ()
  | Blank -> ()
  | Repeat body -> iter_ident f body
  | Repeat1 body -> iter_ident f body
  | Choice cases -> List.iter (fun (_name, body) -> iter_ident f body) cases
  | Optional body -> iter_ident f body
  | Seq body_list -> List.iter (iter_ident f) body_list
  | Alias (ident, _) -> f ident

let usage_tracker () =
  let used = Hashtbl.create 100 in
  let mark_used name =
    let count =
      try Hashtbl.find used name
      with Not_found -> 0
    in
    Hashtbl.replace used name (count + 1)
  in
  let get_times_used name =
    try Hashtbl.find used name
    with Not_found -> 0
  in
  mark_used, get_times_used

(* Look up a rule definition. *)
let resolver grammar =
  let tbl = Hashtbl.create 100 in
  List.iter (fun rule_group ->
    List.iter (fun (rule : rule) ->
      let name = rule.name in
      assert (not (Hashtbl.mem tbl name));
      Hashtbl.add tbl name rule.body
    ) rule_group
  ) grammar.rules;
  fun name ->
    Hashtbl.find_opt tbl name

(* Resolve a rule name into something that's not a rule name. *)
let recursive_resolver grammar =
  let resolve = resolver grammar in
  fun orig_ident ->
    let rec aux ident =
      match resolve ident with
      | None -> None
      | Some (Symbol ident) ->
          if ident = orig_ident then
            failwith (sprintf "Cyclic definition for rule %s" ident)
          else
            aux ident
      | Some _ as res -> res
    in
    aux orig_ident

(*
   This is used to determine if a type name is used only once,
   when considering whether it should be inlined.
*)
let count_uses grammar_rules =
  let mark_used, get_times_used = usage_tracker () in
  List.iter (fun rule_group ->
    List.iter (fun rule ->
      iter_ident mark_used rule.body
    ) rule_group
  ) grammar_rules;
  get_times_used

let rewrite_body
    ~resolve
    ~get_times_used_before_inlining
    ~mark_used_after_inlining
    body =
  let rec rewrite_body
      ?(is_root = false)
      ?(is_variant_arg = false)
      body =
    match body with
    | Symbol ident ->
        rewrite_type_ident ~is_variant_arg ident
    | Token tok -> Token {tok with is_inlined = true}
    | Blank -> Blank
    | Repeat body ->
        Repeat (rewrite_body body)
    | Repeat1 body ->
        Repeat1 (rewrite_body body)
    | Choice case_list ->
        Choice (rewrite_choice ~is_root case_list)
    | Optional body ->
        Optional (rewrite_body body)
    | Seq body_list ->
        Seq (rewrite_seq body_list)
    | Alias (ident, x) ->
        mark_used_after_inlining ident;
        Alias (ident, rewrite_body x)

  and rewrite_choice ~is_root l =
    List.map (fun (name, body) ->
      (name, rewrite_body ~is_variant_arg:is_root body)
    ) l

  and rewrite_seq l =
    List.map rewrite_body l

  (*
     Inline symbols that refer to tokens or tuples.

     Tuples are inlined only under the following conditions:
     - must be the argument of a variant
     - variant must be at the root of the rule, i.e. not anonymous*
     - may not be referenced more than once

     *we can't inline arguments of variants without precautions due to
     cases like this:

       type things = (
           thing
         * [
             | `Things of things
             | `Nothing of unit
           ]
       )

     which would become

       type things = (
           thing
         * [
             | `Things of (thing * [ `Things of ... | `Nothing of unit ])
             | `Nothing of unit
           ]
       )

     with infinite recursion.
   *)
  and rewrite_type_ident ~is_variant_arg ident =
    match resolve ident with
    | None -> assert false
    | Some (Token tok) ->
        (match tok.description with
         | Constant _ -> Token {tok with is_inlined = true}
         | Pattern _
         | Token
         | External ->
             mark_used_after_inlining tok.name;
             Token {tok with is_inlined = false}
        )

    | Some ((Seq _) as seq)
      when is_variant_arg
        && get_times_used_before_inlining ident <= 1 ->
        rewrite_body seq
    | Some _ ->
        mark_used_after_inlining ident;
        Symbol ident
  in
  rewrite_body ~is_root:true body

let rewrite_rule
    ~resolve
    ~get_times_used_before_inlining
    ~mark_used_after_inlining
    (rule : rule) =
  let body =
    rewrite_body
      ~resolve
      ~get_times_used_before_inlining
      ~mark_used_after_inlining
      rule.body
  in
  { rule with body }

let inline_rules grammar =
  let get_times_used_before_inlining = count_uses grammar.rules in
  let resolve = recursive_resolver grammar in
  let mark_used_after_inlining, get_definitive_times_used = usage_tracker () in
  mark_used_after_inlining grammar.entrypoint;
  let with_inlined_rules =
    List.map (fun rule_group ->
      List.map
        (rewrite_rule
           ~resolve
           ~get_times_used_before_inlining
           ~mark_used_after_inlining)
        rule_group
    ) grammar.rules
  in
  let reordered_rules =
    let unused = ref [] in
    let used_rules =
      List.filter_map (fun rule_group ->
        let used_rules =
          List.filter_map
            (fun (rule : rule) ->
               if get_definitive_times_used rule.name = 0 then (
                 unused := [{rule with is_inlined_type = true}] :: !unused;
                 None
               )
               else
                 Some rule
            ) rule_group
        in
        match used_rules with
        | [] -> None
        | l -> Some l
      ) with_inlined_rules
    in
    used_rules @ List.rev !unused
  in
  { grammar with rules = reordered_rules }
