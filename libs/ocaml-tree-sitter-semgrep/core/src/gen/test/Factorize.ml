(*
   Unit tests for the Factorize module.
*)

open Printf
open Tree_sitter_gen
open CST_grammar

let make_rule name body : rule =
  {
    name;
    body;
    is_rec = true;
    is_inlined_rule = false;
    is_inlined_type = false;
  }

let make_grammar flat_rules =
  let rules = CST_grammar_conv.tsort_rules flat_rules in
  {
    name = "test";
    entrypoint = "program";
    rules;
    extras = [];
  }

let literal name =
  Token {
    name = name;
    is_inlined = false;
    description = Constant name;
  }

let factorize rules =
  let grammar = make_grammar rules in
  printf "original rules:\n%s\n" (CST_grammar.show_grammar grammar);
  let grammar2 = Factorize.factorize_rules grammar in
  printf "new rules:\n%s\n" (CST_grammar.show_grammar grammar2);
  grammar2.rules

let flatten rule_groups =
  List.flatten rule_groups
  |> List.map (fun (rule : rule) -> (rule.name, rule.body))

(* Ignore the other fields which may be set along the way. *)
let as_pair (rule : rule) = (rule.name, rule.body)

let get_anon_names rules =
  List.filter_map (fun (name, _) ->
    if Util_string.starts_with ~prefix:"anon_" name then
      Some name
    else
      None
  ) (flatten rules)

(*
   Find an expected rule in the result and check if it matches.
*)
let check_rule name expected_rule rule_groups =
  let match_ =
    flatten rule_groups
    |> List.find_opt (fun (name2, _) -> name2 = name)
  in
  match match_ with
  | None -> Alcotest.fail (sprintf "rule %S not found in result" name)
  | Some (_name2, rule) ->
      Alcotest.(check bool)
        (sprintf "match rule %S" name)
        true (rule = expected_rule)

let test_simple () =
  let rule = make_rule "program" (literal "thing") in
  assert (
    factorize [rule] |> flatten
                        = [as_pair rule]
  )

let test_recursive () =
  let rule = make_rule "program" (Symbol "program") in
  assert (
    factorize [rule] |> flatten
                        = [as_pair rule]
  )

let test_no_sharing () =
  let rule1 = make_rule "rule1" (literal "thing1") in
  let rule2 = make_rule "rule2" (literal "thing2") in
  let res = factorize [rule1; rule2] in
  check_rule "rule1" (literal "thing1") res;
  check_rule "rule2" (literal "thing2") res

let test_sharing () =
  let shared =
    Choice [ "A", Optional (Repeat (literal "thing"));
             "B", literal "foo" ]
  in
  let rule1 = make_rule "rule1" (Repeat1 shared) in
  let rule2 = make_rule "rule2" (Optional shared) in
  let res = factorize [rule1; rule2] in
  let anon_names = get_anon_names res in
  let anon_name =
    match anon_names with
    | [name] -> name
    | _ -> assert false
  in
  check_rule "rule1" (Repeat1 (Symbol anon_name)) res;
  check_rule "rule2" (Optional (Symbol anon_name)) res

let test = "Factorize", [
  "simple", `Quick, test_simple;
  "recursive", `Quick, test_recursive;
  "no sharing", `Quick, test_no_sharing;
  "sharing", `Quick, test_sharing;
]
