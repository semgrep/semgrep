(*
   Test the Rectypes module.

   Note that this checks that the Rectypes module works as expected
   based on our understanding of what makes type abbreviations cyclic.
   It doesn't check that the derived OCaml type definitions would be valid.
*)

open Tree_sitter_gen
open CST_grammar

let run rule_group : bool list =
  Rectypes.rectypes rule_group
  |> List.map snd

let create_token s =
  (* none of these fields matter for our tests *)
  {
    name = s;
    is_inlined = false;
    description = Constant s;
  }

let create_rule name body : rule =
  {
    name;
    body;
    is_rec = true; (* doesn't matter for our tests *)
    is_inlined_rule = false; (* doesn't matter *)
    is_inlined_type = false; (* doesn't matter *)
  }

let test_no_cycle () =
  let rule =
    create_rule "a" (
      Seq [
        Symbol "b";
        Token (create_token "x");
        Blank;
        Repeat (Symbol "c");
        Choice [
          "y", Symbol "d";
          "z", Token (create_token "z")
        ];
      ]
    )
  in
  assert (run [rule] = [false])

let test_simple_cycle () =
  let rule =
    create_rule "a" (
      Seq [
        Symbol "b";
        Blank;
        Token (create_token "x");
        Optional (Repeat1 (Repeat (Symbol "a")));
      ]
    )
  in
  assert (run [rule] = [true])

let test_cycle_and_choice () =
  let rule =
    create_rule "a" (
      Seq [
        Symbol "b";
        Token (create_token "x");
        Blank;
        Repeat (Symbol "c");
        Choice [
          "y", Symbol "a";
          "z", Token (create_token "z")
        ];
      ]
    )
  in
  assert (run [rule] = [false])

let test_mutually_recursive () =
  let rule_a =
    create_rule "a" (
      Seq [
        Symbol "b";
        Token (create_token "x");
      ]
    )
  in
  let rule_b =
    create_rule "b" (
      Seq [
        Symbol "a";
        Token (create_token "x");
      ]
    )
  in
  let rule_c =
    create_rule "c" (
      Choice [
        "a", Symbol "a";
        "b", Symbol "b";
        "c", Symbol "c";
      ]
    )
  in
  assert (run [rule_a; rule_b; rule_c] = [true; true; false])

let test = "Rectypes", [
  "no cycle", `Quick, test_no_cycle;
  "simple cycle", `Quick, test_simple_cycle;
  "cycle and choice", `Quick, test_cycle_and_choice;
  "mutually recursive", `Quick, test_mutually_recursive;
]
