(*
   Test document parsing.
*)

open Spacegrep

let t = Testo.create

(* Same as the original AST type but without locations and inlined Atoms.
   It makes it nicer to write the expected AST in test cases. *)
type simplified_node =
  (* inlined atom type *)
  | Word of string
  | Punct of char
  | Byte of char
  | Metavar of string
  (* other *)
  | List of simplified_node list
  | Dots of string option (* both ... and $...MVAR *)
  | End
[@@deriving show { with_path = false }, eq]

type simplified_ast = simplified_node list
[@@deriving show { with_path = false }, eq]

let simplify_atom (x : Pattern_AST.atom) : simplified_node =
  match x with
  | Word s -> Word s
  | Punct c -> Punct c
  | Byte c -> Byte c
  | Metavar s -> Metavar s

let rec simplify_ast (x : Pattern_AST.t) : simplified_ast =
  List_.map simplify_node x

and simplify_node (x : Pattern_AST.node) : simplified_node =
  match x with
  | Atom (_loc, x) -> simplify_atom x
  | List xs -> List (simplify_ast xs)
  | Dots (_loc, x) -> Dots x
  | End -> End

let ast : simplified_ast Alcotest.testable =
  Alcotest.testable pp_simplified_ast equal_simplified_ast

let run_debug input expected_output =
  let output =
    input |> Src_file.of_string
    |> Parse_pattern.of_src ~is_doc:true
    |> Result.get_ok |> Pattern_AST.as_doc |> simplify_ast
  in
  Alcotest.(check ast) "equal" expected_output output

let run_pretty input expected_output =
  let output =
    input |> Src_file.of_string
    |> Parse_pattern.of_src ~is_doc:true
    |> Result.get_ok |> Pattern_AST.as_doc |> Print.to_string
  in
  Alcotest.(check string) "equal" expected_output output

let debug_corpus =
  [
    ("empty", "", []);
    ("atom", "a", [ Word "a" ]);
    ("hello", "hello, world\n", [ Word "hello"; Punct ','; Word "world" ]);
    ("indent", "a\n    b\n", [ Word "a"; List [ Word "b" ] ]);
    ("paragraph", "a b\nc d\n", [ Word "a"; Word "b"; Word "c"; Word "d" ]);
    ("simple empty matching parens", "()", [ Punct '('; Punct ')' ]);
    ( "simple nonempty matching parens",
      "(a)",
      [ Punct '('; List [ Word "a" ]; Punct ')' ] );
    ( "simple nested braces",
      "[()]",
      [ Punct '['; List [ Punct '('; Punct ')' ]; Punct ']' ] );
    ("simple unclosed brace", "(", [ Punct '(' ]);
    ("simple unexpected closing brace", ")", [ Punct ')' ]);
    ( "parens",
      "a (b [c{}]) d\n",
      [
        Word "a";
        Punct '(';
        List
          [
            Word "b";
            Punct '[';
            List [ Word "c"; Punct '{'; Punct '}' ];
            Punct ']';
          ];
        Punct ')';
        Word "d";
      ] );
    ( "nothing to close",
      {|
a ) b
c )
) d
)
|},
      [
        Word "a";
        Punct ')';
        Word "b";
        Word "c";
        Punct ')';
        Punct ')';
        Word "d";
        Punct ')';
      ] );
    ( "not closed",
      {|
a ( b
c (
( d
(
|},
      [
        Word "a";
        Punct '(';
        Word "b";
        Word "c";
        Punct '(';
        Punct '(';
        Word "d";
        Punct '(';
      ] );
    ("mismatched", "(]\n", [ Punct '('; Punct ']' ]);
    ( "mismatched outside",
      "a ( {b} ] c\n",
      [
        Word "a";
        Punct '(';
        Punct '{';
        List [ Word "b" ];
        Punct '}';
        Punct ']';
        Word "c";
      ] );
    ( "mismatched inside",
      "a ( {b] ) c\n",
      [
        Word "a"; Punct '('; Punct '{'; Word "b"; Punct ']'; Punct ')'; Word "c";
      ] );
    ( "mismatched sequence",
      "a ( ] b { ) c\n",
      [
        Word "a"; Punct '('; Punct ']'; Word "b"; Punct '{'; Punct ')'; Word "c";
      ] );
    ( "mismatched after",
      "a ( ) b { ] c\n",
      [
        Word "a"; Punct '('; Punct ')'; Word "b"; Punct '{'; Punct ']'; Word "c";
      ] );
    (* This used to run in exponential time wrt number of unclosed parens: *)
    ( "avoid catastrophic parsing cost",
      "((((((((((((((((((((",
      [
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
        Punct '(';
      ] );
  ]

let pretty_corpus =
  [
    ( "text parens multiline",
      {|
Je t'aime (moi
non plus).
|},
      {|Je
t
'
aime
(
moi
non
plus
)
.
|} );
    ( "text parens same line",
      "Je t'aime (moi non plus).\n",
      {|Je
t
'
aime
(
  moi
  non
  plus
)
.
|} );
  ]

let test =
  let suite =
    List_.map
      (fun (name, input, expected_output) ->
        t name (fun () -> run_debug input expected_output))
      debug_corpus
    @ List_.map
        (fun (name, input, expected_output) ->
          t name (fun () -> run_pretty input expected_output))
        pretty_corpus
  in
  Testo.categorize "Parser" suite
