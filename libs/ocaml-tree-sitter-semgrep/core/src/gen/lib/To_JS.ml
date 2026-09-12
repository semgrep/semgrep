(*
   Convert a JSON grammar to JavaScript for better readability when
   debugging.
*)

open Printf
open Tree_sitter_grammar
open Indent.Types

let flatten nested_list =
  List.map (fun nodes -> Inline nodes) nested_list

let comma is_last s =
  if is_last then s
  else s ^ ","

(* Produce a javascript quoted string literal *)
let str s =
  Yojson.Safe.to_string (`String s)

let pattern ?flags s =
  sprintf "/%s/%s" s (Option.value ~default:"" flags)

let rule s =
  sprintf "$.%s" s

(*
   Move the leading underscore to the end of the string for sorting purposes:
     "_foo" -> "foo_"
*)
let move_leading_underscore str =
  if str <> "" && str.[0] = '_' then
    String.sub str 1 (String.length str - 1) ^ "_"
  else
    str

(*
   Compare rule names alphabetically, using the leading '_' only as a
   last-resort disambiguator to allow 'foo' and '_foo' to appear in the
   same region when comparing two grammars.
*)
let compare_rule_name a b =
  String.compare (move_leading_underscore a) (move_leading_underscore b)

(*
   This is for sorting choice() elements
*)
let rule_body_rank : Rule_body.t -> int = function
  | Blank -> 0
  | Literal _ -> 1
  | Pattern _ -> 2
  | Symbol _ -> 3
  | Seq _ -> 4
  | Choice _ -> 5
  | Repeat _ -> 6
  | Repeat1 _ -> 7
  | Prec (Default, _, _) -> 8
  | Prec (Dynamic, _, _) -> 9
  | Prec (Left, _, _)  -> 10
  | Prec (Right, _, _) -> 11
  | Alias _ -> 12
  | Field _ -> 13
  | Immediate_token _ -> 14
  | Token _ -> 15
  | Reserved _ -> 16

let compare_rule_body (a : Rule_body.t) (b : Rule_body.t) =
  match a, b with
  | Symbol a, Symbol b -> compare_rule_name a b
  | _ -> Int.compare (rule_body_rank a) (rule_body_rank b)

let pp_conflict ?prefix:_ ?(is_last = true) rule_list =
  let rules = List.map rule rule_list |> String.concat ", " in
  [ Line (sprintf "[%s]" rules |> comma is_last) ]

let rec map
    (f : ?prefix:string -> ?is_last:bool -> _ -> _) xs =
  match xs with
  | [] -> []
  | [x] -> [f ~is_last:true x]
  | x :: xs -> f ~is_last:false x :: map f xs

let string_of_prec_value (x : prec_value) =
  match x with
  | Num  n -> string_of_int n
  | Named  name -> str name

let pp_value ~is_last (s : string) =
  [ Line (s |> comma is_last) ]

let pp_prec_value ~is_last x =
  let s = string_of_prec_value x in
  pp_value ~is_last s

(*
   The 'sort_choices' setting is the same for all recursive calls
   unlike the other options 'prefix' and 'is_last'.
*)
let pp_body ~sort_choices ?prefix ?is_last (body : Rule_body.t) =
  let rec pp_body ?(prefix = "") ?(is_last = true) (body : Rule_body.t) =
    let cons, args =
      match body with
      | Symbol s -> rule s, None
      | Literal s -> str s, None
      | Pattern { value; flags } -> pattern ?flags value, None
      | Blank -> "\"\" /* blank */", None
      | Repeat x -> "repeat", Some (pp_body x)
      | Repeat1 x -> "repeat1", Some (pp_body x)
      | Choice [x; Blank] -> "optional", Some (pp_body x)
      | Choice xs ->
          let xs =
            if sort_choices then
              List.sort compare_rule_body xs
            else
              xs
          in
          "choice", Some (map pp_body xs |> flatten)
      | Seq xs -> "seq", Some (map pp_body xs |> flatten)
      | Prec (type_, prec_value, x) ->
          (match type_ with Default -> "prec" | Left -> "prec.left" | Right -> "prec.right" | Dynamic -> "prec.dynamic"), Some [
            Inline (pp_prec_value ~is_last:false prec_value);
            Inline (pp_body ~is_last:true x);
          ]
      | Alias x -> "alias", Some (pp_alias x)
      | Field (name, x) -> "field", Some (pp_field name x)
      | Immediate_token x -> "token.immediate", Some (pp_body x)
      | Token x -> "token", Some (pp_body x)
      | Reserved reserved -> "reserved", Some (pp_reserved reserved)
    in
    match args with
    | None ->
        [ Line (sprintf "%s%s" prefix (cons |> comma is_last)) ]
    | Some args ->
        [
          Line (sprintf "%s%s(" prefix cons);
          Block args;
          Line (")" |> comma is_last)
        ]

  and pp_alias x =
    let new_name =
      match x.named with
      | true -> rule x.value
      | false -> str x.value
    in
    [
      Inline (pp_body ~is_last:false x.content);
      Line new_name;
    ]

  and pp_field name x =
    [
      Line (str name ^ ",");
      Inline (pp_body ~is_last:true x);
    ]
  and pp_reserved reserved =
    [
      Line (str reserved.context_name ^ ",");
      Inline (pp_body ~is_last:true reserved.content);
    ]
  in
  pp_body ?prefix ?is_last body

let pp_rule ~sort_choices ?prefix:_ ?is_last (name, body) =
  pp_body ~sort_choices ~prefix:(sprintf "%s: $ => " name) ?is_last body

let pp_word (x : ident option) =
  match x with
  | None -> []
  | Some s -> [ Line (sprintf "word: $ => %s," (rule s)) ]

let string_of_named_prec_level x =
  match x with
  | Prec_symbol name -> rule name
  | Prec_string s -> str s

let pp_precedence_level ?prefix:_ ?(is_last = true) level =
  let level =
    level
    |> List.map string_of_named_prec_level
    |> String.concat ", "
  in
  [ Line (sprintf "[%s]" level |> comma is_last) ]

let pp_grammar ~sort_choices ~sort_rules (x : grammar) : Indent.t =
  let rules =
    if sort_rules then
      match x.rules with
      | entrypoint :: other_rules ->
          entrypoint
          :: List.sort (fun (a, _) (b, _) -> compare_rule_name a b) other_rules
      | [] -> []
    else
      x.rules
  in
  let pp_body = pp_body ~sort_choices in
  [
    Line
      "// JavaScript grammar recovered from JSON by 'ocaml-tree-sitter to-js'";
    Line "module.exports = grammar({";
    Block [
      Line (sprintf "name: %s," (str x.name));
      Inline (pp_word x.word);
      Line "externals: $ => [";
      Block (map pp_body x.externals |> flatten);
      Line "],";
      Line "conflicts: $ => [";
      Block (map pp_conflict x.conflicts |> flatten);
      Line "],";
      Line "inline: $ => [";
      Block (map (fun ?prefix:_ ?(is_last = true) name ->
        Line (rule name |> comma is_last)
      ) x.inline);
      Line "],";
      Line "precedences: $ => [";
      Block (map pp_precedence_level x.precedences |> flatten);
      Line "],";
      Line "supertypes: $ => [";
      Block (map (fun ?prefix:_ ?(is_last = true) name ->
        Line (rule name |> comma is_last)
      ) x.supertypes);
      Line "],";
      Line "extras: $ => [";
      Block (map pp_body x.extras |> flatten);
      Line "],";
      Line "rules: {";
      Block (map (pp_rule ~sort_choices) rules |> flatten);
      Line "}";
    ];
    Line "});";
  ]

let rec strip (body : Rule_body.t) =
  match body with
  | Symbol _
  | Literal _
  | Pattern _
  | Blank -> body
  | Repeat x -> Repeat (strip x)
  | Repeat1 x -> Repeat1 (strip x)
  | Choice xs -> Choice (List.map strip xs)
  | Seq xs -> Seq (List.map strip xs)
  | Prec (_, _prec_value, x) -> strip x
  | Alias x -> strip x.content
  | Field (_name, x) -> strip x
  | Immediate_token x -> strip x
  | Token x -> strip x
  | Reserved reserved -> strip reserved.content

(*
   Eliminate all the constructs that don't affect the structure of the
   tree as reflected by the types in the generated file CST.ml.
*)
let strip_grammar (grammar : grammar) =
  {
    grammar with
    extras = List.map strip grammar.extras;
    inline = [];
    conflicts = [];
    precedences = [];
    externals = (* is this useful to keep? *) List.map strip grammar.externals;
    supertypes = [];
    rules = List.map (fun (name, body) -> (name, strip body)) grammar.rules;
  }

let run ~sort_choices ~sort_rules ~strip input_path output_path =
  let grammar =
    match input_path with
    | None ->
        (match Yojson.Safe.from_channel stdin |> grammar_of_yojson with
         | Ok g -> g
         | Error e -> failwith (sprintf "Failed to parse stdin: %s" e))
    | Some file ->
        (match Yojson.Safe.from_file file |> grammar_of_yojson with
         | Ok g -> g
         | Error e -> failwith (sprintf "Failed to parse %s: %s" file e))
  in
  let grammar = if strip then strip_grammar grammar else grammar in
  let tree = pp_grammar ~sort_choices ~sort_rules grammar in
  match output_path with
  | None -> Indent.to_channel stdout tree
  | Some file -> Indent.to_file file tree
