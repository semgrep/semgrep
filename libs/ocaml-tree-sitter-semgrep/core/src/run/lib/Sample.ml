(*
   Sample code that would normally be generated.

   The idea is to serve as a guide when writing or modifying the code
   generator.
*)

(*
   Disable warning 42:
     "this use of Foo relies on type-directed disambiguation,
      it will not compile with OCaml 4.00 or earlier."
*)
[@@@warning "-42"]

(* Disable warnings against unused variables. *)
[@@@warning "-26-27"]

module CST = struct
  type number = (Loc.t * string) (* pattern "\\d+" *)
  type variable = (Loc.t * string) (* pattern "\\a\\w*" *)
  type expression = [
    | `Var of variable
    | `Num of number
    | `Exp_PLUS_exp of (expression * (Loc.t * string (* "+" *)) * expression)
  ]
  type statement = (expression * (Loc.t * string (* ";" *)))
  type program = statement list (* zero or more *)
  type comment = (Loc.t * string)
  type extra = [
    | `Comment of comment
  ]
end

module Parse = struct
  (* open Tree_sitter_run *)

  let debug = ref false

  type mt = Run.matcher_token

  (*
  external create_parser :
    unit -> Tree_sitter_API.ts_parser = "octs_create_parser_XXX"
  *)

  let create_parser () : Tree_sitter_bindings.Tree_sitter_API.ts_parser =
    failwith "not implemented"

  let ts_parser = lazy (create_parser ())

  let parse_source_string ?src_file contents =
    Tree_sitter_parsing.parse_source_string ?src_file
      (Lazy.force ts_parser) contents

  let parse_source_file src_file =
    Tree_sitter_parsing.parse_source_file
      (Lazy.force ts_parser) src_file

  (* generated *)
  let extras = [
    "comment";
  ]

  (* generated *)
  let children_regexps : (string * Run.exp option) list = [
    (
      "variable",
      None
    );
    (
      "number",
      None
    );
    (
      "expression",
      Some (
        Alt [|
          Token (Name "variable");
          Token (Name "number");
          Seq [
            Token (Name "expression");
            Token (Literal "+");
            Token (Name "expression");
          ];
        |];
      )
    );
    (
      "comment",
      None
    );
  ]

  (* generated *)
  let trans_variable ((kind, body) : mt) : CST.variable =
    match body with
    | Leaf v -> v
    | _ -> assert false

  (* generated *)
  let trans_number ((kind, body) : mt) : CST.number =
    match body with
    | Leaf v -> v
    | Children _ -> assert false

  (* generated *)
  let rec trans_expression ((kind, body) : mt) : CST.expression =
    match body with
    | Children v ->
        (match v with
         | Alt (0, v) ->
             `Var (trans_variable (Run.matcher_token v))
         | Alt (1, v) ->
             `Num (trans_number (Run.matcher_token v))
         | Alt (2, v) ->
             `Exp_PLUS_exp (
               match v with
               | Seq [v1; v2; v3] ->
                   let v1 = trans_expression (Run.matcher_token v1) in
                   let v2 = Run.trans_token (Run.matcher_token v2) in
                   let v3 = trans_expression (Run.matcher_token v3) in
                   (v1, v2, v3)
               | _ -> assert false
             )
         | _ -> assert false
        )
    | Leaf _ -> assert false

  (* generated *)
  let trans_statement ((kind, body) : mt) =
    match body with
    | Children v ->
        (match v with
         | Seq [v1; v2] ->
             (trans_expression (Run.matcher_token v1),
              Run.trans_token (Run.matcher_token v2))
         | _ -> assert false
        )
    | _ -> assert false

  (* generated - entrypoint *)
  let trans_program ((kind, body) : mt) =
    match body with
    | Children v ->
        Run.repeat (fun v -> trans_statement (Run.matcher_token v)) v
    | _ -> assert false

  (* generated - extra *)
  let trans_comment ((kind, body) : mt) : CST.variable =
    match body with
    | Leaf v -> v
    | _ -> assert false

  let translate_tree src node trans_x =
    let matched_tree = Run.match_tree children_regexps src node in
    Option.map trans_x matched_tree

  (* generated *)
  let translate_extra src
      (node : Tree_sitter_bindings.Tree_sitter_output_t.node)
    : CST.extra option =
    match node.type_ with
    | "comment" ->
        (match translate_tree src node trans_comment with
         | None -> None
         | Some x -> Some (`Comment x))
    | _ -> None

  (* generated *)
  let translate_root src root_node =
    translate_tree src root_node trans_program

  let parse_input_tree input_tree =
    let orig_root_node = Tree_sitter_parsing.root input_tree in
    let src = Tree_sitter_parsing.src input_tree in
    let errors = Run.extract_errors src orig_root_node in
    let opt_program, extras =
      Run.translate
        ~extras
        ~translate_root:(translate_root src)
        ~translate_extra:(translate_extra src)
        orig_root_node
    in
    Parsing_result.create src opt_program extras errors

  let string ?src_file contents =
    let input_tree = parse_source_string ?src_file contents in
    parse_input_tree input_tree

  let file src_file =
    let input_tree = parse_source_file src_file in
    parse_input_tree input_tree

end
