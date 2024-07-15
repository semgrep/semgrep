open Common
open Either_
open Fpath_.Operators
module CST = Tree_sitter_circom.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let _fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying from semgrep-circom/lib/Boilerplate.ml *)

let _map_signal_visability (env : env) (x : CST.signal_visability) =
  (match x with
  | `Input tok -> (* "input" *) str env tok
  | `Output tok -> (* "output" *) str env tok
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) str env tok

let map_circom_version (env : env) (tok : CST.circom_version) =
  (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) str env tok

let map_circom_pragma_token (env : env) (x : CST.circom_pragma_token) =
  (match x with
  | `Circom_circom_vers (v1, v2) ->
      let v1 = (* "circom" *) str env v1 in
      let v2 =
        (* pattern "\"?\\.? ?(\\d|\\*\
  )+(\\. ?(\\d|\\*\
  )+ ?(\\.(\\d|\\*\
  )+)?)?\"?" *) token env v2
      in (v1, Some v2)
  | `Circom_id (v1, v2) ->
      let v1 = (* "circom" *) str env v1 in
      let v2 =
        (* pattern [a-zA-Z$_][a-zA-Z0-9$_]* *) token env v2
      in (v1, Some v2)
  )

let map_string_ (env : env) (x : CST.string_) : string wrap bracket =
  match x with
  | `DQUOT_rep_choice_str_imme_elt_inside_double_quote_DQUOT (v1, v2, v3) ->
      let l = (* "\"" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_double_quote tok ->
                (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "\"" *) token env v3 in
      G.string_ (l, xs, r)
  | `SQUOT_rep_choice_str_imme_elt_inside_quote_SQUOT (v1, v2, v3) ->
      let l = (* "'" *) token env v1 in
      let xs =
        List_.map
          (fun x ->
            match x with
            | `Str_imme_elt_inside_quote tok ->
                (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *) str env tok
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let r = (* "'" *) token env v3 in
      G.string_ (l, xs, r)

let map_directive (env : env) (x : CST.directive) =
  (match x with
  | `Pragma_dire (v1, v2, v3) ->
      let tpragma = (* "pragma" *) token env v1 in
      let tcircom, anys_opt =
        match v2 with
        | `Circom_pragma_tok x ->
            map_circom_pragma_token env x
        | `Circom_custom_templs_tok tok ->
            (* "custom_templates" *) (str env tok, None)
      in 
      let anys_tokens = 
        match anys_opt with
        | Some anys -> [Tk anys]
        | None -> []
      in
      let sc = (* ";" *) token env v3 in
      [ Pragma (tcircom, [ Tk tpragma] @ anys_tokens @ [Tk sc ]) |> G.d ]
  | `Incl_dire (v1, v2, v3) ->
      let timport = (* "include" *) token env v1 in
      let _, path, _ = map_string_ env v2 in
      let tsc = (* ";" *) token env v3 in
      
      [ ImportAll (timport, FileName path, fake "") |> G.d ]
  )

let map_source_unit (env : env) (x : CST.source_unit) : item list =
  match x with
  | `Dire x ->
      let xs = map_directive env x in
      xs |> List_.map (fun dir -> DirectiveStmt dir |> G.s)
  | `Defi x ->
      let def = map_definition env x in
      [ DefStmt def |> G.s ]

let map_source_file(env: env) (x : CST.source_file) = 
  match x with
  | `Rep_source_unit v1 -> 
      let xss = List_.map  (map_source_unit env) v1 in 
      Pr (List.flatten xss)
  (* | `Rep1_stmt xs ->
      let xs = List_.map (map_statement env) xs in
      Ss xs
  | `Exp x -> 
      let e = map_expression env x in
        E e  *)
  
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_circom.Parse.file !!file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_source_file env cst with
      | G.Pr xs
      | G.Ss xs ->
          xs
      | _ -> failwith "not a program")
      
(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_solidity.Parse.string str)
    (fun cst ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      map_source_file env cst)