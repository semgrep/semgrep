(**
   Boilerplate to be used as a template when mapping the bash CST
   to another type of tree.
*)

open Common
module AST = AST_bash
module CST = Tree_sitter_bash.CST
module PI = Parse_info
open AST_bash
module G = AST_generic_
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token

let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

let todo (_env : env) _ = failwith "not implemented"

let string_content (env : env) (tok : CST.string_content) : string wrap =
  str env tok

let simple_heredoc_body (env : env) (tok : CST.simple_heredoc_body) :
    string wrap =
  str env tok

let ansii_c_string (env : env) (tok : CST.ansii_c_string) : string wrap =
  (* pattern "\\$'([^']|\\\\')*'" *)
  str env tok

let variable_name (env : env) (tok : CST.variable_name) : string wrap =
  str env tok

let terminator (env : env) (x : CST.terminator) =
  match x with
  | `SEMI tok -> token env tok (* ";" *)
  | `SEMISEMI tok -> token env tok (* ";;" *)
  | `LF tok -> token env tok (* "\n" *)
  | `AMP tok -> token env tok

(* "&" *)

let empty_value (_env : env) (_tok : CST.empty_value) : unit = ()

let file_descriptor (env : env) (tok : CST.file_descriptor) = token env tok

(* file_descriptor *)

let concat (_env : env) (_tok : CST.concat) : unit = ()

let semgrep_metavariable_name (env : env) (tok : CST.semgrep_metavariable_name)
    =
  (* pattern [A-Z_][A-Z_0-9]* *)
  str env tok

(* -e, -z, etc. *)
let test_operator (env : env) (tok : CST.test_operator) : string wrap =
  str env tok

let simple_variable_name (env : env) (tok : CST.simple_variable_name) :
    string wrap =
  (* pattern \w+ *)
  str env tok

let special_variable_name (env : env) (x : CST.special_variable_name) :
    string wrap =
  match x with
  | `STAR tok -> str env tok (* "*" *)
  | `AT tok -> str env tok (* "@" *)
  | `QMARK tok -> str env tok (* "?" *)
  | `DASH tok -> str env tok (* "-" *)
  | `DOLLAR tok -> str env tok (* "$" *)
  | `X_0 tok -> str env tok (* "0" *)
  | `X__ tok -> str env tok

(* "_" *)

let heredoc_body_beginning (env : env) (tok : CST.heredoc_body_beginning) =
  token env tok

let word (env : env) (tok : CST.word) = token env tok

let heredoc_start (env : env) (tok : CST.heredoc_start) = token env tok

let raw_string (env : env) (tok : CST.raw_string) : string wrap =
  (* pattern "'[^']*'" *)
  str env tok

let regex (env : env) (tok : CST.regex) = token env tok

let heredoc_body_end (env : env) (tok : CST.heredoc_body_end) = token env tok

let heredoc_body_middle (env : env) (tok : CST.heredoc_body_middle) =
  token env tok

let special_character (env : env) (tok : CST.special_character) : string wrap =
  str env tok

let heredoc_redirect (env : env) ((v1, v2) : CST.heredoc_redirect) : todo =
  let _v1 =
    match v1 with
    | `LTLT tok -> token env tok (* "<<" *)
    | `LTLTDASH tok -> token env tok
    (* "<<-" *)
  in
  let _v2 = token env v2 (* heredoc_start *) in
  TODO

let simple_expansion (env : env) ((v1, v2) : CST.simple_expansion) =
  let dollar = token env v1 (* "$" *) in
  let variable_name =
    match v2 with
    | `Pat_42e353e tok -> Simple_variable_name (simple_variable_name env tok)
    | `Choice_STAR x -> Special_variable_name (special_variable_name env x)
    | `BANG tok -> Special_variable_name (str env tok (* "!" *))
    | `HASH tok -> Special_variable_name (str env tok (* "#" *))
  in
  (dollar, variable_name)

let rec prim_exp_or_special_char (env : env)
    (x : CST.anon_choice_prim_exp_618725a) : expression =
  match x with
  | `Choice_semg_ellips x -> primary_expression env x
  | `Spec_char tok -> Special_character (str env tok)

and stmt_with_opt_heredoc (env : env)
    ((v1, v2, v3) : CST.anon_stmt_opt_LF_here_body_term_3efa649) =
  let _v1 = statement env v1 in
  let _v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "\n" *) in
        let _v2 = heredoc_body env v2 in
        TODO
    | None -> TODO
  in
  let _v3 = terminator env v3 in
  TODO

and array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let _v1 = token env v1 (* "(" *) in
  let _v2 = List.map (literal env) v2 in
  let _v3 = token env v3 (* ")" *) in
  TODO

and binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Exp_choice_EQ_exp (v1, v2, v3) ->
      let _v1 = expression env v1 in
      let _v2 =
        match v2 with
        | `EQ tok -> token env tok (* "=" *)
        | `EQEQ tok -> token env tok (* "==" *)
        | `EQTILDE tok -> token env tok (* "=~" *)
        | `BANGEQ tok -> token env tok (* "!=" *)
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `PLUSEQ tok -> token env tok (* "+=" *)
        | `DASHEQ tok -> token env tok (* "-=" *)
        | `LT tok -> token env tok (* "<" *)
        | `GT tok -> token env tok (* ">" *)
        | `LTEQ tok -> token env tok (* "<=" *)
        | `GTEQ tok -> token env tok (* ">=" *)
        | `BARBAR tok -> token env tok (* "||" *)
        | `AMPAMP tok -> token env tok (* "&&" *)
        | `Test_op tok -> token env tok
        (* test_operator *)
      in
      let _v3 = expression env v3 in
      TODO
  | `Exp_choice_EQEQ_regex (v1, v2, v3) ->
      let _v1 = expression env v1 in
      let _v2 =
        match v2 with
        | `EQEQ tok -> token env tok (* "==" *)
        | `EQTILDE tok -> token env tok
        (* "=~" *)
      in
      let _v3 = token env v3 (* regex *) in
      TODO

and case_item (env : env) ((v1, v2, v3, v4, v5) : CST.case_item) =
  let _v1 = literal env v1 in
  let _v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "|" *) in
        let v2 = literal env v2 in
        todo env (v1, v2))
      v2
  in
  let _v3 = token env v3 (* ")" *) in
  let _v4 = program env v4 in
  let _v5 =
    match v5 with
    | `SEMISEMI tok -> token env tok (* ";;" *)
    | `Choice_SEMIAMP x -> (
        match x with
        | `SEMIAMP tok -> token env tok (* ";&" *)
        | `SEMISEMIAMP tok -> token env tok (* ";;&" *) )
  in
  TODO

and command (env : env) ((v1, v2, v3) : CST.command) =
  let assignments, redirects =
    partition_either
      (fun x ->
        match x with
        | `Var_assign x -> Left (variable_assignment env x)
        | `File_redi x -> Right (file_redirect env x))
      v1
  in
  let name = command_name env v2 in
  let arguments =
    List.map
      (fun x ->
        match x with
        | `Choice_conc x -> literal env x
        | `Choice_EQTILDE_choice_choice_conc (v1, v2) ->
            let v1 =
              match v1 with
              | `EQTILDE tok -> token env tok (* "=~" *)
              | `EQEQ tok -> token env tok
              (* "==" *)
            in
            let v2 =
              match v2 with
              | `Choice_conc x -> literal env x
              | `Regex tok -> token env tok
              (* regex *)
            in
            todo env (v1, v2))
      v3
  in
  todo env (v1, v2, v3)

and command_name (env : env) (x : CST.command_name) : expression =
  match x with
  | `Conc x -> Concatenation (concatenation env x)
  | `Choice_semg_ellips x -> primary_expression env x
  | `Rep1_spec_char xs ->
      Concatenation (List.map (fun tok -> Special_character (str env tok)) xs)

and command_substitution (env : env) (x : CST.command_substitution) :
    list_ bracket =
  match x with
  | `DOLLARLPAR_stmts_RPAR (v1, v2, v3) ->
      let open_ = token env v1 (* "$(" *) in
      let list = statements env v2 in
      let close = token env v3 (* ")" *) in
      (open_, list, close)
  | `DOLLARLPAR_file_redi_RPAR (v1, v2, v3) ->
      let open_ = token env v1 (* "$(" *) in
      let _v2 = (* TODO: what's this? *) file_redirect env v2 in
      let close = token env v3 (* ")" *) in
      (open_, Empty, close)
  | `BQUOT_stmts_BQUOT (v1, v2, v3) ->
      let open_ = token env v1 (* "`" *) in
      let list = statements env v2 in
      let close = token env v3 (* "`" *) in
      (open_, list, close)

and compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = token env v1 (* "{" *) in
  let v2 = match v2 with Some x -> statements2 env x | None -> todo env () in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and concatenation (env : env) ((v1, v2, v3) : CST.concatenation) :
    expression list =
  let v1 = prim_exp_or_special_char env v1 in
  let v2 =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* concat *) in
        let v2 = prim_exp_or_special_char env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* concat *) in
        let v2 = token env v2 (* "$" *) in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and do_group (env : env) ((v1, v2, v3) : CST.do_group) =
  let v1 = token env v1 (* "do" *) in
  let v2 = match v2 with Some x -> statements2 env x | None -> todo env () in
  let v3 = token env v3 (* "done" *) in
  todo env (v1, v2, v3)

and elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = token env v1 (* "elif" *) in
  let v2 = terminated_statement env v2 in
  let v3 = token env v3 (* "then" *) in
  let v4 = match v4 with Some x -> statements2 env x | None -> todo env () in
  todo env (v1, v2, v3, v4)

and else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = token env v1 (* "else" *) in
  let v2 = match v2 with Some x -> statements2 env x | None -> todo env () in
  todo env (v1, v2)

and expansion (env : env) ((v1, v2, v3, v4) : CST.expansion) :
    complex_expansion bracket =
  let open_ = token env v1 (* "${" *) in
  let _v2_TODO =
    match v2 with
    | Some x -> (
        match x with
        | `HASH tok -> Some (token env tok (* "#" *))
        | `BANG tok -> Some (token env tok (* "!" *)) )
    | None -> None
  in
  (* TODO: need to handle all the cases other than just a variable
     like ${foo} *)
  let opt_variable =
    match v3 with
    | Some x -> (
        match x with
        | `Var_name_EQ_opt_choice_conc (v1, v2, v3) ->
            (* TODO *)
            let _v1 = token env v1 (* variable_name *) in
            let _v2 = token env v2 (* "=" *) in
            let _v3 =
              match v3 with Some x -> Some (literal env x) | None -> None
            in
            None
        | `Choice_subs_opt_SLASH_opt_regex_rep_choice_choice_conc (v1, v2, v3)
          ->
            let opt_variable =
              match v1 with
              | `Subs _x -> (* TODO: subscript env x *) None
              | `Pat_42e353e tok ->
                  Some (Simple_variable_name (simple_variable_name env tok))
              | `Choice_STAR x ->
                  Some (Special_variable_name (special_variable_name env x))
            in
            let _v2_TODO () =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* / *) in
                  let v2 =
                    match v2 with
                    | Some tok -> token env tok (* regex *)
                    | None -> todo env ()
                  in
                  todo env (v1, v2)
              | None -> todo env ()
            in
            let _v3_TODO () =
              List.map
                (fun x ->
                  match x with
                  | `Choice_conc x -> literal env x
                  | `COLON tok -> todo env tok (* ":" *)
                  | `COLONQMARK tok -> todo env tok (* ":?" *)
                  | `EQ tok -> todo env tok (* "=" *)
                  | `COLONDASH tok -> todo env tok (* ":-" *)
                  | `PERC tok -> todo env tok (* "%" *)
                  | `DASH tok -> todo env tok (* "-" *)
                  | `HASH tok -> todo env tok
                  (* "#" *))
                v3
            in
            opt_variable )
    | None -> None
  in
  let close = token env v4 (* "}" *) in
  let complex_expansion =
    match opt_variable with
    | Some variable_name -> Variable variable_name
    | None -> Complex_expansion_TODO
  in
  (open_, complex_expansion, close)

(* These are expressions used in [[ ... ]], in C-style for loops, etc. *)
and expression (env : env) (x : CST.expression) : todo =
  match x with
  | `Choice_conc x ->
      literal env x |> ignore;
      TODO
  | `Un_exp (v1, v2) ->
      let _v1 =
        match v1 with
        | `BANG tok -> token env tok (* "!" *)
        | `Test_op tok -> token env tok
        (* test_operator *)
      in
      let _v2 = expression env v2 in
      TODO
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let _v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let _v3 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let _v5 = expression env v5 in
      TODO
  | `Bin_exp x -> binary_expression env x
  | `Post_exp (v1, v2) ->
      let _v1 = expression env v1 in
      let _v2 =
        match v2 with
        | `PLUSPLUS tok -> token env tok (* "++" *)
        | `DASHDASH tok -> token env tok
        (* "--" *)
      in
      TODO
  | `Paren_exp (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let _v2 = expression env v2 in
      let _v3 = token env v3 (* ")" *) in
      TODO

and file_redirect (env : env) ((v1, v2, v3) : CST.file_redirect) : todo =
  let _fd =
    match v1 with
    | Some tok -> Some (token env tok (* file_descriptor *))
    | None -> None
  in
  let _v2 =
    match v2 with
    | `LT tok -> token env tok (* "<" *)
    | `GT tok -> token env tok (* ">" *)
    | `GTGT tok -> token env tok (* ">>" *)
    | `AMPGT tok -> token env tok (* "&>" *)
    | `AMPGTGT tok -> token env tok (* "&>>" *)
    | `LTAMP tok -> token env tok (* "<&" *)
    | `GTAMP tok -> token env tok (* ">&" *)
    | `GTBAR tok -> token env tok
    (* ">|" *)
  in
  let _v3 = literal env v3 in
  TODO

and heredoc_body (env : env) (x : CST.heredoc_body) : todo =
  match x with
  | `Simple_here_body tok ->
      token env tok (* simple_heredoc_body *) |> ignore;
      TODO
  | `Here_body_begin_rep_choice_expa_here_body_end (v1, v2, v3) ->
      let _v1 = token env v1 (* heredoc_body_beginning *) in
      let _v2 =
        List.map
          (fun x ->
            match x with
            | `Expa x ->
                expansion env x |> ignore;
                TODO
            | `Simple_expa x ->
                simple_expansion env x |> ignore;
                TODO
            | `Cmd_subs x ->
                command_substitution env x |> ignore;
                TODO
            | `Here_body_middle tok ->
                token env tok (* heredoc_body_middle *) |> ignore;
                TODO)
          v2
      in
      let _v3 = token env v3 (* heredoc_body_end *) in
      TODO

and herestring_redirect (env : env) ((v1, v2) : CST.herestring_redirect) =
  let _v1 = token env v1 (* "<<<" *) in
  let _v2 = literal env v2 in
  TODO

and last_case_item (env : env) ((v1, v2, v3, v4, v5) : CST.last_case_item) =
  let _v1 = literal env v1 in
  let _v2 =
    List.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "|" *) in
        let _v2 = literal env v2 in
        TODO)
      v2
  in
  let _v3 = token env v3 (* ")" *) in
  let _v4 = program env v4 in
  let _v5 =
    match v5 with Some tok -> Some (token env tok (* ";;" *)) | None -> None
  in
  TODO

and literal (env : env) (x : CST.literal) : expression =
  match x with
  | `Conc x -> Concatenation (concatenation env x)
  | `Choice_semg_ellips x -> primary_expression env x
  | `Rep1_spec_char xs ->
      Concatenation (List.map (fun tok -> Special_character (str env tok)) xs)

and primary_expression (env : env) (x : CST.primary_expression) : expression =
  match x with
  | `Semg_ellips tok -> Semgrep_ellipsis (token env tok (* "..." *))
  | `Semg_double_curl_meta (v1, v2, v3) ->
      let _open_ = token env v1 (* "${{" *) in
      let name = str env v2 (* pattern [A-Z_][A-Z_0-9]* *) in
      let _close = token env v3 (* "}}" *) in
      Semgrep_metavariable name
  | `Word tok -> Word (str env tok (* word *))
  | `Str x -> String (string_ env x)
  | `Raw_str tok -> Raw_string (str env tok (* pattern "'[^']*'" *))
  | `Ansii_c_str tok ->
      Ansii_c_string (str env tok (* pattern "\\$'([^']|\\\\')*'" *))
  | `Expa x -> String_fragment (Expansion (Complex_expansion (expansion env x)))
  | `Simple_expa x ->
      String_fragment (Expansion (Simple_expansion (simple_expansion env x)))
  | `Str_expa (v1, v2) ->
      let v1 = token env v1 (* "$" *) in
      let v2 =
        match v2 with
        | `Str x -> string_ env x
        | `Raw_str tok ->
            (* TODO: how is it different from ANSI C strings?
               e.g. $'hello\nbye' *)
            token env tok
        (* pattern "'[^']*'" *)
      in
      todo env (v1, v2)
  | `Cmd_subs x -> command_substitution env x
  | `Proc_subs (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `LTLPAR tok -> token env tok (* "<(" *)
        | `GTLPAR tok -> token env tok
        (* ">(" *)
      in
      let v2 = statements env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)

and program (env : env) (opt : CST.program) =
  match opt with Some x -> statements env x | None -> todo env ()

and statement (env : env) (x : CST.statement) =
  match x with
  | `Redi_stmt (v1, v2) ->
      let v1 = statement env v1 in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `File_redi x -> file_redirect env x
            | `Here_redi_a9657de x -> heredoc_redirect env x
            | `Here_redi_7d3292d x -> herestring_redirect env x)
          v2
      in
      todo env (v1, v2)
  | `Var_assign x -> variable_assignment env x
  | `Cmd x -> command env x
  | `Decl_cmd (v1, v2) ->
      let v1 =
        match v1 with
        | `Decl tok -> token env tok (* "declare" *)
        | `Type tok -> token env tok (* "typeset" *)
        | `Export tok -> token env tok (* "export" *)
        | `Read tok -> token env tok (* "readonly" *)
        | `Local tok -> token env tok
        (* "local" *)
      in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Choice_conc x -> literal env x
            | `Pat_42e353e tok -> simple_variable_name env tok
            | `Var_assign x -> variable_assignment env x)
          v2
      in
      todo env (v1, v2)
  | `Unset_cmd (v1, v2) ->
      let v1 =
        match v1 with
        | `Unset tok -> token env tok (* "unset" *)
        | `Unse tok -> token env tok
        (* "unsetenv" *)
      in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Choice_conc x -> literal env x
            | `Pat_42e353e tok -> simple_variable_name env tok)
          v2
      in
      todo env (v1, v2)
  | `Test_cmd x -> test_command env x
  | `Nega_cmd (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 =
        match v2 with
        | `Cmd x -> command env x
        | `Test_cmd x -> test_command env x
        | `Subs x -> subshell env x
      in
      todo env (v1, v2)
  | `For_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* pattern \w+ *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "in" *) in
            let v2 = List.map (literal env) v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v4 = terminator env v4 in
      let v5 = do_group env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `C_style_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "((" *) in
      let v3 =
        match v3 with Some x -> expression env x | None -> todo env ()
      in
      let v4 = terminator env v4 in
      let v5 =
        match v5 with Some x -> expression env x | None -> todo env ()
      in
      let v6 = terminator env v6 in
      let v7 =
        match v7 with Some x -> expression env x | None -> todo env ()
      in
      let v8 = token env v8 (* "))" *) in
      let v9 =
        match v9 with
        | Some tok -> token env tok (* ";" *)
        | None -> todo env ()
      in
      let v10 =
        match v10 with
        | `Do_group x -> do_group env x
        | `Comp_stmt x -> compound_statement env x
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = terminated_statement env v2 in
      let v3 = do_group env v3 in
      todo env (v1, v2, v3)
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = terminated_statement env v2 in
      let v3 = token env v3 (* "then" *) in
      let v4 =
        match v4 with Some x -> statements2 env x | None -> todo env ()
      in
      let v5 = List.map (elif_clause env) v5 in
      let v6 =
        match v6 with Some x -> else_clause env x | None -> todo env ()
      in
      let v7 = token env v7 (* "fi" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Case_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "case" *) in
      let v2 = literal env v2 in
      let v3 =
        match v3 with Some x -> terminator env x | None -> todo env ()
      in
      let v4 = token env v4 (* "in" *) in
      let v5 = terminator env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = List.map (case_item env) v1 in
            let v2 = last_case_item env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v7 = token env v7 (* "esac" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Pipe (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 =
        match v2 with
        | `BAR tok -> token env tok (* "|" *)
        | `BARAMP tok -> token env tok
        (* "|&" *)
      in
      let v3 = statement env v3 in
      todo env (v1, v2, v3)
  | `List (v1, v2, v3) ->
      let v1 = statement env v1 in
      let v2 =
        match v2 with
        | `AMPAMP tok -> token env tok (* "&&" *)
        | `BARBAR tok -> token env tok
        (* "||" *)
      in
      let v3 = statement env v3 in
      todo env (v1, v2, v3)
  | `Subs x -> subshell env x
  | `Comp_stmt x -> compound_statement env x
  | `Func_defi (v1, v2) ->
      let v1 =
        match v1 with
        | `Func_word_opt_LPAR_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "function" *) in
            let v2 = token env v2 (* word *) in
            let v3 =
              match v3 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* "(" *) in
                  let v2 = token env v2 (* ")" *) in
                  todo env (v1, v2)
              | None -> todo env ()
            in
            todo env (v1, v2, v3)
        | `Word_LPAR_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* word *) in
            let v2 = token env v2 (* "(" *) in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
      in
      let v2 =
        match v2 with
        | `Comp_stmt x -> compound_statement env x
        | `Subs x -> subshell env x
        | `Test_cmd x -> test_command env x
      in
      todo env (v1, v2)

and statements (env : env) ((v1, v2, v3, v4) : CST.statements) =
  let v1 = List.map (stmt_with_opt_heredoc env) v1 in
  let v2 = statement env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "\n" *) in
        let v2 = heredoc_body env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v4 = match v4 with Some x -> terminator env x | None -> todo env () in
  todo env (v1, v2, v3, v4)

and statements2 (env : env) (xs : CST.statements2) =
  List.map (stmt_with_opt_heredoc env) xs

and string_ (env : env) ((v1, v2, v3, v4) : CST.string_) :
    string_fragment list bracket =
  let open_ = token env v1 (* "\"" *) in
  let fragments =
    List.map
      (fun (v1, v2) ->
        let fragments =
          match v1 with
          | `Opt_DOLLAR_str_content (v1, v2) ->
              let dollar =
                match v1 with
                | Some tok -> [ String_content (str env tok) (* "$" *) ]
                | None -> []
              in
              let content =
                [ String_content (str env v2) (* string_content *) ]
              in
              dollar @ content
          | `Expa x -> [ Expansion (Complex_expansion (expansion env x)) ]
          | `Simple_expa x ->
              [ Expansion (Simple_expansion (simple_expansion env x)) ]
          | `Cmd_subs x -> [ Command_substitution (command_substitution env x) ]
        in
        let _concat = match v2 with Some _tok -> () | None -> () in
        fragments)
      v2
    |> List.flatten
  in
  let fragment =
    match v3 with
    | Some tok -> [ String_content (str env tok (* "$" *)) ]
    | None -> []
  in
  let close = token env v4 (* "\"" *) in
  (open_, fragments @ [ fragment ], close)

and subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let v1 = token env v1 (* variable_name *) in
  let v2 = token env v2 (* "[" *) in
  let v3 = literal env v3 in
  let v4 =
    match v4 with Some tok -> token env tok (* concat *) | None -> todo env ()
  in
  let v5 = token env v5 (* "]" *) in
  let v6 =
    match v6 with Some tok -> token env tok (* concat *) | None -> todo env ()
  in
  todo env (v1, v2, v3, v4, v5, v6)

and subshell (env : env) ((v1, v2, v3) : CST.subshell) : subshell =
  let v1 = token env v1 (* "(" *) in
  let v2 = statements env v2 in
  let v3 = token env v3 (* ")" *) in
  TODO

and terminated_statement (env : env) ((v1, v2) : CST.terminated_statement) =
  let v1 = statement env v1 in
  let v2 = terminator env v2 in
  todo env (v1, v2)

and test_command (env : env) (v1 : CST.test_command) =
  match v1 with
  | `LBRACK_exp_RBRACK (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = expression env v2 in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  | `LBRACKLBRACK_exp_RBRACKRBRACK (v1, v2, v3) ->
      let v1 = token env v1 (* "[[" *) in
      let v2 = expression env v2 in
      let v3 = token env v3 (* "]]" *) in
      todo env (v1, v2, v3)
  | `LPARLPAR_exp_RPARRPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "((" *) in
      let v2 = expression env v2 in
      let v3 = token env v3 (* "))" *) in
      todo env (v1, v2, v3)

and variable_assignment (env : env) ((v1, v2, v3) : CST.variable_assignment) =
  let v1 =
    match v1 with
    | `Var_name tok -> token env tok (* variable_name *)
    | `Subs x -> subscript env x
  in
  let v2 =
    match v2 with
    | `EQ tok -> token env tok (* "=" *)
    | `PLUSEQ tok -> token env tok
    (* "+=" *)
  in
  let v3 =
    match v3 with
    | `Choice_conc x -> literal env x
    | `Array x -> array_ env x
    | `Empty_value tok -> token env tok
    (* empty_value *)
  in
  todo env (v1, v2, v3)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_bash.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let x = program env cst in
      x)
