(**
   Boilerplate to be used as a template when mapping the bash CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

open Common
module AST = AST_bash
module CST = Tree_sitter_bash.CST
module PI = Parse_info
open AST_bash
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type ts_tok = Tree_sitter_run.Token.t

type env = unit H.env

let token = H.token

let str = H.str

(*
   Replace the last element of a list.
   This is usually a sign that something wasn't done right.
*)
let map_last l f =
  match List.rev l with [] -> [] | last :: other -> List.rev (f last :: other)

(*
   The 'statement' rule returns one of 3 possible levels of constructs:
   - list = list of pipelines
   - pipeline = one of
      - list of commands
      - pipeline && pipeline
      - pipeline || pipeline
   - command
*)
type tmp_stmt =
  | Tmp_blist of blist
  | Tmp_pipeline of pipeline
  | Tmp_command of (command_with_redirects * unary_control_operator wrap option)

let blist_of_pipeline (pip : pipeline) =
  let loc = pipeline_loc pip in
  Pipelines (loc, [ pip ])

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

let todo (_env : env) _ =
  failwith "Parse_bash_tree_sitter: feature not implemented"

let unary_test_operator (env : env) (tok : ts_tok) : unary_test_operator wrap =
  let s, tok = str env tok in
  let op =
    match s with
    | "-t" -> FD_refers_to_terminal
    | "-o" -> Is_shell_option_enabled
    | "-v" -> Is_shell_variable_set
    | "-R" -> Is_shell_variable_a_name_ref
    | "-z" -> Is_empty_string
    | "-n" -> Is_nonempty_string
    | "-a" | "-e " -> File_exists
    | "-b" -> Is_block_special_file
    | "-c" -> Is_character_special_file
    | "-d" -> Is_directory
    | "-f" -> Is_regular_file
    | "-g" -> Has_SGID_bit
    | "-h" | "-L" -> Is_symlink
    | "-k" -> Has_sticky_bit
    | "-p" -> Is_named_pipe
    | "-r" -> Is_readable
    | "-s" -> Is_nonempty_file
    | "-u" -> Has_SUID_bit
    | "-w" -> Is_writable
    | "-x" -> Is_executable
    | "-G" -> Is_owned_by_effective_group_id
    | "-N" -> Was_modified_since_last_read
    | "-O" -> Is_owned_by_effective_user_id
    | "-S" -> Is_socket
    | other -> Other_unary_test_operator
  in
  (op, tok)

let binary_operator (env : env) (tok : ts_tok) : binary_test_operator wrap =
  let s, tok = str env tok in
  let op =
    match s with
    | "-ef" -> Same_physical_file
    | "-nt" -> File_newer_than
    | "-ot" -> File_older_than
    | "=" -> String_equal
    | "==" -> String_pattern_matching
    | "!=" -> String_not_equal
    | "<" -> String_lesser_than
    | ">" -> String_greater_than
    | "-eq" -> Int_equal
    | "-ne" -> Int_not_equal
    | "-lt" -> Int_lesser_than
    | "-le" -> Int_lesser_equal
    | "-gt" -> Int_greater_than
    | "-ge" -> Int_greater_equal
    | other -> Other_binary_test_operator
  in
  (op, tok)

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

let terminator (env : env) (x : CST.terminator) : unary_control_operator wrap =
  match x with
  | `SEMI tok -> (Foreground, token env tok (* ";" *))
  | `SEMISEMI tok -> (Foreground, token env tok (* ";;" *))
  | `LF tok -> (Foreground, token env tok (* "\n" *))
  | `AMP tok -> (Background, token env tok (* "&" *))

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
  | `X__ tok -> (* "_" *) str env tok

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
  let start_ =
    match v1 with
    | `LTLT tok -> token env tok (* "<<" *)
    | `LTLTDASH tok -> (* "<<-" *) token env tok
  in

  let heredoc_start = token env v2 (* heredoc_start *) in
  TODO (start_, heredoc_start)

let simple_expansion (env : env) ((v1, v2) : CST.simple_expansion) =
  let dollar = token env v1 (* "$" *) in
  let variable_name =
    match v2 with
    | `Pat_42e353e tok -> Simple_variable_name (simple_variable_name env tok)
    | `Choice_STAR x -> Special_variable_name (special_variable_name env x)
    | `BANG tok -> Special_variable_name (str env tok (* "!" *))
    | `HASH tok -> Special_variable_name (str env tok (* "#" *))
  in
  let loc = (dollar, snd (variable_name_loc variable_name)) in
  (loc, dollar, variable_name)

let rec prim_exp_or_special_char (env : env)
    (x : CST.anon_choice_prim_exp_618725a) : expression =
  match x with
  | `Choice_semg_ellips x -> primary_expression env x
  | `Spec_char tok -> Special_character (str env tok)

and stmt_with_opt_heredoc (env : env)
    ((v1, v2, v3) : CST.anon_stmt_opt_LF_here_body_term_3efa649) : blist =
  (*
     This handling of heredocs is incorrect but usually works
     in practice. Code like the following is allowed by bash:

       a <<A; b <<B

     ... after which two successive heredoc bodies are expected.
  *)
  let blist = blist_statement env v1 in
  let _opt_heredoc =
    (* needs to be paired with the correct command
     * in one of the pipeline in the list occurring on the previous line. *)
    match v2 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "\n" *) in
        let _v2 = heredoc_body env v2 in
        None
    | None -> None
  in
  let _trailing_newline = terminator env v3 in
  blist

and array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let open_ = token env v1 (* "(" *) in
  let _v2 = List.map (literal env) v2 in
  let close = token env v3 (* ")" *) in
  Expression_TODO (open_, close)

and binary_expression (env : env) (x : CST.binary_expression) : test_expression
    =
  match x with
  | `Exp_choice_EQ_exp (v1, v2, v3) ->
      let left = expression env v1 in
      let op =
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
        | `Test_op tok -> (* test_operator *) token env tok
      in
      let right = expression env v3 in
      T_todo (range (test_expression_loc left) (test_expression_loc right))
  | `Exp_choice_EQEQ_regex (v1, v2, v3) ->
      let left = expression env v1 in
      let _op =
        match v2 with
        | `EQEQ tok -> token env tok (* "==" *)
        | `EQTILDE tok -> (* "=~" *) token env tok
      in

      let right = token env v3 (* regex *) in
      T_todo (fst (test_expression_loc left), right)

and case_item (env : env) ((v1, v2, v3, v4, v5) : CST.case_item) =
  let _v1 = literal env v1 in
  let _v2 () =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* "|" *) in
        let v2 = literal env v2 in
        ())
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
        | `SEMISEMIAMP tok -> token env tok (* ";;&" *))
  in
  ()

and command (env : env) ((v1, v2, v3) : CST.command) : command_with_redirects =
  let assignments, redirects =
    partition_either
      (fun x ->
        match x with
        | `Var_assign x -> Left (variable_assignment env x)
        | `File_redi x -> Right (file_redirect env x))
      v1
  in
  let name = command_name env v2 in
  let args =
    List.map
      (fun x ->
        match x with
        | `Choice_conc x -> literal env x
        | `Choice_EQTILDE_choice_choice_conc (v1, v2) ->
            (* Not sure why we have this here. Should be only within
               test commands [[ ... ]]. *)
            let eq =
              match v1 with
              | `EQTILDE tok -> (* "=~" *) EQTILDE (token env tok)
              | `EQEQ tok -> (* "==" *) EQEQ (token env tok)
            in
            let right =
              match v2 with
              | `Choice_conc x ->
                  let e = literal env x in
                  Literal (expression_loc e, e)
              | `Regex tok -> (* regex *) Regexp (str env tok)
            in
            let loc = range (eq_op_loc eq) (right_eq_operand_loc right) in
            Equality_test (loc, eq, right))
      v3
  in
  let arguments = name :: args in
  let loc =
    let loc1 = list_loc assignment_loc assignments in
    let loc2 = list_loc expression_loc arguments in
    union_loc loc1 loc2
  in
  let command = Simple_command { loc; assignments; arguments } in
  { loc; command; redirects }

and command_name (env : env) (x : CST.command_name) : expression =
  match x with
  | `Conc x ->
      let el = concatenation env x in
      let loc = list_loc expression_loc el in
      Concatenation (loc, el)
  | `Choice_semg_ellips x -> primary_expression env x
  | `Rep1_spec_char xs ->
      let el = List.map (fun tok -> Special_character (str env tok)) xs in
      let loc = list_loc expression_loc el in
      Concatenation (loc, el)

and command_substitution (env : env) (x : CST.command_substitution) :
    blist bracket =
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
      let loc = (open_, close) in
      (open_, Empty loc, close)
  | `BQUOT_stmts_BQUOT (v1, v2, v3) ->
      let open_ = token env v1 (* "`" *) in
      let list = statements env v2 in
      let close = token env v3 (* "`" *) in
      (open_, list, close)

and compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) :
    blist bracket =
  let open_ = token env v1 (* "{" *) in
  let close = token env v3 (* "}" *) in
  let loc = (open_, close) in
  let blist = match v2 with Some x -> statements2 env x | None -> Empty loc in
  (open_, blist, close)

and concatenation (env : env) ((v1, v2, v3) : CST.concatenation) :
    expression list =
  let v1 = prim_exp_or_special_char env v1 in
  let v2 () =
    List.map
      (fun (v1, v2) ->
        let v1 = token env v1 (* concat *) in
        let v2 = prim_exp_or_special_char env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 () =
    match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* concat *) in
        let v2 = token env v2 (* "$" *) in
        todo env (v1, v2)
    | None -> todo env ()
  in
  []

and do_group (env : env) ((v1, v2, v3) : CST.do_group) : blist bracket =
  let do_ = token env v1 (* "do" *) in
  let done_ = token env v3 (* "done" *) in
  let blist =
    match v2 with Some x -> statements2 env x | None -> Empty (do_, done_)
  in
  (do_, blist, done_)

and elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) : elif =
  let elif = token env v1 (* "elif" *) in
  let cond = terminated_statement env v2 |> blist_of_pipeline in
  let then_ = token env v3 (* "then" *) in
  let body =
    match v4 with Some x -> statements2 env x | None -> Empty (then_, then_)
  in
  let loc = (elif, snd (blist_loc body)) in
  (loc, elif, cond, then_, body)

and else_clause (env : env) ((v1, v2) : CST.else_clause) : else_ =
  let else_ = token env v1 (* "else" *) in
  let body =
    match v2 with Some x -> statements2 env x | None -> Empty (else_, else_)
  in
  let loc = (else_, snd (blist_loc body)) in
  (loc, else_, body)

and expansion (env : env) ((v1, v2, v3, v4) : CST.expansion) :
    complex_expansion bracket =
  let open_ = token env v1 (* "${" *) in
  let _v2_TODO =
    match v2 with
    | Some x -> (
        match x with
        | `HASH tok -> Some (token env tok (* "#" *))
        | `BANG tok -> Some (token env tok (* "!" *)))
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
                  | `HASH tok -> (* "#" *) todo env tok)
                v3
            in
            opt_variable)
    | None -> None
  in
  let close = token env v4 (* "}" *) in
  let complex_expansion =
    match opt_variable with
    | Some varname ->
        let loc = variable_name_loc varname in
        Variable (loc, varname)
    | None ->
        let loc = (open_, close) in
        Complex_expansion_TODO loc
  in
  (open_, complex_expansion, close)

(* This covers
   - sh test commands: [ ... ]
   - bash test commands: [[ ... ]]
   - arithmetic expressions: (( ... ))
   But it doesn't fit arithmetic expressions, which are really a different
   language.
*)
and expression (env : env) (x : CST.expression) : test_expression =
  match x with
  | `Choice_conc x ->
      let e = literal env x in
      T_expr (expression_loc e, e)
  | `Un_exp (v1, v2) -> (
      let e = expression env v2 in
      let e_loc = test_expression_loc e in
      match v1 with
      | `BANG tok ->
          let bang = token env tok in
          let loc = (bang, snd e_loc) in
          T_not (loc, bang, e)
      | `Test_op tok -> (
          match e with
          | T_expr (e_loc, e) ->
              let ((_, op_tok) as op) = unary_test_operator env tok in
              let loc = (op_tok, snd e_loc) in
              T_unop (loc, op, e)
          | _ ->
              (* This doesn't make sense since unary operators like '-e'
                 can only be applied to strings. *)
              e))
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      (* This is a construct valid in arithmetic mode only. *)
      let cond = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let branch1 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let branch2 = expression env v5 in
      let start, _ = test_expression_loc cond in
      let _, end_ = test_expression_loc branch2 in
      let loc = (start, end_) in
      T_todo loc
  | `Bin_exp x -> binary_expression env x
  | `Post_exp (v1, v2) ->
      (* This is a construct valid in arithmetic mode only. *)
      let e = expression env v1 in
      let op_tok =
        match v2 with
        | `PLUSPLUS tok -> (* "++" *) token env tok
        | `DASHDASH tok -> (* "--" *) token env tok
      in
      let start, _ = test_expression_loc e in
      let loc = (start, op_tok) in
      T_todo loc
  | `Paren_exp (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let e = expression env v2 in
      let _v3 = token env v3 (* ")" *) in
      e

and file_redirect (env : env) ((v1, v2, v3) : CST.file_redirect) : todo =
  let src_fd =
    match v1 with
    | Some tok -> Some (token env tok (* file_descriptor *))
    | None -> None
  in
  let op_tok =
    match v2 with
    | `LT tok -> token env tok (* "<" *)
    | `GT tok -> token env tok (* ">" *)
    | `GTGT tok -> token env tok (* ">>" *)
    | `AMPGT tok -> token env tok (* "&>" *)
    | `AMPGTGT tok -> token env tok (* "&>>" *)
    | `LTAMP tok -> token env tok (* "<&" *)
    | `GTAMP tok -> token env tok (* ">&" *)
    | `GTBAR tok -> (* ">|" *) token env tok
  in
  (* TODO: dst_fd is missing e.g. the '2' in '>&2' *)
  let file = literal env v3 in
  let start = match src_fd with None -> op_tok | Some tok -> tok in
  let _, end_ = expression_loc file in
  let loc = (start, end_) in
  TODO loc

and heredoc_body (env : env) (x : CST.heredoc_body) : todo =
  match x with
  | `Simple_here_body tok ->
      let tok = token env tok (* simple_heredoc_body *) in
      TODO (tok, tok)
  | `Here_body_begin_rep_choice_expa_here_body_end (v1, v2, v3) ->
      let start = token env v1 (* heredoc_body_beginning *) in
      let _body =
        List.map
          (fun x ->
            match x with
            | `Expa x -> expansion env x |> ignore
            | `Simple_expa x -> simple_expansion env x |> ignore
            | `Cmd_subs x -> command_substitution env x |> ignore
            | `Here_body_middle tok ->
                token env tok (* heredoc_body_middle *) |> ignore)
          v2
      in
      let end_ = token env v3 (* heredoc_body_end *) in
      let loc = (start, end_) in
      TODO loc

and herestring_redirect (env : env) ((v1, v2) : CST.herestring_redirect) =
  let op = token env v1 (* "<<<" *) in
  let e = literal env v2 in
  let loc = (op, snd (expression_loc e)) in
  TODO loc

and last_case_item (env : env) ((v1, v2, v3, v4, v5) : CST.last_case_item) =
  let pat = literal env v1 in
  let pats =
    List.map
      (fun (v1, v2) ->
        let _bar = token env v1 (* "|" *) in
        let pat = literal env v2 in
        pat)
      v2
  in
  let _end_pats = token env v3 (* ")" *) in
  let case_body = program env v4 in
  let end_tok =
    match v5 with
    | Some tok -> token env tok (* ";;" *)
    | None -> snd (blist_loc case_body)
  in
  let loc = (fst (expression_loc pat), end_tok) in
  TODO loc

and literal (env : env) (x : CST.literal) : expression =
  match x with
  | `Conc x ->
      let el = concatenation env x in
      let loc = list_loc expression_loc el in
      Concatenation (loc, el)
  | `Choice_semg_ellips x -> primary_expression env x
  | `Rep1_spec_char xs ->
      let el = List.map (fun tok -> Special_character (str env tok)) xs in
      let loc = list_loc expression_loc el in
      Concatenation (loc, el)

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
  | `Expa x ->
      let e = expansion env x in
      let loc = bracket_loc e in
      String_fragment (loc, Expansion (loc, Complex_expansion e))
  | `Simple_expa x ->
      let loc, dollar, varname = simple_expansion env x in
      String_fragment
        (loc, Expansion (loc, Simple_expansion (loc, dollar, varname)))
  | `Str_expa (v1, v2) ->
      let dollar = token env v1 (* "$" *) in
      let e =
        match v2 with
        | `Str x ->
            (* TODO: do something with the dollar sign *)
            String (string_ env x)
        | `Raw_str tok ->
            (* TODO: how is it different from ANSI C strings?
               e.g. $'hello\nbye' *)
            (* pattern "'[^']*'" *)
            Ansii_c_string (str env tok)
      in
      e
  | `Cmd_subs x ->
      let frag = Command_substitution (command_substitution env x) in
      let loc = string_fragment_loc frag in
      String_fragment (loc, frag)
  | `Proc_subs (v1, v2, v3) ->
      let open_ =
        match v1 with
        | `LTLPAR tok -> token env tok (* "<(" *)
        | `GTLPAR tok -> (* ">(" *) token env tok
      in
      let _body = statements env v2 in
      let close = token env v3 (* ")" *) in
      let loc = (open_, close) in
      Expression_TODO loc

and program (env : env) (opt : CST.program) : blist =
  match opt with
  | Some x -> statements env x
  | None ->
      (* TODO: use an empty token at the current position in the source
         file rather than a completely fake token. *)
      Empty fake_loc

(*
   Read a statement as a list (of pipelines).
*)
and blist_statement (env : env) (x : CST.statement) : blist =
  match statement env x with
  | Tmp_blist x -> x
  | Tmp_pipeline x -> Pipelines (pipeline_loc x, [ x ])
  | Tmp_command (cmd_redir, control_op) -> (
      let loc = command_with_redirects_loc cmd_redir in
      let cmd = Command (loc, cmd_redir) in
      match control_op with
      | None -> Pipelines (loc, [ cmd ])
      | Some op ->
          let _, end_ = wrap_loc op in
          let loc2 = (fst loc, end_) in
          Pipelines (loc2, [ Control_operator (loc2, cmd, op) ]))

(*
   Read a statement as a pipeline where a pipeline or a single command
   is expected.
*)
and pipeline_statement (env : env) (x : CST.statement) : pipeline =
  match statement env x with
  | Tmp_blist _ -> assert false
  | Tmp_pipeline x -> x
  | Tmp_command (cmd_redir, control_op) -> (
      let loc = command_with_redirects_loc cmd_redir in
      let cmd = Command (loc, cmd_redir) in
      match control_op with
      | None -> cmd
      | Some op ->
          let _, end_ = wrap_loc op in
          let loc2 = (fst loc, end_) in
          Control_operator (loc, cmd, op))

(*
   Read a statement as a single command where a single command is
   expected.
*)
and command_statement (env : env) (x : CST.statement) :
    command_with_redirects * unary_control_operator wrap option =
  match statement env x with
  | Tmp_blist _ -> assert false
  | Tmp_pipeline _ -> assert false
  | Tmp_command x -> x

(*
   Do not call this function directly. Instead use one of the
   following depending on which construct is expected:
   - blist_statement
   - pipeline_statement
   - command_statement
*)
and statement (env : env) (x : CST.statement) : tmp_stmt =
  match x with
  | `Redi_stmt (v1, v2) ->
      (* TODO: report or fix bug in original grammar: redirects can
               occur in-between arguments, not just at the end.
         test case:
         echo a > /tmp/foo b
      *)
      let cmd_redir_ctrl = command_statement env v1 in
      let _redirects_TODO () =
        List.map
          (fun x ->
            match x with
            | `File_redi x -> file_redirect env x
            | `Here_redi_a9657de x -> heredoc_redirect env x
            | `Here_redi_7d3292d x -> herestring_redirect env x)
          v2
      in
      Tmp_command cmd_redir_ctrl
  | `Var_assign x ->
      let a = variable_assignment env x in
      let loc = assignment_loc a in
      let command = Assignment (loc, a) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Cmd x -> Tmp_command (command env x, None)
  | `Decl_cmd (v1, _v2) ->
      let start =
        match v1 with
        | `Decl tok -> token env tok (* "declare" *)
        | `Type tok -> token env tok (* "typeset" *)
        | `Export tok -> token env tok (* "export" *)
        | `Read tok -> token env tok (* "readonly" *)
        | `Local tok -> (* "local" *) token env tok
      in
      (*
      let _v2 () =
        List.map
          (fun x ->
            match x with
            | `Choice_conc x -> literal env x
            | `Pat_42e353e tok -> simple_variable_name env tok
            | `Var_assign x -> variable_assignment env x)
          v2
      in
*)
      let loc = (start, start) (* TODO *) in
      let command = Declaration (loc, TODO loc) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Unset_cmd (v1, _v2) ->
      let start =
        match v1 with
        | `Unset tok -> token env tok (* "unset" *)
        | `Unse tok -> (* "unsetenv" *) token env tok
      in
      (*
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Choice_conc x -> literal env x
            | `Pat_42e353e tok -> simple_variable_name env tok)
          v2
      in
*)
      let loc = (start, start) (* TODO *) in
      let command = Declaration (loc, TODO loc) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Test_cmd x ->
      let command = test_command env x in
      let loc = command_loc command in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Nega_cmd (v1, v2) ->
      let excl = token env v1 (* "!" *) in
      let loc, command, redirects =
        match v2 with
        | `Cmd x ->
            let { loc; command; redirects } = command env x in
            (loc, command, redirects)
        | `Test_cmd x ->
            let command = test_command env x in
            let loc = command_loc command in
            (loc, command, [])
        | `Subs x ->
            let sub = subshell env x in
            let loc = bracket_loc sub in
            (loc, Subshell (loc, sub), [])
      in
      let loc = extend_left excl loc in
      let command = Negated_command (loc, excl, command) in
      Tmp_command ({ loc; command; redirects }, None)
  | `For_stmt (v1, v2, v3, v4, v5) ->
      let for_ = token env v1 (* "for" *) in
      let _loop_var = token env v2 (* pattern \w+ *) in
      let _loop_vals =
        (* TODO *)
        match v3 with
        | Some (v1, v2) ->
            let _in = token env v1 (* "in" *) in
            let _values = List.map (literal env) v2 in
            ()
        | None ->
            (* iterate over $1, $2, ..., $# *)
            ()
      in
      let _semi = terminator env v4 in
      let do_, body, done_ = do_group env v5 in
      let loc = (for_, done_) in
      let command = For_loop (loc, body) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `C_style_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let for_ = token env v1 (* "for" *) in
      let header_open_ = token env v2 (* "((" *) in
      let _x () =
        match v3 with Some x -> expression env x | None -> todo env ()
      in
      let _x () = terminator env v4 in
      let _x () =
        match v5 with Some x -> expression env x | None -> todo env ()
      in
      let _x () = terminator env v6 in
      let _x () =
        match v7 with Some x -> expression env x | None -> todo env ()
      in
      let header_close = token env v8 (* "))" *) in
      let _x () =
        match v9 with
        | Some tok -> token env tok (* ";" *)
        | None -> todo env ()
      in
      let body_open, body, body_close =
        match v10 with
        | `Do_group x -> do_group env x
        | `Comp_stmt x -> compound_statement env x
      in
      let loc = (for_, body_close) in
      let command = For_loop_c_style (loc, body) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `While_stmt (v1, v2, v3) ->
      let while_ = token env v1 (* "while" *) in
      let _header () = terminated_statement env v2 in
      let open_body, body, close_body = do_group env v3 in
      let loc = (while_, close_body) in
      let command = While_loop (loc, body) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let if_ = token env v1 (* "if" *) in
      let cond = terminated_statement env v2 |> blist_of_pipeline in
      let then_ = token env v3 (* "then" *) in
      let body =
        match v4 with
        | Some x -> statements2 env x
        | None -> Empty (then_, then_)
      in
      let elif_branches = List.map (elif_clause env) v5 in
      let else_branch =
        match v6 with Some x -> Some (else_clause env x) | None -> None
      in
      let fi = token env v7 (* "fi" *) in
      let loc = (if_, fi) in
      let command =
        If (loc, if_, cond, then_, body, elif_branches, else_branch, fi)
      in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Case_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let case = token env v1 (* "case" *) in
      let subject = literal env v2 in
      let _term =
        match v3 with Some x -> Some (terminator env x) | None -> None
      in
      let in_ = token env v4 (* "in" *) in
      let _term = terminator env v5 in
      let cases =
        match v6 with
        | Some (v1, v2) ->
            (* TODO *)
            let cases = List.map (case_item env) v1 in
            let last_case = last_case_item env v2 in
            []
        | None -> []
      in
      let esac = token env v7 (* "esac" *) in
      let loc = (case, esac) in
      let command = Case (loc, TODO loc) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Pipe (v1, v2, v3) ->
      (*
         Pipes are parsed with left associativity, so the statement on the
         left is the pipeline we're extending with the one extra
         command on the right.
      *)
      let left_pipeline = pipeline_statement env v1 in
      let bar =
        match v2 with
        | `BAR tok -> (Bar, token env tok (* "|" *))
        | `BARAMP tok -> (Bar_ampersand, token env tok (* "|&" *))
      in
      let extra_cmd, control_op = command_statement env v3 in
      let start, _ = pipeline_loc left_pipeline in
      let _, end_ = extra_cmd.loc in
      let loc = (start, end_) in
      let pipeline = Pipeline (loc, left_pipeline, bar, extra_cmd) in
      let pipeline =
        match control_op with
        | None -> pipeline
        | Some ((_, tok) as op) ->
            let loc = extend_right loc tok in
            Control_operator (loc, pipeline, op)
      in
      Tmp_pipeline pipeline
  | `List (v1, v2, v3) ->
      let left = blist_statement env v1 in

      (*
         && and || are left-associative, so we should have a pipeline
         on the right, not a list. The following doesn't work, though,
         when parsing just 'a || b'.

      let pipeline = pipeline_statement env v3 in
       *)
      let right = blist_statement env v3 in
      let loc = range (blist_loc left) (blist_loc right) in
      let blist =
        match v2 with
        | `AMPAMP tok -> And (loc, left, token env tok, right)
        | `BARBAR tok -> Or (loc, left, token env tok, right)
      in
      Tmp_blist blist
  | `Subs x ->
      let sub = subshell env x in
      let loc = bracket_loc sub in
      let command = Subshell (loc, sub) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Comp_stmt x ->
      let res = compound_statement env x in
      let loc = bracket_loc res in
      let command = Command_group (loc, res) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Func_defi (v1, v2) ->
      let start, function_, name =
        match v1 with
        | `Func_word_opt_LPAR_RPAR (v1, v2, v3) ->
            let function_ = token env v1 (* "function" *) in
            let name = str env v2 (* word *) in
            let _empty_parens =
              match v3 with
              | Some (v1, v2) ->
                  let _v1 = token env v1 (* "(" *) in
                  let _v2 = token env v2 (* ")" *) in
                  ()
              | None -> ()
            in
            (function_, Some function_, name)
        | `Word_LPAR_RPAR (v1, v2, v3) ->
            let name = str env v1 (* word *) in
            let _v2 = token env v2 (* "(" *) in
            let _v3 = token env v3 (* ")" *) in
            (snd name, None, name)
      in
      let body =
        match v2 with
        | `Comp_stmt x ->
            let br = compound_statement env x in
            Command_group (bracket_loc br, br)
        | `Subs x ->
            let br = subshell env x in
            Subshell (bracket_loc br, br)
        | `Test_cmd x -> test_command env x
      in
      let loc = extend_left start (command_loc body) in
      let command = Function_definition (loc, { loc; function_; name; body }) in
      Tmp_command ({ loc; command; redirects = [] }, None)

and statements (env : env) ((v1, v2, v3, v4) : CST.statements) : blist =
  let blist = List.map (stmt_with_opt_heredoc env) v1 |> concat_blists in
  (* See stmt_with_opt_heredoc, which is almost identical except for
     the optional trailing newline. *)
  let last_blist = blist_statement env v2 in
  let _last_heredoc =
    (* TODO *)
    match v3 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "\n" *) in
        let _v2 = heredoc_body env v2 in
        ()
    | None -> ()
  in
  let _trailing_newline =
    match v4 with Some x -> Some (terminator env x) | None -> None
  in
  let loc = range (blist_loc blist) (blist_loc last_blist) in
  Seq (loc, blist, last_blist)

and statements2 (env : env) (xs : CST.statements2) : blist =
  List.map (stmt_with_opt_heredoc env) xs |> concat_blists

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
          | `Expa x ->
              let e = expansion env x in
              let loc = bracket_loc e in
              [ Expansion (loc, Complex_expansion e) ]
          | `Simple_expa x ->
              let loc, dollar, e = simple_expansion env x in
              [ Expansion (loc, Simple_expansion (loc, dollar, e)) ]
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
  (open_, fragments @ fragment, close)

and subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) :
    string wrap * tok * expression * tok =
  let var = str env v1 (* variable_name *) in
  let open_ = token env v2 (* "[" *) in
  let index = literal env v3 in
  let _concat = match v4 with Some _tok -> () (* concat *) | None -> () in
  let close = token env v5 (* "]" *) in
  let _concat = match v6 with Some _tok -> () (* concat *) | None -> () in
  (var, open_, index, close)

and subshell (env : env) ((v1, v2, v3) : CST.subshell) : blist bracket =
  let open_ = token env v1 (* "(" *) in
  let blist = statements env v2 in
  let close = token env v3 (* ")" *) in
  (open_, blist, close)

and terminated_statement (env : env) ((v1, v2) : CST.terminated_statement) :
    pipeline =
  let pip = pipeline_statement env v1 in
  let op = terminator env v2 in
  Control_operator (pipeline_loc pip, pip, op)

and test_command (env : env) (v1 : CST.test_command) : command =
  match v1 with
  | `LBRACK_exp_RBRACK (v1, v2, v3) ->
      let open_ = token env v1 (* "[" *) in
      let e = expression env v2 in
      let close = token env v3 (* "]" *) in
      let loc = (open_, close) in
      Sh_test (loc, (open_, e, close))
  | `LBRACKLBRACK_exp_RBRACKRBRACK (v1, v2, v3) ->
      let open_ = token env v1 (* "[[" *) in
      let e = expression env v2 in
      let close = token env v3 (* "]]" *) in
      let loc = (open_, close) in
      Bash_test (loc, (open_, e, close))
  | `LPARLPAR_exp_RPARRPAR (v1, v2, v3) ->
      let open_ = token env v1 (* "((" *) in
      let e = expression env v2 in
      let close = token env v3 (* "))" *) in
      let loc = (open_, close) in
      Arithmetic_expression (loc, (open_, TODO loc, close))

and variable_assignment (env : env) ((v1, v2, v3) : CST.variable_assignment) :
    assignment =
  let lhs =
    match v1 with
    | `Var_name tok -> str env tok (* variable_name *)
    | `Subs x ->
        (* TODO: assignment to array cell *)
        let var, _open, _index, _close = subscript env x in
        var
  in
  let assign_op =
    match v2 with
    | `EQ tok -> (Set, token env tok (* "=" *))
    | `PLUSEQ tok -> (Add, token env tok (* "+=" *))
  in
  let rhs =
    match v3 with
    | `Choice_conc x -> literal env x
    | `Array x ->
        let array_literal = array_ env x in
        let loc = expression_loc array_literal in
        Expression_TODO loc
    | `Empty_value tok ->
        let empty = token env tok in
        let loc = (empty, empty) in
        Empty_expression loc
  in
  let loc = (snd lhs, snd (expression_loc rhs)) in
  { loc; lhs; assign_op; rhs }

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
      let bash_ast = program env cst in
      Bash_to_generic.program bash_ast)

let parse_pattern str =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_bash.Parse.string str ())
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      let bash_ast = program env cst in
      Bash_to_generic.any bash_ast)
