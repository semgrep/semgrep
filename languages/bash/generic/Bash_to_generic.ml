(*
   Convert Bash-specific AST to generic AST.

   A Bash program is organized into the following hierarchy:

   - List: a list of pipelines. A whole program is a list (type blist).

       echo hello | tr h y; echo bye

   - Pipeline: a list of commands whose standard output and input are
     connected

       echo hello | tr h y

   - Command: everything else including calls to external programs and
     control structures that the language has to offer.

     Simple command:

       echo hello

     For loop, a compound command:

       for i in 1 2 3; do echo "$i"; done

     Variable assignment:

       answer=42

     + many others

   - Literals: space-separated elements within a simple command, which will
     be expanded into a possibly different number of elements.

       hello
       "$x"
       $items
       "$(cat foo/bar)"
       $?

       etc.

   Mapping to the generic AST:

   We use statements wherever we need to represent a control flow.
   This means all declarations and assignments must be statements.
   Members of a pipeline on the other hand execute independently and
   can be expressions.

   pipeline -> expression with '|' operator between commands
   command -> expression or pure statement
   simple command -> expression: Constructor ("sh_cmd", args)
   variable assignment -> statement: variable assignment
   function definition -> statement: function definition
   for loop -> statement: for loop

   Note that a bash pipeline may contain definitions. This is legal but
   useless and it has no effect:

     a=42 | echo hello
     echo "$a"  # variable 'a' is undefined!

   It makes more sense when a command group is used within a pipeline:

     { a=42; echo "$a"; } | cat  # prints '42'

   So, some shell commands are represented preferentially as statements
   (definitions, command groups, loops, if-then-else) and others
   are represented preferentially as expressions (simple commands, pipelines),
   and we wrap expressions in statements and vice-versa as required by
   the context.

   Shell variables and semgrep metavariables:

   - In a pattern, '$XXX' is interpreted as a semgrep metavariable,
     at matching time, represented as '$XXX'.
   - In a program, '$XXX' is interpreted as the dereferencing of a
     shell variable, at matching time, represented as 'XXX'.
   - '${XXX}' is always interpreted as the dereferencing of a shell variable
     and should be used in semgrep patterns to disambiguate from the
     metavariable '$XXX'.
*)

open! Common
open AST_bash
module G = AST_generic

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fb = Tok.unsafe_fake_bracket

(*
   Convert a pair (loc, x) to a bracket, which uses a leading and trailing
   token to indicate the location.
*)
let bracket (loc : loc) x : 'a bracket =
  let start, end_ = loc in
  (start, x, end_)

(* We apply a different mapping whether we're parsing a pattern or target
   program. *)
type env = AST_bash.input_kind

(* Temporary representation.
   Avoids superfluous early wrapping of expressions in statements and
   vice-versa. *)
type stmt_or_expr = Stmt of loc * G.stmt | Expr of loc * G.expr

let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))
let expr_of_stmt (st : G.stmt) : G.expr = G.stmt_to_expr st

let as_stmt : stmt_or_expr -> G.stmt = function
  | Stmt (_loc, st) -> st
  | Expr (loc, e) -> stmt_of_expr loc e

let as_expr : stmt_or_expr -> G.expr = function
  | Stmt (_loc, st) -> expr_of_stmt st
  | Expr (_loc, e) -> e

let stmt_or_expr_loc = function
  | Stmt (loc, _)
  | Expr (loc, _) ->
      loc

let block : stmt_or_expr list -> stmt_or_expr = function
  | [ x ] -> x
  | several ->
      let loc = Tok_range.of_list stmt_or_expr_loc several in
      let stmts = List_.map as_stmt several in
      Stmt (loc, G.s (G.Block (bracket loc stmts)))

let mk_name (str_wrap : string wrap) : G.name =
  let id_info = G.empty_id_info () in
  G.Id (str_wrap, id_info)

let get_var_name (var : variable_name) : string wrap =
  match var with
  | Simple_variable_name name
  | Special_variable_name name
  | Var_semgrep_metavar name ->
      name

let mk_var_expr (var : variable_name) : G.expr =
  let name = get_var_name var in
  G.N (mk_name name) |> G.e

module C = struct
  let mk (loc : loc) (name : string) =
    let id = "!sh_" ^ name ^ "!" in
    let id_info = G.empty_id_info ~hidden:true () in
    G.N (G.Id ((id, fst loc), id_info)) |> G.e

  (* For simple commands, e.g.
       $echo "$@"
  *)
  let cmd loc = mk loc "cmd"

  (* Split a string according to whitespace defined by "$IFS".
     This is done on the result of unquoted $ expansions:
       $args
       $(cat foo.txt)
       `cat foo.txt`
       ${args}
       ${args%.c}
  *)
  let _split loc = mk loc "split"

  (*
     Expand a variable X referenced as $X or ${X} in a program.
     In a pattern, $X is a metavariable that stands for anything
     (no expansion necessary), whereas ${$X} in a pattern is the expansion
     of any variable represented by the metavariable $X.
  *)
  let expand loc = mk loc "expand"

  (* Concatenate two string fragments e.g.
       foo"$bar"
  *)
  let concat loc = mk loc "concat"

  (* Concatenate two string fragments within double-quotes e.g.
       "foo $bar"
  *)
  let quoted_concat loc = mk loc "quoted_concat"

  (* Command substitution: $(...) *)
  let cmd_subst loc = mk loc "cmd_subst"

  (* Process substitution: <(...) *)
  let proc_subst loc = mk loc "proc_subst"

  (* Subshell: (...) *)
  let subshell loc = mk loc "subshell"
end

(*
   Constructors used to represent bash expressions that don't have
   an equivalent in the generic AST.

   Usage: call C.cmd args
*)
let call loc name exprs =
  G.Call (name loc, bracket loc (List_.map (fun e -> G.Arg e) exprs)) |> G.e

let todo_tokens ((start, end_) : loc) =
  let wrap tok = (Tok.content_of_tok tok, tok) in
  if start =*= end_ then [ G.TodoK (wrap start) ]
  else [ G.TodoK (wrap start); G.TodoK (wrap end_) ]

let _todo_stmt (loc : loc) : G.stmt =
  G.s (G.OtherStmt (G.OS_Todo, todo_tokens loc))

let todo_expr (loc : loc) : G.expr =
  let t = fst loc in
  G.e (G.OtherExpr (("BashTodo", t), todo_tokens loc))

let todo_expr2 (loc : loc) : stmt_or_expr = Expr (loc, todo_expr loc)

(*****************************************************************************)
(* Converter from bash AST to generic AST *)
(*****************************************************************************)

let negate_expr (neg_tok : tok) (_cmd_loc : loc) (cmd : G.expr) : G.expr =
  G.opcall (G.Not, neg_tok) [ cmd ]

(*
   Redirect stderr in the last command of the pipeline.
*)
let redirect_pipeline_stderr_to_stdout pip =
  (* TODO: don't ignore redirects *)
  pip

let rec blist (env : env) (l : blist) : stmt_or_expr list =
  match l with
  | Seq (_loc, left, right) -> blist env left @ blist env right
  | Pipelines (_loc, pl) -> List_.map (fun x -> pipeline env x) pl
  | Empty _loc -> []

and pipeline (env : env) (x : pipeline) : stmt_or_expr =
  match x with
  | Command cmd_r -> cmd_redir env cmd_r
  | Pipeline (loc, pip, pipe_op, cmd_r) ->
      let pip, bar_tok =
        match pipe_op with
        | Bar, tok -> (pip, tok)
        | Bar_ampersand, tok ->
            (* Transpile:

                 a |& b

               -->

                 a 2>&1 | b
            *)
            (redirect_pipeline_stderr_to_stdout pip, tok)
      in
      let left = pipeline env pip |> as_expr in
      let right = cmd_redir env cmd_r |> as_expr in
      Expr (loc, G.opcall (G.Pipe, bar_tok) [ left; right ])
  | Control_operator (loc, pip, control_op) -> (
      match control_op with
      | Foreground _, _tok -> pipeline env pip
      | Background, amp_tok ->
          let arg = pipeline env pip |> as_expr in
          Expr (loc, G.opcall (G.Background, amp_tok) [ arg ]))

and cmd_redir (env : env) (x : cmd_redir) : stmt_or_expr =
  (* TODO: don't ignore redirects *)
  let { loc = _; command = cmd; redirects } = x in
  ignore redirects;
  command env cmd

(*
  The preference between stmt or expr depends on whether the most fitting
  construct of the generic AST is an stmt or an expr. For the todos, it
  may be little arbitrary.
*)
and command (env : env) (cmd : command) : stmt_or_expr =
  match cmd with
  | Simple_command { loc = _; assignments; arguments = [] | [ Word ("", _) ] }
    ->
      (* TODO: fix the tree-sitter grammar so it doesn't insert a "MISSING"
         node set to the empty string when there's no command following
         multiple assignments. *)
      List_.map (assignment env) assignments |> block
  | Simple_command { loc; assignments = _; arguments } -> (
      match arguments with
      | [ (Expr_semgrep_ellipsis tok as e) ] ->
          Expr ((tok, tok), expression env e)
      | [ (String_fragment (loc, Frag_semgrep_named_ellipsis _) as e) ] ->
          Expr (loc, expression env e)
      | arguments ->
          let args = List_.map (expression env) arguments in
          Expr (loc, call loc C.cmd args))
  | And (loc, left, and_tok, right) ->
      let left = pipeline env left |> as_expr in
      let right = pipeline env right |> as_expr in
      Expr (loc, G.opcall (G.And, and_tok) [ left; right ])
  | Or (loc, left, or_tok, right) ->
      let left = pipeline env left |> as_expr in
      let right = pipeline env right |> as_expr in
      Expr (loc, G.opcall (G.Or, or_tok) [ left; right ])
  | Subshell (loc, (_open_, bl, _close)) ->
      let args = [ stmt_group env loc (blist env bl) |> as_expr ] in
      Expr (loc, call loc C.subshell args)
  | Command_group (loc, (_open_, bl, _close)) ->
      stmt_group env loc (blist env bl)
  | Sh_test (loc, _) -> todo_expr2 loc
  | Bash_test (loc, _) -> todo_expr2 loc
  | Arithmetic_expression (loc, _) -> todo_expr2 loc
  | For_loop (loc, for_, loop_var, opt_in, do_, body, _done_) ->
      let header =
        let in_tk, expr =
          match opt_in with
          | Some (in_tk, vals) ->
              ( in_tk,
                G.Container
                  (List, fb (List_.map (fun x -> expression env x) vals))
                |> G.e )
          | None ->
              (*
                 Pretend there's a '"$@"', which is semantically correct
                 and avoids exception in IL_to_AST raised when the list
                 is empty.
              *)
              let fake_arg_array =
                let tok = do_ in
                let loc = (tok, tok) in
                let var_name = Special_variable_name ("@", tok) in
                let frag = Expansion (loc, Simple_expansion (loc, var_name)) in
                String (tok, [ frag ], tok)
              in
              (G.fake "", expression env fake_arg_array)
        in
        match loop_var with
        | Simple_variable_name var
        | Special_variable_name var
        | Var_semgrep_metavar var ->
            let entity = G.basic_entity var in
            let pat =
              match entity.name with
              | EN (Id (id, idinfo)) -> G.PatId (id, idinfo) |> G.p
              | EPattern pat -> pat
              | _ -> G.OtherPat (("PatEnt", G.fake "PatEnt"), [ G.En entity ])
            in
            G.ForEach (pat, in_tk, expr)
      in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.For (for_, header, body) |> G.s)
  | For_loop_c_style (loc, bl) ->
      (* TODO: loop *) stmt_group env loc (blist env bl)
  | Select (loc, _select, _loop_var, _opt_in, _do_, body, _done_) ->
      (* TODO *)
      stmt_group env loc (blist env body)
  | Case (loc, case, subject, _in_, clauses, _esac) ->
      let subject = expression env subject in
      let clauses =
        List_.map
          (fun (_loc, patterns, _paren, stmts, _opt_term) ->
            (* TODO: handle the different kinds of terminators. Insert breaks. *)
            let patterns =
              List_.map
                (fun e ->
                  let tok, _ = AST_bash_loc.expression_loc e in
                  let pat =
                    (* TODO: convert bash expression to generic pattern *)
                    G.OtherPat (("", tok), [ G.E (expression env e) ])
                  in
                  G.Case (tok, pat))
                patterns
            in
            G.CasesAndBody (patterns, blist env stmts |> block |> as_stmt))
          clauses
      in
      Stmt (loc, G.Switch (case, Some (Cond subject), clauses) |> G.s)
  | If (loc, if_, cond, then_, body, elifs, else_, _fi) ->
      let ifs = (loc, if_, cond, then_, body) :: elifs in
      let else_stmt =
        match else_ with
        | None -> None
        | Some (_loc, _else_tok, body) ->
            Some (blist env body |> block |> as_stmt)
      in
      let opt_stmt =
        List_.fold_right
          (fun if_ (else_stmt : G.stmt option) ->
            let _loc1, if_, cond, _then_, body = if_ in
            (* pad: TODO:  use more complex CondDecl when ready? *)
            let cond_expr = blist env cond |> block |> as_expr in
            let body_stmt = blist env body |> block |> as_stmt in
            Some (G.s (G.If (if_, G.Cond cond_expr, body_stmt, else_stmt))))
          ifs else_stmt
      in
      let stmt =
        match opt_stmt with
        | None -> (* there's at least one 'if ... then ...' *) assert false
        | Some stmt -> stmt
      in
      Stmt (loc, stmt)
  | While_loop (loc, while_, cond, _do_, body, _done_) ->
      let cond = stmt_group env loc (blist env cond) |> as_expr in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.While (while_, G.Cond cond, body) |> G.s)
  | Until_loop (loc, until, cond, _do_, body, _done_) ->
      let cond_loc = AST_bash_loc.blist_loc cond in
      let neg_cond =
        blist env cond |> stmt_group env loc |> as_expr
        |> negate_expr until cond_loc
      in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.While (until, G.Cond neg_cond, body) |> G.s)
  | Coprocess (_loc, _opt_name, cmd) -> (* TODO: coproc *) command env cmd
  | Assignment ass -> assignment env ass
  | Declaration x ->
      let assignments = List_.map (assignment env) x.assignments in
      (* TODO: don't ignore the "unknown" arguments that contain variables
         and such. *)
      assignments |> block
  | Negated_command (loc, excl_tok, cmd) ->
      let cmd_loc = AST_bash_loc.command_loc cmd in
      let cmd = command env cmd |> as_expr in
      let e = negate_expr excl_tok cmd_loc cmd in
      Expr (loc, e)
  | Function_definition (loc, def) ->
      let first_tok =
        match def.function_ with
        | Some function_tok -> function_tok
        | None -> fst (AST_bash_loc.variable_name_loc def.name)
      in
      let def_kind =
        G.FuncDef
          {
            G.fkind = (G.Function, first_tok);
            fparams = fb [];
            frettype = None;
            fbody = G.FBStmt (command env def.body |> as_stmt);
          }
      in
      (* Function names are in another namespace than ordinary variables.
         They can't be accessed with the '$' notation. No need to prepend
         a '$' to the name like for variables. *)
      let def = (G.basic_entity (get_var_name def.name), def_kind) in
      Stmt (loc, G.DefStmt def |> G.s)

and assignment (env : env) ass =
  let var = G.N (mk_name ass.lhs) |> G.e in
  let value = expression env ass.rhs in
  let e =
    match ass.assign_op with
    | Set, tok -> G.Assign (var, tok, value)
    | Add, tok -> G.AssignOp (var, (G.Plus, tok), value)
  in
  Expr (ass.loc, G.e e)

(* This returns a Block on purpose e.g. for the body of a 'for' loop, which
   is needed for matching. We can't simplify it into a single expression
   if there's only one statement. *)
and stmt_group (_env : env) (loc : loc) (l : stmt_or_expr list) : stmt_or_expr =
  let stmts = List_.map as_stmt l in
  let start, end_ = loc in
  Stmt (loc, G.s (G.Block (start, stmts, end_)))

and expression (env : env) (e : expression) : G.expr =
  match e with
  | Word ("...", tok) when env =*= Pattern ->
      (* occurs in unquoted concatenations e.g. ...$x or $x... *)
      G.Ellipsis tok |> G.e
  | Word str -> G.L (G.String (fb str)) |> G.e
  | String (* "foo" *) (open_, frags, close) -> (
      match frags with
      | [ String_content ((str, _) as wrap) ]
        when not (env =*= Pattern && str = "...") ->
          (* normalization to enable matching of e.g. "foo" against foo *)
          G.L (G.String (fb wrap)) |> G.e
      | _ ->
          let loc = (open_, close) in
          List_.map (string_fragment env) frags |> call loc C.quoted_concat)
  | String_fragment (_loc, frag) -> string_fragment env frag
  | Raw_string (* 'foo' *) (str, tok) ->
      (* normalization to enable matching of e.g. 'foo' against foo *)
      let without_quotes =
        let len = String.length str in
        if len >= 2 && str.[0] =$= '\'' && str.[len - 1] =$= '\'' then
          String.sub str 1 (len - 2)
        else (* it's a bug but let's not fail *)
          str
      in
      G.L (G.String (fb (without_quotes, tok))) |> G.e
  | Ansii_c_string str -> G.L (G.String (fb str)) |> G.e
  | Special_character str -> G.L (G.String (fb str)) |> G.e
  | Concatenation (loc, el) ->
      List_.map (expression env) el |> call loc C.concat
  | Expr_semgrep_ellipsis tok -> G.Ellipsis tok |> G.e
  | Expr_semgrep_deep_ellipsis (_loc, (open_, e, close)) ->
      G.DeepEllipsis (open_, expression env e, close) |> G.e
  | Expr_semgrep_metavar mv -> G.N (mk_name mv) |> G.e
  | Equality_test (loc, _, _) -> (* don't know what this is *) todo_expr loc
  | Empty_expression loc -> G.L (G.String (fb ("", fst loc))) |> G.e
  | Array (_loc, (open_, elts, close)) ->
      let elts = List_.map (expression env) elts in
      G.Container (G.Array, (open_, elts, close)) |> G.e
  | Process_substitution (loc, (_open_, x, _close)) ->
      let arg = blist env x |> block |> as_expr in
      call loc C.proc_subst [ arg ]

and string_fragment (env : env) (frag : string_fragment) : G.expr =
  match frag with
  | String_content ("...", tok) when env =*= Pattern ->
      (* convert the '...' in '"${foo}...${bar}"' into an ellipsis *)
      G.Ellipsis tok |> G.e
  | String_content ((_, _tok) as wrap) -> G.e (G.L (G.String (fb wrap)))
  | Expansion (_loc, ex) -> expansion env ex
  | Command_substitution (open_, x, close) ->
      let loc = (open_, close) in
      let arg = blist env x |> block |> as_expr in
      call loc C.cmd_subst [ arg ]
  | Frag_semgrep_metavar mv -> G.N (mk_name mv) |> G.e
  | Frag_semgrep_named_ellipsis x -> G.N (mk_name x) |> G.e

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (_env : env) (x : expansion) : G.expr =
  match x with
  | Simple_expansion (loc, var_name) -> expand loc (mk_var_expr var_name)
  | Complex_expansion (_open_, x, _close) -> (
      match x with
      | Variable (loc, var) -> expand loc (mk_var_expr var)
      | Complex_expansion_TODO loc -> todo_expr loc)

and expand loc (var_expr : G.expr) : G.expr = call loc C.expand [ var_expr ]

let program_with_env (env : env) x = blist (env : env) x |> List_.map as_stmt

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*
   Unwrap the pattern tree as much as possible to maximize matches.

   For example, 'echo' is parsed as a list of statements but occurs
   in the target program as a single stmt ('If' branch) or as an expr
   ('If' condition). Unwrapping into an expr allows the expr to match those
   cases.
*)
let any (x : blist) =
  let env = Pattern in
  match AST_bash_builder.blist_as_expression x with
  | Some e -> G.E (expression env e)
  (* TODO: simply | None -> G.Ss (program env x) but got regressions
   * problem with Parse_pattern.normalize_any probably
   *)
  | None -> (
      match program_with_env env x with
      | [ { G.s = G.ExprStmt (e, _semicolon); _ } ] -> G.E e
      | [ stmt ] -> G.S stmt
      | stmts -> G.Ss stmts)

let program x =
  let env = Program in
  program_with_env env x
