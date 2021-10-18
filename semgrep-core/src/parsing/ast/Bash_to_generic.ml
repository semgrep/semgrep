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

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

open! Common
open AST_bash
module G = AST_generic

(*module H = AST_generic_helpers*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* We apply a different mapping whether we're parsing a pattern or target
   program. *)
type env = AST_bash.input_kind

let is_pattern = function
  | Pattern -> true
  | Program -> false

let is_program = function
  | Program -> true
  | Pattern -> false

(* Temporary representation.
   Avoids superfluous early wrapping of expressions in statements and
   vice-versa. *)
type stmt_or_expr = Stmt of loc * G.stmt | Expr of loc * G.expr

let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))

let expr_of_stmt loc (st : G.stmt) : G.expr = G.stmt_to_expr st

let as_stmt : stmt_or_expr -> G.stmt = function
  | Stmt (loc, st) -> st
  | Expr (loc, e) -> stmt_of_expr loc e

let as_expr : stmt_or_expr -> G.expr = function
  | Stmt (loc, st) -> expr_of_stmt loc st
  | Expr (loc, e) -> e

let stmt_or_expr_loc = function
  | Stmt (loc, _)
  | Expr (loc, _) ->
      loc

let block : stmt_or_expr list -> stmt_or_expr = function
  | [ x ] -> x
  | several ->
      let loc = list_loc stmt_or_expr_loc several in
      let stmts = List.map as_stmt several in
      Stmt (loc, G.s (G.Block (bracket loc stmts)))

let mk_name (str_wrap : string wrap) : G.name =
  let id_info = G.empty_id_info () in
  G.Id (str_wrap, id_info)

let get_var_name (var : variable_name) : string wrap =
  match var with
  | Simple_variable_name name
  | Special_variable_name name
  | Var_metavar name ->
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
  let split loc = mk loc "split"

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
end

(*
   Constructors used to represent bash expressions that don't have
   an equivalent in the generic AST.

   Usage: call C.cmd args
*)
let call loc name exprs =
  G.Call (name loc, bracket loc (List.map (fun e -> G.Arg e) exprs)) |> G.e

let todo_tokens ((start, end_) : loc) =
  let wrap tok = (Parse_info.str_of_info tok, tok) in
  if start = end_ then [ G.TodoK (wrap start) ]
  else [ G.TodoK (wrap start); G.TodoK (wrap end_) ]

let todo_stmt (loc : loc) : G.stmt =
  G.s (G.OtherStmt (G.OS_Todo, todo_tokens loc))

let todo_expr (loc : loc) : G.expr =
  G.e (G.OtherExpr (G.OE_Todo, todo_tokens loc))

let todo_stmt2 (loc : loc) : stmt_or_expr = Stmt (loc, todo_stmt loc)

let todo_expr2 (loc : loc) : stmt_or_expr = Expr (loc, todo_expr loc)

(*****************************************************************************)
(* Converter from bash AST to generic AST *)
(*****************************************************************************)

let negate_expr (neg_tok : tok) (cmd_loc : loc) (cmd : G.expr) : G.expr =
  let func = G.IdSpecial (G.Op G.Not, neg_tok) |> G.e in
  let args = [ G.Arg cmd ] in
  G.Call (func, bracket cmd_loc args) |> G.e

(*
   Redirect stderr in the last command of the pipeline.
*)
let redirect_pipeline_stderr_to_stdout pip =
  (* TODO: don't ignore redirects *)
  pip

let rec blist (env : env) (l : blist) : stmt_or_expr list =
  match l with
  | Seq (_loc, left, right) -> blist env left @ blist env right
  | And (_loc, left, and_tok, right) -> [ transpile_and env left and_tok right ]
  | Or (_loc, left, or_tok, right) -> [ transpile_or env left or_tok right ]
  | Pipelines (_loc, pl) -> List.map (fun x -> pipeline env x) pl
  | Empty _loc -> []

and pipeline (env : env) (x : pipeline) : stmt_or_expr =
  match x with
  | Command (loc, cmd_redir) -> command_with_redirects env cmd_redir
  | Pipeline (loc, pip, pipe_op, cmd_redir) ->
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
      let func = G.IdSpecial (G.Op G.Pipe, bar_tok) |> G.e in
      let left = pipeline env pip |> as_expr in
      let right = command_with_redirects env cmd_redir |> as_expr in
      Expr (loc, G.Call (func, bracket loc [ G.Arg left; G.Arg right ]) |> G.e)
  | Control_operator (loc, pip, control_op) -> (
      match control_op with
      | Foreground, tok -> pipeline env pip
      | Background, amp_tok ->
          let func = G.IdSpecial (G.Op G.Background, amp_tok) |> G.e in
          let arg = pipeline env pip |> as_expr in
          Expr (loc, G.Call (func, bracket loc [ G.Arg arg ]) |> G.e))

and command_with_redirects (env : env) (x : command_with_redirects) :
    stmt_or_expr =
  (* TODO: don't ignore redirects *)
  let { loc; command = cmd; redirects } = x in
  ignore redirects;
  command env cmd

(*
  The preference between stmt or expr depends on whether the most fitting
  construct of the generic AST is an stmt or an expr. For the todos, it
  may be little arbitrary.
*)
and command (env : env) (cmd : command) : stmt_or_expr =
  match cmd with
  | Simple_command { loc; assignments = _; arguments } -> (
      match arguments with
      | [ (Expr_ellipsis tok as e) ] -> Expr ((tok, tok), expression env e)
      | arguments ->
          let args = List.map (expression env) arguments in
          Expr (loc, call loc C.cmd args))
  | Subshell (loc, (open_, bl, close)) ->
      (* TODO: subshell *) stmt_group env loc (blist env bl)
  | Command_group (loc, (open_, bl, close)) -> stmt_group env loc (blist env bl)
  | Sh_test (loc, _) -> todo_expr2 loc
  | Bash_test (loc, _) -> todo_expr2 loc
  | Arithmetic_expression (loc, _) -> todo_expr2 loc
  | For_loop (loc, for_, loop_var, opt_in, do_, body, done_) ->
      let header =
        let values : G.expr list =
          match opt_in with
          | Some (_in, vals) -> List.map (fun x -> expression env x) vals
          | None ->
              (* TODO/FIXME: transpile implicit "$@" or make this optional *)
              []
        in
        match loop_var with
        | Simple_variable_name var
        | Special_variable_name var
        | Var_metavar var ->
            let entity = G.basic_entity var in
            G.ForIn ([ ForInitVar (entity, G.empty_var) ], values)
      in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.For (for_, header, body) |> G.s)
  | For_loop_c_style (loc, bl) ->
      (* TODO: loop *) stmt_group env loc (blist env bl)
  | Select (loc, _select, _loop_var, _opt_in, _do_, body, _done_) ->
      (* TODO *)
      stmt_group env loc (blist env body)
  | Case (loc, case, subject, in_, clauses, esac) ->
      let subject = expression env subject in
      let clauses =
        List.map
          (fun (loc, patterns, paren, stmts, _opt_term) ->
            (* TODO: handle the different kinds of terminators. Insert breaks. *)
            let patterns =
              List.map
                (fun e ->
                  let tok, _ = expression_loc e in
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
      Stmt (loc, G.Switch (case, Some subject, clauses) |> G.s)
  | If (loc, if_, cond, then_, body, elifs, else_, fi) ->
      let ifs = (loc, if_, cond, then_, body) :: elifs in
      let else_stmt =
        match else_ with
        | None -> None
        | Some (loc, else_tok, body) -> Some (blist env body |> block |> as_stmt)
      in
      let opt_stmt =
        List.fold_right
          (fun if_ (else_stmt : G.stmt option) ->
            let loc1, if_, cond, then_, body = if_ in
            let cond_expr = blist env cond |> block |> as_expr in
            let body_stmt = blist env body |> block |> as_stmt in
            Some (G.s (G.If (if_, cond_expr, body_stmt, else_stmt))))
          ifs else_stmt
      in
      let stmt =
        match opt_stmt with
        | None -> (* there's at least one 'if ... then ...' *) assert false
        | Some stmt -> stmt
      in
      Stmt (loc, stmt)
  | While_loop (loc, while_, cond, do_, body, done_) ->
      let cond = stmt_group env loc (blist env cond) |> as_expr in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.While (while_, cond, body) |> G.s)
  | Until_loop (loc, until, cond, do_, body, done_) ->
      let cond_loc = blist_loc cond in
      let neg_cond =
        blist env cond |> stmt_group env loc |> as_expr
        |> negate_expr until cond_loc
      in
      let body = stmt_group env loc (blist env body) |> as_stmt in
      Stmt (loc, G.While (until, neg_cond, body) |> G.s)
  | Coprocess (loc, opt_name, cmd) -> (* TODO: coproc *) command env cmd
  | Assignment (loc, ass) ->
      let var = G.N (mk_name ass.lhs) |> G.e in
      let value = expression env ass.rhs in
      let e =
        match ass.assign_op with
        | Set, tok -> G.Assign (var, tok, value)
        | Add, tok -> G.AssignOp (var, (G.Plus, tok), value)
      in
      Expr (loc, G.e e)
  | Declaration (loc, _) -> todo_stmt2 loc
  | Negated_command (loc, excl_tok, cmd) ->
      let cmd_loc = command_loc cmd in
      let cmd = command env cmd |> as_expr in
      let e = negate_expr excl_tok cmd_loc cmd in
      Expr (loc, e)
  | Function_definition (loc, def) ->
      let first_tok =
        match def.function_ with
        | Some function_tok -> function_tok
        | None -> fst (variable_name_loc def.name)
      in
      let def_kind =
        G.FuncDef
          {
            G.fkind = (G.Function, first_tok);
            fparams = [];
            frettype = None;
            fbody = G.FBStmt (command env def.body |> as_stmt);
          }
      in
      (* Function names are in another namespace than ordinary variables.
         They can't be accessed with the '$' notation. No need to prepend
         a '$' to the name like for variables. *)
      let def = (G.basic_entity (get_var_name def.name), def_kind) in
      Stmt (loc, G.DefStmt def |> G.s)

and stmt_group (env : env) (loc : loc) (l : stmt_or_expr list) : stmt_or_expr =
  let stmts = List.map as_stmt l in
  let start, end_ = loc in
  Stmt (loc, G.s (G.Block (start, stmts, end_)))

and expression (env : env) (e : expression) : G.expr =
  match e with
  | Word str -> G.L (G.String str) |> G.e
  | String (* "foo" *) (open_, frags, close) -> (
      match frags with
      | [ String_content ((str, _) as wrap) ]
        when not (env = Pattern && str = "...") ->
          (* normalization to enable matching of e.g. "foo" against foo *)
          G.L (G.String wrap) |> G.e
      | _ ->
          let loc = (open_, close) in
          List.map (string_fragment env) frags |> call loc C.quoted_concat)
  | String_fragment (loc, frag) -> string_fragment env frag
  | Raw_string (* 'foo' *) (str, tok) ->
      (* normalization to enable matching of e.g. 'foo' against foo *)
      let without_quotes =
        let len = String.length str in
        if len >= 2 && str.[0] = '\'' && str.[len - 1] = '\'' then
          String.sub str 1 (len - 2)
        else (* it's a bug but let's not fail *)
          str
      in
      G.L (G.String (without_quotes, tok)) |> G.e
  | Ansii_c_string str -> G.L (G.String str) |> G.e
  | Special_character str -> G.L (G.String str) |> G.e
  | Concatenation (loc, el) -> List.map (expression env) el |> call loc C.concat
  | Expr_ellipsis tok -> G.Ellipsis tok |> G.e
  | Expr_metavar mv -> G.N (mk_name mv) |> G.e
  | Equality_test (loc, _, _) -> (* don't know what this is *) todo_expr loc
  | Empty_expression loc -> G.L (G.String ("", fst loc)) |> G.e
  | Array (loc, (open_, elts, close)) ->
      let elts = List.map (expression env) elts in
      G.Container (G.Array, (open_, elts, close)) |> G.e
  | Process_substitution (loc, (open_, x, close)) ->
      let arg = blist env x |> block |> as_expr in
      call loc C.proc_subst [ arg ]

and string_fragment (env : env) (frag : string_fragment) : G.expr =
  match frag with
  | String_content (("...", tok) as wrap) when env = Pattern ->
      (* convert the '...' in '"${foo}...${bar}"' into an ellipsis *)
      G.Ellipsis tok |> G.e
  | String_content ((_, tok) as wrap) -> G.e (G.L (G.String wrap))
  | Expansion (loc, ex) -> expansion env ex
  | Command_substitution (open_, x, close) ->
      let loc = (open_, close) in
      let arg = blist env x |> block |> as_expr in
      call loc C.cmd_subst [ arg ]
  | Frag_metavar mv -> G.N (mk_name mv) |> G.e

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (env : env) (x : expansion) : G.expr =
  match x with
  | Simple_expansion (loc, var_name) -> expand loc (mk_var_expr var_name)
  | Complex_expansion (open_, x, close) -> (
      match x with
      | Variable (loc, var) -> expand loc (mk_var_expr var)
      | Complex_expansion_TODO loc -> todo_expr loc)

and expand loc (var_expr : G.expr) : G.expr = call loc C.expand [ var_expr ]

(*
   'a && b' and 'a || b' looks like expressions but they're really
   conditional statements. We make such translation rather than introducing
   special statement constructs into the generic AST.

      a && b

     -->

     if a; then
       b
     else
       false
     fi
*)
and transpile_and (env : env) (left : blist) tok_and (right : blist) :
    stmt_or_expr =
  let cond = blist env left |> block in
  let body = blist env right |> block in
  let loc = range (stmt_or_expr_loc cond) (stmt_or_expr_loc body) in
  let fail = stmt_of_expr loc (G.L (G.Bool (false, snd loc)) |> G.e) in
  Stmt (loc, G.s (G.If (tok_and, as_expr cond, as_stmt body, Some fail)))

(*
   This is similar to 'transpile_and', with a negated condition.

     a || b

   -->

     if ! a; then
       b
     fi
*)
and transpile_or (env : env) (left : blist) tok_or (right : blist) :
    stmt_or_expr =
  let e = blist env left |> block in
  let ((start, _) as cond_loc) = stmt_or_expr_loc e in
  let not_ = G.IdSpecial (G.Op G.Not, tok_or) |> G.e in
  let cond = G.Call (not_, bracket cond_loc [ G.Arg (as_expr e) ]) |> G.e in
  let body = blist env right |> block in
  let _, end_ = stmt_or_expr_loc body in
  let loc = (start, end_) in
  Stmt (loc, G.s (G.If (tok_or, cond, as_stmt body, None)))

let program (env : env) x = blist (env : env) x |> List.map as_stmt

(*
   Unwrap the tree as much as possible to maximize matches.

   For example 'echo' is parsed as a list of statements but occurs
   in the target program as a single stmt ('If' branch) or as an expr
   ('If' condition). Unwrapping into an expr allows the expr to match those
   cases.
*)
let any (env : env) x =
  match program env x with
  | [ { G.s = G.ExprStmt (e, _semicolon); _ } ] -> G.E e
  | [ stmt ] -> G.S stmt
  | stmts -> G.Ss stmts
