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

(* Temporary representation.
   Avoids superfluous early wrapping of expressions in statements and
   vice-versa. *)
type stmt_or_expr = Stmt of loc * G.stmt | Expr of loc * G.expr

let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))

let expr_of_stmt loc (st : G.stmt) : G.expr =
  G.OtherExpr (G.OE_StmtExpr, [ G.S st ])

let as_stmt : stmt_or_expr -> G.stmt = function
  | Stmt (loc, st) -> st
  | Expr (loc, e) -> stmt_of_expr loc e

let as_expr : stmt_or_expr -> G.expr = function
  | Stmt (loc, st) -> expr_of_stmt loc st
  | Expr (loc, e) -> e

let stmt_or_expr_loc = function Stmt (loc, _) | Expr (loc, _) -> loc

let block : stmt_or_expr list -> stmt_or_expr = function
  | [ x ] -> x
  | several ->
      let loc = list_loc stmt_or_expr_loc several in
      let stmts = List.map as_stmt several in
      Stmt (loc, G.s (G.Block (bracket loc stmts)))

module C = struct
  let mk (loc : loc) (name : string) =
    let id = "!sh_" ^ name ^ "!" in
    let id_info = G.empty_id_info () in
    G.N (G.Id ((id, fst loc), id_info))

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

  (* Concatenate two string fragments e.g.
       foo"$bar"
  *)
  let concat loc = mk loc "concat"
end

(*
   Constructors used to represent bash expressions that don't have
   an equivalent in the generic AST.

   Usage: call C.cmd args
*)
let call loc name exprs =
  G.Call (name loc, bracket loc (List.map (fun e -> G.Arg e) exprs))

let todo_tokens ((start, end_) : loc) =
  let wrap tok = (Parse_info.string_of_info tok, tok) in
  if start = end_ then [ G.TodoK (wrap start) ]
  else [ G.TodoK (wrap start); G.TodoK (wrap end_) ]

let todo_stmt (loc : loc) : G.stmt =
  G.s (G.OtherStmt (G.OS_Todo, todo_tokens loc))

let todo_expr (loc : loc) : G.expr = G.OtherExpr (G.OE_Todo, todo_tokens loc)

let todo_stmt2 (loc : loc) : stmt_or_expr = Stmt (loc, todo_stmt loc)

let todo_expr2 (loc : loc) : stmt_or_expr = Expr (loc, todo_expr loc)

(*****************************************************************************)
(* Converter from bash AST to generic AST *)
(*****************************************************************************)

(*
   Redirect stderr in the last command of the pipeline.
*)
let redirect_pipeline_stderr_to_stdout pip =
  (* TODO: don't ignore redirects *)
  pip

let rec blist (l : blist) : stmt_or_expr list =
  match l with
  | Seq (_loc, left, right) -> blist left @ blist right
  | And (_loc, left, and_tok, right) -> [ transpile_and left and_tok right ]
  | Or (_loc, left, or_tok, right) -> [ transpile_or left or_tok right ]
  | Pipelines (_loc, pl) -> List.map (fun x -> pipeline x) pl
  | Empty _loc -> []

and pipeline (x : pipeline) : stmt_or_expr =
  match x with
  | Command (loc, cmd_redir) -> command_with_redirects cmd_redir
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
      let func = G.IdSpecial (G.Op G.Pipe, bar_tok) in
      let left = pipeline pip |> as_expr in
      let right = command_with_redirects cmd_redir |> as_expr in
      Expr (loc, G.Call (func, bracket loc [ G.Arg left; G.Arg right ]))
  | Control_operator (loc, pip, control_op) -> (
      match control_op with
      | Foreground, tok -> pipeline pip
      | Background, amp_tok ->
          let func = G.IdSpecial (G.Op G.Pipe, amp_tok) in
          let arg = pipeline pip |> as_expr in
          Expr (loc, G.Call (func, bracket loc [ G.Arg arg ])))

and command_with_redirects (x : command_with_redirects) : stmt_or_expr =
  (* TODO: don't ignore redirects *)
  let { loc; command = cmd; redirects } = x in
  ignore redirects;
  command cmd

(*
  The preference between stmt or expr depends on whether the most fitting
  construct of the generic AST is an stmt or an expr. For the todos, it
  may be little arbitrary.
*)
and command (cmd : command) : stmt_or_expr =
  match cmd with
  | Simple_command { loc; assignments = _; arguments } ->
      let args = List.map expression arguments in
      Expr (loc, call loc C.cmd args)
  | Subshell (loc, (open_, bl, close)) ->
      (* TODO: subshell *) stmt_group loc (blist bl)
  | Command_group (loc, (open_, bl, close)) -> stmt_group loc (blist bl)
  | Sh_test (loc, _) -> todo_expr2 loc
  | Bash_test (loc, _) -> todo_expr2 loc
  | Arithmetic_expression (loc, _) -> todo_expr2 loc
  | For_loop (loc, bl) -> (* TODO: loop *) stmt_group loc (blist bl)
  | For_loop_c_style (loc, bl) -> (* TODO: loop *) stmt_group loc (blist bl)
  | Select (loc, _) -> todo_expr2 loc
  | Case (loc, _) -> todo_stmt2 loc
  | If (loc, if_, cond, then_, body, elifs, else_, fi) ->
      let ifs = (loc, if_, cond, then_, body) :: elifs in
      let else_stmt =
        match else_ with
        | None -> None
        | Some (loc, else_tok, body) -> Some (blist body |> block |> as_stmt)
      in
      let opt_stmt =
        List.fold_right
          (fun if_ (else_stmt : G.stmt option) ->
            let loc1, if_, cond, then_, body = if_ in
            let cond_expr = blist cond |> block |> as_expr in
            let body_stmt = blist body |> block |> as_stmt in
            Some (G.s (G.If (if_, cond_expr, body_stmt, else_stmt))))
          ifs else_stmt
      in
      let stmt =
        match opt_stmt with
        | None -> (* there's at least one 'if ... then ...' *) assert false
        | Some stmt -> stmt
      in
      Stmt (loc, stmt)
  | While_loop (loc, bl) -> (* TODO: loop *) stmt_group loc (blist bl)
  | Until_loop (loc, bl) -> (* TODO: loop *) stmt_group loc (blist bl)
  | Coprocess (loc, opt_name, cmd) -> (* TODO: coproc *) command cmd
  | Assignment (loc, _) -> todo_expr2 loc
  | Declaration (loc, _) -> todo_stmt2 loc
  | Negated_command (loc, _, _) -> todo_expr2 loc
  | Function_definition (loc, _) -> todo_stmt2 loc

and stmt_group (loc : loc) (l : stmt_or_expr list) : stmt_or_expr =
  let stmts = List.map as_stmt l in
  let start, end_ = loc in
  Stmt (loc, G.s (G.Block (start, stmts, end_)))

and expression (e : expression) : G.expr =
  match e with
  | Word ((_, tok) as wrap) -> G.L (G.Atom (tok, wrap))
  | String x -> todo_expr (bracket_loc x)
  | String_fragment (loc, frag) -> (
      match frag with
      | String_content ((_, tok) as wrap) -> G.L (G.Atom (tok, wrap))
      | Expansion (loc, ex) -> expansion ex
      | Command_substitution (open_, _, close) ->
          let loc = (open_, close) in
          todo_expr loc)
  | Raw_string x -> todo_expr (wrap_loc x)
  | Ansii_c_string x -> todo_expr (wrap_loc x)
  | Special_character x -> todo_expr (wrap_loc x)
  | String_expansion x -> todo_expr (wrap_loc x)
  | Concatenation (loc, _) -> todo_expr loc
  | Semgrep_ellipsis tok -> G.Ellipsis tok
  | Semgrep_metavariable x -> todo_expr (wrap_loc x)
  | Equality_test (loc, _, _) -> todo_expr loc
  | Empty_expression loc ->
      let start, _ = loc in
      G.L (G.Atom (start, ("", start)))
  | Expression_TODO loc -> todo_expr loc

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (x : expansion) : G.expr =
  match x with
  | Simple_expansion (loc, dollar_tok, var_name) ->
      let arg =
        match var_name with
        | Simple_variable_name name | Special_variable_name name ->
            G.Arg (G.N (G.Id (name, G.empty_id_info ())))
      in
      let func = G.N (G.Id (("$", dollar_tok), G.empty_id_info ())) in
      let e = G.Call (func, bracket loc [ arg ]) in
      e
  | Complex_expansion br -> todo_expr (bracket_loc br)

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
and transpile_and (left : blist) tok_and (right : blist) : stmt_or_expr =
  let cond = blist left |> block in
  let body = blist right |> block in
  let loc = range (stmt_or_expr_loc cond) (stmt_or_expr_loc body) in
  let fail = stmt_of_expr loc (G.L (G.Bool (false, snd loc))) in
  Stmt (loc, G.s (G.If (tok_and, as_expr cond, as_stmt body, Some fail)))

(*
   This is similar to 'transpile_and', with a negated condition.

     a || b

   -->

     if ! a; then
       b
     fi
*)
and transpile_or (left : blist) tok_or (right : blist) : stmt_or_expr =
  let e = blist left |> block in
  let ((start, _) as cond_loc) = stmt_or_expr_loc e in
  let not_ = G.IdSpecial (G.Op G.Not, tok_or) in
  let cond = G.Call (not_, bracket cond_loc [ G.Arg (as_expr e) ]) in
  let body = blist right |> block in
  let _, end_ = stmt_or_expr_loc body in
  let loc = (start, end_) in
  Stmt (loc, G.s (G.If (tok_or, cond, as_stmt body, None)))

let program x = blist x |> List.map as_stmt

let any x = G.Ss (program x)
