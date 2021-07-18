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
type stmt_or_expr = Stmt of G.stmt | Expr of G.expr

let fake_tok = G.fake ""

let stmt_of_expr (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fake_tok))

let expr_of_stmt (st : G.stmt) : G.expr = G.OtherExpr (G.OE_StmtExpr, [ G.S st ])

let as_stmt : stmt_or_expr -> G.stmt = function
  | Stmt st -> st
  | Expr e -> stmt_of_expr e

let as_expr : stmt_or_expr -> G.expr = function
  | Stmt st -> expr_of_stmt st
  | Expr e -> e

let block : stmt_or_expr list -> stmt_or_expr = function
  | [ x ] -> x
  | several ->
      let stmts = List.map as_stmt several in
      Stmt (G.s (G.Block (G.fake_bracket stmts)))

module C = struct
  let mk (name : string) =
    let id = "!sh_" ^ name ^ "!" in
    let id_info = G.empty_id_info () in
    G.N (G.Id ((id, fake_tok), id_info))

  (* For simple commands, e.g.
       $echo "$@"
  *)
  let cmd = mk "cmd"

  (* Split a string according to whitespace defined by "$IFS".
     This is done on the result of unquoted $ expansions:
       $args
       $(cat foo.txt)
       `cat foo.txt`
       ${args}
       ${args%.c}
  *)
  let _split = mk "split"

  (* Concatenate two string fragments e.g.
       foo"$bar"
  *)
  let _concat = mk "concat"
end

(*
   Constructors used to represent bash expressions that don't have
   an equivalent in the generic AST.

   Usage: call C.cmd args
*)
let call name exprs =
  G.Call (name, G.fake_bracket (List.map (fun e -> G.Arg e) exprs))

let todo_stmt tok = G.s (G.OtherStmt (G.OS_Todo, [ G.TodoK tok ]))

let todo_expr tok = G.OtherExpr (G.OE_Todo, [ G.TodoK tok ])

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
  | Seq (loc, left, right) -> blist left @ blist right
  | And (loc, left, and_tok, right) -> [ transpile_and left and_tok right ]
  | Or (loc, left, or_tok, right) -> [ transpile_or left or_tok right ]
  | Pipelines (loc, pl) -> List.map (fun x -> pipeline x) pl
  | Empty loc -> []

and pipeline (x : pipeline) : stmt_or_expr =
  match x with
  | Command (loc, cmd_redir) -> command_with_redirects cmd_redir
  | Pipeline (pip, pipe_op, cmd_redir) ->
      let pip, bar_tok =
        match pipe_op with
        | Bar tok -> (pip, tok)
        | Bar_ampersand tok ->
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
      Expr (G.Call (func, G.fake_bracket [ G.Arg left; G.Arg right ]))
  | Control_operator (pip, control_op) -> (
      match control_op with
      | Foreground _tok -> pipeline pip
      | Background amp_tok ->
          let func = G.IdSpecial (G.Op G.Pipe, amp_tok) in
          let arg = pipeline pip |> as_expr in
          Expr (G.Call (func, G.fake_bracket [ G.Arg arg ])))

and command_with_redirects (x : command_with_redirects) : stmt_or_expr =
  (* TODO: don't ignore redirects *)
  let { command = cmd; redirects } = x in
  ignore redirects;
  command cmd

and command (cmd : command) : stmt_or_expr =
  match cmd with
  | Simple_command { assignments = _; arguments } ->
      let args = List.map expression arguments in
      Expr (call C.cmd args)
  | Compound_command _ -> Expr todo_expr
  | Coprocess _ -> Expr todo_expr
  | Assignment _ -> Stmt todo_stmt
  | Declaration _ -> Stmt todo_stmt
  | Negated_command _ -> Expr todo_expr
  | Function_definition _ -> Stmt todo_stmt

and expression (e : expression) : G.expr =
  match e with
  | Word x -> G.L (G.Atom (G.fake "", x))
  | String _ -> todo_expr
  | String_fragment frag -> (
      match frag with
      | String_content x -> G.L (G.Atom (G.fake "", x))
      | Expansion ex -> expansion ex
      | Command_substitution (_open, _x, _close) -> todo_expr)
  | Raw_string _ -> todo_expr
  | Ansii_c_string _ -> todo_expr
  | Special_character _ -> todo_expr
  | String_expansion _ -> todo_expr
  | Concatenation _ -> todo_expr
  | Semgrep_ellipsis _ -> todo_expr
  | Semgrep_metavariable _ -> todo_expr
  | Expression_TODO -> todo_expr

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (x : expansion) : G.expr =
  match x with
  | Simple_expansion (dollar_tok, var_name) ->
      let arg =
        match var_name with
        | Simple_variable_name name | Special_variable_name name ->
            G.Arg (G.N (G.Id (name, G.empty_id_info ())))
      in
      let func = G.N (G.Id (("$", dollar_tok), G.empty_id_info ())) in
      let e = G.Call (func, G.fake_bracket [ arg ]) in
      e
  | Complex_expansion _ -> todo_expr

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
  let cond = blist left |> block |> as_expr in
  let body = blist right |> block |> as_stmt in
  let fail = stmt_of_expr (G.L (G.Bool (false, G.fake "false"))) in
  Stmt (G.s (G.If (tok_and, cond, body, Some fail)))

(*
   This is similar to 'transpile_and', with a negated condition.

     a || b

   -->

     if ! a; then
       b
     fi
*)
and transpile_or (left : blist) tok_and (right : blist) : stmt_or_expr =
  let cond =
    let e = blist left |> block |> as_expr in
    let not_ = G.IdSpecial (G.Op G.Not, G.fake "!") in
    G.Call (not_, G.fake_bracket [ G.Arg e ])
  in
  let body = blist right |> block |> as_stmt in
  Stmt (G.s (G.If (tok_and, cond, body, None)))

let program x = blist x |> List.map as_stmt

let any x = G.Ss (program x)
