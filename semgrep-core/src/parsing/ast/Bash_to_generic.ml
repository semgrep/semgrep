(*
   Convert Bash-specific AST to generic AST.

   A Bash program is organized into the following hierarchy:

   - List: a list of pipelines. A whole program is a list.

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

(* Temporary representation *)
type stmt_or_expr = Stmt of G.stmt | Expr of G.expr

let fake_tok = G.fake ""

let as_stmt : stmt_or_expr -> G.stmt = function
  | Stmt st -> st
  | Expr e -> G.s (G.ExprStmt (e, fake_tok))

let as_expr : stmt_or_expr -> G.expr = function
  | Stmt st -> G.OtherExpr (G.OE_StmtExpr, [ G.S st ])
  | Expr e -> e

let list_as_expr : stmt_or_expr list -> G.expr = function
  | [x] -> as_expr x
  | several ->
      (* make sequence of statements, then wrap in an expression *)
      ???

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
  let split = mk "split"

  (* Concatenate two string fragments e.g.
       foo"$bar"
  *)
  let concat = mk "concat"

  (* Represents a pipeline terminator: ';', '&', '||', '&&' *)
  let ctrl_op = mk "ctrl_op"
end

(*
   Constructors used to represent bash expressions that don't have
   an equivalent in the generic AST.

   Usage: call C.cmd args
*)
let call name exprs =
  G.Call (name, exprs)

let todo_stmt = G.OtherStmt (G.OS_Todo, [])
let todo_expr = G.OtherExpr (G.OE_Todo, [])

(*****************************************************************************)
(* Converter from bash AST to generic AST *)
(*****************************************************************************)

let apply_control_operator (e : G.expr) (op : unary_control_operator)
  : stmt_or_expr =
  match op with
  | Foreground tok -> Stmt (G.s (G.ExprStmt (e, tok)))
  | Background tok ->
      let function_ = G.IdSpecial ((G.Op G.Background), tok) in
      Expr (G.Call (function_, G.fake_bracket [G.Arg e]))

let rec list_ (l : list_) : stmt_or_expr list =
  match l with
  | Seq (l1, l2) -> list_ l1 @ list_ l2
  | And (l1, tok, l2) ->
      (* TODO: can't compile this to an expression because the left-handside
         updates the environment, affecting the right-handside.
         Maybe translate to an if-then-else, which is also a statement.

         a && b

         -->

         a
         if [[ $? = 0 ]]; then
           b
         fi

      *)
      let e1 = list_ l1 |> as_expr in
      let e2 = list_ l2 |> as_expr in
      let function_ = G.IdSpecial ((G.Op G.And), tok) in
      (*[Expr (G.Call (function_, G.fake_bracket [G.Arg e1; G.Arg e2]))]*)
  | Or (l1, tok, l2) ->
      let e1 = list_ l1 |> as_expr in
      let e2 = list_ l2 |> as_expr in
      let function_ = G.IdSpecial ((G.Op G.Or), tok) in
      (*[Expr (G.Call (function_, G.fake_bracket [G.Arg e1; G.Arg e2]))]*)
  | Pipelines pl -> List.map (fun x -> pipeline x) pl
  | Empty -> []

and pipeline (x : pipeline) : stmt_or_expr list =
  let commands, control_op = x in
  match commands with
  | [] -> assert false
  | [ (_none, single_cmd) ] -> single_cmd
  | (_none, first) :: other ->
      let init = command_with_redirects first |> as_expr in
      let e =
        List.fold_left
          (fun left (opt_bar, cmd_redir) ->
             let bar, tok =
               match opt_bar with
               | None -> assert false
               | Some bar -> bar
             in
             let op = G.IdSpecial ((G.Op G.Pipe), tok) in
             let right = command_with_redirects cmd_redir |> as_expr in
             G.Call (op, [left; right])
          ) init other
      in
      Expr e

and command_with_redirects (x : command_with_redirects) : stmt_or_expr =
  let { command = cmd; redirects } = x in
  ignore redirects;
  command cmd

and command (cmd : command) : G.stmt option =
  match cmd with
  | Simple_command { assignments = _; arguments } -> (
      match arguments with
      | [] -> None
      | arg0 :: args ->
          let args = List.map (fun x -> G.Arg (expression x)) args in
          let e = G.Call (expression arg0, G.fake_bracket args) in
          Some (G.s (ExprStmt (e, G.fake ""))))
  | Compound_command _ -> None
  | Coprocess _ -> None
  | Assignment _ -> None
  | Declaration _ -> None
  | Negated_command _ -> None
  | Function_definition _ -> None

and expression (e : expression) : G.expr =
  let todo = G.L (String ("", G.fake "")) in
  match e with
  | Word x -> G.L (G.Atom (G.fake "", x))
  | String _ -> todo
  | String_fragment frag -> (
      match frag with
      | String_content x -> G.L (G.Atom (G.fake "", x))
      | Expansion ex -> expansion ex
      | Command_substitution (_open, _x, _close) -> todo)
  | Raw_string _ -> todo
  | Ansii_c_string _ -> todo
  | Special_character _ -> todo
  | String_expansion _ -> todo
  | Concatenation _ -> todo
  | Semgrep_ellipsis _ -> todo
  | Semgrep_metavariable _ -> todo
  | Expression_TODO -> todo

(*
   '$' followed by a variable to transform and expand into a list.
   We treat this as a function call.
*)
and expansion (x : expansion) : G.expr =
  let todo = G.L (String ("", G.fake "")) in
  match x with
  | Simple_expansion (dollar_tok, var_name) ->
      let arg =
        match var_name with
        | Simple_variable_name name | Special_variable_name name ->
            G.Arg (G.N (G.Id (name, G.empty_id_info ())))
      in
      let function_ = G.N (G.Id (("$", dollar_tok), G.empty_id_info ())) in
      let e = G.Call (function_, G.fake_bracket [ arg ]) in
      e
  | Complex_expansion _ -> todo

let program x = list_ x

let any x = G.Ss (program x)
