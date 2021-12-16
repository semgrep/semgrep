(*
   Convert Dockerfile-specific AST to generic AST.
*)

module G = AST_generic
open AST_dockerfile

type env = AST_bash.input_kind

let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))

let call ((orig_name, name_tok) : string wrap) ((args_start, args_end) : loc)
    (args : G.expr list) : G.expr =
  let name = (String.uppercase_ascii orig_name, name_tok) in
  let func = G.N (G.Id (name, G.empty_id_info ())) |> G.e in
  let args = Common.map (fun e -> G.Arg e) args in
  G.Call (func, (args_start, args, args_end)) |> G.e

let make_hidden_function loc name : G.expr =
  let id = "!dockerfile_" ^ name ^ "!" in
  let id_info = G.empty_id_info ~hidden:true () in
  G.N (G.Id ((id, fst loc), id_info)) |> G.e

let call_shell loc (shell_compat : shell_compatibility) args =
  let shell_name =
    match shell_compat with
    | Sh -> "sh"
    | Cmd -> "cmd"
    | Powershell -> "powershell"
    | Other name -> name
  in
  let func = make_hidden_function loc shell_name in
  let args = Common.map (fun e -> G.Arg e) args in
  let args_start, args_end = loc in
  G.Call (func, (args_start, args, args_end)) |> G.e

let bracket (loc : loc) x : 'a bracket =
  let start, end_ = loc in
  (start, x, end_)

let expr_of_stmt (st : G.stmt) : G.expr = G.stmt_to_expr st

let expr_of_stmts loc (stmts : G.stmt list) : G.expr =
  G.Block (bracket loc stmts) |> G.s |> expr_of_stmt

let quoted_string s : G.expr = G.L (G.String s) |> G.e

(*
   Return the arguments to pass to the dockerfile command e.g. the arguments
   to CMD.
*)
let argv_or_shell env x : G.expr list =
  match x with
  | Argv (_open, args, _close) -> Common.map quoted_string args
  | Sh_command (loc, x) ->
      let args = Bash_to_generic.program env x |> expr_of_stmts loc in
      [ call_shell loc Sh [ args ] ]
  | Other_shell_command (shell_compat, code) ->
      let args = [ G.L (G.String code) |> G.e ] in
      let loc = wrap_loc code in
      [ call_shell loc shell_compat args ]

let instruction env (x : instruction) : G.stmt =
  let expr =
    match x with
    | From (loc, name, _, _, _) -> call name loc []
    | Run (loc, name, x) -> call name loc (argv_or_shell env x)
    | Cmd (loc, name, x) -> call name loc (argv_or_shell env x)
    | Label (loc, name, _) -> call name loc []
    | Expose (loc, name, _, _) -> call name loc []
    | Env (loc, name, _) -> call name loc []
    | Add (loc, name, _, _, _) -> call name loc []
    | Copy (loc, name, _, _, _) -> call name loc []
    | Entrypoint (loc, name, _) -> call name loc []
    | Volume (loc, name, _) -> call name loc []
    | User (loc, name, _, _) -> call name loc []
    | Workdir (loc, name, _) -> call name loc []
    | Arg (loc, name, _) -> call name loc []
    | Onbuild (loc, name, _) -> call name loc []
    | Stopsignal (loc, name, _) -> call name loc []
    | Healthcheck (loc, name, _) -> call name loc []
    | Shell (loc, name, _) -> call name loc []
    | Maintainer (loc, name, _) -> call name loc []
    | Cross_build_xxx (loc, name, _) -> call name loc []
    | Instr_semgrep_ellipsis tok -> G.Ellipsis tok |> G.e
    | Instr_TODO ((_, tok) as name) -> call name (tok, tok) []
  in
  stmt_of_expr (instruction_loc x) expr

let program (env : env) (x : program) : G.stmt list =
  Common.map (instruction env) x

let any (env : env) x : G.any =
  match program env x with
  | [ stmt ] -> G.S stmt
  | stmts -> G.Ss stmts
