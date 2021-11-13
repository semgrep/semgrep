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
  let loc = instruction_loc x in
  let expr =
    match x with
    | From (loc, name, _, _, _) -> call name loc []
    | Run (_loc, name, x) -> call name loc (argv_or_shell env x)
    | Cmd (_loc, name, x) -> call name loc (argv_or_shell env x)
    | Label (_loc, name, _) -> call name loc []
    | Expose (_loc, name, _, _) -> call name loc []
    | Env (_loc, name, _) -> call name loc []
    | Add (_loc, name, _, _, _) -> call name loc []
    | Copy (_loc, name, _, _, _) -> call name loc []
    | Entrypoint (_loc, name, _) -> call name loc []
    | Volume (_loc, name, _) -> call name loc []
    | User (_loc, name, _, _) -> call name loc []
    | Workdir (_loc, name, _) -> call name loc []
    | Arg (_loc, name, _) -> call name loc []
    | Onbuild (_loc, name, _) -> call name loc []
    | Stopsignal (_loc, name, _) -> call name loc []
    | Healthcheck (_loc, name, _) -> call name loc []
    | Shell (_loc, name, _) -> call name loc []
    | Maintainer (_loc, name, _) -> call name loc []
    | Cross_build_xxx (_loc, name, _) -> call name loc []
    | Instr_semgrep_ellipsis tok -> G.Ellipsis tok |> G.e
    | Instr_TODO name -> call name loc []
  in
  stmt_of_expr loc expr

let program (env : env) (x : program) : G.stmt list =
  Common.map (instruction env) x

let any (env : env) x : G.any = G.Ss (program env x)
