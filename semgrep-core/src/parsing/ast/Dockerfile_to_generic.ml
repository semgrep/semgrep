(*
   Convert Dockerfile-specific AST to generic AST.
*)

module G = AST_generic
open AST_dockerfile

type env = AST_bash.input_kind

let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))

let call (name : string wrap) ((args_start, args_end) : loc)
    (args : G.expr list) : G.expr =
  let func = G.N (G.Id (name, G.empty_id_info ())) |> G.e in
  let args = Common.map (fun e -> G.Arg e) args in
  G.Call (func, (args_start, args, args_end)) |> G.e

let instruction _env (x : instruction) : G.stmt =
  let loc = instruction_loc x in
  let expr =
    match x with
    | From (loc, name, _, _, _) -> call name loc []
    | Run (_loc, name, _argv_or_shell) -> call name loc []
    | Cmd (_loc, name, _argv_or_shell) -> call name loc []
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
  in
  stmt_of_expr loc expr

let program (env : env) (x : program) : G.stmt list =
  Common.map (instruction env) x

let any (env : env) x : G.any = G.Ss (program env x)
