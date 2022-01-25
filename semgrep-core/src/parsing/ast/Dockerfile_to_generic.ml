(*
   Convert Dockerfile-specific AST to generic AST.
*)

module PI = Parse_info
module G = AST_generic
open AST_dockerfile

type env = AST_bash.input_kind

let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))

let call ((orig_name, name_tok) : string wrap) ((args_start, args_end) : Loc.t)
    (args : G.argument list) : G.expr =
  let name = (String.uppercase_ascii orig_name, name_tok) in
  let func = G.N (G.Id (name, G.empty_id_info ())) |> G.e in
  G.Call (func, (args_start, args, args_end)) |> G.e

(* Same as 'call' but assumes all the arguments are ordinary, non-optional
   arguments, specified as 'expr'. *)
let call_exprs (name : string wrap) (loc : Loc.t) (args : G.expr list) : G.expr
    =
  let args = Common.map (fun e -> G.Arg e) args in
  call name loc args

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

let bracket (loc : Loc.t) x : 'a bracket =
  let start, end_ = loc in
  (start, x, end_)

let expr_of_stmt (st : G.stmt) : G.expr = G.stmt_to_expr st

let expr_of_stmts loc (stmts : G.stmt list) : G.expr =
  G.Block (bracket loc stmts) |> G.s |> expr_of_stmt

let string_expr s : G.expr = G.L (G.String s) |> G.e

let id_expr (x : string wrap) : G.expr =
  G.N (G.Id (x, G.empty_id_info ())) |> G.e

let metavar_expr (x : string wrap) : G.expr = id_expr x

let ellipsis_expr (tok : tok) : G.expr = G.Ellipsis tok |> G.e

let expansion_expr loc (x : expansion) =
  let arg =
    match x with
    | Expand_var var -> id_expr var
    | Expand_semgrep_metavar mv -> metavar_expr mv
  in
  let func = make_hidden_function loc "expand" in
  let start, end_ = loc in
  G.Call (func, (start, [ G.Arg arg ], end_)) |> G.e

let string_fragment_expr (x : string_fragment) : G.expr =
  match x with
  | String_content s -> string_expr s
  | Expansion (loc, x) -> expansion_expr loc x
  | Frag_semgrep_metavar s -> metavar_expr s

let str_expr ((loc, frags) : str) : G.expr =
  let frags = Common.map string_fragment_expr frags in
  match frags with
  | [ x ] -> x
  | _ ->
      let func = make_hidden_function loc "concat" in
      let args = Common.map (fun x -> G.Arg x) frags in
      let start, end_ = loc in
      G.Call (func, (start, args, end_)) |> G.e

let array_elt_expr (x : array_elt) : G.expr =
  match x with
  | Arr_string x -> str_expr x
  | Arr_metavar x -> metavar_expr x
  | Arr_ellipsis x -> ellipsis_expr x

let string_array ((open_, args, close) : string_array) : G.expr =
  G.Container (G.Array, (open_, Common.map array_elt_expr args, close)) |> G.e

(*
   Return the arguments to pass to the dockerfile command e.g. the arguments
   to CMD.
*)
let argv_or_shell env x : G.expr list =
  match x with
  | Argv (_loc, array) -> [ string_array array ]
  | Sh_command (loc, x) ->
      let args = Bash_to_generic.program env x |> expr_of_stmts loc in
      [ call_shell loc Sh [ args ] ]
  | Other_shell_command (shell_compat, code) ->
      let args = [ string_expr code ] in
      let loc = wrap_loc code in
      [ call_shell loc shell_compat args ]

let param_arg (x : param) : G.argument =
  let _loc, (dashdash, (name_str, name_tok), _eq, value) = x in
  let option_tok = PI.combine_infos dashdash [ name_tok ] in
  let option_str = PI.str_of_info dashdash ^ name_str in
  G.ArgKwdOptional ((option_str, option_tok), string_expr value)

let opt_param_arg (x : param option) : G.argument list =
  match x with
  | None -> []
  | Some x -> [ param_arg x ]

let from (opt_param : param option) (image_spec : image_spec) _TODO_opt_alias :
    G.argument list =
  (* TODO: metavariable for image name *)
  (* TODO: metavariable for image tag, metavariable for image digest *)
  let opt_param = opt_param_arg opt_param in
  let name = G.Arg (str_expr image_spec.name) in
  let tag =
    match image_spec.tag with
    | None -> []
    | Some (colon, tag) -> [ G.ArgKwdOptional ((":", colon), str_expr tag) ]
  in
  let digest =
    match image_spec.digest with
    | None -> []
    | Some (at, digest) -> [ G.ArgKwdOptional (("@", at), str_expr digest) ]
  in
  let optional_params (* must be placed last *) = tag @ digest @ opt_param in
  name :: optional_params

let label_pairs (kv_pairs : label_pair list) : G.argument list =
  kv_pairs
  |> Common.map (fun (key, _eq, value) -> G.ArgKwd (key, str_expr value))

let add_or_copy (opt_param : param option) (src : path) (dst : path) =
  let opt_param = opt_param_arg opt_param in
  [ G.Arg (str_expr src); G.Arg (str_expr dst) ] @ opt_param

let user_args (user : str) (group : (tok * str) option) =
  let user = G.Arg (str_expr user) in
  let group =
    match group with
    | None -> []
    | Some (colon, group) -> [ G.ArgKwdOptional ((":", colon), str_expr group) ]
  in
  user :: group

(* RUN, CMD, ENTRYPOINT, HEALTHCHECK CMD *)
let cmd_instr_expr (env : env) loc name (cmd : argv_or_shell) : G.expr =
  call_exprs name loc (argv_or_shell env cmd)

let healthcheck_cmd_args env (params : param list) (cmd : cmd) : G.argument list
    =
  let opt_args = Common.map param_arg params in
  let cmd_arg =
    let loc, name, cmd = cmd in
    G.Arg (cmd_instr_expr env loc name cmd)
  in
  cmd_arg :: opt_args

let arg_args key opt_value : G.expr list =
  let key = string_expr key in
  let value =
    match opt_value with
    | None -> []
    | Some (_eq, x) -> [ str_expr x ]
  in
  key :: value

let array_or_paths (x : array_or_paths) : G.expr list =
  match x with
  | Array (_loc, ar) -> [ string_array ar ]
  | Paths (_loc, paths) -> Common.map str_expr paths

let rec instruction_expr env (x : instruction) : G.expr =
  match x with
  | From (loc, name, opt_param, image_spec, opt_alias) ->
      let args = from opt_param image_spec opt_alias in
      call name loc args
  | Run (loc, name, x) -> cmd_instr_expr env loc name x
  | Cmd (loc, name, x) -> cmd_instr_expr env loc name x
  | Label (loc, name, kv_pairs) -> call name loc (label_pairs kv_pairs)
  | Expose (loc, name, port_protos) ->
      let args = Common.map string_fragment_expr port_protos in
      call_exprs name loc args
  | Env (loc, name, pairs) -> call name loc (label_pairs pairs)
  | Add (loc, name, param, src, dst) ->
      call name loc (add_or_copy param src dst)
  | Copy (loc, name, param, src, dst) ->
      call name loc (add_or_copy param src dst)
  | Entrypoint (loc, name, x) -> cmd_instr_expr env loc name x
  | Volume (loc, name, x) -> call_exprs name loc (array_or_paths x)
  | User (loc, name, user, group) -> call name loc (user_args user group)
  | Workdir (loc, name, dir) -> call_exprs name loc [ str_expr dir ]
  | Arg (loc, name, key, opt_value) ->
      call_exprs name loc (arg_args key opt_value)
  | Onbuild (loc, name, instr) ->
      call_exprs name loc [ instruction_expr env instr ]
  | Stopsignal (loc, name, signal) -> call_exprs name loc [ str_expr signal ]
  | Healthcheck (loc, name, Healthcheck_none tok) ->
      call_exprs name loc [ string_expr (PI.str_of_info tok, tok) ]
  | Healthcheck (loc, name, Healthcheck_cmd (_cmd_loc, params, cmd)) ->
      let args = healthcheck_cmd_args env params cmd in
      call name loc args
  | Shell (loc, name, array) -> call_exprs name loc [ string_array array ]
  | Maintainer (loc, name, maintainer) ->
      call_exprs name loc [ string_expr maintainer ]
  | Cross_build_xxx (loc, name, data) ->
      call_exprs name loc [ string_expr data ]
  | Instr_semgrep_ellipsis tok -> G.Ellipsis tok |> G.e
  | Instr_semgrep_metavar x -> metavar_expr x

let instruction env (x : instruction) : G.stmt =
  let expr = instruction_expr env x in
  stmt_of_expr (instruction_loc x) expr

let program (env : env) (x : program) : G.stmt list =
  Common.map (instruction env) x

let any (env : env) x : G.any =
  match program env x with
  | [ stmt ] -> G.S stmt
  | stmts -> G.Ss stmts
