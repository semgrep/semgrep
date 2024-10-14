(*
   Convert Dockerfile-specific AST to generic AST.
*)

module G = AST_generic
open AST_dockerfile
module Loc = AST_dockerfile_loc

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = AST_bash.input_kind

let fb = Tok.unsafe_fake_bracket
let stmt_of_expr loc (e : G.expr) : G.stmt = G.s (G.ExprStmt (e, fst loc))

let call ((orig_name, name_tok) : string wrap)
    ((args_start, args_end) : Tok_range.t) (args : G.argument list) : G.expr =
  let name = (String.uppercase_ascii orig_name, name_tok) in
  let func = G.N (G.Id (name, G.empty_id_info ())) |> G.e in
  G.Call (func, (args_start, args, args_end)) |> G.e

(* Same as 'call' but assumes all the arguments are ordinary, non-optional
   arguments, specified as 'expr'. *)
let call_exprs (name : string wrap) (loc : Tok_range.t)
    ?(opt_args : (G.ident * G.expr) list = []) (args : G.expr list) : G.expr =
  let opt_args =
    List_.map (fun (name, e) -> G.ArgKwdOptional (name, e)) opt_args
  in
  let pos_args = List_.map (fun e -> G.Arg e) args in
  (* optional arguments must be placed last according to AST_generic.ml *)
  let args = pos_args @ opt_args in
  call name loc args

let make_hidden_function loc name : G.expr =
  let id = "!dockerfile_" ^ name ^ "!" in
  let id_info = G.empty_id_info ~hidden:true () in
  G.N (G.Id ((id, fst loc), id_info)) |> G.e

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let call_shell loc (shell_compat : shell_compatibility) args =
  let shell_name =
    match shell_compat with
    | Sh -> "sh"
    | Cmd -> "cmd"
    | Powershell -> "powershell"
    | Other name -> name
  in
  let func = make_hidden_function loc shell_name in
  let args = List_.map (fun e -> G.Arg e) args in
  let args_start, args_end = loc in
  G.Call (func, (args_start, args, args_end)) |> G.e

let bracket (loc : Tok_range.t) x : 'a bracket =
  let start, end_ = loc in
  (start, x, end_)

let expr_of_stmt (st : G.stmt) : G.expr = G.stmt_to_expr st

let expr_of_stmts loc (stmts : G.stmt list) : G.expr =
  G.Block (bracket loc stmts) |> G.s |> expr_of_stmt

let unquoted_string_expr (s : string wrap) : G.expr =
  G.L (G.String (fb s)) |> G.e

let quoted_string_expr (x : string wrap bracket) : G.expr =
  G.L (G.String x) |> G.e

let id_expr (x : string wrap) : G.expr =
  G.N (G.Id (x, G.empty_id_info ())) |> G.e

let metavar_expr (x : string wrap) : G.expr = id_expr x

let string_or_metavar_expr (x : string wrap) : G.expr =
  let s, _ = x in
  if AST_generic.is_metavar_name s then metavar_expr x
  else unquoted_string_expr x

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

let simple_double_quoted_string_expr
    ((open_, x, close) : double_quoted_string_fragment bracket) : G.expr =
  match x with
  | Dbl_string_content s -> quoted_string_expr (open_, s, close)
  | Dbl_expansion (loc, x) -> expansion_expr loc x
  | Dbl_frag_semgrep_metavar s -> metavar_expr s

let simple_docker_string_expr (x : docker_string_fragment) : G.expr =
  match x with
  | Unquoted x -> unquoted_string_expr x
  | Single_quoted (_loc, x) -> quoted_string_expr x
  | Double_quoted (loc, (open_, fragments, close)) -> (
      match fragments with
      | [ x ] -> simple_double_quoted_string_expr (open_, x, close)
      | fragments ->
          let fragments =
            List_.map
              (fun x -> simple_double_quoted_string_expr (fb x))
              fragments
          in
          let func = make_hidden_function loc "concat" in
          let args = List_.map (fun x -> G.Arg x) fragments in
          let start, end_ = loc in
          G.Call (func, (start, args, end_)) |> G.e)
  | Expansion (loc, x) -> expansion_expr loc x
  | Frag_semgrep_metavar s -> metavar_expr s

let docker_string_expr ((loc, fragments) : docker_string) : G.expr =
  match fragments with
  | [ x ] -> simple_docker_string_expr x
  | fragments ->
      let exprs = List_.map simple_docker_string_expr fragments in
      let func = make_hidden_function loc "concat" in
      let args = List_.map (fun x -> G.Arg x) exprs in
      let start, end_ = loc in
      G.Call (func, (start, args, end_)) |> G.e

let str_or_ellipsis_expr = function
  | Str_str str -> docker_string_expr str
  | Str_template x ->
      (* TODO: distinguish heredocs from ordinary strings *)
      unquoted_string_expr x.body
  | Str_semgrep_ellipsis tok -> ellipsis_expr tok

let array_elt_expr (x : array_elt) : G.expr =
  match x with
  | Arr_string (_loc, str) -> quoted_string_expr str
  | Arr_metavar x -> metavar_expr x
  | Arr_ellipsis x -> ellipsis_expr x

let string_array ((open_, args, close) : string_array) : G.expr =
  G.Container (G.Array, (open_, List_.map array_elt_expr args, close)) |> G.e

(*
   Return the arguments to pass to the dockerfile command e.g. the arguments
   to CMD.
*)
let command (env : env) (x : command) : G.expr list =
  match x with
  | Command_semgrep_ellipsis tok -> [ G.Ellipsis tok |> G.e ]
  | Command_semgrep_named_ellipsis x -> [ id_expr x ]
  | Argv (_loc, array) -> [ string_array array ]
  | Sh_command (loc, x) ->
      (* !!! Calling Bash_to_generic !!! *)
      let args = Bash_to_generic.program_with_env env x |> expr_of_stmts loc in
      [ call_shell loc Sh [ args ] ]
  | Other_shell_command (shell_compat, code) ->
      let args = [ unquoted_string_expr code ] in
      let loc = Loc.wrap_loc code in
      [ call_shell loc shell_compat args ]
  | Shell_command_template (_loc, _args) ->
      (* TODO: heredocs *)
      []

let param_arg (x : param) : G.argument =
  let _loc, (dashdash, (name_str, name_tok), _eq, value) = x in
  let option_tok = Tok.combine_toks dashdash [ name_tok ] in
  let option_str = Tok.content_of_tok dashdash ^ name_str in
  G.ArgKwdOptional ((option_str, option_tok), string_or_metavar_expr value)

let from (params : param list) (image_spec : image_spec) opt_alias :
    G.argument list =
  (* TODO: metavariable for image name *)
  (* TODO: metavariable for image tag, metavariable for image digest *)
  let params = List_.map param_arg params in
  let name = G.Arg (docker_string_expr image_spec.name) in
  let tag =
    match image_spec.tag with
    | None -> []
    | Some (colon, tag) ->
        [ G.ArgKwdOptional ((":", colon), docker_string_expr tag) ]
  in
  let digest =
    match image_spec.digest with
    | None -> []
    | Some (at, digest) ->
        [ G.ArgKwdOptional (("@", at), docker_string_expr digest) ]
  in
  let alias =
    match opt_alias with
    | None -> []
    | Some (as_, alias) ->
        [ G.ArgKwdOptional (("as", as_), docker_string_expr alias) ]
  in
  let optional_params (* must be placed last *) =
    params @ tag @ digest @ alias
  in
  name :: optional_params

let ident_or_metavar_expr (x : ident_or_metavar) =
  match x with
  | Ident key -> id_expr key
  | Semgrep_metavar mv -> metavar_expr mv

let key_or_metavar_expr (x : key_or_metavar) =
  match x with
  | Key key -> simple_docker_string_expr key
  | Semgrep_metavar mv -> metavar_expr mv

let env_decl pairs =
  let decls =
    pairs
    |> List_.map (function
         | Env_semgrep_ellipsis tok ->
             G.ExprStmt (G.Ellipsis tok |> G.e, G.sc) |> G.s
         | Env_pair (_loc, key, _eq, value) -> (
             match key with
             | Ident v
             | Semgrep_metavar v ->
                 let entity = G.basic_entity v in
                 let vardef =
                   G.VarDef
                     {
                       vinit = Some (docker_string_expr value);
                       vtype = None;
                       vtok = G.no_sc;
                     }
                 in
                 G.DefStmt (entity, vardef) |> G.s))
  in
  G.StmtExpr (G.Block (Tok.unsafe_fake_bracket decls) |> G.s) |> G.e

(*
   Convert a LABEL list of key/value pairs into a list of
   pairs of function arguments.
   The goal is to turn 'LABEL a=b c=d' into two function calls
   'LABEL(a, b)' and 'LABEL(c, d)'.
*)
let label_pair_exprs (instr_name : string wrap) (kv_pairs : label_pair list) :
    G.expr list =
  let is_one_element =
    match kv_pairs with
    | [ _ ] -> true
    | _ -> false
  in
  kv_pairs
  |> List_.filter_map (function
       | Label_semgrep_ellipsis tok ->
           if is_one_element then
             (* LABEL ... *)
             let loc = (tok, tok) in
             Some (call instr_name loc [ G.Arg (G.Ellipsis tok |> G.e) ])
           else
             (* LABEL a=b ... c=d *)
             (* This can no longer be translated into something that makes
                sense since each key=value pair gets its own ENV call.
                Such an ellipsis is ignored. *)
             None
       | Label_pair (loc, key, _eq, value) ->
           let key_expr = key_or_metavar_expr key in
           let value_expr = docker_string_expr value in
           Some (call instr_name loc [ G.Arg key_expr; G.Arg value_expr ]))

let add_or_copy (params : param list) (src : path_or_ellipsis list)
    (dst : docker_string) =
  let params = List_.map param_arg params in
  let src = List_.map (fun x -> G.Arg (str_or_ellipsis_expr x)) src in
  src @ [ G.Arg (docker_string_expr dst) ] @ params

let user_args (user : docker_string) (group : (tok * docker_string) option) =
  let user = G.Arg (docker_string_expr user) in
  let group =
    match group with
    | None -> []
    | Some (colon, group) ->
        [ G.ArgKwdOptional ((":", colon), docker_string_expr group) ]
  in
  user :: group

(* Convert RUN options to optional labeled arguments. *)
let run_param (x : run_param) =
  match x with
  | Param (_loc, (_dashdash, name, _eq, value)) ->
      (name, unquoted_string_expr value)
  | Mount_param (loc, name, options) ->
      (* Convert --mount=--mount=foo=bar,baz=42 to a call to a mount function
         that takes optional labeled arguments. *)
      let opt_args =
        List_.map
          (fun (_loc, name, value) -> (name, unquoted_string_expr value))
          options
      in
      let e = call_exprs name loc ~opt_args [] in
      (name, e)

(* RUN, CMD, ENTRYPOINT, HEALTHCHECK CMD *)
let cmd_instr_expr (env : env) loc name (params : run_param list)
    (cmd : command) : G.expr =
  call_exprs name loc ~opt_args:(List_.map run_param params) (command env cmd)

let healthcheck_cmd_args env (params : param_or_ellipsis list) (cmd : cmd_instr)
    : G.argument list =
  let opt_args =
    params
    |> List_.map (function
         | ParamParam x -> param_arg x
         | ParamEllipsis tok -> G.Arg (G.Ellipsis tok |> G.e))
  in
  let cmd_arg =
    let loc, name, params, cmd = cmd in
    G.Arg (cmd_instr_expr env loc name params cmd)
  in
  cmd_arg :: opt_args

let string_or_metavar_expr = function
  | Str_string x -> unquoted_string_expr x
  | Str_semgrep_metavar mv -> metavar_expr mv

let arg_args key opt_value : G.expr list =
  let key = ident_or_metavar_expr key in
  let value =
    match opt_value with
    | None -> []
    | Some (_eq, x) -> [ docker_string_expr x ]
  in
  key :: value

let array_or_paths (x : array_or_paths) : G.expr list =
  match x with
  | Array (_loc, ar) -> [ string_array ar ]
  | Paths (_loc, paths) -> List_.map str_or_ellipsis_expr paths

let expose_port_expr (x : expose_port) : G.expr list =
  match x with
  | Expose_semgrep_ellipsis tok -> [ ellipsis_expr tok ]
  | Expose_port (port_tok, None) -> [ unquoted_string_expr port_tok ]
  | Expose_port (port_tok, Some protocol_tok) ->
      [
        G.Container
          ( G.Tuple,
            Tok.unsafe_fake_bracket
              [
                unquoted_string_expr port_tok; unquoted_string_expr protocol_tok;
              ] )
        |> G.e;
      ]
  | Expose_fragment x -> [ simple_docker_string_expr x ]

let healthcheck env loc name (x : healthcheck) =
  match x with
  | Healthcheck_semgrep_metavar id -> call_exprs name loc [ metavar_expr id ]
  | Healthcheck_ellipsis tok -> G.Ellipsis tok |> G.e
  | Healthcheck_none tok ->
      call_exprs name loc [ unquoted_string_expr (Tok.content_of_tok tok, tok) ]
  | Healthcheck_cmd (_cmd_loc, params, cmd) ->
      let args = healthcheck_cmd_args env params cmd in
      call name loc args

let rec instruction_exprs env (x : instruction) : G.expr list =
  match x with
  | From (loc, name, opt_param, image_spec, opt_alias) ->
      let args = from (Option.to_list opt_param) image_spec opt_alias in
      [ call name loc args ]
  | Run (loc, name, params, x) -> [ cmd_instr_expr env loc name params x ]
  | Cmd (loc, name, params, x) -> [ cmd_instr_expr env loc name params x ]
  | Label (_loc, name, kv_pairs) -> label_pair_exprs name kv_pairs
  | Expose (loc, name, port_protos) ->
      let args = List.concat_map expose_port_expr port_protos in
      [ call_exprs name loc args ]
  | Env (_loc, _name, pairs) -> [ env_decl pairs ]
  | Add (loc, name, param, src, dst) ->
      [ call name loc (add_or_copy param src dst) ]
  | Copy (loc, name, param, src, dst) ->
      [ call name loc (add_or_copy param src dst) ]
  | Entrypoint (loc, name, x) -> [ cmd_instr_expr env loc name [] x ]
  | Volume (loc, name, x) -> [ call_exprs name loc (array_or_paths x) ]
  | User (loc, name, user, group) -> [ call name loc (user_args user group) ]
  | Workdir (loc, name, dir) ->
      [ call_exprs name loc [ docker_string_expr dir ] ]
  | Arg (loc, name, key, opt_value) ->
      [ call_exprs name loc (arg_args key opt_value) ]
  | Onbuild (loc, name, instr) ->
      [ call_exprs name loc (instruction_exprs env instr) ]
  | Stopsignal (loc, name, signal) ->
      [ call_exprs name loc [ docker_string_expr signal ] ]
  | Healthcheck (loc, name, x) -> [ healthcheck env loc name x ]
  | Shell (loc, name, array) -> [ call_exprs name loc [ string_array array ] ]
  | Maintainer (loc, name, maintainer) ->
      [ call_exprs name loc [ string_or_metavar_expr maintainer ] ]
  | Cross_build_xxx (loc, name, data) ->
      [ call_exprs name loc [ unquoted_string_expr data ] ]
  | Instr_semgrep_ellipsis tok -> [ G.Ellipsis tok |> G.e ]
  | Instr_semgrep_metavar x -> [ metavar_expr x ]

let instruction env (x : instruction) : G.stmt list =
  instruction_exprs env x
  |> List_.map (fun (expr : G.expr) ->
         match expr.e with
         | StmtExpr stmt -> stmt
         | _ -> stmt_of_expr (Loc.instruction_loc x) expr)

let program_with_env (env : env) (x : program) : G.stmt list =
  List_.map (instruction env) x |> List_.flatten

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let any (x : program) : G.any = G.Ss (program_with_env AST_bash.Pattern x)
let program (x : program) : G.program = program_with_env AST_bash.Program x
