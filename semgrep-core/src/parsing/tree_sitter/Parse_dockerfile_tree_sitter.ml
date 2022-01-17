(**
   Mapping from tree-sitter-dockerfile tree-sitter's CST to the Dockerfile
   AST type, which itself includes nodes of the Bash AST.

   Derived from generated code 'dockerfile/lib/Boilerplate.ml'
*)

open! Common
module AST = AST_dockerfile
module CST = Tree_sitter_dockerfile.CST
module PI = Parse_info
open! AST_dockerfile
module H = Parse_tree_sitter_helpers

(*
   This is preferred for debugging since it raises Assert_failures where
   there are bugs. In production, it's best to avoid raising exceptions
   when possible.
*)
let strict = true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The 'extra' field indicates:
   - if we're parsing a pattern or a program;
   - the current shell, which can change when encountering a SHELL directive.
*)
type env = (AST_bash.input_kind * shell_compatibility) H.env

let token = H.token

let str = H.str

let concat_tokens first_tok other_toks : string wrap =
  let tok = PI.combine_infos first_tok other_toks in
  (PI.str_of_info tok, tok)

let opt_concat_tokens toks : string wrap option =
  match toks with
  | first_tok :: other_toks -> Some (concat_tokens first_tok other_toks)
  | [] -> None

(* Requires at least one token, which must be guaranteed statically. *)
let unsafe_concat_tokens toks : string wrap =
  match opt_concat_tokens toks with
  | Some res -> res
  | None ->
      if strict then assert false
      else
        let s = "" in
        (s, PI.unsafe_fake_info s)

(* best effort to extract the name of the shell *)
let classify_shell ((_open, ar, _close) : string_array) :
    shell_compatibility option =
  match ar with
  | Arr_string ("/bin/bash", _) :: _
  | Arr_string ("/bin/sh", _) :: _
  | Arr_string ("/usr/bin/env", _) :: Arr_string (("bash" | "sh"), _) :: _ ->
      Some Sh
  | Arr_string ("cmd", _) :: _ -> Some Cmd
  | Arr_string ("powershell", _) :: _ -> Some Powershell
  | Arr_string ("/usr/bin/env", _) :: Arr_string (name, _) :: _
  | Arr_string (name, _) :: _ ->
      Some (Other name)
  | Arr_metavar _ :: _
  | Arr_ellipsis _ :: _
  | [] ->
      None

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(*
   I don't know why this exists in the tree-sitter-dockerfile grammar
   since it's not part of the dockerfile specification.
   Here, we just return all the tokens so they can be concatenated back
   together.
*)
let expansion (env : env) ((v1, v2) : CST.expansion) : string wrap =
  let dollar = token env v1 (* "$" *) in
  let more_tokens =
    match v2 with
    | `Var tok -> [ token env tok (* pattern [a-zA-Z][a-zA-Z0-9_]* *) ]
    | `LCURL_pat_8713919_RCURL (v1, v2, v3) ->
        let v1 = token env v1 (* "{" *) in
        let v2 = token env v2 (* pattern [^\}]+ *) in
        let v3 = token env v3 (* "}" *) in
        [ v1; v2; v3 ]
  in
  concat_tokens dollar more_tokens

let expansion_tok (env : env) (x : CST.expansion) : tok = expansion env x |> snd

let param (env : env) ((v1, v2, v3, v4) : CST.param) =
  let dashdash = token env v1 (* "--" *) in
  let key = str env v2 (* pattern [a-z][-a-z]* *) in
  let equal = token env v3 (* "=" *) in
  let value = str env v4 (* pattern [^\s]+ *) in
  let loc = (dashdash, snd value) in
  (loc, (dashdash, key, equal, value))

let expose_port (env : env) ((v1, v2) : CST.expose_port) : string wrap =
  let port = token env v1 (* pattern \d+ *) in
  let protocol =
    match v2 with
    | Some x ->
        [
          (match x with
          | `SLAS_ce91595 tok -> token env tok (* "/tcp" *)
          | `SLAS_c773c8d tok -> token env tok)
          (* "/udp" *);
        ]
    | None -> []
  in
  let tok = PI.combine_infos port protocol in
  (PI.str_of_info tok, tok)

let image_tag (env : env) ((v1, v2) : CST.image_tag) =
  let colon = token env v1 (* ":" *) in
  let tag =
    match v2 with
    | [] -> ("", colon)
    | fragments ->
        fragments
        |> List.map (fun x ->
               match x with
               | `Imm_tok_pat_bcfc287 tok ->
                   token env tok (* pattern [^@\s\$]+ *)
               | `Expa x -> expansion_tok env x)
        |> unsafe_concat_tokens
  in
  (colon, tag)

let user_name_or_group (env : env) (xs : CST.user_name_or_group) : string wrap =
  List.map
    (fun x ->
      match x with
      | `Pat_660c06c tok -> token env tok (* pattern [a-z][-a-z0-9_]* *)
      | `Expa x -> expansion_tok env x)
    xs
  |> unsafe_concat_tokens

let unquoted_string (env : env) (xs : CST.unquoted_string) : string wrap =
  List.map
    (fun x ->
      match x with
      | `Imm_tok_pat_24a1611 tok ->
          token env tok (* pattern "[^\\s\\n\\\"\\\\\\$]+" *)
      | `BSLASHSPACE tok -> token env tok (* "\\ " *)
      | `Expa x -> expansion_tok env x)
    xs
  |> unsafe_concat_tokens

let path (env : env) ((v1, v2) : CST.path) : path =
  let first_tok =
    match v1 with
    | `Pat_1167a92 tok -> token env tok (* pattern [^-\s\$] *)
    | `Expa x -> expansion_tok env x
  in
  let more_toks =
    List.map
      (fun x ->
        match x with
        | `Pat_0c7fc22 tok -> token env tok (* pattern [^\s\$]+ *)
        | `Expa x -> expansion_tok env x)
      v2
  in
  let tok = PI.combine_infos first_tok more_toks in
  (PI.str_of_info tok, tok)

let image_digest (env : env) ((v1, v2) : CST.image_digest) =
  let at = token env v1 (* "@" *) in
  let digest =
    List.map
      (fun x ->
        match x with
        | `Imm_tok_pat_d2727a0 tok -> token env tok (* pattern [a-zA-Z0-9:]+ *)
        | `Expa x -> expansion_tok env x)
      v2
    |> unsafe_concat_tokens
  in
  (at, digest)

let image_name (env : env) (xs : CST.image_name) =
  List.map
    (fun x ->
      match x with
      | `Pat_2b37705 tok -> token env tok (* pattern [^@:\s\$]+ *)
      | `Expa x -> expansion_tok env x)
    xs
  |> unsafe_concat_tokens

let image_alias (env : env) (xs : CST.image_alias) =
  List.map
    (fun x ->
      match x with
      | `Pat_9a14b5c tok -> token env tok (* pattern [-a-zA-Z0-9_]+ *)
      | `Expa x -> expansion_tok env x)
    xs
  |> unsafe_concat_tokens

let stopsignal_value (env : env) (xs : CST.stopsignal_value) : string wrap =
  List.map
    (fun x ->
      match x with
      | `Pat_441cd81 tok -> token env tok (* pattern [A-Z0-9]+ *)
      | `Expa x -> expansion_tok env x)
    xs
  |> unsafe_concat_tokens

let double_quoted_string (env : env) ((v1, v2, v3) : CST.double_quoted_string) :
    string wrap =
  let open_ = token env v1 (* "\"" *) in
  let contents =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_pat_589b0f8 tok ->
            token env tok (* pattern "[^\"\\n\\\\\\$]+" *)
        | `Esc_seq tok -> token env tok (* escape_sequence *)
        | `Expa x -> expansion_tok env x)
      v2
  in
  let close = [ token env v3 (* "\"" *) ] in
  concat_tokens open_ (contents @ close)

let shell_fragment (env : env) (xs : CST.shell_fragment) : tok =
  List.map
    (fun x ->
      match x with
      | `Pat_4b81dfc tok -> (* pattern [^\\\[\n#\s][^\\\n]* *) token env tok
      | `Pat_f05eb95 tok -> (* pattern \\[^\n] *) token env tok)
    xs
  |> unsafe_concat_tokens |> snd

let image_spec (env : env) ((v1, v2, v3) : CST.image_spec) : image_spec =
  let name = image_name env v1 in
  let tag =
    match v2 with
    | Some x -> Some (image_tag env x)
    | None -> None
  in
  let digest =
    match v3 with
    | Some x -> Some (image_digest env x)
    | None -> None
  in
  let loc =
    let start = wrap_loc name in
    let end_ = start in
    let end_ =
      match tag with
      | None -> end_
      | Some (_, x) -> wrap_loc x
    in
    let end_ =
      match digest with
      | None -> end_
      | Some (_, x) -> wrap_loc x
    in
    Loc.range start end_
  in
  { loc; name; tag; digest }

let array_element (env : env) (x : CST.array_element) : array_elt =
  match x with
  | `Double_quoted_str x -> Arr_string (double_quoted_string env x)
  | `Semg_ellips tok -> Arr_ellipsis (token env tok)
  | `Semg_meta tok -> Arr_metavar (str env tok)

let string (env : env) (x : CST.anon_choice_double_quoted_str_6b200ac) :
    string wrap =
  match x with
  | `Double_quoted_str x -> double_quoted_string env x
  | `Unqu_str x -> unquoted_string env x

let string_array (env : env) ((v1, v2, v3) : CST.string_array) :
    Loc.t * string_array =
  let open_ = token env v1 (* "[" *) in
  let argv =
    match v2 with
    | Some (v1, v2) ->
        let x0 = array_element env v1 in
        let xs =
          List.map
            (fun (v1, v2) ->
              let _comma = token env v1 (* "," *) in
              let arg = array_element env v2 in
              arg)
            v2
        in
        x0 :: xs
    | None -> []
  in
  let close = token env v3 (* "]" *) in
  let loc = (open_, close) in
  (loc, (open_, argv, close))

let env_pair (env : env) ((v1, v2, v3) : CST.env_pair) : label_pair =
  let k = str env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *) in
  let eq = token env v2 (* "=" *) in
  let v = string env v3 in
  (k, eq, v)

let spaced_env_pair (env : env) ((v1, v2, v3) : CST.spaced_env_pair) :
    label_pair =
  let k = str env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *) in
  let blank = token env v2 (* pattern \s+ *) in
  let v = string env v3 in
  (k, blank, v)

let label_pair (env : env) ((v1, v2, v3) : CST.label_pair) : label_pair =
  let key = str env v1 (* pattern [-a-zA-Z0-9\._]+ *) in
  let eq = token env v2 (* "=" *) in
  let value = string env v3 in
  (key, eq, value)

(* hack to obtain correct locations when parsing a string extracted from
   a larger file. *)
let shift_locations (str, tok) =
  let line (* 0-based *) = max 0 (PI.line_of_info tok - 1) (* 1-based *) in
  let column (* 0-based *) = max 0 (PI.col_of_info tok) in
  String.make line '\n' ^ String.make column ' ' ^ str

let parse_bash (env : env) shell_cmd : AST_bash.blist option =
  let input_kind, _ = env.extra in
  let ts_res =
    H.wrap_parser
      (fun () ->
        let str = shift_locations shell_cmd in
        Tree_sitter_bash.Parse.string str)
      (fun cst ->
        let bash_env : Parse_bash_tree_sitter.env =
          { env with extra = input_kind }
        in
        Parse_bash_tree_sitter.program bash_env ~tok:(snd shell_cmd) cst)
  in
  (* TODO: don't ignore tree-sitter parsing errors. See Parsing_result
     module of ocaml-tree-sitter-core. *)
  ts_res.program

(* This is for reconstructing a shell snippet and preserve line/column
   location.
*)
let comment_line (env : env)
    (((hash_tok, comment_tok), backslash_tok) : CST.comment_line) : tok =
  let tok =
    PI.combine_infos (token env hash_tok)
      [ token env comment_tok; token env backslash_tok ]
  in
  (* TODO: the token called backslash_tok should be a newline according
     to the grammar, not a backslash. Looks like a bug in the parser.
     We have to add the newline here to end the comment and get correct
     line locations. *)
  PI.tok_add_s "\n" tok

let argv_or_shell (env : env) (x : CST.anon_choice_str_array_878ad0b) =
  match x with
  | `Str_array x ->
      let loc, ar = string_array env x in
      Argv (loc, ar)
  | `Shell_cmd (v1, v2) -> (
      (* Stitch back the fragments together, then parse using the correct
         shell language. *)
      let first_frag = shell_fragment env v1 in
      let more_frags =
        List.map
          (fun (v1, comment_lines, v3) ->
            (* Keep the line continuation so as to preserve the original
               locations when parsing the shell command.

               Warning: dockerfile line continuation character may be different
               than '\'. Since we reinject a line continuation into
               the shell code to preserve locations, we must ensure that
               we inject a backslash, not whatever dockerfile is using.
            *)
            let dockerfile_line_cont =
              (* dockerfile's line continuation character without \n *)
              token env v1
            in
            let shell_line_cont =
              (* we would omit this if it weren't for preserving
                 line numbers *)
              PI.rewrap_str "\\\n" dockerfile_line_cont
            in
            let comment_lines = Common.map (comment_line env) comment_lines in
            let shell_frag = shell_fragment env v3 in
            (shell_line_cont :: comment_lines) @ [ shell_frag ])
          v2
        |> List.flatten
      in
      let raw_shell_code = concat_tokens first_frag more_frags in
      let _, shell_compat = env.extra in
      match shell_compat with
      | Sh -> (
          match parse_bash env raw_shell_code with
          | Some bash_program ->
              let loc = wrap_loc raw_shell_code in
              Sh_command (loc, bash_program)
          | None -> Other_shell_command (Sh, raw_shell_code))
      | (Cmd | Powershell | Other _) as shell ->
          Other_shell_command (shell, raw_shell_code))

let runlike_instruction (env : env) name cmd =
  let name = str env name (* RUN, CMD, ... *) in
  let cmd = argv_or_shell env cmd in
  let _, end_ = argv_or_shell_loc cmd in
  let loc = (wrap_tok name, end_) in
  (loc, name, cmd)

let rec instruction (env : env) (x : CST.instruction) : env * instruction =
  match x with
  | `Semg_ellips tok -> (* "..." *) (env, Instr_semgrep_ellipsis (token env tok))
  | `Semg_meta tok ->
      (* pattern \$[A-Z_][A-Z_0-9]* *)
      (env, Instr_semgrep_metavar (str env tok))
  | `Choice_from_inst x -> (
      match x with
      | `From_inst (v1, v2, v3, v4) ->
          let name = str env v1 (* pattern [fF][rR][oO][mM] *) in
          let loc =
            let tok = snd name in
            (tok, tok)
          in
          let param, loc =
            match v2 with
            | Some x ->
                let param = param env x in
                (Some param, Loc.range loc (param_loc param))
            | None -> (None, loc)
          in
          let image_spec = image_spec env v3 in
          let loc = Loc.range loc (image_spec_loc image_spec) in
          let alias, loc =
            match v4 with
            | Some (v1, v2) ->
                let as_ = token env v1 (* pattern [aA][sS] *) in
                let alias = image_alias env v2 in
                (Some (as_, alias), Loc.extend loc (snd alias))
            | None -> (None, loc)
          in
          (env, From (loc, name, param, image_spec, alias))
      | `Run_inst (v1, v2) ->
          let loc, name, cmd = runlike_instruction (env : env) v1 v2 in
          (env, Run (loc, name, cmd))
      | `Cmd_inst (v1, v2) ->
          let loc, name, cmd = runlike_instruction (env : env) v1 v2 in
          (env, Cmd (loc, name, cmd))
      | `Label_inst (v1, v2) ->
          let name = str env v1 (* pattern [lL][aA][bB][eE][lL] *) in
          let label_pairs = List.map (label_pair env) v2 in
          let loc = Loc.of_list label_pair_loc label_pairs in
          let loc = Loc.extend loc (snd name) in
          (env, Label (loc, name, label_pairs))
      | `Expose_inst (v1, v2) ->
          let name = str env v1 (* pattern [eE][xX][pP][oO][sS][eE] *) in
          let port_protos =
            List.map
              (fun x ->
                match x with
                | `Expose_port x -> expose_port env x
                | `Expa x -> expansion env x)
              v2
          in
          let _, end_ = Loc.of_toks wrap_tok port_protos in
          let loc = (wrap_tok name, end_) in
          (env, Expose (loc, name, port_protos))
      | `Env_inst (v1, v2) ->
          let name = str env v1 (* pattern [eE][nN][vV] *) in
          let pairs =
            match v2 with
            | `Rep1_env_pair xs -> List.map (env_pair env) xs
            | `Spaced_env_pair x -> [ spaced_env_pair env x ]
          in
          let _, end_ = Loc.of_list label_pair_loc pairs in
          let loc = (wrap_tok name, end_) in
          (env, Env (loc, name, pairs))
      | `Add_inst (v1, v2, v3, v4, v5) ->
          let name = str env v1 (* pattern [aA][dD][dD] *) in
          let param =
            match v2 with
            | Some x -> Some (param env x)
            | None -> None
          in
          let src = path env v3 in
          let _blank = token env v4 (* pattern [\t ]+ *) in
          let dst = path env v5 in
          let loc = (wrap_tok name, wrap_tok dst) in
          (env, Add (loc, name, param, src, dst))
      | `Copy_inst (v1, v2, v3, v4, v5) ->
          (*
             COPY is the same as ADD but with less magic in the interpretation
             of the arguments.
             See https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#add-or-copy
          *)
          let name = str env v1 (* pattern [cC][oO][pP][yY] *) in
          let param =
            match v2 with
            | Some x -> Some (param env x)
            | None -> None
          in
          let src = path env v3 in
          let _blank = token env v4 (* pattern [\t ]+ *) in
          let dst = path env v5 in
          let loc = (wrap_tok name, wrap_tok dst) in
          (env, Copy (loc, name, param, src, dst))
      | `Entr_inst (v1, v2) ->
          let loc, name, cmd = runlike_instruction (env : env) v1 v2 in
          (env, Entrypoint (loc, name, cmd))
      | `Volume_inst (v1, v2) ->
          let name = str env v1 (* pattern [vV][oO][lL][uU][mM][eE] *) in
          let args =
            match v2 with
            | `Str_array x ->
                let loc, ar = string_array env x in
                Array (loc, ar)
            | `Path_rep_non_nl_whit_path (v1, v2) ->
                let path0 = path env v1 in
                let paths =
                  List.map
                    (fun (v1, v2) ->
                      let _blank = token env v1 (* pattern [\t ]+ *) in
                      path env v2)
                    v2
                in
                let paths = path0 :: paths in
                let loc = Loc.of_toks wrap_tok paths in
                Paths (loc, paths)
          in
          let loc = Loc.extend (array_or_paths_loc args) (wrap_tok name) in
          (env, Volume (loc, name, args))
      | `User_inst (v1, v2, v3) ->
          let name = str env v1 (* pattern [uU][sS][eE][rR] *) in
          let user = user_name_or_group env v2 in
          let end_ = wrap_tok user in
          let opt_group, end_ =
            match v3 with
            | Some (v1, v2) ->
                let colon = token env v1 (* ":" *) in
                let group = user_name_or_group env v2 in
                (Some (colon, group), wrap_tok group)
            | None -> (None, end_)
          in
          let loc = (wrap_tok name, end_) in
          (env, User (loc, name, user, opt_group))
      | `Work_inst (v1, v2) ->
          let name = str env v1 (* pattern [wW][oO][rR][kK][dD][iI][rR] *) in
          let dir = path env v2 in
          let loc = (wrap_tok name, wrap_tok dir) in
          (env, Workdir (loc, name, dir))
      | `Arg_inst (v1, v2, v3) ->
          let name = str env v1 (* pattern [aA][rR][gG] *) in
          let key = str env v2 (* pattern [a-zA-Z0-9_]+ *) in
          let loc = (wrap_tok name, wrap_tok key) in
          let opt_value, loc =
            match v3 with
            | Some (v1, v2) ->
                let eq = token env v1 (* "=" *) in
                let value = string env v2 in
                (Some (eq, value), Loc.extend loc (wrap_tok value))
            | None -> (None, loc)
          in
          (env, Arg (loc, name, key, opt_value))
      | `Onbu_inst (v1, v2) ->
          let name = str env v1 (* pattern [oO][nN][bB][uU][iI][lL][dD] *) in
          let _env, instr = instruction env v2 in
          let _, end_ = instruction_loc instr in
          let loc = (wrap_tok name, end_) in
          (env, Onbuild (loc, name, instr))
      | `Stop_inst (v1, v2) ->
          let name =
            str env v1
            (* pattern [sS][tT][oO][pP][sS][iI][gG][nN][aA][lL] *)
          in
          let signal = stopsignal_value env v2 in
          let loc = (wrap_tok name, wrap_tok signal) in
          (env, Stopsignal (loc, name, signal))
      | `Heal_inst (v1, v2) ->
          let name =
            str env v1
            (* pattern [hH][eE][aA][lL][tT][hH][cC][hH][eE][cC][kK] *)
          in
          let arg =
            match v2 with
            | `NONE tok -> Healthcheck_none (token env tok (* "NONE" *))
            | `Rep_param_cmd_inst (v1, (name (* CMD *), args)) ->
                let params = List.map (param env) v1 in
                let params_loc = Loc.of_list param_loc params in
                let cmd_loc, name, args = runlike_instruction env name args in
                let loc = Loc.range params_loc cmd_loc in
                Healthcheck_cmd (loc, params, (cmd_loc, name, args))
          in
          let loc = Loc.extend (healthcheck_loc arg) (wrap_tok name) in
          (env, Healthcheck (loc, name, arg))
      | `Shell_inst (v1, v2) ->
          let ((_, start_tok) as name) =
            str env v1
            (* pattern [sS][hH][eE][lL][lL] *)
          in
          let cmd_loc, cmd = string_array env v2 in
          let env =
            match classify_shell cmd with
            | None -> env
            | Some shell_compat ->
                let input_kind, _cur_shell = env.extra in
                { env with extra = (input_kind, shell_compat) }
          in
          let _, end_tok = cmd_loc in
          let loc = (start_tok, end_tok) in
          (env, Shell (loc, name, cmd))
      | `Main_inst (v1, v2) ->
          (* deprecated feature *)
          let name =
            str env v1
            (* pattern [mM][aA][iI][nN][tT][aA][iI][nN][eE][rR] *)
          in
          let maintainer = str env v2 (* pattern .* *) in
          let loc = (wrap_tok name, wrap_tok maintainer) in
          (env, Maintainer (loc, name, maintainer))
      | `Cross_build_inst (v1, v2) ->
          (* undocumented *)
          let name =
            str env v1
            (* pattern [cC][rR][oO][sS][sS]_[bB][uU][iI][lL][dD][a-zA-Z_]* *)
          in
          let data = str env v2 (* pattern .* *) in
          let loc = (wrap_tok name, wrap_tok data) in
          (env, Cross_build_xxx (loc, name, data)))

let source_file (env : env) (xs : CST.source_file) =
  let _env, instrs =
    List.fold_left
      (fun (env, instrs) (v1, v2) ->
        let acc =
          match v1 with
          | `Inst x ->
              let env, instr = instruction env x in
              (env, instr :: instrs)
          | `Comm tok ->
              let _comment = (* pattern #.* *) token env tok in
              (env, instrs)
        in
        let _newline = token env v2 (* "\n" *) in
        acc)
      (env, []) xs
  in
  List.rev instrs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  let input_kind = AST_bash.Program in
  H.wrap_parser
    (fun () -> Tree_sitter_dockerfile.Parse.file file)
    (fun cst ->
      let env =
        { H.file; conv = H.line_col_to_pos file; extra = (input_kind, Sh) }
      in
      let dockerfile_ast = source_file env cst in
      Dockerfile_to_generic.(program Program dockerfile_ast))

let parse_dockerfile_pattern str =
  let input_kind = AST_bash.Pattern in
  H.wrap_parser
    (fun () -> Tree_sitter_dockerfile.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = (input_kind, Sh) } in
      let dockerfile_ast = source_file env cst in
      Dockerfile_to_generic.(any input_kind dockerfile_ast))

let parse_pattern str =
  let dockerfile_res = parse_dockerfile_pattern str in
  if dockerfile_res.errors = [] then dockerfile_res
  else
    let bash_res = Parse_bash_tree_sitter.parse_pattern str in
    if bash_res.errors = [] then bash_res else dockerfile_res
