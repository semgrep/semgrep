(**
   Mapping from tree-sitter-dockerfile tree-sitter's CST to the Dockerfile
   AST type, which itself includes nodes of the Bash AST.

   Derived from generated code 'dockerfile/lib/Boilerplate.ml'
*)

open! Common
module AST = AST_dockerfile
module CST = Tree_sitter_dockerfile.CST
open AST_dockerfile
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
  let tok = Tok.combine_toks first_tok other_toks in
  (Tok.content_of_tok tok, tok)

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
        (s, Tok.unsafe_fake_tok s)

(*
   Collapse consecutive literal string fragments.

   This is useful to detect special fragments that otherwise could get split,
   such as the ellipsis for COPY/ADD that get split into "." and "..".
*)
let simplify_fragments (fragments : string_fragment list) : string_fragment list
    =
  let concat toks tail =
    match toks with
    | [] -> tail
    | first :: others ->
        let tok = Tok.combine_toks first others in
        String_content (Tok.content_of_tok tok, tok) :: tail
  in
  let rec simplify acc = function
    | [] -> concat (List.rev acc) []
    | String_content (_, tok) :: xs -> simplify (tok :: acc) xs
    | special :: xs -> concat (List.rev acc) (special :: simplify [] xs)
  in
  simplify [] fragments

(* best effort to extract the name of the shell *)
let classify_shell ((_open, ar, _close) : string_array) :
    shell_compatibility option =
  let command =
    match ar with
    | Arr_string (_, [ String_content ("/usr/bin/env", _) ])
      :: Arr_string (_loc, [ String_content (name, _) ])
      :: _ ->
        Some name
    | Arr_string (_loc, [ String_content (path, _) ]) :: _ -> Some path
    | _ -> None
  in
  match command with
  | Some ("/bin/bash" | "/bin/sh" | "bash" | "sh") -> Some Sh
  | Some "cmd" -> Some Cmd
  | Some "powershell" -> Some Powershell
  | Some name -> Some (Other name)
  | None -> None

let is_metavar (env : env) (x : string wrap) =
  match env.extra with
  | Pattern, _ when AST_generic.is_metavar_name (fst x) -> true
  | _ -> false

(*
   Return the position of the first non-blank character, if any.
   This implementation turns out to be simpler than using Pcre.
*)
let find_nonblank (s : string) =
  let pos = ref 0 in
  try
    for i = 0 to String.length s - 1 do
      pos := i;
      match s.[i] with
      | ' '
      | '\t'
      | '\r'
      | '\n' ->
          ()
      | _ -> raise Exit
    done;
    None
  with
  | Exit -> Some !pos

let remove_blank_prefix (x : string wrap) : string wrap =
  let s, tok = x in
  match find_nonblank s with
  | None -> x
  | Some pos ->
      let _blanks, tok = Tok.split_tok_at_bytepos pos tok in
      (Tok.content_of_tok tok, tok)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(*
   $FOO or ${FOO} expansion of a variable passed at docker build time.

   The scope of an ARG is a stage, i.e. the section between a FROM instruction
   and the next FROM instruction or the end of the file.

   The braceless syntax $FOO, like we do for Bash, is co-opted for Semgrep
   metavariables when parsing a Semgrep pattern. In a pattern, ${FOO}
   is the expansion of the FOO argument while ${$FOO} is the expansion
   of any argument represented by the metavariable $FOO.
*)
let expansion (env : env) ((v1, v2) : CST.expansion) : string_fragment =
  let dollar = token env v1 (* "$" *) in
  match v2 with
  | `Var tok -> (
      let name = str env tok (* pattern [a-zA-Z][a-zA-Z0-9_]* *) in
      let mv_tok = Tok.combine_toks dollar [ snd name ] in
      let mv_s = Tok.content_of_tok mv_tok in
      match env.extra with
      | Pattern, _ when AST_generic.is_metavar_name mv_s ->
          Frag_semgrep_metavar (mv_s, mv_tok)
      | _ ->
          let loc = (dollar, wrap_tok name) in
          Expansion (loc, Expand_var name))
  | `Imm_tok_lcurl_imm_tok_pat_8713919_imm_tok_rcurl (v1, v2, v3) ->
      let _open = token env v1 (* "{" *) in
      let var_or_mv = str env v2 (* pattern [^\}]+ *) in
      let name, _tok = var_or_mv in
      let expansion =
        match env.extra with
        | Pattern, _ when AST_generic.is_metavar_name name ->
            Expand_semgrep_metavar var_or_mv
        | _ -> Expand_var var_or_mv
      in
      let close = token env v3 (* "}" *) in
      let loc = (dollar, close) in
      Expansion (loc, expansion)

let param (env : env) ((v1, v2, v3, v4) : CST.param) : param =
  let dashdash = token env v1 (* "--" *) in
  let key = str env v2 (* pattern [a-z][-a-z]* *) in
  let equal = token env v3 (* "=" *) in
  let value = str env v4 (* pattern [^\s]+ *) in
  let loc = (dashdash, snd value) in
  (loc, (dashdash, key, equal, value))

let expose_port (env : env) (x : CST.expose_port) : expose_port =
  match x with
  | `Semg_ellips tok -> Expose_semgrep_ellipsis (token env tok (* "..." *))
  | `Pat_217c202_opt_choice_SLAS (v1, v2) ->
      let port_tok = token env v1 (* pattern \d+ *) in
      let protocol =
        match v2 with
        | Some x ->
            let tok =
              match x with
              | `SLAS_ce91595 tok -> token env tok (* "/tcp" *)
              | `SLAS_c773c8d tok -> token env tok (* "/udp" *)
            in
            Some (Tok.content_of_tok tok, tok)
        | None -> None
      in
      let port_num = port_tok |> Tok.content_of_tok in
      Expose_port ((port_num, port_tok), protocol)

let image_tag (env : env) ((v1, v2) : CST.image_tag) : tok * str =
  let colon = token env v1 (* ":" *) in
  let tag =
    match v2 with
    | [] ->
        let loc = (colon, colon) in
        (loc, [])
    | fragments ->
        let fragments =
          fragments
          |> Common.map (fun x ->
                 match x with
                 | `Imm_tok_pat_bcfc287 tok ->
                     String_content (str env tok (* pattern [^@\s\$]+ *))
                 | `Imme_expa x -> expansion env x)
          |> simplify_fragments
        in
        let loc = Tok_range.of_list string_fragment_loc fragments in
        (loc, fragments)
  in
  (colon, tag)

let image_digest (env : env) ((v1, v2) : CST.image_digest) : tok * str =
  let at = token env v1 (* "@" *) in
  let digest =
    match v2 with
    | [] ->
        let loc = (at, at) in
        (loc, [])
    | fragments ->
        let fragments =
          fragments
          |> Common.map (fun x ->
                 match x with
                 | `Imm_tok_pat_d2727a0 tok ->
                     String_content (str env tok (* pattern [a-zA-Z0-9:]+ *))
                 | `Imme_expa x -> expansion env x)
          |> simplify_fragments
        in
        let loc = Tok_range.of_list string_fragment_loc fragments in
        (loc, fragments)
  in
  (at, digest)

let image_name (env : env) ((x, xs) : CST.image_name) =
  let first_fragment =
    match x with
    | `Pat_8165e5f tok -> String_content (str env tok (* pattern [^@:\s\$-]+ *))
    | `Expa x -> expansion env x
  in
  let fragments =
    xs
    |> Common.map (fun x ->
           match x with
           | `Imm_tok_pat_2b37705 tok ->
               String_content (str env tok (* pattern [^@:\s\$]+ *))
           | `Imme_expa x -> expansion env x)
  in
  let fragments = first_fragment :: fragments |> simplify_fragments in
  let loc = Tok_range.of_list string_fragment_loc fragments in
  (loc, fragments)

let image_alias (env : env) ((x, xs) : CST.image_alias) : str =
  let first_fragment =
    match x with
    | `Pat_9a14b5c tok -> String_content (str env tok)
    | `Expa x -> expansion env x
  in
  let other_fragments =
    xs
    |> Common.map (fun x ->
           match x with
           | `Imm_tok_pat_9a14b5c tok ->
               String_content (str env tok (* pattern [-a-zA-Z0-9_]+ *))
           | `Imme_expa x -> expansion env x)
  in
  let fragments = first_fragment :: other_fragments |> simplify_fragments in
  let loc = Tok_range.of_list string_fragment_loc fragments in
  (loc, fragments)

let immediate_user_name_or_group_fragment (env : env)
    (x : CST.immediate_user_name_or_group_fragment) : string_fragment =
  match x with
  | `Imm_tok_pat_7642c4f tok ->
      String_content (str env tok (* pattern ([a-zA-Z][-a-zA-Z0-9_]*|[0-9]+) *))
  | `Imme_expa x -> expansion env x

let immediate_user_name_or_group (env : env)
    (xs : CST.immediate_user_name_or_group) : str =
  let fragments = Common.map (immediate_user_name_or_group_fragment env) xs in
  let loc = Tok_range.of_list string_fragment_loc fragments in
  (loc, fragments)

let user_name_or_group (env : env) ((x, xs) : CST.user_name_or_group) : str =
  let head =
    match x with
    | `Pat_05444c2 tok -> String_content (str env tok)
    | `Expa x -> expansion env x
  in
  let tail = Common.map (immediate_user_name_or_group_fragment env) xs in
  let fragments = head :: tail |> simplify_fragments in
  let loc = Tok_range.of_list string_fragment_loc fragments in
  (loc, fragments)

let unquoted_string (env : env) (xs : CST.unquoted_string) : str =
  let fragments =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_pat_9f6bbb9 tok ->
            String_content (str env tok (* pattern "[^\\s\\n\\\"\\\\\\$]+" *))
        | `Imm_tok_bsla tok -> String_content (str env tok (* "\\ " *))
        | `Imme_expa x -> expansion env x)
      xs
    |> simplify_fragments
  in
  let loc = Tok_range.of_list string_fragment_loc fragments in
  (loc, fragments)

let path0 (env : env) ((v1, v2) : CST.path) : string_fragment list =
  let first_fragment =
    match v1 with
    | `Pat_1167a92 tok -> String_content (str env tok (* pattern [^-\s\$] *))
    | `Expa x -> expansion env x
  in
  let more_fragments =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_pat_0c7fc22 tok ->
            String_content (str env tok (* pattern [^\s\$]+ *))
        | `Imme_expa x -> expansion env x)
      v2
  in
  first_fragment :: more_fragments |> simplify_fragments

let path (env : env) (x : CST.path) : str =
  let fragments = path0 env x in
  let loc = Tok_range.of_list string_fragment_loc fragments in
  (loc, fragments)

let path_or_ellipsis (env : env) (x : CST.path) : str_or_ellipsis =
  match (env.extra, path0 env x) with
  | (Pattern, _), [ String_content ("...", tok) ] -> Str_semgrep_ellipsis tok
  | _, fragments ->
      let loc = Tok_range.of_list string_fragment_loc fragments in
      Str_str (loc, fragments)

let stopsignal_value (env : env) ((x, xs) : CST.stopsignal_value) : str =
  let first_fragment =
    match x with
    | `Pat_441cd81 tok -> String_content (str env tok)
    | `Expa x -> expansion env x
  in
  let other_fragments =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_pat_441cd81 tok ->
            String_content (str env tok (* pattern [A-Z0-9]+ *))
        | `Imme_expa x -> expansion env x)
      xs
  in
  let fragments = first_fragment :: other_fragments |> simplify_fragments in
  let loc = Tok_range.of_list string_fragment_loc fragments in
  (loc, fragments)

let double_quoted_string (env : env) ((v1, v2, v3) : CST.double_quoted_string) :
    str =
  let open_ = str env v1 (* "\"" *) in
  let contents =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_pat_589b0f8 tok ->
            let s = str env tok (* pattern "[^\"\\n\\\\\\$]+" *) in
            String_content s
        | `Double_quoted_esc_seq tok ->
            let s = str env tok (* escape_sequence *) in
            String_content s
        | `BSLASH tok ->
            let s = str env tok in
            String_content s (* lone, unescaped backslash *)
        | `Imme_expa x -> expansion env x)
      v2
  in
  let close = str env v3 (* "\"" *) in
  let loc = (wrap_tok open_, wrap_tok close) in
  let fragments =
    (String_content open_ :: contents) @ [ String_content close ]
    |> simplify_fragments
  in
  (loc, fragments)

let single_quoted_string (env : env) ((v1, v2, v3) : CST.single_quoted_string) :
    str =
  let open_ = str env v1 (* "'" *) in
  let contents =
    Common.map
      (fun x ->
        match x with
        | `Imm_tok_pat_0ab9261 tok ->
            let s = str env tok (* literal characters *) in
            String_content s
        | `Single_quoted_esc_seq tok ->
            let s = str env tok (* single_quoted_escape_sequence *) in
            String_content s
        | `BSLASH tok ->
            let s = str env tok in
            String_content s (* lone, unescaped backslash *))
      v2
  in
  let close = str env v3 (* "'" *) in
  let loc = (wrap_tok open_, wrap_tok close) in
  let fragments =
    (String_content open_ :: contents) @ [ String_content close ]
    |> simplify_fragments
  in
  (loc, fragments)

let shell_fragment (env : env) (xs : CST.shell_fragment) : tok =
  Common.map
    (fun x ->
      match x with
      | `Pat_b1120d3 tok
      | `Pat_dea634e tok
      | `Pat_eda9032 tok ->
          token env tok)
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
    let start = str_loc name in
    let end_ = start in
    let end_ =
      match tag with
      | None -> end_
      | Some (_colon, x) -> str_loc x
    in
    let end_ =
      match digest with
      | None -> end_
      | Some (_at, x) -> str_loc x
    in
    Tok_range.range start end_
  in
  { loc; name; tag; digest }

let array_element (env : env) (x : CST.array_element) : array_elt =
  match x with
  | `Json_str (v1, v2, v3) ->
      let open_ = str env v1 (* "\"" *) in
      let contents =
        Common.map
          (fun x ->
            match x with
            | `Imm_tok_pat_3a2a380 tok ->
                let s = str env tok (* literal characters *) in
                String_content s
            | `Json_esc_seq tok ->
                let s = str env tok (* JSON escape sequence *) in
                String_content s)
          v2
      in
      let close = str env v3 (* "\"" *) in
      let loc = (wrap_tok open_, wrap_tok close) in
      let fragments =
        (String_content open_ :: contents) @ [ String_content close ]
        |> simplify_fragments
      in
      Arr_string (loc, fragments)
  | `Semg_ellips tok -> Arr_ellipsis (token env tok)
  | `Semg_meta tok -> Arr_metavar (str env tok)

let string (env : env) (x : CST.anon_choice_double_quoted_str_6156383) : str =
  match x with
  | `Double_quoted_str x -> double_quoted_string env x
  | `Single_quoted_str x -> single_quoted_string env x
  | `Unqu_str x -> unquoted_string env x

let json_string_array (env : env) ((v1, v2, v3) : CST.json_string_array) :
    Tok_range.t * string_array =
  let open_ = token env v1 (* "[" *) in
  let argv =
    match v2 with
    | Some (v1, v2) ->
        let x0 = array_element env v1 in
        let xs =
          Common.map
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

(*
   Create the empty token that sits right after a given token.

   TODO: move this function to Parse_info?
*)
let empty_token_after tok : tok =
  match Tok.loc_of_tok tok with
  | Ok loc ->
      let prev_len = String.length loc.str in
      let loc =
        {
          Tok.str = "";
          pos =
            {
              loc.pos with
              charpos = loc.pos.charpos + prev_len;
              column = loc.pos.column + prev_len;
            };
        }
      in
      Tok.tok_of_loc loc
  | Error _ -> Tok.rewrap_str "" tok

let env_pair (env : env) (x : CST.env_pair) : label_pair =
  match x with
  | `Semg_ellips tok -> Label_semgrep_ellipsis (token env tok (* "..." *))
  | `Env_key_imm_tok_eq_opt_choice_double_quoted_str (v1, v2, v3) ->
      let k =
        Var_ident (str env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *))
      in
      let eq = token env v2 (* "=" *) in
      let v =
        match v3 with
        | None ->
            (* the empty token gives us the correct location which we need
               even if we returned an empty list of fragments. *)
            let tok = empty_token_after eq in
            let loc = (tok, tok) in
            (loc, [ String_content (Tok.content_of_tok tok, tok) ])
        | Some x -> string env x
      in
      let loc = (var_or_metavar_tok k, str_loc v |> snd) in
      Label_pair (loc, k, eq, v)

let spaced_env_pair (env : env) ((v1, v2, v3) : CST.spaced_env_pair) :
    label_pair =
  let k =
    Var_ident (str env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *))
  in
  let blank = token env v2 (* pattern \s+ *) in
  let v = string env v3 in
  let loc = (var_or_metavar_tok k, str_loc v |> snd) in
  Label_pair (loc, k, blank, v)

let label_pair (env : env) (x : CST.label_pair) : label_pair =
  match x with
  | `Semg_ellips tok -> Label_semgrep_ellipsis (token env tok (* "..." *))
  | `Choice_semg_meta_imm_tok_eq_choice_double_quoted_str (v1, v2, v3) ->
      let key =
        match v1 with
        | `Semg_meta tok ->
            Var_semgrep_metavar (str env tok (* pattern \$[A-Z_][A-Z_0-9]* *))
        | `Pat_4128122 tok ->
            Var_ident (str env tok (* pattern [-a-zA-Z0-9\._]+ *))
      in
      let eq = token env v2 (* "=" *) in
      let value = string env v3 in
      let loc = (var_or_metavar_tok key, str_loc value |> snd) in
      Label_pair (loc, key, eq, value)

(* hack to obtain correct locations when parsing a string extracted from
   a larger file. *)
let shift_locations (str, tok) =
  let line (* 0-based *) = max 0 (Tok.line_of_tok tok - 1) (* 1-based *) in
  let column (* 0-based *) = max 0 (Tok.col_of_tok tok) in
  String.make line '\n' ^ String.make column ' ' ^ str

(* A plain ellipsis such as '...' (not e.g. '...;') is identified so
   that we can treat it as special dockerfile syntax rather than bash
   syntax.

   Alternatively, this can be done be extending the tree-sitter-dockerfile
   grammar.
*)
let is_plain_ellipsis =
  let rex = SPcre.regexp "\\A[ \t\r\n]*[.]{3}[ \t\r\n]*\\z" in
  fun s ->
    match SPcre.pmatch ~rex s with
    | Ok res -> res
    | Error _err -> false

type ellipsis_or_bash =
  | Semgrep_ellipsis of tok
  | Bash of AST_bash.blist option

let parse_bash (env : env) shell_cmd : ellipsis_or_bash =
  let input_kind, _ = env.extra in
  match input_kind with
  | Pattern when is_plain_ellipsis (fst shell_cmd) ->
      Semgrep_ellipsis (snd shell_cmd)
  | _ ->
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
      Bash ts_res.program

(* This is for reconstructing a shell snippet and preserve line/column
   location.
*)
let comment_line (env : env)
    (((hash_tok, comment_tok), backslash_tok) : CST.comment_line) : tok =
  let tok =
    Tok.combine_toks (token env hash_tok)
      [ token env comment_tok; token env backslash_tok ]
  in
  (* TODO: the token called backslash_tok should be a newline according
     to the grammar, not a backslash. Looks like a bug in the parser.
     We have to add the newline here to end the comment and get correct
     line locations. *)
  Tok.tok_add_s "\n" tok

let shell_command (env : env) (x : CST.shell_command) =
  match x with
  | `Semg_ellips tok -> Command_semgrep_ellipsis (token env tok)
  | `Rep_comm_line_shell_frag_rep_requ_line_cont_rep_comm_line_shell_frag
      (v1, v2, v3) -> (
      (* Stitch back the fragments together, then parse using the correct
         shell language. *)
      let _comment_lines = Common.map (comment_line env) v1 in
      let first_frag = shell_fragment env v2 in
      let more_frags =
        v3
        |> Common.map (fun (v1, comment_lines, v3) ->
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
                 Tok.rewrap_str "\\\n" dockerfile_line_cont
               in
               let comment_lines =
                 Common.map (comment_line env) comment_lines
               in
               let shell_frag = shell_fragment env v3 in
               (shell_line_cont :: comment_lines) @ [ shell_frag ])
        |> List.flatten
      in
      let raw_shell_code = concat_tokens first_frag more_frags in
      let _, shell_compat = env.extra in
      match shell_compat with
      | Sh -> (
          match parse_bash env raw_shell_code with
          | Semgrep_ellipsis tok -> Command_semgrep_ellipsis tok
          | Bash (Some bash_program) ->
              let loc = wrap_loc raw_shell_code in
              Sh_command (loc, bash_program)
          | Bash None -> Other_shell_command (Sh, raw_shell_code))
      | (Cmd | Powershell | Other _) as shell ->
          Other_shell_command (shell, raw_shell_code))

let argv_or_shell (env : env) (x : CST.anon_choice_json_str_array_0106ace) =
  match x with
  | `Json_str_array x ->
      let loc, ar = json_string_array env x in
      Argv (loc, ar)
  | `Shell_cmd x -> shell_command env x

let mount_param_param (env : env) ((v1, v2, v3) : CST.mount_param_param) =
  let key = str env v1 in
  let _eq = token env v2 in
  let value = str env v3 in
  let loc = (snd key, snd value) in
  (loc, key, value)

let rec unsafe_list_last = function
  | [] -> assert false
  | [ x ] -> x
  | _ :: xs -> unsafe_list_last xs

let mount_param (env : env) ((v1, v2, v3, v4, v5) : CST.mount_param) =
  let start = (* "--" *) token env v1 in
  let mount = str env v2 in
  let _eq = token env v3 in
  let param1 = mount_param_param env v4 in
  let params =
    Common.map
      (fun (v1, v2) ->
        let _comma = token env v1 in
        let kv = mount_param_param env v2 in
        kv)
      v5
  in
  let params = param1 :: params in
  let (_, end_), _k, _v = unsafe_list_last params in
  let loc = (start, end_) in
  Mount_param (loc, mount, params)

let runlike_instruction (env : env) name params cmd =
  let name = str env name (* RUN, CMD, ... *) in
  let params =
    Common.map
      (fun x ->
        match x with
        | `Param x -> Param (param env x)
        | `Mount_param x -> mount_param env x)
      params
  in
  let cmd = argv_or_shell env cmd in
  let _, end_ = argv_or_shell_loc cmd in
  let loc = (wrap_tok name, end_) in
  (loc, name, params, cmd)

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
                (Some param, Tok_range.range loc (param_loc param))
            | None -> (None, loc)
          in
          let image_spec = image_spec env v3 in
          let loc = Tok_range.range loc (image_spec_loc image_spec) in
          let alias, loc =
            match v4 with
            | Some (v1, v2) ->
                let as_ = token env v1 (* pattern [aA][sS] *) in
                let alias = image_alias env v2 in
                (Some (as_, alias), Tok_range.union loc (str_loc alias))
            | None -> (None, loc)
          in
          (env, From (loc, name, param, image_spec, alias))
      | `Run_inst (v1, v2, v3) ->
          let loc, name, params, cmd =
            runlike_instruction (env : env) v1 v2 v3
          in
          (env, Run (loc, name, params, cmd))
      | `Cmd_inst (v1, v2) ->
          let loc, name, params, cmd =
            runlike_instruction (env : env) v1 [] v2
          in
          (env, Cmd (loc, name, params, cmd))
      | `Label_inst (v1, v2) ->
          let name = str env v1 (* pattern [lL][aA][bB][eE][lL] *) in
          let label_pairs = Common.map (label_pair env) v2 in
          let loc = Tok_range.of_list label_pair_loc label_pairs in
          let loc = Tok_range.extend loc (snd name) in
          (env, Label (loc, name, label_pairs))
      | `Expose_inst (v1, v2) ->
          let name = str env v1 (* pattern [eE][xX][pP][oO][sS][eE] *) in
          let port_protos =
            Common.map
              (fun x ->
                match x with
                | `Expose_port x -> expose_port env x
                | `Expa x -> Expose_fragment (expansion env x))
              v2
          in
          let _, end_ = Tok_range.of_list expose_port_loc port_protos in
          let loc = (wrap_tok name, end_) in
          (env, Expose (loc, name, port_protos))
      | `Env_inst (v1, v2) ->
          let name = str env v1 (* pattern [eE][nN][vV] *) in
          let pairs =
            match v2 with
            | `Rep1_env_pair xs -> Common.map (env_pair env) xs
            | `Spaced_env_pair x -> [ spaced_env_pair env x ]
          in
          let _, end_ = Tok_range.of_list label_pair_loc pairs in
          let loc = (wrap_tok name, end_) in
          (env, Env (loc, name, pairs))
      | `Add_inst (v1, v2, v3, v4) ->
          let name = str env v1 (* pattern [aA][dD][dD] *) in
          let param =
            match v2 with
            | Some x -> Some (param env x)
            | None -> None
          in
          let src =
            v3
            |> Common.map (fun (v1, v2) ->
                   let _blank = token env v2 (* pattern [\t ]+ *) in
                   path_or_ellipsis env v1)
          in
          let dst = path env v4 in
          let loc = (wrap_tok name, str_loc dst |> snd) in
          (env, Add (loc, name, param, src, dst))
      | `Copy_inst (v1, v2, v3, v4) ->
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
          let src =
            v3
            |> Common.map (fun (v1, v2) ->
                   let _blank = token env v2 (* pattern [\t ]+ *) in
                   path_or_ellipsis env v1)
          in
          let dst = path env v4 in
          let loc = (wrap_tok name, str_loc dst |> snd) in
          (env, Copy (loc, name, param, src, dst))
      | `Entr_inst (v1, v2) ->
          let loc, name, _params, cmd =
            runlike_instruction (env : env) v1 [] v2
          in
          (env, Entrypoint (loc, name, cmd))
      | `Volume_inst (v1, v2) ->
          let name = str env v1 (* pattern [vV][oO][lL][uU][mM][eE] *) in
          let args =
            match v2 with
            | `Json_str_array x ->
                let loc, ar = json_string_array env x in
                Array (loc, ar)
            | `Path_rep_non_nl_whit_path (v1, v2) ->
                let path0 = path_or_ellipsis env v1 in
                let paths =
                  Common.map
                    (fun (v1, v2) ->
                      let _blank = token env v1 (* pattern [\t ]+ *) in
                      path_or_ellipsis env v2)
                    v2
                in
                let paths = path0 :: paths in
                let loc = Tok_range.of_list str_or_ellipsis_loc paths in
                Paths (loc, paths)
          in
          let loc =
            Tok_range.extend (array_or_paths_loc args) (wrap_tok name)
          in
          (env, Volume (loc, name, args))
      | `User_inst (v1, v2, v3) ->
          let name = str env v1 (* pattern [uU][sS][eE][rR] *) in
          let user = user_name_or_group env v2 in
          let end_ = str_loc user |> snd in
          let opt_group, end_ =
            match v3 with
            | Some (v1, v2) ->
                let colon = token env v1 (* ":" *) in
                let group = immediate_user_name_or_group env v2 in
                (Some (colon, group), str_loc group |> snd)
            | None -> (None, end_)
          in
          let loc = (wrap_tok name, end_) in
          (env, User (loc, name, user, opt_group))
      | `Work_inst (v1, v2) ->
          let name = str env v1 (* pattern [wW][oO][rR][kK][dD][iI][rR] *) in
          let dir = path env v2 in
          let loc = (wrap_tok name, str_loc dir |> snd) in
          (env, Workdir (loc, name, dir))
      | `Arg_inst (v1, v2, v3) ->
          let name = str env v1 (* pattern [aA][rR][gG] *) in
          let key =
            match v2 with
            | `Semg_meta tok ->
                Var_semgrep_metavar
                  (str env tok (* pattern \$[A-Z_][A-Z_0-9]* *))
            | `Pat_4de4cb9 tok ->
                Var_ident (str env tok (* pattern [a-zA-Z0-9_]+ *))
          in
          let loc = (wrap_tok name, var_or_metavar_tok key) in
          let opt_value, loc =
            match v3 with
            | Some (v1, v2) ->
                let eq = token env v1 (* "=" *) in
                let value = string env v2 in
                (Some (eq, value), Tok_range.extend loc (str_loc value |> snd))
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
          let loc = (wrap_tok name, str_loc signal |> snd) in
          (env, Stopsignal (loc, name, signal))
      | `Heal_inst (v1, v2) ->
          let name =
            str env v1
            (* pattern [hH][eE][aA][lL][tT][hH][cC][hH][eE][cC][kK] *)
          in
          let arg =
            match v2 with
            | `Semg_meta tok ->
                Healthcheck_semgrep_metavar
                  (str env tok (* pattern \$[A-Z_][A-Z_0-9]* *))
            | `NONE tok -> Healthcheck_none (token env tok (* "NONE" *))
            | `Rep_param_cmd_inst (v1, (name (* CMD *), args)) ->
                let params = Common.map (param env) v1 in
                let params_loc = Tok_range.of_list param_loc params in
                let cmd_loc, name, run_params, args =
                  runlike_instruction env name [] args
                in
                let loc = Tok_range.range params_loc cmd_loc in
                Healthcheck_cmd (loc, params, (cmd_loc, name, run_params, args))
          in
          let loc = Tok_range.extend (healthcheck_loc arg) (wrap_tok name) in
          (env, Healthcheck (loc, name, arg))
      | `Shell_inst (v1, v2) ->
          let ((_, start_tok) as name) =
            str env v1
            (* pattern [sS][hH][eE][lL][lL] *)
          in
          let cmd_loc, cmd = json_string_array env v2 in
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
          let maintainer_data = str env v2 (* pattern .* *) in
          let maintainer = remove_blank_prefix maintainer_data in
          let loc = (wrap_tok name, wrap_tok maintainer) in
          let string_or_mv =
            if is_metavar env maintainer then Str_semgrep_metavar maintainer
            else Str_string maintainer
          in
          (env, Maintainer (loc, name, string_or_mv))
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
  H.wrap_parser
    (fun () -> Tree_sitter_dockerfile.Parse.file file)
    (fun cst ->
      let env =
        {
          H.file;
          conv = H.line_col_to_pos file;
          extra = (AST_bash.Program, Sh);
        }
      in
      source_file env cst)

let ensure_trailing_newline str =
  if str <> "" then
    let len = String.length str in
    match str.[len - 1] with
    | '\n' -> str
    | _ -> str ^ "\n"
  else str

let parse_pattern str =
  let input_kind = AST_bash.Pattern in
  H.wrap_parser
    (fun () ->
      (* tree-sitter-dockerfile parser requires a trailing newline.
         Not sure if it's intentional but we add one to simplify user
         experience. *)
      str |> ensure_trailing_newline |> Tree_sitter_dockerfile.Parse.string)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = (input_kind, Sh) } in
      source_file env cst)

(*
   The input can be a sequence of dockerfile instructions
   or a bash snippet.
*)
let parse_docker_or_bash_pattern str =
  let dockerfile_res = parse_pattern str in
  let any_opt =
    match dockerfile_res.program with
    | None -> None
    | Some program -> Some (Dockerfile_to_generic.any program)
  in
  let dockerfile_res = { dockerfile_res with program = any_opt } in
  if dockerfile_res.errors =*= [] then dockerfile_res
  else
    let bash_res = Parse_bash_tree_sitter.parse_pattern str in
    let any_opt =
      match bash_res.program with
      | None -> None
      | Some program -> Some (Bash_to_generic.any program)
    in
    if bash_res.errors =*= [] then { bash_res with program = any_opt }
    else dockerfile_res
