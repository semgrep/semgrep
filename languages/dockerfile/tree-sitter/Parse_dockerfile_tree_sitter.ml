(**
   Mapping from tree-sitter-dockerfile tree-sitter's CST to the Dockerfile
   AST type, which itself includes nodes of the Bash AST.

   Derived from generated code 'dockerfile/lib/Boilerplate.ml'
*)

open! Common
open Fpath_.Operators
module AST = AST_dockerfile
module CST = Tree_sitter_dockerfile.CST
open AST_dockerfile
module H = Parse_tree_sitter_helpers
open AST_dockerfile_loc (* provides functions that end in '_loc' or '_tok' *)
module R = Tree_sitter_run.Raw_tree
module Token = Tree_sitter_run.Token

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

(* Tricky: insert newlines and blanks corresponding to Docker comments
   that were removed during parsing.
   For example:

     RUN a && \
       # comment
       b

   results in "a &&   b" if we concatenate the shell fragments naively.
   This results in a wrong location for "b". What we want is:
   - preserve the global byte offset count: if the backslash is gone,
     put it back or replace it by a space.
   - preserve the line numbering: insert a newline for each comment line
     that was ignored.

   In the end, we want something like "a && \\\n\\\n            b".
*)
let concat_shell_fragments first_tok other_toks : string wrap =
  let tok_opt =
    Tok.combine_sparse_toks ~ignorable_newline:"\\\n" ~ignorable_blank:' '
      first_tok other_toks
  in
  match tok_opt with
  (* TODO What should I do here *)
  | None -> failwith "concat of fake token"
  | Some tok -> (Tok.content_of_tok tok, tok)

(* TODO: This basic stuff should not exist here. Move it to Tok. *)
let opt_concat_tokens toks : string wrap option =
  match toks with
  | first_tok :: other_toks -> Some (concat_tokens first_tok other_toks)
  | [] -> None

(* TODO: This basic stuff should not exist here. Move it to Tok. *)
(* Requires at least one token, which must be guaranteed statically. *)
let unsafe_concat_tokens toks : string wrap =
  match opt_concat_tokens toks with
  | Some res -> res
  | None ->
      if strict then assert false
      else
        let s = "" in
        (s, Tok.unsafe_fake_tok s)

let concat_string_wraps ((_open, xs, _close) as x) =
  let tok = Tok.combine_bracket_contents x in
  let str = List_.map fst xs |> String.concat "" in
  (str, tok)

(*
   Collapse consecutive literal string fragments.

   This is useful to detect special fragments that otherwise could get split,
   such as the ellipsis for COPY/ADD that get split into "." and "..".
   This could also avoid complications during matching.
*)
let collapse_fragments ~(view_collapsible_fragment : 'a -> tok option)
    ~(reconstruct_collapsed_fragment : string wrap -> 'a) (fragments : 'a list)
    : 'a list =
  let concat toks tail =
    match toks with
    | [] -> tail
    | first :: others ->
        let tok = Tok.combine_toks first others in
        reconstruct_collapsed_fragment (Tok.content_of_tok tok, tok) :: tail
  in
  let rec simplify acc = function
    | [] -> concat (List.rev acc) []
    | x :: xs -> (
        match view_collapsible_fragment x with
        | None -> concat (List.rev acc) (x :: simplify [] xs)
        | Some tok -> simplify (tok :: acc) xs)
  in
  simplify [] fragments

let collapse_unquoted_fragments (fragments : docker_string_fragment list) :
    docker_string_fragment list =
  collapse_fragments
    ~view_collapsible_fragment:(function
      | Unquoted (_, tok) -> Some tok
      | _ -> None)
    ~reconstruct_collapsed_fragment:(fun s -> Unquoted s)
    fragments

(* Same as 'collapse_unquoted_fragments' but within a double-quoted string *)
let collapse_double_quoted_fragments
    (fragments : double_quoted_string_fragment list) :
    double_quoted_string_fragment list =
  collapse_fragments
    ~view_collapsible_fragment:(function
      | Dbl_string_content (_, tok) -> Some tok
      | _ -> None)
    ~reconstruct_collapsed_fragment:(fun s -> Dbl_string_content s)
    fragments

(* best effort to extract the name of the shell *)
let classify_shell ((_open, ar, _close) : string_array) :
    shell_compatibility option =
  let command =
    match ar with
    | Arr_string (_, (_, ("/usr/bin/env", _), _))
      :: Arr_string (_, (_, (name, _), _))
      :: _ ->
        Some name
    | Arr_string (_, (_, (path, _), _)) :: _ -> Some path
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
(* Dockerfile heredocs *)
(*****************************************************************************)
(*
   Dockerfile supports its own syntax for heredocs which is similar to
   the heredoc syntax supported by Bash.

   This syntax allows the user to pass multiline string templates
   instead of files for COPY or ADD. For the RUN instruction which executes
   a shell command, using a Dockerfile heredoc rather than a Bash heredoc
   avoids the use of backlashes at the end of each line.

   Other instructions (CMD, ...) don't support heredocs.

   The unquoted heredoc markers '<<XXX' and '<<-XXX' will substitute
   Docker variables set with ARG or --build-arg at build time.

   Quoted heredoc markers '<<"XXX"' and '<<-"XXX"' will block the expansion
   of build-time variables (ARG or --build-arg).
*)

(*****************************************************************************)
(* Parsing bash *)
(*****************************************************************************)

type ellipsis_or_bash =
  | Semgrep_ellipsis of tok
  | Semgrep_named_ellipsis of string wrap
  | Bash of AST_bash.blist option

(* A plain ellipsis such as '...' (not e.g. '...;') is identified so
   that we can treat it as special dockerfile syntax rather than bash
   syntax.

   Alternatively, this can be done be extending the tree-sitter-dockerfile
   grammar.
*)
let is_plain_ellipsis =
  let rex = Pcre2_.regexp {|\A[ \t\r\n]*[.]{3}[ \t\r\n]*\z|} in
  fun s ->
    match Pcre2_.pmatch ~rex s with
    | Ok res -> res
    | Error _err -> false

(* A named ellipsis such as '$...ARGS'.
   See remarks for 'is_plain_ellipsis'. *)
let is_named_ellipsis =
  let rex =
    Pcre2_.regexp {|\A[ \t\r\n]*(\$[.]{3}[A-Z_][A-Z_0-9]*)[ \t\r\n]*\z|}
  in
  fun s ->
    match Pcre2_.pmatch ~rex s with
    | Ok res -> res
    | Error _err -> false

(* hack to obtain correct locations when parsing a string extracted from
   a larger file. *)
let shift_locations (str, tok) =
  let line (* 0-based *) = max 0 (Tok.line_of_tok tok - 1) (* 1-based *) in
  let column (* 0-based *) = max 0 (Tok.col_of_tok tok) in
  String.make line '\n' ^ String.make column ' ' ^ str

let parse_bash (env : env) shell_cmd : ellipsis_or_bash =
  let input_kind, _ = env.extra in
  let contents, tok = shell_cmd in
  match input_kind with
  | Pattern when is_plain_ellipsis contents -> Semgrep_ellipsis (snd shell_cmd)
  | Pattern when is_named_ellipsis contents ->
      Semgrep_named_ellipsis (String.trim contents, tok)
  | _ ->
      let ts_res =
        H.wrap_parser
          (fun () ->
            let str = shift_locations shell_cmd in
            Tree_sitter_bash.Parse.string str)
          (fun cst _extras ->
            let bash_env : Parse_bash_tree_sitter.env =
              { env with extra = input_kind }
            in
            Parse_bash_tree_sitter.program bash_env ~tok:(snd shell_cmd) cst)
      in
      (* TODO: don't ignore tree-sitter parsing errors. See Parsing_result
         module of ocaml-tree-sitter-core. *)
      Bash ts_res.program

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
let expansion (env : env) ((v1, v2) : CST.expansion) : docker_string_fragment =
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

let double_quoted_expansion (env : env) (x : CST.expansion) :
    double_quoted_string_fragment =
  match expansion env x with
  | Frag_semgrep_metavar x -> Dbl_frag_semgrep_metavar x
  | Expansion x -> Dbl_expansion x
  | _ -> assert false

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
  | `Pat_e0f3805_opt_choice_SLAS (v1, v2) ->
      let port_tok = token env v1 (* pattern \d+(-\d+)? *) in
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

let image_tag (env : env) ((v1, v2) : CST.image_tag) : tok * docker_string =
  let colon = token env v1 (* ":" *) in
  let tag =
    match v2 with
    | [] ->
        let loc = (colon, colon) in
        (loc, [])
    | fragments ->
        let fragments =
          fragments
          |> List_.map (fun x ->
                 match x with
                 | `Imm_tok_pat_bcfc287 tok ->
                     Unquoted (str env tok (* pattern [^@\s\$]+ *))
                 | `Imme_expa x -> expansion env x)
          |> collapse_unquoted_fragments
        in
        let loc = Tok_range.of_list docker_string_fragment_loc fragments in
        (loc, fragments)
  in
  (colon, tag)

let image_digest (env : env) ((v1, v2) : CST.image_digest) : tok * docker_string
    =
  let at = token env v1 (* "@" *) in
  let digest =
    match v2 with
    | [] ->
        let loc = (at, at) in
        (loc, [])
    | fragments ->
        let fragments =
          fragments
          |> List_.map (fun x ->
                 match x with
                 | `Imm_tok_pat_d2727a0 tok ->
                     Unquoted (str env tok (* pattern [a-zA-Z0-9:]+ *))
                 | `Imme_expa x -> expansion env x)
          |> collapse_unquoted_fragments
        in
        let loc = Tok_range.of_list docker_string_fragment_loc fragments in
        (loc, fragments)
  in
  (at, digest)

let image_name (env : env) ((x, xs) : CST.image_name) =
  let first_fragment =
    match x with
    | `Pat_8165e5f tok -> Unquoted (str env tok (* pattern [^@:\s\$-]+ *))
    | `Expa x -> expansion env x
  in
  let fragments =
    xs
    |> List_.map (fun x ->
           match x with
           | `Imm_tok_pat_2b37705 tok ->
               Unquoted (str env tok (* pattern [^@:\s\$]+ *))
           | `Imme_expa x -> expansion env x)
  in
  let fragments = first_fragment :: fragments |> collapse_unquoted_fragments in
  let loc = Tok_range.of_list docker_string_fragment_loc fragments in
  (loc, fragments)

let image_alias (env : env) ((x, xs) : CST.image_alias) : docker_string =
  let first_fragment =
    match x with
    | `Pat_9a14b5c tok -> Unquoted (str env tok)
    | `Expa x -> expansion env x
  in
  let other_fragments =
    xs
    |> List_.map (fun x ->
           match x with
           | `Imm_tok_pat_9a14b5c tok ->
               Unquoted (str env tok (* pattern [-a-zA-Z0-9_]+ *))
           | `Imme_expa x -> expansion env x)
  in
  let fragments =
    first_fragment :: other_fragments |> collapse_unquoted_fragments
  in
  let loc = Tok_range.of_list docker_string_fragment_loc fragments in
  (loc, fragments)

let immediate_user_name_or_group_fragment (env : env)
    (x : CST.immediate_user_name_or_group_fragment) : docker_string_fragment =
  match x with
  | `Imm_tok_pat_7642c4f tok ->
      Unquoted (str env tok (* pattern ([a-zA-Z][-a-zA-Z0-9_]*|[0-9]+) *))
  | `Imme_expa x -> expansion env x

let immediate_user_name_or_group (env : env)
    (xs : CST.immediate_user_name_or_group) : docker_string =
  let fragments = List_.map (immediate_user_name_or_group_fragment env) xs in
  let loc = Tok_range.of_list docker_string_fragment_loc fragments in
  (loc, fragments)

let user_name_or_group (env : env) ((x, xs) : CST.user_name_or_group) :
    docker_string =
  let head =
    match x with
    | `Pat_05444c2 tok -> Unquoted (str env tok)
    | `Expa x -> expansion env x
  in
  let tail = List_.map (immediate_user_name_or_group_fragment env) xs in
  let fragments = head :: tail |> collapse_unquoted_fragments in
  let loc = Tok_range.of_list docker_string_fragment_loc fragments in
  (loc, fragments)

let unquoted_string (env : env) (xs : CST.unquoted_string) : docker_string =
  let fragments =
    List_.map
      (fun x ->
        match x with
        | `Imm_tok_pat_9f6bbb9 tok ->
            Unquoted (str env tok (* pattern "[^\\s\\n\\\"\\\\\\$]+" *))
        | `Imm_tok_bsla tok -> Unquoted (str env tok (* "\\ " *))
        | `Imme_expa x -> expansion env x)
      xs
    |> collapse_unquoted_fragments
  in
  let loc = Tok_range.of_list docker_string_fragment_loc fragments in
  (loc, fragments)

(*
   Union of different types that represent fragments of strings.
   The first fragment typically uses a different regexp than the
   following fragments, and it's different from one instruction to another.
   All these simple string fragments end up being treated the same.
*)
type generic_path_fragment =
  [ `Pat_9873c86 of Token.t (* pattern [^-\s\$<] *)
  | `Pat_0851d06 of Token.t (* pattern <[^-\s\$<] *)
  | `Pat_a667757 of Token.t (* pattern <[^<] *)
  | `Imm_tok_pat_0c7fc22 of Token.t (* pattern [^\s\$]+ *)
  | `Expa of CST.expansion (* = imm_expansion = Token.t * expansion_body *)
  | `Imme_expa of CST.expansion ]

type generic_path = generic_path_fragment * generic_path_fragment list

let docker_string_fragment env (x : generic_path_fragment) :
    docker_string_fragment =
  match x with
  | `Pat_9873c86 tok
  | `Pat_0851d06 tok
  | `Pat_a667757 tok
  | `Imm_tok_pat_0c7fc22 tok ->
      Unquoted (str env tok)
  | `Expa x
  | `Imme_expa x ->
      expansion env x

let generic_path (env : env) ((v1, v2) : generic_path) :
    docker_string_fragment list =
  let first_fragment = docker_string_fragment env v1 in
  let more_fragments = List_.map (docker_string_fragment env) v2 in
  first_fragment :: more_fragments |> collapse_unquoted_fragments

let generic_path_or_ellipsis (env : env) (x : generic_path) : str_or_ellipsis =
  match (env.extra, generic_path env x) with
  | (Pattern, _), [ Unquoted ("...", tok) ] -> Str_semgrep_ellipsis tok
  | _, fragments ->
      let loc = Tok_range.of_list docker_string_fragment_loc fragments in
      Str_str (loc, fragments)

let path (env : env) (x : CST.path) : docker_string =
  let fragments = generic_path env (x :> generic_path) in
  let loc = Tok_range.of_list docker_string_fragment_loc fragments in
  (loc, fragments)

let path_with_heredoc_but_ignore_heredoc (env : env) (x : CST.path_with_heredoc)
    : docker_string option =
  match x with
  | `Here_marker _heredoc_marker -> None
  | `Choice_pat_9873c86_rep_choice_imm_tok_pat_0c7fc22 x ->
      let fragments = generic_path env (x :> generic_path) in
      let loc = Tok_range.of_list docker_string_fragment_loc fragments in
      Some (loc, fragments)

let assemble_heredoc_template (env : env) (opening : Token.t)
    ((_heredoc_nl, v2, closing) : CST.heredoc_block) : heredoc_template =
  let opening = (* heredoc_nl *) token env opening in
  let closing = (* heredoc_end *) token env closing in
  let fragments =
    v2
    |> List_.map (fun (v1, v2) ->
           let v1 = (* heredoc_line *) token env v1 in
           let v2 = (* "\n" *) token env v2 in
           Tok.combine_toks v1 [ v2 ])
  in
  let body : string wrap =
    match fragments with
    | [] -> ("", closing)
    | x :: xs ->
        let tok = Tok.combine_toks x xs in
        (Tok.content_of_tok tok, tok)
  in
  { opening; closing; body }

let path_with_heredoc_or_ellipsis (env : env) (x : CST.path_with_heredoc)
    (take_heredoc_body : unit -> CST.heredoc_block option) :
    str_or_ellipsis option =
  match x with
  | `Here_marker heredoc_marker -> (
      match take_heredoc_body () with
      | Some x ->
          Some (Str_template (assemble_heredoc_template env heredoc_marker x))
      | None -> None)
  | `Choice_pat_9873c86_rep_choice_imm_tok_pat_0c7fc22 x ->
      Some (generic_path_or_ellipsis env (x :> generic_path))

(*
   Some of the src_paths are the start of a heredoc and they should be
   attached with the next heredoc body in the list.
   If there's a mismatch between the number of heredoc openings and
   heredoc bodies, the half-missing heredocs are dropped silently.
*)
let reattach_heredoc_bodies env
    (src_paths : (CST.path_with_heredoc * Token.t) list)
    (heredoc_bodies : CST.heredoc_block list) : path_or_ellipsis list =
  let queue = ref heredoc_bodies in
  let take_heredoc_body () =
    match !queue with
    | [] -> None
    | x :: xs ->
        queue := xs;
        Some x
  in
  (* Assume that 'List.filter_map' proceeds from left-to-right *)
  src_paths
  |> List_.filter_map (fun ((v1 : CST.path_with_heredoc), _blank) ->
         path_with_heredoc_or_ellipsis env v1 take_heredoc_body)

let stopsignal_value (env : env) ((x, xs) : CST.stopsignal_value) :
    docker_string =
  let first_fragment =
    match x with
    | `Pat_441cd81 tok -> Unquoted (str env tok)
    | `Expa x -> expansion env x
  in
  let other_fragments =
    List_.map
      (fun x ->
        match x with
        | `Imm_tok_pat_441cd81 tok ->
            Unquoted (str env tok (* pattern [A-Z0-9]+ *))
        | `Imme_expa x -> expansion env x)
      xs
  in
  let fragments =
    first_fragment :: other_fragments |> collapse_unquoted_fragments
  in
  let loc = Tok_range.of_list docker_string_fragment_loc fragments in
  (loc, fragments)

let double_quoted_string (env : env) ((v1, v2, v3) : CST.double_quoted_string) :
    docker_string_fragment =
  let open_ = str env v1 (* "\"" *) in
  let contents =
    List_.map
      (fun x ->
        match x with
        | `Imm_tok_pat_589b0f8 tok ->
            let s = str env tok (* pattern "[^\"\\n\\\\\\$]+" *) in
            Dbl_string_content s
        | `Double_quoted_esc_seq tok ->
            (* TODO: provide unescaped version *)
            let s = str env tok (* escape_sequence *) in
            Dbl_string_content s
        | `BSLASH tok ->
            let s = str env tok in
            Dbl_string_content s (* lone, unescaped backslash *)
        | `Imme_expa x -> double_quoted_expansion env x)
      v2
  in
  let close = str env v3 (* "\"" *) in
  let loc = (wrap_tok open_, wrap_tok close) in
  let fragments = collapse_double_quoted_fragments contents in
  Double_quoted (loc, (wrap_tok open_, fragments, wrap_tok close))

let single_quoted_string (env : env) ((v1, v2, v3) : CST.single_quoted_string) :
    docker_string_fragment =
  let open_ = token env v1 (* "'" *) in
  let contents =
    List_.map
      (fun x ->
        match x with
        | `Imm_tok_pat_0ab9261 tok -> str env tok (* literal characters *)
        | `Single_quoted_esc_seq tok ->
            (* TODO: provide unescaped version *)
            str env tok (* single_quoted_escape_sequence *)
        | `BSLASH tok -> str env tok (* lone, unescaped backslash *))
      v2
  in
  let close = token env v3 (* "'" *) in
  let contents = concat_string_wraps (open_, contents, close) in
  let loc = (open_, close) in
  Single_quoted (loc, (open_, contents, close))

let shell_fragment (env : env) (xs : CST.shell_fragment) : tok =
  List_.filter_map
    (fun x ->
      match x with
      | `Here_marker_pat_ea34a52 (_v1, _v2) ->
          (* TODO:
             R.Case (
               "Here_marker_pat_ea34a52",
               let v1 = (* heredoc_marker *) token env v1 in
               let v2 = map_pat_ea34a52 env v2 in
               R.Tuple [v1; v2]
             )
          *)
          None
      | `Pat_b1120d3 tok
      | `Pat_f8ab07f tok
      | `Pat_eda9032 tok
      | `Pat_a667757 tok ->
          Some (token env tok))
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
    let start = docker_string_loc name in
    let end_ = start in
    let end_ =
      match tag with
      | None -> end_
      | Some (_colon, x) -> docker_string_loc x
    in
    let end_ =
      match digest with
      | None -> end_
      | Some (_at, x) -> docker_string_loc x
    in
    Tok_range.range start end_
  in
  { loc; name; tag; digest }

let array_element (env : env) (x : CST.array_element) : array_elt =
  match x with
  | `Json_str (v1, v2, v3) ->
      let open_ = token env v1 (* "\"" *) in
      let contents =
        List_.map
          (fun x ->
            match x with
            | `Imm_tok_pat_3a2a380 tok -> str env tok (* literal characters *)
            | `Json_esc_seq tok ->
                (* TODO: produce also an unescaped version *)
                str env tok (* JSON escape sequence *))
          v2
      in
      let close = token env v3 (* "\"" *) in
      let contents = concat_string_wraps (open_, contents, close) in
      let loc = (open_, close) in
      Arr_string (loc, (open_, contents, close))
  | `Semg_ellips tok -> Arr_ellipsis (token env tok)
  | `Semg_meta tok -> Arr_metavar (str env tok)

let string (env : env) (x : CST.anon_choice_double_quoted_str_6156383) :
    docker_string =
  (* TODO: fix the tree-sitter-dockerfile grammar so that it accepts
     multifragment strings such as a'b'$C"d$E" *)
  match x with
  | `Double_quoted_str x ->
      let fragment = double_quoted_string env x in
      (docker_string_fragment_loc fragment, [ fragment ])
  | `Single_quoted_str x ->
      let fragment = single_quoted_string env x in
      (docker_string_fragment_loc fragment, [ fragment ])
  | `Unqu_str x -> unquoted_string env x

let json_string_array (env : env) ((v1, v2, v3) : CST.json_string_array) :
    Tok_range.t * string_array =
  let open_ = token env v1 (* "[" *) in
  let argv =
    match v2 with
    | Some (v1, v2) ->
        let x0 = array_element env v1 in
        let xs =
          List_.map
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

let env_pair (env : env) (x : CST.env_pair) : env_pair =
  match x with
  | `Semg_ellips tok -> Env_semgrep_ellipsis (token env tok (* "..." *))
  | `Env_key_imm_tok_eq_opt_choice_double_quoted_str (v1, v2, v3) ->
      let k =
        Ident (str env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *))
      in
      let eq = token env v2 (* "=" *) in
      let v =
        match v3 with
        | None ->
            (* the empty token gives us the correct location which we need
               even if we returned an empty list of fragments. *)
            let tok = Tok.empty_tok_after eq in
            let loc = (tok, tok) in
            (loc, [ Unquoted (Tok.content_of_tok tok, tok) ])
        | Some x -> string env x
      in
      let loc = ident_or_metavar_loc k in
      Env_pair (loc, k, eq, v)

let spaced_env_pair (env : env) ((v1, v2, v3) : CST.spaced_env_pair) : env_pair
    =
  let k = Ident (str env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *)) in
  let blank = token env v2 (* pattern \s+ *) in
  let v = string env v3 in
  let loc = ident_or_metavar_loc k in
  Env_pair (loc, k, blank, v)

let label_pair (env : env) (x : CST.label_pair) : label_pair =
  match x with
  | `Semg_ellips tok -> Label_semgrep_ellipsis (token env tok (* "..." *))
  | `Choice_semg_meta_imm_tok_eq_choice_double_quoted_str (v1, v2, v3) ->
      let key : key_or_metavar =
        match v1 with
        | `Semg_meta tok ->
            Semgrep_metavar (str env tok (* pattern \$[A-Z_][A-Z_0-9]* *))
        | `Pat_4128122 tok ->
            Key (Unquoted (str env tok (* pattern [-a-zA-Z0-9\._]+ *)))
        | `Double_quoted_str x -> Key (double_quoted_string env x)
        | `Single_quoted_str x -> Key (single_quoted_string env x)
      in
      let eq = token env v2 (* "=" *) in
      let value = string env v3 in
      let loc = key_or_metavar_loc key in
      Label_pair (loc, key, eq, value)

let shell_command (env : env) (x : CST.shell_command) =
  match x with
  | `Semg_ellips tok -> Command_semgrep_ellipsis (token env tok)
  | `Shell_frag_rep_requ_line_cont_shell_frag (v1, v2) -> (
      (* Stitch back the fragments together, then parse using the correct
         shell language. *)
      let first_frag = shell_fragment env v1 in
      let more_frags =
        v2
        |> List_.map (fun (v1, v2) ->
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
               let shell_frag = shell_fragment env v2 in
               [ shell_line_cont; shell_frag ])
        |> List_.flatten
      in
      let raw_shell_code = concat_shell_fragments first_frag more_frags in
      let _, shell_compat = env.extra in
      match shell_compat with
      | Sh -> (
          match parse_bash env raw_shell_code with
          | Semgrep_ellipsis tok -> Command_semgrep_ellipsis tok
          | Semgrep_named_ellipsis x -> Command_semgrep_named_ellipsis x
          | Bash (Some bash_program) ->
              let loc = wrap_loc raw_shell_code in
              Sh_command (loc, bash_program)
          | Bash None -> Other_shell_command (Sh, raw_shell_code))
      | (Cmd | Powershell | Other _) as shell ->
          Other_shell_command (shell, raw_shell_code))

let command (env : env) (x : CST.anon_choice_json_str_array_0106ace) =
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
    List_.map
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
    List_.map
      (fun x ->
        match x with
        | `Param x -> Param (param env x)
        | `Mount_param x -> mount_param env x)
      params
  in
  let cmd = command env cmd in
  let _, end_ = command_loc cmd in
  let loc = (wrap_tok name, end_) in
  (loc, name, params, cmd)

let add_or_copy (env : env) (v1, v2, src_paths, v4, heredoc_bodies) :
    add_or_copy =
  let name = str env v1 in
  let param = List_.map (param env) v2 in
  let src = reattach_heredoc_bodies env src_paths heredoc_bodies in
  (* heredocs are not allowed as the destination file name *)
  let dst =
    match path_with_heredoc_but_ignore_heredoc env v4 with
    | Some x -> x
    | None ->
        (* TODO: at least report the location of the syntax error *)
        failwith
          "syntax error: heredocs are not allowed as the destination file name \
           in a COPY or ADD instruction"
  in
  let loc = (wrap_tok name, docker_string_loc dst |> snd) in
  (loc, name, param, src, dst)

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
                ( Some (as_, alias),
                  Tok_range.union loc (docker_string_loc alias) )
            | None -> (None, loc)
          in
          (env, From (loc, name, param, image_spec, alias))
      | `Run_inst (v1, v2, v3, _v4TODO) ->
          (* TODO: heredocs *)
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
          let label_pairs = List_.map (label_pair env) v2 in
          let loc = Tok_range.of_list label_pair_loc label_pairs in
          let loc = Tok_range.extend loc (snd name) in
          (env, Label (loc, name, label_pairs))
      | `Expose_inst (v1, v2) ->
          let name = str env v1 (* pattern [eE][xX][pP][oO][sS][eE] *) in
          let port_protos =
            List_.map
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
            | `Rep1_env_pair xs -> List_.map (env_pair env) xs
            | `Spaced_env_pair x -> [ spaced_env_pair env x ]
          in
          let _, end_ = Tok_range.of_list env_pair_loc pairs in
          let loc = (wrap_tok name, end_) in
          (env, Env (loc, name, pairs))
      | `Add_inst x -> (env, Add (add_or_copy env x))
      | `Copy_inst x ->
          (*
             COPY is the same as ADD but with less magic in the interpretation
             of the arguments.
             See https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#add-or-copy
          *)
          (env, Copy (add_or_copy env x))
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
                let path0 = generic_path_or_ellipsis env (v1 :> generic_path) in
                let paths =
                  List_.map
                    (fun (v1, v2) ->
                      let _blank = token env v1 (* pattern [\t ]+ *) in
                      generic_path_or_ellipsis env (v2 :> generic_path))
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
          let end_ = docker_string_loc user |> snd in
          let opt_group, end_ =
            match v3 with
            | Some (v1, v2) ->
                let colon = token env v1 (* ":" *) in
                let group = immediate_user_name_or_group env v2 in
                (Some (colon, group), docker_string_loc group |> snd)
            | None -> (None, end_)
          in
          let loc = (wrap_tok name, end_) in
          (env, User (loc, name, user, opt_group))
      | `Work_inst (v1, v2) ->
          let name = str env v1 (* pattern [wW][oO][rR][kK][dD][iI][rR] *) in
          let dir = path env v2 in
          let loc = (wrap_tok name, docker_string_loc dir |> snd) in
          (env, Workdir (loc, name, dir))
      | `Arg_inst (v1, v2, v3) ->
          let name = str env v1 (* pattern [aA][rR][gG] *) in
          let key : ident_or_metavar =
            match v2 with
            | `Semg_meta tok ->
                Semgrep_metavar (str env tok (* pattern \$[A-Z_][A-Z_0-9]* *))
            | `Pat_4de4cb9 tok ->
                Ident (str env tok (* pattern [a-zA-Z0-9_]+ *))
          in
          let loc = (wrap_tok name, ident_or_metavar_loc key |> snd) in
          let opt_value, loc =
            match v3 with
            | Some (v1, v2) ->
                let eq = token env v1 (* "=" *) in
                let value = string env v2 in
                ( Some (eq, value),
                  Tok_range.extend loc (docker_string_loc value |> snd) )
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
          let loc = (wrap_tok name, docker_string_loc signal |> snd) in
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
            | `Semg_ellips tok ->
                let tok = token env tok in
                Healthcheck_ellipsis tok
            | `NONE tok -> Healthcheck_none (token env tok (* "NONE" *))
            | `Rep_choice_semg_ellips_cmd_inst (v1, (name (* CMD *), args)) ->
                let params =
                  List_.map
                    (function
                      | `Param x -> ParamParam (param env x)
                      | `Semg_ellips tok ->
                          let tok = token env tok in
                          ParamEllipsis tok)
                    v1
                in
                let params_loc =
                  Tok_range.of_list param_or_ellipsis_loc params
                in
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
        let env, instr = instruction env v1 in
        let acc = (env, instr :: instrs) in
        let _newline = token env v2 (* "\n" *) in
        acc)
      (env, []) xs
  in
  List.rev instrs

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* tree-sitter-dockerfile parser requires a trailing newline, unlike the
   official 'docker' command.

   This may be fixed in the grammar eventually but for now, tree-sitter
   inserts a "missing token" for the missing newline, and semgrep doesn't
   report it as an error.

   However, in some cases, a segfault occurs. This is fixed in recent
   versions of tree-sitter but we need to upgrade (as of 2024-05-03,
   we're using 0.20.6).
   See https://github.com/camdencheek/tree-sitter-dockerfile/issues/22
   Appending a trailing newline to the input works around this segfault.
*)
let ensure_trailing_newline str =
  if str <> "" then
    let len = String.length str in
    match str.[len - 1] with
    | '\n' -> str
    | _ -> str ^ "\n"
  else str

let parse file =
  H.wrap_parser
    (fun () ->
      let contents = UFile.read_file file |> ensure_trailing_newline in
      Tree_sitter_dockerfile.Parse.string ~src_file:!!file contents)
    (fun cst _extras ->
      let env =
        {
          H.file;
          conv = H.line_col_to_pos file;
          extra = (AST_bash.Program, Sh);
        }
      in
      source_file env cst)

let parse_pattern str =
  let input_kind = AST_bash.Pattern in
  H.wrap_parser
    (fun () ->
      str |> ensure_trailing_newline |> Tree_sitter_dockerfile.Parse.string)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        {
          H.file;
          conv = H.line_col_to_pos_pattern str;
          extra = (input_kind, Sh);
        }
      in
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
