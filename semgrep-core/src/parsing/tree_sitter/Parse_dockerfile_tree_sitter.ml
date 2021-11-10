(**
   Mapping from tree-sitter-dockerfile tree-sitter's CST to the Dockerfile
   AST type, which itself includes nodes of the Bash AST.

   Derived from generated code 'dockerfile/lib/Boilerplate.ml'
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

open! Common
module AST = AST_dockerfile
module CST = Tree_sitter_dockerfile.CST
module PI = Parse_info
open! AST_dockerfile
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The 'extra' field indicates if we're parsing a pattern or a program. *)
type env = AST_bash.input_kind H.env

let token = H.token

let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(* This is to satisfy the type system. Make sure it never gets called
   on any input. *)
let todo (env : env) _ =
  failwith "Parse_bash_tree_sitter: feature not implemented"

let variable (env : env) (tok : CST.variable) = token env tok

(* pattern [a-zA-Z][a-zA-Z0-9_]* *)

let pat_5cf3c2c (env : env) (tok : CST.pat_5cf3c2c) = token env tok

(* pattern [mM][aA][iI][nN][tT][aA][iI][nN][eE][rR] *)

let imm_tok_pat_d2727a0 (env : env) (tok : CST.imm_tok_pat_d2727a0) =
  token env tok

(* pattern [a-zA-Z0-9:]+ *)

let pat_9374c10 (env : env) (tok : CST.pat_9374c10) = token env tok

(* pattern [sS][hH][eE][lL][lL] *)

let pat_845d48b (env : env) (tok : CST.pat_845d48b) = token env tok

(* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *)

let pat_808c5a1 (env : env) (tok : CST.pat_808c5a1) = token env tok

(* pattern [eE][nN][tT][rR][yY][pP][oO][iI][nN][tT] *)

let comment (env : env) (tok : CST.comment) = token env tok

(* pattern #.* *)

let escape_sequence (env : env) (tok : CST.escape_sequence) = token env tok

(* escape_sequence *)

let imm_tok_pat_3d340f6 (env : env) (tok : CST.imm_tok_pat_3d340f6) =
  token env tok

(* pattern \s+ *)

let pat_5eaae97 (env : env) (tok : CST.pat_5eaae97) = token env tok

(* pattern [fF][rR][oO][mM] *)

let non_newline_whitespace (env : env) (tok : CST.non_newline_whitespace) =
  token env tok

(* pattern [\t ]+ *)

let pat_8713919 (env : env) (tok : CST.pat_8713919) = token env tok

(* pattern [^\}]+ *)

let imm_tok_pat_f43f746 (env : env) (tok : CST.imm_tok_pat_f43f746) =
  token env tok

(* pattern [a-z][-a-z]* *)

let pat_79b8891 (env : env) (tok : CST.pat_79b8891) = token env tok

(* pattern [aA][sS] *)

let pat_aa82a08 (env : env) (tok : CST.pat_aa82a08) = token env tok

(* pattern [oO][nN][bB][uU][iI][lL][dD] *)

let pat_433746d (env : env) (tok : CST.pat_433746d) = token env tok

(* pattern [lL][aA][bB][eE][lL] *)

let pat_f05eb95 (env : env) (tok : CST.pat_f05eb95) = token env tok

(* pattern \\[^\n] *)

let pat_239fcac (env : env) (tok : CST.pat_239fcac) = token env tok

(* pattern [cC][mM][dD] *)

let pat_56bd329 (env : env) (tok : CST.pat_56bd329) = token env tok

(* pattern [sS][tT][oO][pP][sS][iI][gG][nN][aA][lL] *)

let semgrep_metavariable_name (env : env) (tok : CST.semgrep_metavariable_name)
    =
  token env tok

(* pattern [A-Z_][A-Z_0-9]* *)

let imm_tok_pat_589b0f8 (env : env) (tok : CST.imm_tok_pat_589b0f8) =
  token env tok

(* pattern "[^\"\\n\\\\\\$]+" *)

let pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) = token env tok

(* pattern .* *)

let pat_9a14b5c (env : env) (tok : CST.pat_9a14b5c) = token env tok

(* pattern [-a-zA-Z0-9_]+ *)

let pat_4128122 (env : env) (tok : CST.pat_4128122) = token env tok

(* pattern [-a-zA-Z0-9\._]+ *)

let pat_fd0037e (env : env) (tok : CST.pat_fd0037e) = token env tok

(* pattern [hH][eE][aA][lL][tT][hH][cC][hH][eE][cC][kK] *)

let pat_217c202 (env : env) (tok : CST.pat_217c202) = token env tok

(* pattern \d+ *)

let pat_ed84199 (env : env) (tok : CST.pat_ed84199) = token env tok

(* pattern [aA][dD][dD] *)

let pat_f2a2f8f (env : env) (tok : CST.pat_f2a2f8f) = token env tok

(* pattern [rR][uU][nN] *)

let pat_985c208 (env : env) (tok : CST.pat_985c208) = token env tok

(* pattern [aA][rR][gG] *)

let pat_660c06c (env : env) (tok : CST.pat_660c06c) = token env tok

(* pattern [a-z][-a-z0-9_]* *)

let pat_a6040bb (env : env) (tok : CST.pat_a6040bb) = token env tok

(* pattern [wW][oO][rR][kK][dD][iI][rR] *)

let pat_0c7fc22 (env : env) (tok : CST.pat_0c7fc22) = token env tok

(* pattern [^\s\$]+ *)

let imm_tok_pat_24a1611 (env : env) (tok : CST.imm_tok_pat_24a1611) =
  token env tok

(* pattern "[^\\s\\n\\\"\\\\\\$]+" *)

let pat_1167a92 (env : env) (tok : CST.pat_1167a92) = token env tok

(* pattern [^-\s\$] *)

let pat_2b37705 (env : env) (tok : CST.pat_2b37705) = token env tok

(* pattern [^@:\s\$]+ *)

let pat_9818882 (env : env) (tok : CST.pat_9818882) = token env tok

(* pattern [eE][xX][pP][oO][sS][eE] *)

let pat_9c0a769 (env : env) (tok : CST.pat_9c0a769) = token env tok

(* pattern [cC][oO][pP][yY] *)

let pat_4b81dfc (env : env) (tok : CST.pat_4b81dfc) = token env tok

(* pattern [^\\\[\n#\s][^\\\n]* *)

let imm_tok_pat_bcfc287 (env : env) (tok : CST.imm_tok_pat_bcfc287) =
  token env tok

(* pattern [^@\s\$]+ *)

let pat_1c663f5 (env : env) (tok : CST.pat_1c663f5) = token env tok

(* pattern [vV][oO][lL][uU][mM][eE] *)

let expansion (env : env) ((v1, v2) : CST.expansion) =
  let v1 = token env v1 (* "$" *) in
  let v2 =
    match v2 with
    | `Var tok -> token env tok (* pattern [a-zA-Z][a-zA-Z0-9_]* *)
    | `LCURL_pat_8713919_RCURL (v1, v2, v3) ->
        let v1 = token env v1 (* "{" *) in
        let v2 = token env v2 (* pattern [^\}]+ *) in
        let v3 = token env v3 (* "}" *) in
        todo env (v1, v2, v3)
  in
  todo env (v1, v2)

let param (env : env) ((v1, v2, v3, v4) : CST.param) =
  let v1 = token env v1 (* "--" *) in
  let v2 = token env v2 (* pattern [a-z][-a-z]* *) in
  let v3 = token env v3 (* "=" *) in
  let v4 = token env v4 (* pattern [^\s]+ *) in
  todo env (v1, v2, v3, v4)

let semgrep_double_curly_metavariable (env : env)
    ((v1, v2, v3) : CST.semgrep_double_curly_metavariable) =
  let v1 = token env v1 (* "${{" *) in
  let v2 = token env v2 (* pattern [A-Z_][A-Z_0-9]* *) in
  let v3 = token env v3 (* "}}" *) in
  todo env (v1, v2, v3)

let anon_comment (env : env) ((v1, v2) : CST.anon_comment) =
  let v1 = token env v1 (* "#" *) in
  let v2 = token env v2 (* pattern .* *) in
  todo env (v1, v2)

let expose_port (env : env) ((v1, v2) : CST.expose_port) =
  let v1 = token env v1 (* pattern \d+ *) in
  let v2 =
    match v2 with
    | Some x -> (
        match x with
        | `SLAS_ce91595 tok -> token env tok (* "/tcp" *)
        | `SLAS_c773c8d tok -> token env tok (* "/udp" *))
    | None -> todo env ()
  in
  todo env (v1, v2)

let image_tag (env : env) ((v1, v2) : CST.image_tag) =
  let v1 = token env v1 (* ":" *) in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Imm_tok_pat_bcfc287 tok -> token env tok (* pattern [^@\s\$]+ *)
        | `Expa x -> expansion env x)
      v2
  in
  todo env (v1, v2)

let user_name_or_group (env : env) (xs : CST.user_name_or_group) =
  List.map
    (fun x ->
      match x with
      | `Pat_660c06c tok -> token env tok (* pattern [a-z][-a-z0-9_]* *)
      | `Expa x -> expansion env x)
    xs

let unquoted_string (env : env) (xs : CST.unquoted_string) =
  List.map
    (fun x ->
      match x with
      | `Imm_tok_pat_24a1611 tok ->
          token env tok (* pattern "[^\\s\\n\\\"\\\\\\$]+" *)
      | `BSLASHSPACE tok -> token env tok (* "\\ " *)
      | `Expa x -> expansion env x)
    xs

let path (env : env) ((v1, v2) : CST.path) =
  let v1 =
    match v1 with
    | `Pat_1167a92 tok -> token env tok (* pattern [^-\s\$] *)
    | `Expa x -> expansion env x
  in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Pat_0c7fc22 tok -> token env tok (* pattern [^\s\$]+ *)
        | `Expa x -> expansion env x)
      v2
  in
  todo env (v1, v2)

let image_digest (env : env) ((v1, v2) : CST.image_digest) =
  let v1 = token env v1 (* "@" *) in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Imm_tok_pat_d2727a0 tok -> token env tok (* pattern [a-zA-Z0-9:]+ *)
        | `Expa x -> expansion env x)
      v2
  in
  todo env (v1, v2)

let image_name (env : env) (xs : CST.image_name) =
  List.map
    (fun x ->
      match x with
      | `Pat_2b37705 tok -> token env tok (* pattern [^@:\s\$]+ *)
      | `Expa x -> expansion env x)
    xs

let image_alias (env : env) (xs : CST.image_alias) =
  List.map
    (fun x ->
      match x with
      | `Pat_9a14b5c tok -> token env tok (* pattern [-a-zA-Z0-9_]+ *)
      | `Expa x -> expansion env x)
    xs

let stopsignal_value (env : env) (xs : CST.stopsignal_value) =
  List.map
    (fun x ->
      match x with
      | `Pat_441cd81 tok -> token env tok (* pattern [A-Z0-9]+ *)
      | `Expa x -> expansion env x)
    xs

let double_quoted_string (env : env) ((v1, v2, v3) : CST.double_quoted_string) =
  let v1 = token env v1 (* "\"" *) in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Imm_tok_pat_589b0f8 tok ->
            token env tok (* pattern "[^\"\\n\\\\\\$]+" *)
        | `Esc_seq tok -> token env tok (* escape_sequence *)
        | `Expa x -> expansion env x)
      v2
  in
  let v3 = token env v3 (* "\"" *) in
  todo env (v1, v2, v3)

let shell_fragment (env : env) (xs : CST.shell_fragment) =
  List.map
    (fun x ->
      match x with
      | `Semg_ellips tok -> token env tok (* "..." *)
      | `Semg_double_curl_meta x -> semgrep_double_curly_metavariable env x
      | `Pat_4b81dfc tok -> token env tok (* pattern [^\\\[\n#\s][^\\\n]* *)
      | `Pat_f05eb95 tok -> token env tok
      (* pattern \\[^\n] *))
    xs

let comment_line (env : env) ((v1, v2) : CST.comment_line) =
  let v1 = anon_comment env v1 in
  let v2 = token env v2 (* "\n" *) in
  todo env (v1, v2)

let image_spec (env : env) ((v1, v2, v3) : CST.image_spec) =
  let v1 = image_name env v1 in
  let v2 =
    match v2 with
    | Some x -> image_tag env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x -> image_digest env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

let anon_choice_double_quoted_str_6b200ac (env : env)
    (x : CST.anon_choice_double_quoted_str_6b200ac) =
  match x with
  | `Double_quoted_str x -> double_quoted_string env x
  | `Unqu_str x -> unquoted_string env x

let string_array (env : env) ((v1, v2, v3) : CST.string_array) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = double_quoted_string env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = double_quoted_string env v2 in
              todo env (v1, v2))
            v2
        in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

let env_pair (env : env) ((v1, v2, v3) : CST.env_pair) =
  let v1 = token env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *) in
  let v2 = token env v2 (* "=" *) in
  let v3 = anon_choice_double_quoted_str_6b200ac env v3 in
  todo env (v1, v2, v3)

let spaced_env_pair (env : env) ((v1, v2, v3) : CST.spaced_env_pair) =
  let v1 = token env v1 (* pattern [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9] *) in
  let v2 = token env v2 (* pattern \s+ *) in
  let v3 = anon_choice_double_quoted_str_6b200ac env v3 in
  todo env (v1, v2, v3)

let label_pair (env : env) ((v1, v2, v3) : CST.label_pair) =
  let v1 = token env v1 (* pattern [-a-zA-Z0-9\._]+ *) in
  let v2 = token env v2 (* "=" *) in
  let v3 = anon_choice_double_quoted_str_6b200ac env v3 in
  todo env (v1, v2, v3)

let anon_choice_str_array_878ad0b (env : env)
    (x : CST.anon_choice_str_array_878ad0b) =
  match x with
  | `Str_array x -> string_array env x
  | `Shell_cmd (v1, v2) ->
      let v1 = shell_fragment env v1 in
      let v2 =
        List.map
          (fun (v1, v2, v3) ->
            let v1 = token env v1 (* "\\\n" *) in
            let v2 = List.map (comment_line env) v2 in
            let v3 = shell_fragment env v3 in
            todo env (v1, v2, v3))
          v2
      in
      todo env (v1, v2)

let cmd_instruction (env : env) ((v1, v2) : CST.cmd_instruction) =
  let v1 = token env v1 (* pattern [cC][mM][dD] *) in
  let v2 = anon_choice_str_array_878ad0b env v2 in
  todo env (v1, v2)

let rec instruction (env : env) (x : CST.instruction) =
  match x with
  | `From_inst (v1, v2, v3, v4) ->
      let v1 = token env v1 (* pattern [fF][rR][oO][mM] *) in
      let v2 =
        match v2 with
        | Some x -> param env x
        | None -> todo env ()
      in
      let v3 = image_spec env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* pattern [aA][sS] *) in
            let v2 = image_alias env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4)
  | `Run_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [rR][uU][nN] *) in
      let v2 = anon_choice_str_array_878ad0b env v2 in
      todo env (v1, v2)
  | `Cmd_inst x -> cmd_instruction env x
  | `Label_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [lL][aA][bB][eE][lL] *) in
      let v2 = List.map (label_pair env) v2 in
      todo env (v1, v2)
  | `Expose_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [eE][xX][pP][oO][sS][eE] *) in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Expose_port x -> expose_port env x
            | `Expa x -> expansion env x)
          v2
      in
      todo env (v1, v2)
  | `Env_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [eE][nN][vV] *) in
      let v2 =
        match v2 with
        | `Rep1_env_pair xs -> List.map (env_pair env) xs
        | `Spaced_env_pair x -> spaced_env_pair env x
      in
      todo env (v1, v2)
  | `Add_inst (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern [aA][dD][dD] *) in
      let v2 =
        match v2 with
        | Some x -> param env x
        | None -> todo env ()
      in
      let v3 = path env v3 in
      let v4 = token env v4 (* pattern [\t ]+ *) in
      let v5 = path env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Copy_inst (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* pattern [cC][oO][pP][yY] *) in
      let v2 =
        match v2 with
        | Some x -> param env x
        | None -> todo env ()
      in
      let v3 = path env v3 in
      let v4 = token env v4 (* pattern [\t ]+ *) in
      let v5 = path env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Entr_inst (v1, v2) ->
      let v1 =
        token env v1
        (* pattern [eE][nN][tT][rR][yY][pP][oO][iI][nN][tT] *)
      in
      let v2 = anon_choice_str_array_878ad0b env v2 in
      todo env (v1, v2)
  | `Volume_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [vV][oO][lL][uU][mM][eE] *) in
      let v2 =
        match v2 with
        | `Str_array x -> string_array env x
        | `Path_rep_non_nl_whit_path (v1, v2) ->
            let v1 = path env v1 in
            let v2 =
              List.map
                (fun (v1, v2) ->
                  let v1 = token env v1 (* pattern [\t ]+ *) in
                  let v2 = path env v2 in
                  todo env (v1, v2))
                v2
            in
            todo env (v1, v2)
      in
      todo env (v1, v2)
  | `User_inst (v1, v2, v3) ->
      let v1 = token env v1 (* pattern [uU][sS][eE][rR] *) in
      let v2 = user_name_or_group env v2 in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = user_name_or_group env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Work_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [wW][oO][rR][kK][dD][iI][rR] *) in
      let v2 = path env v2 in
      todo env (v1, v2)
  | `Arg_inst (v1, v2, v3) ->
      let v1 = token env v1 (* pattern [aA][rR][gG] *) in
      let v2 = token env v2 (* pattern [a-zA-Z0-9_]+ *) in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = anon_choice_double_quoted_str_6b200ac env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Onbu_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [oO][nN][bB][uU][iI][lL][dD] *) in
      let v2 = instruction env v2 in
      todo env (v1, v2)
  | `Stop_inst (v1, v2) ->
      let v1 =
        token env v1
        (* pattern [sS][tT][oO][pP][sS][iI][gG][nN][aA][lL] *)
      in
      let v2 = stopsignal_value env v2 in
      todo env (v1, v2)
  | `Heal_inst (v1, v2) ->
      let v1 =
        token env v1
        (* pattern [hH][eE][aA][lL][tT][hH][cC][hH][eE][cC][kK] *)
      in
      let v2 =
        match v2 with
        | `NONE tok -> token env tok (* "NONE" *)
        | `Rep_param_cmd_inst (v1, v2) ->
            let v1 = List.map (param env) v1 in
            let v2 = cmd_instruction env v2 in
            todo env (v1, v2)
      in
      todo env (v1, v2)
  | `Shell_inst (v1, v2) ->
      let v1 = token env v1 (* pattern [sS][hH][eE][lL][lL] *) in
      let v2 = string_array env v2 in
      todo env (v1, v2)
  | `Main_inst (v1, v2) ->
      let v1 =
        token env v1
        (* pattern [mM][aA][iI][nN][tT][aA][iI][nN][eE][rR] *)
      in
      let v2 = token env v2 (* pattern .* *) in
      todo env (v1, v2)
  | `Cross_build_inst (v1, v2) ->
      let v1 =
        token env v1
        (* pattern [cC][rR][oO][sS][sS]_[bB][uU][iI][lL][dD][a-zA-Z_]* *)
      in
      let v2 = token env v2 (* pattern .* *) in
      todo env (v1, v2)

let source_file (env : env) (xs : CST.source_file) =
  List.map
    (fun (v1, v2) ->
      let v1 =
        match v1 with
        | `Inst x -> instruction env x
        | `Comm tok -> token env tok
        (* pattern #.* *)
      in
      let v2 = token env v2 (* "\n" *) in
      todo env (v1, v2))
    xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
