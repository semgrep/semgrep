(**
   Boilerplate to be used as a template when mapping the objc CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_keyword_identifier (env : env) (x : CST.keyword_identifier) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* "id" *) token env tok
    )
  | `In tok -> R.Case ("In",
      (* "in" *) token env tok
    )
  | `Struct tok -> R.Case ("Struct",
      (* "struct" *) token env tok
    )
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  )

let map_anon_choice___cova_4925ac1 (env : env) (x : CST.anon_choice___cova_4925ac1) =
  (match x with
  | `X___cova tok -> R.Case ("X___cova",
      (* "__covariant" *) token env tok
    )
  | `X___cont tok -> R.Case ("X___cont",
      (* "__contravariant" *) token env tok
    )
  )

let map_primitive_type (env : env) (tok : CST.primitive_type) =
  (* primitive_type *) token env tok

let map_protocol_qualifier (env : env) (x : CST.protocol_qualifier) =
  (match x with
  | `Out tok -> R.Case ("Out",
      (* "out" *) token env tok
    )
  | `Inout tok -> R.Case ("Inout",
      (* "inout" *) token env tok
    )
  | `Bycopy tok -> R.Case ("Bycopy",
      (* "bycopy" *) token env tok
    )
  | `Byref tok -> R.Case ("Byref",
      (* "byref" *) token env tok
    )
  | `Oneway tok -> R.Case ("Oneway",
      (* "oneway" *) token env tok
    )
  | `In tok -> R.Case ("In",
      (* "in" *) token env tok
    )
  )

let map_visibility_specification (env : env) (x : CST.visibility_specification) =
  (match x with
  | `ATpr_8232993 tok -> R.Case ("ATpr_8232993",
      (* "@private" *) token env tok
    )
  | `ATpr_fea00dc tok -> R.Case ("ATpr_fea00dc",
      (* "@protected" *) token env tok
    )
  | `ATpa tok -> R.Case ("ATpa",
      (* "@package" *) token env tok
    )
  | `ATpu tok -> R.Case ("ATpu",
      (* "@public" *) token env tok
    )
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_system_lib_string (env : env) (tok : CST.system_lib_string) =
  (* system_lib_string *) token env tok

let map_pat_7a545ba (env : env) (tok : CST.pat_7a545ba) =
  (* pattern [^}]* *) token env tok

let map_imm_tok_lt (env : env) (tok : CST.imm_tok_lt) =
  (* "<" *) token env tok

let map_preproc_arg (env : env) (tok : CST.preproc_arg) =
  (* preproc_arg *) token env tok

let map_storage_class_specifier (env : env) (x : CST.storage_class_specifier) =
  (match x with
  | `Choice_extern x -> R.Case ("Choice_extern",
      (match x with
      | `Extern tok -> R.Case ("Extern",
          (* "extern" *) token env tok
        )
      | `Static tok -> R.Case ("Static",
          (* "static" *) token env tok
        )
      | `Auto tok -> R.Case ("Auto",
          (* "auto" *) token env tok
        )
      | `Regi tok -> R.Case ("Regi",
          (* "register" *) token env tok
        )
      | `Inline tok -> R.Case ("Inline",
          (* "inline" *) token env tok
        )
      | `X___inline tok -> R.Case ("X___inline",
          (* "__inline" *) token env tok
        )
      | `X___inline__ tok -> R.Case ("X___inline__",
          (* "__inline__" *) token env tok
        )
      | `X___forc tok -> R.Case ("X___forc",
          (* "__forceinline" *) token env tok
        )
      | `Thread_local tok -> R.Case ("Thread_local",
          (* "thread_local" *) token env tok
        )
      | `X___thread tok -> R.Case ("X___thread",
          (* "__thread" *) token env tok
        )
      )
    )
  | `X___inline__ tok -> R.Case ("X___inline__",
      (* "__inline__" *) token env tok
    )
  | `CG_EXTERN tok -> R.Case ("CG_EXTERN",
      (* "CG_EXTERN" *) token env tok
    )
  | `CG_INLINE tok -> R.Case ("CG_INLINE",
      (* "CG_INLINE" *) token env tok
    )
  | `FOUNDATION_EXPORT tok -> R.Case ("FOUNDATION_EXPORT",
      (* "FOUNDATION_EXPORT" *) token env tok
    )
  | `FOUNDATION_EXTERN tok -> R.Case ("FOUNDATION_EXTERN",
      (* "FOUNDATION_EXTERN" *) token env tok
    )
  | `FOUNDATION_STATIC_INLINE tok -> R.Case ("FOUNDATION_STATIC_INLINE",
      (* "FOUNDATION_STATIC_INLINE" *) token env tok
    )
  | `IBOu tok -> R.Case ("IBOu",
      (* "IBOutlet" *) token env tok
    )
  | `IBIn tok -> R.Case ("IBIn",
      (* "IBInspectable" *) token env tok
    )
  | `IB_DESIGNABLE tok -> R.Case ("IB_DESIGNABLE",
      (* "IB_DESIGNABLE" *) token env tok
    )
  | `NS_INLINE tok -> R.Case ("NS_INLINE",
      (* "NS_INLINE" *) token env tok
    )
  | `NS_VALID_UNTIL_END_OF_SCOPE tok -> R.Case ("NS_VALID_UNTIL_END_OF_SCOPE",
      (* "NS_VALID_UNTIL_END_OF_SCOPE" *) token env tok
    )
  | `OBJC_EXPORT tok -> R.Case ("OBJC_EXPORT",
      (* "OBJC_EXPORT" *) token env tok
    )
  | `OBJC_ROOT_CLASS tok -> R.Case ("OBJC_ROOT_CLASS",
      (* "OBJC_ROOT_CLASS" *) token env tok
    )
  | `UIKIT_EXTERN tok -> R.Case ("UIKIT_EXTERN",
      (* "UIKIT_EXTERN" *) token env tok
    )
  )

let map_anon_choice_BANG_67174d6 (env : env) (x : CST.anon_choice_BANG_67174d6) =
  (match x with
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  )

let map_typedefed_identifier (env : env) (x : CST.typedefed_identifier) =
  (match x with
  | `BOOL tok -> R.Case ("BOOL",
      (* "BOOL" *) token env tok
    )
  | `IMP tok -> R.Case ("IMP",
      (* "IMP" *) token env tok
    )
  | `SEL tok -> R.Case ("SEL",
      (* "SEL" *) token env tok
    )
  | `Class tok -> R.Case ("Class",
      (* "Class" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* "id" *) token env tok
    )
  )

let map_break_statement (env : env) ((v1, v2) : CST.break_statement) =
  let v1 = (* "break" *) token env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_platform (env : env) (x : CST.platform) =
  (match x with
  | `Ios tok -> R.Case ("Ios",
      (* "ios" *) token env tok
    )
  | `Tvos tok -> R.Case ("Tvos",
      (* "tvos" *) token env tok
    )
  | `Macos tok -> R.Case ("Macos",
      (* "macos" *) token env tok
    )
  | `Macosx tok -> R.Case ("Macosx",
      (* "macosx" *) token env tok
    )
  | `Watchos tok -> R.Case ("Watchos",
      (* "watchos" *) token env tok
    )
  )

let map_pat_9d92f6a (env : env) (tok : CST.pat_9d92f6a) =
  (* pattern #[ 	]*ifndef *) token env tok

let map_anon_choice_PLUS_da42005 (env : env) (x : CST.anon_choice_PLUS_da42005) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  )

let map_pat_cbe6e28 (env : env) (tok : CST.pat_cbe6e28) =
  (* pattern [^)]* *) token env tok

let map_pat_6e98ba5 (env : env) (tok : CST.pat_6e98ba5) =
  (* pattern #[ 	]*import *) token env tok

let map_anon_choice_signed_a0bfc19 (env : env) (x : CST.anon_choice_signed_a0bfc19) =
  (match x with
  | `Signed tok -> R.Case ("Signed",
      (* "signed" *) token env tok
    )
  | `Unsi tok -> R.Case ("Unsi",
      (* "unsigned" *) token env tok
    )
  | `Long tok -> R.Case ("Long",
      (* "long" *) token env tok
    )
  | `Short tok -> R.Case ("Short",
      (* "short" *) token env tok
    )
  )

let map_pat_0307ca2 (env : env) (tok : CST.pat_0307ca2) =
  (* pattern #[ 	]*elifdef *) token env tok

let map_false_ (env : env) (tok : CST.false_) =
  (* false *) token env tok

let map_gnu_asm_qualifier (env : env) (x : CST.gnu_asm_qualifier) =
  (match x with
  | `Vola tok -> R.Case ("Vola",
      (* "volatile" *) token env tok
    )
  | `X___vola__ tok -> R.Case ("X___vola__",
      (* "__volatile__" *) token env tok
    )
  | `Inline tok -> R.Case ("Inline",
      (* "inline" *) token env tok
    )
  | `Goto tok -> R.Case ("Goto",
      (* "goto" *) token env tok
    )
  )

let map_pat_56631e5 (env : env) (tok : CST.pat_56631e5) =
  (* pattern #[ 	]*else *) token env tok

let map_version_number (env : env) (tok : CST.version_number) =
  (* pattern \d+([\._]\d+)* *) token env tok

let map_null (env : env) (x : CST.null) =
  (match x with
  | `NULL tok -> R.Case ("NULL",
      (* "NULL" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "nullptr" *) token env tok
    )
  )

let map_semgrep_named_ellipsis (env : env) (tok : CST.semgrep_named_ellipsis) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_number_literal (env : env) (tok : CST.number_literal) =
  (* number_literal *) token env tok

let map_ms_unaligned_ptr_modifier (env : env) (x : CST.ms_unaligned_ptr_modifier) =
  (match x with
  | `X__unal tok -> R.Case ("X__unal",
      (* "_unaligned" *) token env tok
    )
  | `X___unal tok -> R.Case ("X___unal",
      (* "__unaligned" *) token env tok
    )
  )

let map_preproc_directive (env : env) (tok : CST.preproc_directive) =
  (* pattern #[ \t]*[a-zA-Z0-9]\w* *) token env tok

let map_pat_c46d1b2 (env : env) (tok : CST.pat_c46d1b2) =
  (* pattern #[ 	]*endif *) token env tok

let map_pat_ca8830e (env : env) (tok : CST.pat_ca8830e) =
  (* pattern #[ 	]*include *) token env tok

let map_imm_tok_prec_p1_pat_c7f65b4 (env : env) (tok : CST.imm_tok_prec_p1_pat_c7f65b4) =
  (* pattern "[^\\\\\"\\n]+" *) token env tok

let map_pat_a6d4183 (env : env) (tok : CST.pat_a6d4183) =
  (* pattern #[ 	]*elifndef *) token env tok

let map_ms_call_modifier (env : env) (x : CST.ms_call_modifier) =
  (match x with
  | `X___cdecl tok -> R.Case ("X___cdecl",
      (* "__cdecl" *) token env tok
    )
  | `X___clrc tok -> R.Case ("X___clrc",
      (* "__clrcall" *) token env tok
    )
  | `X___stdc tok -> R.Case ("X___stdc",
      (* "__stdcall" *) token env tok
    )
  | `X___fast tok -> R.Case ("X___fast",
      (* "__fastcall" *) token env tok
    )
  | `X___this tok -> R.Case ("X___this",
      (* "__thiscall" *) token env tok
    )
  | `X___vect tok -> R.Case ("X___vect",
      (* "__vectorcall" *) token env tok
    )
  )

let map_pat_3df6e71 (env : env) (tok : CST.pat_3df6e71) =
  (* pattern #[ 	]*if *) token env tok

let map_imm_tok_pat_509ec78 (env : env) (tok : CST.imm_tok_pat_509ec78) =
  (* pattern \r?\n *) token env tok

let map_pat_c3ea183 (env : env) (tok : CST.pat_c3ea183) =
  (* pattern #[ 	]*define *) token env tok

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_anon_choice_nore_63e931c (env : env) (x : CST.anon_choice_nore_63e931c) =
  (match x with
  | `Nore tok -> R.Case ("Nore",
      (* "noreturn" *) token env tok
    )
  | `Noth tok -> R.Case ("Noth",
      (* "nothrow" *) token env tok
    )
  )

let map_pat_bfeb4bb (env : env) (tok : CST.pat_bfeb4bb) =
  (* pattern #[ 	]*elif *) token env tok

let map_pat_25b90ba (env : env) (tok : CST.pat_25b90ba) =
  (* pattern #[ 	]*ifdef *) token env tok

let map_semgrep_metavar (env : env) (tok : CST.semgrep_metavar) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok

let map_imm_tok_pat_36637e2 (env : env) (tok : CST.imm_tok_pat_36637e2) =
  (* pattern "[^\\n']" *) token env tok

let map_pat_4946ad9 (env : env) (tok : CST.pat_4946ad9) =
  (* pattern #[ 	]*undef *) token env tok

let map_continue_statement (env : env) ((v1, v2) : CST.continue_statement) =
  let v1 = (* "continue" *) token env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_true_ (env : env) (tok : CST.true_) =
  (* true *) token env tok

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_anon_choice_DASHDASH_d11def2 (env : env) (x : CST.anon_choice_DASHDASH_d11def2) =
  (match x with
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  )

let map_ms_pointer_modifier (env : env) (x : CST.ms_pointer_modifier) =
  (match x with
  | `Ms_unal_ptr_modi x -> R.Case ("Ms_unal_ptr_modi",
      map_ms_unaligned_ptr_modifier env x
    )
  | `Ms_rest_modi tok -> R.Case ("Ms_rest_modi",
      (* "__restrict" *) token env tok
    )
  | `Ms_unsi_ptr_modi tok -> R.Case ("Ms_unsi_ptr_modi",
      (* "__uptr" *) token env tok
    )
  | `Ms_signed_ptr_modi tok -> R.Case ("Ms_signed_ptr_modi",
      (* "__sptr" *) token env tok
    )
  )

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 =
    (match v1 with
    | `AT_DQUOT (v1, v2) -> R.Case ("AT_DQUOT",
        let v1 = (* "@" *) token env v1 in
        let v2 = (* "\"" *) token env v2 in
        R.Tuple [v1; v2]
      )
    | `LDQUOT tok -> R.Case ("LDQUOT",
        (* "L\"" *) token env tok
      )
    | `UDQUOT_c163aae tok -> R.Case ("UDQUOT_c163aae",
        (* "u\"" *) token env tok
      )
    | `UDQUOT_df3447d tok -> R.Case ("UDQUOT_df3447d",
        (* "U\"" *) token env tok
      )
    | `U8DQUOT tok -> R.Case ("U8DQUOT",
        (* "u8\"" *) token env tok
      )
    | `DQUOT tok -> R.Case ("DQUOT",
        (* "\"" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Imm_tok_prec_p1_pat_c7f65b4 x -> R.Case ("Imm_tok_prec_p1_pat_c7f65b4",
          map_imm_tok_prec_p1_pat_c7f65b4 env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_pat_0307ca2_dbf6a9d (env : env) (x : CST.anon_choice_pat_0307ca2_dbf6a9d) =
  (match x with
  | `Pat_0307ca2 x -> R.Case ("Pat_0307ca2",
      map_pat_0307ca2 env x
    )
  | `Pat_a6d4183 x -> R.Case ("Pat_a6d4183",
      map_pat_a6d4183 env x
    )
  )

let map_preproc_call (env : env) ((v1, v2, v3) : CST.preproc_call) =
  let v1 = (* pattern #[ \t]*[a-zA-Z0-9]\w* *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_imm_tok_pat_509ec78 env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_pat_25b90ba_4a37f8c (env : env) (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  (match x with
  | `Pat_25b90ba x -> R.Case ("Pat_25b90ba",
      map_pat_25b90ba env x
    )
  | `Pat_9d92f6a x -> R.Case ("Pat_9d92f6a",
      map_pat_9d92f6a env x
    )
  )

let map_char_literal (env : env) ((v1, v2, v3) : CST.char_literal) =
  let v1 =
    (match v1 with
    | `LSQUOT tok -> R.Case ("LSQUOT",
        (* "L'" *) token env tok
      )
    | `USQUOT_d861d39 tok -> R.Case ("USQUOT_d861d39",
        (* "u'" *) token env tok
      )
    | `USQUOT_2701bdc tok -> R.Case ("USQUOT_2701bdc",
        (* "U'" *) token env tok
      )
    | `U8SQUOT tok -> R.Case ("U8SQUOT",
        (* "u8'" *) token env tok
      )
    | `SQUOT tok -> R.Case ("SQUOT",
        (* "'" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      | `Imm_tok_pat_36637e2 x -> R.Case ("Imm_tok_pat_36637e2",
          map_imm_tok_pat_36637e2 env x
        )
      )
    ) v2)
  in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_linemarker (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_linemarker) =
  let v1 = (* "#" *) token env v1 in
  let v2 = (* number_literal *) token env v2 in
  let v3 = map_string_literal env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* number_literal *) token env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* number_literal *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 = map_imm_tok_pat_509ec78 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_method_identifier (env : env) ((v1, v2, v3) : CST.method_identifier) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    R.List (List.map (fun x ->
      map_imm_tok_colon env x
    ) v2)
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 =
        R.List (List.map (fun x ->
          map_imm_tok_colon env x
        ) v2)
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_class_implementation_inheritance (env : env) (x : CST.class_implementation_inheritance) =
  (match x with
  | `COLON_id (v1, v2) -> R.Case ("COLON_id",
      let v1 = (* ":" *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  | `LPAR_id_RPAR (v1, v2, v3) -> R.Case ("LPAR_id_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_property_attribute (env : env) (x : CST.property_attribute) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Id_EQ_id_opt_COLON (v1, v2, v3, v4) -> R.Case ("Id_EQ_id_opt_COLON",
      let v1 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* ":" *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_anon_choice_type_id_d3c4b5f (env : env) (x : CST.anon_choice_type_id_d3c4b5f) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

let map_preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_imm_tok_pat_509ec78 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_preproc_undef (env : env) ((v1, v2, v3) : CST.preproc_undef) =
  let v1 = map_pat_4946ad9 env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = map_imm_tok_pat_509ec78 env v3 in
  R.Tuple [v1; v2; v3]

let map_ms_declspec_modifier (env : env) ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let v1 = (* "__declspec" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_type_id_fe6e1ce (env : env) (x : CST.anon_choice_type_id_fe6e1ce) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Vari_param tok -> R.Case ("Vari_param",
      (* "..." *) token env tok
    )
  )

let map_property_implementation (env : env) (x : CST.property_implementation) =
  (match x with
  | `ATsy_id_opt_EQ_id_rep_COMMA_id_opt_EQ_id_SEMI (v1, v2, v3, v4, v5) -> R.Case ("ATsy_id_opt_EQ_id_rep_COMMA_id_opt_EQ_id_SEMI",
      let v1 = (* "@synthesize" *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 =
              (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 =
        R.List (List.map (fun (v1, v2, v3) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
          in
          let v3 =
            (match v3 with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = (* "=" *) token env v1 in
                let v2 =
                  (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
                in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2; v3]
        ) v4)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `ATdy_opt_LPAR_id_rep_COMMA_id_SEMI (v1, v2, v3, v4, v5) -> R.Case ("ATdy_opt_LPAR_id_rep_COMMA_id_SEMI",
      let v1 = (* "@dynamic" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "(class)" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
      in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
          in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_anon_choice_type_id_1a79fc3 (env : env) (x : CST.anon_choice_type_id_1a79fc3) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  )

let map_module_import (env : env) ((v1, v2, v3, v4) : CST.module_import) =
  let v1 = (* "@import" *) token env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_protocol_reference_list (env : env) ((v1, v2, v3, v4) : CST.protocol_reference_list) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_rep_COMMA_opt_choice___cova_type_id_0fed85a (env : env) (xs : CST.anon_rep_COMMA_opt_choice___cova_type_id_0fed85a) =
  R.List (List.map (fun (v1, v2, v3) ->
    let v1 = (* "," *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_anon_choice___cova_4925ac1 env x
        ))
      | None -> R.Option None)
    in
    let v3 =
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
    in
    R.Tuple [v1; v2; v3]
  ) xs)

let map_anon_choice_num_lit_0c439c1 (env : env) (x : CST.anon_choice_num_lit_0c439c1) =
  (match x with
  | `Num_lit tok -> R.Case ("Num_lit",
      (* number_literal *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  )

let map_compatibility_alias_declaration (env : env) ((v1, v2, v3) : CST.compatibility_alias_declaration) =
  let v1 = (* "@compatibility_alias" *) token env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  R.Tuple [v1; v2; v3]

let map_concatenated_string (env : env) ((v1, v2) : CST.concatenated_string) =
  let v1 =
    (match v1 with
    | `Id_str_lit (v1, v2) -> R.Case ("Id_str_lit",
        let v1 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 = map_string_literal env v2 in
        R.Tuple [v1; v2]
      )
    | `Str_lit_str_lit (v1, v2) -> R.Case ("Str_lit_str_lit",
        let v1 = map_string_literal env v1 in
        let v2 = map_string_literal env v2 in
        R.Tuple [v1; v2]
      )
    | `Str_lit_id (v1, v2) -> R.Case ("Str_lit_id",
        let v1 = map_string_literal env v1 in
        let v2 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Str_lit x -> R.Case ("Str_lit",
          map_string_literal env x
        )
      | `Id tok -> R.Case ("Id",
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      )
    ) v2)
  in
  R.Tuple [v1; v2]

let map_generic_arguments (env : env) ((v1, v2, v3, v4) : CST.generic_arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_goto_statement (env : env) ((v1, v2, v3) : CST.goto_statement) =
  let v1 = (* "goto" *) token env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_gnu_asm_goto_list (env : env) ((v1, v2) : CST.gnu_asm_goto_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
            in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_preproc_defined (env : env) (x : CST.preproc_defined) =
  (match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) -> R.Case ("Defi_LPAR_id_RPAR",
      let v1 = (* "defined" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Defi_id (v1, v2) -> R.Case ("Defi_id",
      let v1 = (* "defined" *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  )

let map_field_identifier (env : env) (x : CST.field_identifier) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

let map_property_attributes_declaration (env : env) ((v1, v2, v3) : CST.property_attributes_declaration) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_property_attribute env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_property_attribute env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_params (env : env) ((v1, v2, v3, v4) : CST.preproc_params) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_type_id_d3c4b5f env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_type_id_d3c4b5f env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "..." *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_old_style_parameter_list (env : env) ((v1, v2, v3) : CST.old_style_parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_type_id_fe6e1ce env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_type_id_fe6e1ce env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_version (env : env) (x : CST.version) =
  (match x with
  | `Plat x -> R.Case ("Plat",
      map_platform env x
    )
  | `Vers_num tok -> R.Case ("Vers_num",
      (* pattern \d+([\._]\d+)* *) token env tok
    )
  | `Plat_LPAR_choice_num_lit_rep_COMMA_choice_num_lit_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Plat_LPAR_choice_num_lit_rep_COMMA_choice_num_lit_RPAR",
      let v1 = map_platform env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_anon_choice_num_lit_0c439c1 env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_anon_choice_num_lit_0c439c1 env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Conc_str x -> R.Case ("Conc_str",
      map_concatenated_string env x
    )
  )

let rec map_preproc_argument_list (env : env) ((v1, v2, v3) : CST.preproc_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_preproc_expression env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_preproc_expression env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_preproc_binary_expression (env : env) (x : CST.preproc_binary_expression) =
  (match x with
  | `Prep_exp_PLUS_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_PLUS_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_DASH_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_DASH_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_STAR_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_STAR_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_SLASH_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_SLASH_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_PERC_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_PERC_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BARBAR_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_AMPAMP_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_BAR_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BAR_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_HAT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_HAT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_AMP_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_AMP_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_EQEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BANGEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_GT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_GT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_GTEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_GTEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_LTEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_LTEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_LT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_LT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_LTLT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_LTLT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_GTGT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_GTGT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_preproc_call_expression (env : env) ((v1, v2) : CST.preproc_call_expression) =
  let v1 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = map_preproc_argument_list env v2 in
  R.Tuple [v1; v2]

and map_preproc_expression (env : env) (x : CST.preproc_expression) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      (match x with
      | `Id tok -> R.Case ("Id",
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      | `Prep_call_exp x -> R.Case ("Prep_call_exp",
          map_preproc_call_expression env x
        )
      | `Num_lit tok -> R.Case ("Num_lit",
          (* number_literal *) token env tok
        )
      | `Char_lit x -> R.Case ("Char_lit",
          map_char_literal env x
        )
      | `Prep_defi x -> R.Case ("Prep_defi",
          map_preproc_defined env x
        )
      | `Prep_un_exp x -> R.Case ("Prep_un_exp",
          map_preproc_unary_expression env x
        )
      | `Prep_bin_exp x -> R.Case ("Prep_bin_exp",
          map_preproc_binary_expression env x
        )
      | `Prep_paren_exp x -> R.Case ("Prep_paren_exp",
          map_preproc_parenthesized_expression env x
        )
      )
    )
  | `System_lib_str tok -> R.Case ("System_lib_str",
      (* system_lib_string *) token env tok
    )
  )

and map_preproc_parenthesized_expression (env : env) ((v1, v2, v3) : CST.preproc_parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_preproc_unary_expression (env : env) ((v1, v2) : CST.preproc_unary_expression) =
  let v1 = map_anon_choice_BANG_67174d6 env v1 in
  let v2 = map_preproc_expression env v2 in
  R.Tuple [v1; v2]

let map_field_designator (env : env) ((v1, v2) : CST.field_designator) =
  let v1 = (* "." *) token env v1 in
  let v2 = map_field_identifier env v2 in
  R.Tuple [v1; v2]

let map_preproc_function_def (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_function_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = map_preproc_params env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = map_imm_tok_pat_509ec78 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_anon_choice_type_id_f6d2043 (env : env) (x : CST.anon_choice_type_id_f6d2043) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Id_vers (v1, v2) -> R.Case ("Id_vers",
      let v1 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 = map_version env v2 in
      R.Tuple [v1; v2]
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

let map_anon_choice_str_lit_c4b005a (env : env) (x : CST.anon_choice_str_lit_c4b005a) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Conc_str x -> R.Case ("Conc_str",
      map_concatenated_string env x
    )
  | `Vers x -> R.Case ("Vers",
      map_version env x
    )
  | `Meth_id x -> R.Case ("Meth_id",
      map_method_identifier env x
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Id_LPAR_opt_meth_id_RPAR (v1, v2, v3, v4) -> R.Case ("Id_LPAR_opt_meth_id_RPAR",
      let v1 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_method_identifier env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_gnu_asm_clobber_list (env : env) ((v1, v2) : CST.gnu_asm_clobber_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_string_ env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_string_ env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_preproc_include (env : env) ((v1, v2, v3) : CST.preproc_include) =
  let v1 =
    (match v1 with
    | `Pat_ca8830e x -> R.Case ("Pat_ca8830e",
        map_pat_ca8830e env x
      )
    | `Pat_6e98ba5 x -> R.Case ("Pat_6e98ba5",
        map_pat_6e98ba5 env x
      )
    )
  in
  let v2 =
    (match v2 with
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    | `System_lib_str tok -> R.Case ("System_lib_str",
        (* system_lib_string *) token env tok
      )
    | `Id tok -> R.Case ("Id",
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    | `Prep_call_exp x -> R.Case ("Prep_call_exp",
        map_preproc_call_expression env x
      )
    )
  in
  let v3 = map_imm_tok_pat_509ec78 env v3 in
  R.Tuple [v1; v2; v3]

let map_availability_attribute_specifier (env : env) (x : CST.availability_attribute_specifier) =
  (match x with
  | `NS_AUTOMATED_REFCOUNT_UNAVAILABLE tok -> R.Case ("NS_AUTOMATED_REFCOUNT_UNAVAILABLE",
      (* "NS_AUTOMATED_REFCOUNT_UNAVAILABLE" *) token env tok
    )
  | `NS_ROOT_CLASS tok -> R.Case ("NS_ROOT_CLASS",
      (* "NS_ROOT_CLASS" *) token env tok
    )
  | `NS_UNAVAILABLE tok -> R.Case ("NS_UNAVAILABLE",
      (* "NS_UNAVAILABLE" *) token env tok
    )
  | `NS_REQUIRES_NIL_TERMINATION tok -> R.Case ("NS_REQUIRES_NIL_TERMINATION",
      (* "NS_REQUIRES_NIL_TERMINATION" *) token env tok
    )
  | `CF_RETURNS_RETAINED tok -> R.Case ("CF_RETURNS_RETAINED",
      (* "CF_RETURNS_RETAINED" *) token env tok
    )
  | `CF_RETURNS_NOT_RETAINED tok -> R.Case ("CF_RETURNS_NOT_RETAINED",
      (* "CF_RETURNS_NOT_RETAINED" *) token env tok
    )
  | `DEPRECATED_ATTRIBUTE tok -> R.Case ("DEPRECATED_ATTRIBUTE",
      (* "DEPRECATED_ATTRIBUTE" *) token env tok
    )
  | `UI_APPEARANCE_SELECTOR tok -> R.Case ("UI_APPEARANCE_SELECTOR",
      (* "UI_APPEARANCE_SELECTOR" *) token env tok
    )
  | `UNAVAILABLE_ATTRIBUTE tok -> R.Case ("UNAVAILABLE_ATTRIBUTE",
      (* "UNAVAILABLE_ATTRIBUTE" *) token env tok
    )
  | `Choice_CF_FORMAT_FUNCTION_LPAR_choice_str_lit_rep_COMMA_choice_str_lit_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Choice_CF_FORMAT_FUNCTION_LPAR_choice_str_lit_rep_COMMA_choice_str_lit_RPAR",
      let v1 =
        (match v1 with
        | `CF_FORMAT_FUNCTION tok -> R.Case ("CF_FORMAT_FUNCTION",
            (* "CF_FORMAT_FUNCTION" *) token env tok
          )
        | `NS_AVAILABLE tok -> R.Case ("NS_AVAILABLE",
            (* "NS_AVAILABLE" *) token env tok
          )
        | `X___IOS_AVAILABLE tok -> R.Case ("X___IOS_AVAILABLE",
            (* "__IOS_AVAILABLE" *) token env tok
          )
        | `NS_AVAILABLE_IOS tok -> R.Case ("NS_AVAILABLE_IOS",
            (* "NS_AVAILABLE_IOS" *) token env tok
          )
        | `API_AVAILABLE tok -> R.Case ("API_AVAILABLE",
            (* "API_AVAILABLE" *) token env tok
          )
        | `API_UNAVAILABLE tok -> R.Case ("API_UNAVAILABLE",
            (* "API_UNAVAILABLE" *) token env tok
          )
        | `API_DEPRECATED tok -> R.Case ("API_DEPRECATED",
            (* "API_DEPRECATED" *) token env tok
          )
        | `NS_ENUM_AVAILABLE_IOS tok -> R.Case ("NS_ENUM_AVAILABLE_IOS",
            (* "NS_ENUM_AVAILABLE_IOS" *) token env tok
          )
        | `NS_DEPRECATED_IOS tok -> R.Case ("NS_DEPRECATED_IOS",
            (* "NS_DEPRECATED_IOS" *) token env tok
          )
        | `NS_ENUM_DEPRECATED_IOS tok -> R.Case ("NS_ENUM_DEPRECATED_IOS",
            (* "NS_ENUM_DEPRECATED_IOS" *) token env tok
          )
        | `NS_FORMAT_FUNCTION tok -> R.Case ("NS_FORMAT_FUNCTION",
            (* "NS_FORMAT_FUNCTION" *) token env tok
          )
        | `DEPRECATED_MSG_ATTRIBUTE tok -> R.Case ("DEPRECATED_MSG_ATTRIBUTE",
            (* "DEPRECATED_MSG_ATTRIBUTE" *) token env tok
          )
        | `X___depr_msg tok -> R.Case ("X___depr_msg",
            (* "__deprecated_msg" *) token env tok
          )
        | `X___depr_enum_msg tok -> R.Case ("X___depr_enum_msg",
            (* "__deprecated_enum_msg" *) token env tok
          )
        | `NS_SWIFT_NAME tok -> R.Case ("NS_SWIFT_NAME",
            (* "NS_SWIFT_NAME" *) token env tok
          )
        | `NS_SWIFT_UNAVAILABLE tok -> R.Case ("NS_SWIFT_UNAVAILABLE",
            (* "NS_SWIFT_UNAVAILABLE" *) token env tok
          )
        | `NS_EXTENSION_UNAVAILABLE_IOS tok -> R.Case ("NS_EXTENSION_UNAVAILABLE_IOS",
            (* "NS_EXTENSION_UNAVAILABLE_IOS" *) token env tok
          )
        | `NS_CLASS_AVAILABLE_IOS tok -> R.Case ("NS_CLASS_AVAILABLE_IOS",
            (* "NS_CLASS_AVAILABLE_IOS" *) token env tok
          )
        | `NS_CLASS_DEPRECATED_IOS tok -> R.Case ("NS_CLASS_DEPRECATED_IOS",
            (* "NS_CLASS_DEPRECATED_IOS" *) token env tok
          )
        | `X___OSX_AVAILABLE_STARTING tok -> R.Case ("X___OSX_AVAILABLE_STARTING",
            (* "__OSX_AVAILABLE_STARTING" *) token env tok
          )
        )
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_anon_choice_str_lit_c4b005a env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_anon_choice_str_lit_c4b005a env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let rec map_abstract_array_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.abstract_array_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_type_qual_b00a56a env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_508611b env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_abstract_declarator (env : env) (x : CST.abstract_declarator) =
  (match x with
  | `Choice_abst_poin_decl x -> R.Case ("Choice_abst_poin_decl",
      (match x with
      | `Abst_poin_decl x -> R.Case ("Abst_poin_decl",
          map_abstract_pointer_declarator env x
        )
      | `Abst_func_decl x -> R.Case ("Abst_func_decl",
          map_abstract_function_declarator env x
        )
      | `Abst_array_decl x -> R.Case ("Abst_array_decl",
          map_abstract_array_declarator env x
        )
      | `Abst_paren_decl x -> R.Case ("Abst_paren_decl",
          map_abstract_parenthesized_declarator env x
        )
      )
    )
  | `Abst_blk_poin_decl (v1, v2, v3) -> R.Case ("Abst_blk_poin_decl",
      let v1 = (* "^" *) token env v1 in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_abstract_declarator env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_abstract_function_declarator (env : env) ((v1, v2, v3) : CST.abstract_function_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_parameter_list env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_attr_spec_73f6bce env) v3)
  in
  R.Tuple [v1; v2; v3]

and map_abstract_parenthesized_declarator (env : env) ((v1, v2, v3, v4) : CST.abstract_parenthesized_declarator) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (map_declaration_modifiers env) v2)
  in
  let v3 = map_abstract_declarator env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_abstract_pointer_declarator (env : env) ((v1, v2, v3) : CST.abstract_pointer_declarator) =
  let v1 = (* "*" *) token env v1 in
  let v2 =
    R.List (List.map (map_declaration_modifiers env) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_alignas_qualifier (env : env) ((v1, v2, v3, v4) : CST.alignas_qualifier) =
  let v1 =
    (match v1 with
    | `Alignas tok -> R.Case ("Alignas",
        (* "alignas" *) token env tok
      )
    | `X__Alignas tok -> R.Case ("X__Alignas",
        (* "_Alignas" *) token env tok
      )
    )
  in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_anon_choice_exp_86a4e82 env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_alignof_expression (env : env) ((v1, v2, v3, v4) : CST.alignof_expression) =
  let v1 =
    (match v1 with
    | `X___alig__ tok -> R.Case ("X___alig__",
        (* "__alignof__" *) token env tok
      )
    | `X___alig tok -> R.Case ("X___alig",
        (* "__alignof" *) token env tok
      )
    | `X__alig tok -> R.Case ("X__alig",
        (* "_alignof" *) token env tok
      )
    | `Alig tok -> R.Case ("Alig",
        (* "alignof" *) token env tok
      )
    | `X__Alig tok -> R.Case ("X__Alig",
        (* "_Alignof" *) token env tok
      )
    )
  in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_descriptor env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_anon_choice_DOTDOTDOT_18da608 (env : env) (x : CST.anon_choice_DOTDOTDOT_18da608) =
  (match x with
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  | `C_meth_param_rep_COMMA_c_meth_param (v1, v2) -> R.Case ("C_meth_param_rep_COMMA_c_meth_param",
      let v1 = map_c_method_parameter env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_c_method_parameter env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_attr_spec_73f6bce (env : env) (x : CST.anon_choice_attr_spec_73f6bce) =
  (match x with
  | `Attr_spec x -> R.Case ("Attr_spec",
      map_attribute_specifier env x
    )
  | `Type_qual x -> R.Case ("Type_qual",
      map_type_qualifier env x
    )
  )

and map_anon_choice_attr_spec_c763cd2 (env : env) (x : CST.anon_choice_attr_spec_c763cd2) =
  (match x with
  | `Attr_spec x -> R.Case ("Attr_spec",
      map_attribute_specifier env x
    )
  | `Attr_decl x -> R.Case ("Attr_decl",
      map_attribute_declaration env x
    )
  )

and map_anon_choice_blk_item_e6161e0 (env : env) (x : CST.anon_choice_blk_item_e6161e0) =
  (match x with
  | `Blk_item x -> R.Case ("Blk_item",
      map_block_item env x
    )
  | `Attr_spec x -> R.Case ("Attr_spec",
      map_attribute_specifier env x
    )
  | `Prop_impl x -> R.Case ("Prop_impl",
      map_property_implementation env x
    )
  )

and map_anon_choice_decl_f8b0ff3 (env : env) (x : CST.anon_choice_decl_f8b0ff3) =
  (match x with
  | `Decl x -> R.Case ("Decl",
      map_declarator env x
    )
  | `Init_decl x -> R.Case ("Init_decl",
      map_init_declarator env x
    )
  )

and map_anon_choice_decl_opt_gnu_asm_exp_9a4dcce (env : env) (x : CST.anon_choice_decl_opt_gnu_asm_exp_9a4dcce) =
  (match x with
  | `Decl_opt_gnu_asm_exp (v1, v2) -> R.Case ("Decl_opt_gnu_asm_exp",
      let v1 = map_declarator env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_gnu_asm_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Init_decl x -> R.Case ("Init_decl",
      map_init_declarator env x
    )
  | `Type_qual_id (v1, v2) -> R.Case ("Type_qual_id",
      let v1 = map_type_qualifier env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_enum_opt_decl_modifs_b6b2bce (env : env) (x : CST.anon_choice_enum_opt_decl_modifs_b6b2bce) =
  (match x with
  | `Enum_opt_decl_modifs (v1, v2) -> R.Case ("Enum_opt_decl_modifs",
      let v1 = map_enumerator env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_declaration_modifiers env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_ifdef_in_enum (v1, v2, v3, v4, v5) -> R.Case ("Prep_ifdef_in_enum",
      let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            (match x with
            | `Choice_prep_else_in_enum x -> R.Case ("Choice_prep_else_in_enum",
                map_anon_choice_prep_else_in_enum_fd2b048 env x
              )
            | `Prep_elif x -> R.Case ("Prep_elif",
                map_preproc_elifdef env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v5 = map_pat_c46d1b2 env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_exp_3078596 (env : env) (x : CST.anon_choice_exp_3078596) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Init_list x -> R.Case ("Init_list",
      map_initializer_list env x
    )
  )

and map_anon_choice_exp_508611b (env : env) (x : CST.anon_choice_exp_508611b) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

and map_anon_choice_exp_55b4dba (env : env) (x : CST.anon_choice_exp_55b4dba) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Comma_exp x -> R.Case ("Comma_exp",
      map_comma_expression env x
    )
  )

and map_anon_choice_exp_7add206 (env : env) (x : CST.anon_choice_exp_7add206) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Typeof_spec x -> R.Case ("Typeof_spec",
      map_typeof_specifier env x
    )
  )

and map_anon_choice_exp_86a4e82 (env : env) (x : CST.anon_choice_exp_86a4e82) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Type_desc x -> R.Case ("Type_desc",
      map_type_descriptor env x
    )
  )

and map_anon_choice_field_decl_ac513ea (env : env) (x : CST.anon_choice_field_decl_ac513ea) =
  (match x with
  | `Field_decl x -> R.Case ("Field_decl",
      map_field_declarator env x
    )
  | `Enum_spec x -> R.Case ("Enum_spec",
      map_enum_specifier env x
    )
  )

and map_anon_choice_init_pair_1a6981e (env : env) (x : CST.anon_choice_init_pair_1a6981e) =
  (match x with
  | `Init_pair x -> R.Case ("Init_pair",
      map_initializer_pair env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Init_list x -> R.Case ("Init_list",
      map_initializer_list env x
    )
  )

and map_anon_choice_opt_type_qual_choice_exp_8a9ade8 (env : env) (x : CST.anon_choice_opt_type_qual_choice_exp_8a9ade8) =
  (match x with
  | `Opt_type_qual_choice_exp (v1, v2) -> R.Case ("Opt_type_qual_choice_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_qualifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_anon_choice_exp_7add206 env v2 in
      R.Tuple [v1; v2]
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  )

and map_anon_choice_param_decl_4ac2852 (env : env) (x : CST.anon_choice_param_decl_4ac2852) =
  (match x with
  | `Param_decl (v1, v2) -> R.Case ("Param_decl",
      let v1 = map_declaration_specifiers env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Decl_opt_decl_modifs (v1, v2) -> R.Case ("Decl_opt_decl_modifs",
                let v1 = map_declarator env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_declaration_modifiers env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              )
            | `Abst_decl x -> R.Case ("Abst_decl",
                map_abstract_declarator env x
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Vari_param tok -> R.Case ("Vari_param",
      (* "..." *) token env tok
    )
  )

and map_anon_choice_prep_else_8b52b0f (env : env) (x : CST.anon_choice_prep_else_8b52b0f) =
  (match x with
  | `Prep_else (v1, v2) -> R.Case ("Prep_else",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_blk_item_e6161e0 env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_anon_choice_blk_item_e6161e0 env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_8b52b0f env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_enum_fd2b048 (env : env) (x : CST.anon_choice_prep_else_in_enum_fd2b048) =
  (match x with
  | `Prep_else_in_enum (v1, v2) -> R.Case ("Prep_else_in_enum",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_enum (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_fd2b048 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_field_decl_list_1fef6b2 (env : env) (x : CST.anon_choice_prep_else_in_field_decl_list_1fef6b2) =
  (match x with
  | `Prep_else_in_field_decl_list (v1, v2) -> R.Case ("Prep_else_in_field_decl_list",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (map_field_declaration_list_item env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_field_decl_list_65bc06e (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_field_decl_list_65bc06e",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_field_declaration_list_item env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_field_decl_list_1fef6b2 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Prep_elif_in_field_decl_list_3f47a97 (v1, v2, v3, v4) -> R.Case ("Prep_elif_in_field_decl_list_3f47a97",
      let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        R.List (List.map (map_field_declaration_list_item env) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_field_decl_list_1fef6b2 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_choice_prep_else_in_impl_defi_a30bcf7 (env : env) (x : CST.anon_choice_prep_else_in_impl_defi_a30bcf7) =
  (match x with
  | `Prep_else_in_impl_defi (v1, v2) -> R.Case ("Prep_else_in_impl_defi",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (map_implementation_definition env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_impl_defi (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_impl_defi",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_implementation_definition env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_impl_defi_a30bcf7 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_inte_decl_eefbd14 (env : env) (x : CST.anon_choice_prep_else_in_inte_decl_eefbd14) =
  (match x with
  | `Prep_else_in_inte_decl (v1, v2) -> R.Case ("Prep_else_in_inte_decl",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (map_interface_declaration env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_inte_decl (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_inte_decl",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_interface_declaration env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_inte_decl_eefbd14 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_type_id_opt_field_decl_list_9aebd83 (env : env) (x : CST.anon_choice_type_id_opt_field_decl_list_9aebd83) =
  (match x with
  | `Id_opt_field_decl_list (v1, v2) -> R.Case ("Id_opt_field_decl_list",
      let v1 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_field_declaration_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Field_decl_list x -> R.Case ("Field_decl_list",
      map_field_declaration_list env x
    )
  )

and map_anon_choice_type_name_2868ede (env : env) (x : CST.anon_choice_type_name_2868ede) =
  (match x with
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Para_args x -> R.Case ("Para_args",
      map_parameterized_arguments env x
    )
  )

and map_anon_choice_type_qual_b00a56a (env : env) (x : CST.anon_choice_type_qual_b00a56a) =
  (match x with
  | `Type_qual x -> R.Case ("Type_qual",
      map_type_qualifier env x
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  )

and map_anon_choice_vers_21e491c (env : env) (x : CST.anon_choice_vers_21e491c) =
  (match x with
  | `Vers x -> R.Case ("Vers",
      map_version env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_anon_rep_opt_meth_sele_meth_param_f14b947 (env : env) (xs : CST.anon_rep_opt_meth_sele_meth_param_f14b947) =
  R.List (List.map (fun (v1, v2) ->
    let v1 =
      (match v1 with
      | Some x -> R.Option (Some (
          map_method_selector env x
        ))
      | None -> R.Option None)
    in
    let v2 = map_method_parameter env v2 in
    R.Tuple [v1; v2]
  ) xs)

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Opt_choice_opt_type_qual_choice_exp_rep_COMMA_choice_opt_type_qual_choice_exp opt -> R.Case ("Opt_choice_opt_type_qual_choice_exp_rep_COMMA_choice_opt_type_qual_choice_exp",
        (match opt with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              map_anon_choice_opt_type_qual_choice_exp_8a9ade8 env v1
            in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  map_anon_choice_opt_type_qual_choice_exp_8a9ade8 env v2
                in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      )
    | `Id_imm_tok_lt_type_name_rep_COMMA_type_name_GT (v1, v2, v3, v4, v5) -> R.Case ("Id_imm_tok_lt_type_name_rep_COMMA_type_name_GT",
        let v1 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 = map_imm_tok_lt env v2 in
        let v3 = map_type_name env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_name env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        let v5 = (* ">" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `Objc_bridge x -> R.Case ("Objc_bridge",
        map_objc_bridge env x
      )
    | `Avai x -> R.Case ("Avai",
        map_availability env x
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.array_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_type_qual_b00a56a env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_508611b env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_array_field_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.array_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_type_qual_b00a56a env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_508611b env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_assignment_expression (env : env) ((v1, v2, v3) : CST.assignment_expression) =
  let v1 = map_assignment_left_expression env v1 in
  let v2 =
    (match v2 with
    | `EQ tok -> R.Case ("EQ",
        (* "=" *) token env tok
      )
    | `STAREQ tok -> R.Case ("STAREQ",
        (* "*=" *) token env tok
      )
    | `SLASHEQ tok -> R.Case ("SLASHEQ",
        (* "/=" *) token env tok
      )
    | `PERCEQ tok -> R.Case ("PERCEQ",
        (* "%=" *) token env tok
      )
    | `PLUSEQ tok -> R.Case ("PLUSEQ",
        (* "+=" *) token env tok
      )
    | `DASHEQ tok -> R.Case ("DASHEQ",
        (* "-=" *) token env tok
      )
    | `LTLTEQ tok -> R.Case ("LTLTEQ",
        (* "<<=" *) token env tok
      )
    | `GTGTEQ tok -> R.Case ("GTGTEQ",
        (* ">>=" *) token env tok
      )
    | `AMPEQ tok -> R.Case ("AMPEQ",
        (* "&=" *) token env tok
      )
    | `HATEQ tok -> R.Case ("HATEQ",
        (* "^=" *) token env tok
      )
    | `BAREQ tok -> R.Case ("BAREQ",
        (* "|=" *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_assignment_left_expression (env : env) (x : CST.assignment_left_expression) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Field_exp x -> R.Case ("Field_exp",
      map_field_expression env x
    )
  | `Poin_exp x -> R.Case ("Poin_exp",
      map_pointer_expression env x
    )
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_atomic_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.atomic_declaration) =
  let v1 = (* "_Atomic" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_specifier env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_field_identifier env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 = (* "::" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_expression env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_expression env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_attribute_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_declaration) =
  let v1 = (* "[" *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_attribute env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* "]" *) token env v5 in
  let v6 = (* "]" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_attribute_specifier (env : env) ((v1, v2, v3, v4) : CST.attribute_specifier) =
  let v1 =
    (match v1 with
    | `X___attr__ tok -> R.Case ("X___attr__",
        (* "__attribute__" *) token env tok
      )
    | `X___attr tok -> R.Case ("X___attr",
        (* "__attribute" *) token env tok
      )
    )
  in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | `Arg_list x -> R.Case ("Arg_list",
        map_argument_list env x
      )
    | `LPAR_choice_nore_rep_COMMA_choice_nore_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_choice_nore_rep_COMMA_choice_nore_RPAR",
        let v1 = (* "(" *) token env v1 in
        let v2 = map_anon_choice_nore_63e931c env v2 in
        let v3 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_nore_63e931c env v2 in
            R.Tuple [v1; v2]
          ) v3)
        in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_attributed_field_declarator (env : env) ((v1, v2) : CST.attributed_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 =
    R.List (List.map (map_attribute_declaration env) v2)
  in
  R.Tuple [v1; v2]

and map_attributed_statement (env : env) ((v1, v2) : CST.attributed_statement) =
  let v1 =
    R.List (List.map (map_attribute_declaration env) v1)
  in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_availability (env : env) ((v1, v2, v3, v4, v5, v6) : CST.availability) =
  let v1 = (* "availability" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_anon_choice_vers_21e491c env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_anon_choice_vers_21e491c env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v5)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_block_item (env : env) (x : CST.block_item) =
  (match x with
  | `Choice_choice_func_defi x -> R.Case ("Choice_choice_func_defi",
      (match x with
      | `Choice_func_defi x -> R.Case ("Choice_func_defi",
          (match x with
          | `Func_defi x -> R.Case ("Func_defi",
              map_function_definition env x
            )
          | `Old_style_func_defi x -> R.Case ("Old_style_func_defi",
              map_old_style_function_definition env x
            )
          | `Link_spec x -> R.Case ("Link_spec",
              map_linkage_specification env x
            )
          | `Decl x -> R.Case ("Decl",
              map_declaration env x
            )
          | `Stmt x -> R.Case ("Stmt",
              map_statement env x
            )
          | `Attr_stmt x -> R.Case ("Attr_stmt",
              map_attributed_statement env x
            )
          | `Type_defi x -> R.Case ("Type_defi",
              map_type_definition env x
            )
          | `Empty_decl x -> R.Case ("Empty_decl",
              map_empty_declaration env x
            )
          | `Prep_if x -> R.Case ("Prep_if",
              map_preproc_if env x
            )
          | `Prep_ifdef x -> R.Case ("Prep_ifdef",
              map_preproc_ifdef env x
            )
          | `Prep_incl x -> R.Case ("Prep_incl",
              map_preproc_include env x
            )
          | `Prep_def x -> R.Case ("Prep_def",
              map_preproc_def env x
            )
          | `Prep_func_def x -> R.Case ("Prep_func_def",
              map_preproc_function_def env x
            )
          | `Prep_call x -> R.Case ("Prep_call",
              map_preproc_call env x
            )
          )
        )
      | `Class_decl x -> R.Case ("Class_decl",
          map_class_declaration env x
        )
      | `Class_inte x -> R.Case ("Class_inte",
          map_class_interface env x
        )
      | `Class_impl x -> R.Case ("Class_impl",
          map_class_implementation env x
        )
      | `Prot_decl x -> R.Case ("Prot_decl",
          map_protocol_declaration env x
        )
      | `Prot_forw_decl x -> R.Case ("Prot_forw_decl",
          map_protocol_forward_declaration env x
        )
      | `Module_import x -> R.Case ("Module_import",
          map_module_import env x
        )
      | `Comp_alias_decl x -> R.Case ("Comp_alias_decl",
          map_compatibility_alias_declaration env x
        )
      | `Prep_undef x -> R.Case ("Prep_undef",
          map_preproc_undef env x
        )
      | `Prep_line x -> R.Case ("Prep_line",
          map_preproc_linemarker env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_c_method_parameter (env : env) ((v1, v2, v3) : CST.c_method_parameter) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 = map_anon_choice_decl_f8b0ff3 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_decl_f8b0ff3 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_case_statement (env : env) ((v1, v2, v3) : CST.case_statement) =
  let v1 =
    (match v1 with
    | `Case_exp (v1, v2) -> R.Case ("Case_exp",
        let v1 = (* "case" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      )
    | `Defa tok -> R.Case ("Defa",
        (* "default" *) token env tok
      )
    )
  in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    R.List (List.map (fun x ->
      (match x with
      | `Choice_attr_stmt x -> R.Case ("Choice_attr_stmt",
          map_non_case_statement env x
        )
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Type_defi x -> R.Case ("Type_defi",
          map_type_definition env x
        )
      )
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_cast_expression (env : env) (x : CST.cast_expression) =
  (match x with
  | `LPAR_choice_type_desc_RPAR_exp (v1, v2, v3, v4) -> R.Case ("LPAR_choice_type_desc_RPAR_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | `Type_desc x -> R.Case ("Type_desc",
            map_type_descriptor env x
          )
        | `Typeof_spec x -> R.Case ("Typeof_spec",
            map_typeof_specifier env x
          )
        | `Para_args x -> R.Case ("Para_args",
            map_parameterized_arguments env x
          )
        )
      in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Choice___real_exp (v1, v2) -> R.Case ("Choice___real_exp",
      let v1 =
        (match v1 with
        | `X___real tok -> R.Case ("X___real",
            (* "__real" *) token env tok
          )
        | `X___imag tok -> R.Case ("X___imag",
            (* "__imag" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 =
    (match v1 with
    | `ATca tok -> R.Case ("ATca",
        (* "@catch" *) token env tok
      )
    | `X___catch tok -> R.Case ("X___catch",
        (* "__catch" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
              (* "..." *) token env tok
            )
          | `Type_name x -> R.Case ("Type_name",
              map_type_name env x
            )
          )
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_class_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_declaration) =
  let v1 = (* "@" *) token env v1 in
  let v2 = (* "class" *) token env v2 in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_parameterized_arguments env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_parameterized_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v5)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_implementation (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_implementation) =
  let v1 = map_class_implementation_header env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_class_implementation_inheritance env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_instance_variables env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    R.List (List.map (map_implementation_definition env) v5)
  in
  let v6 = (* "@end" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_implementation_header (env : env) ((v1, v2, v3, v4) : CST.class_implementation_header) =
  let v1 =
    R.List (List.map (map_declaration_modifiers env) v1)
  in
  let v2 = (* "@implementation" *) token env v2 in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_class_interface (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_interface) =
  let v1 = map_class_interface_header env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_class_interface_inheritance env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_parameterized_arguments env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_instance_variables env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    R.List (List.map (map_interface_declaration env) v6)
  in
  let v7 = (* "@end" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_class_interface_header (env : env) ((v1, v2, v3, v4) : CST.class_interface_header) =
  let v1 =
    R.List (List.map (map_declaration_modifiers env) v1)
  in
  let v2 = (* "@interface" *) token env v2 in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_class_interface_inheritance (env : env) (x : CST.class_interface_inheritance) =
  (match x with
  | `COLON_id_opt_para_args (v1, v2, v3) -> R.Case ("COLON_id_opt_para_args",
      let v1 = (* ":" *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_parameterized_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `LPAR_opt_id_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_id_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_comma_expression (env : env) ((v1, v2, v3) : CST.comma_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 = map_anon_choice_exp_55b4dba env v3 in
  R.Tuple [v1; v2; v3]

and map_compound_literal_expression (env : env) ((v1, v2, v3, v4) : CST.compound_literal_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_descriptor env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_initializer_list env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_compound_statement (env : env) ((v1, v2, v3, v4) : CST.compound_statement) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "@autoreleasepool" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "{" *) token env v2 in
  let v3 = R.List (List.map (map_block_item env) v3) in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_conditional_expression (env : env) ((v1, v2, v3, v4, v5) : CST.conditional_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 =
    map_anon_choice_decl_opt_gnu_asm_exp_9a4dcce env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        map_anon_choice_decl_opt_gnu_asm_exp_9a4dcce env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_declaration_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_block_item env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_modifiers (env : env) (x : CST.declaration_modifiers) =
  (match x with
  | `Choice_stor_class_spec x -> R.Case ("Choice_stor_class_spec",
      (match x with
      | `Stor_class_spec x -> R.Case ("Stor_class_spec",
          map_storage_class_specifier env x
        )
      | `Type_qual x -> R.Case ("Type_qual",
          map_type_qualifier env x
        )
      | `Attr_spec x -> R.Case ("Attr_spec",
          map_attribute_specifier env x
        )
      | `Attr_decl x -> R.Case ("Attr_decl",
          map_attribute_declaration env x
        )
      | `Ms_decl_modi x -> R.Case ("Ms_decl_modi",
          map_ms_declspec_modifier env x
        )
      )
    )
  | `Avai_attr_spec x -> R.Case ("Avai_attr_spec",
      map_availability_attribute_specifier env x
    )
  | `Attr_decl x -> R.Case ("Attr_decl",
      map_attribute_declaration env x
    )
  )

and map_declaration_specifiers (env : env) ((v1, v2, v3) : CST.declaration_specifiers) =
  let v1 =
    R.List (List.map (map_declaration_modifiers env) v1)
  in
  let v2 = map_type_specifier env v2 in
  let v3 =
    R.List (List.map (map_declaration_modifiers env) v3)
  in
  R.Tuple [v1; v2; v3]

and map_declarator (env : env) (x : CST.declarator) =
  (match x with
  | `Poin_decl x -> R.Case ("Poin_decl",
      map_pointer_declarator env x
    )
  | `Func_decl (v1, v2, v3, v4) -> R.Case ("Func_decl",
      let v1 = map_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_gnu_asm_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        R.List (List.map (map_attribute_specifier env) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Array_decl x -> R.Case ("Array_decl",
      map_array_declarator env x
    )
  | `Paren_decl x -> R.Case ("Paren_decl",
      map_parenthesized_declarator env x
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Blk_poin_decl (v1, v2, v3) -> R.Case ("Blk_poin_decl",
      let v1 = (* "^" *) token env v1 in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      let v3 = map_declarator env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_dictionary_pair (env : env) ((v1, v2, v3) : CST.dictionary_pair) =
  let v1 = map_expression env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = (* "while" *) token env v3 in
  let v4 = map_parenthesized_expression env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_empty_declaration (env : env) ((v1, v2) : CST.empty_declaration) =
  let v1 = map_type_specifier env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_enum_specifier (env : env) ((v1, v2, v3, v4, v5) : CST.enum_specifier) =
  let v1 = (* "enum" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_ms_declspec_modifier env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | `Id_opt_COLON_choice_id_opt_enum_list (v1, v2, v3) -> R.Case ("Id_opt_COLON_choice_id_opt_enum_list",
        let v1 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* ":" *) token env v1 in
              let v2 = map_anon_choice_type_id_1a79fc3 env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_enumerator_list env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Opt_COLON_id_enum_list (v1, v2) -> R.Case ("Opt_COLON_id_enum_list",
        let v1 =
          (match v1 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* ":" *) token env v1 in
              let v2 =
                (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v2 = map_enumerator_list env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_enumerator (env : env) ((v1, v2) : CST.enumerator) =
  let v1 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.enumerator_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          map_anon_choice_enum_opt_decl_modifs_b6b2bce env v1
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 =
              map_anon_choice_enum_opt_decl_modifs_b6b2bce env v2
            in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_choice_choice_cond_exp x -> R.Case ("Choice_choice_choice_cond_exp",
      (match x with
      | `Choice_choice_cond_exp x -> R.Case ("Choice_choice_cond_exp",
          map_expression_not_binary env x
        )
      | `Bin_exp x -> R.Case ("Bin_exp",
          map_binary_expression env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_named_ellips tok -> R.Case ("Semg_named_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_expression_not_binary (env : env) (x : CST.expression_not_binary) =
  (match x with
  | `Choice_cond_exp x -> R.Case ("Choice_cond_exp",
      (match x with
      | `Cond_exp x -> R.Case ("Cond_exp",
          map_conditional_expression env x
        )
      | `Assign_exp x -> R.Case ("Assign_exp",
          map_assignment_expression env x
        )
      | `Un_exp x -> R.Case ("Un_exp",
          map_unary_expression env x
        )
      | `Update_exp x -> R.Case ("Update_exp",
          map_update_expression env x
        )
      | `Cast_exp x -> R.Case ("Cast_exp",
          map_cast_expression env x
        )
      | `Poin_exp x -> R.Case ("Poin_exp",
          map_pointer_expression env x
        )
      | `Sizeof_exp x -> R.Case ("Sizeof_exp",
          map_sizeof_expression env x
        )
      | `Alig_exp x -> R.Case ("Alig_exp",
          map_alignof_expression env x
        )
      | `Offs_exp x -> R.Case ("Offs_exp",
          map_offsetof_expression env x
        )
      | `Gene_exp x -> R.Case ("Gene_exp",
          map_generic_expression env x
        )
      | `Subs_exp x -> R.Case ("Subs_exp",
          map_subscript_expression env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      | `Field_exp x -> R.Case ("Field_exp",
          map_field_expression env x
        )
      | `Comp_lit_exp x -> R.Case ("Comp_lit_exp",
          map_compound_literal_expression env x
        )
      | `Id tok -> R.Case ("Id",
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      | `Num_lit tok -> R.Case ("Num_lit",
          (* number_literal *) token env tok
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `True tok -> R.Case ("True",
          (* true *) token env tok
        )
      | `False tok -> R.Case ("False",
          (* false *) token env tok
        )
      | `Null x -> R.Case ("Null",
          map_null env x
        )
      | `Char_lit x -> R.Case ("Char_lit",
          map_char_literal env x
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      | `Gnu_asm_exp x -> R.Case ("Gnu_asm_exp",
          map_gnu_asm_expression env x
        )
      | `Exte_exp x -> R.Case ("Exte_exp",
          map_extension_expression env x
        )
      )
    )
  | `Mess_exp (v1, v2, v3, v4) -> R.Case ("Mess_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Gene_spec x -> R.Case ("Gene_spec",
            map_generic_specifier env x
          )
        )
      in
      let v3 =
        R.List (List.map (fun x ->
          (match x with
          | `Id_rep_COLON_exp_rep_COMMA_exp (v1, v2) -> R.Case ("Id_rep_COLON_exp_rep_COMMA_exp",
              let v1 =
                (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
              in
              let v2 =
                R.List (List.map (fun (v1, v2, v3) ->
                  let v1 = (* ":" *) token env v1 in
                  let v2 = map_expression env v2 in
                  let v3 =
                    R.List (List.map (fun (v1, v2) ->
                      let v1 = (* "," *) token env v1 in
                      let v2 = map_expression env v2 in
                      R.Tuple [v1; v2]
                    ) v3)
                  in
                  R.Tuple [v1; v2; v3]
                ) v2)
              in
              R.Tuple [v1; v2]
            )
          | `Semg_ellips tok -> R.Case ("Semg_ellips",
              (* "..." *) token env tok
            )
          )
        ) v3)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Sele_exp (v1, v2, v3, v4) -> R.Case ("Sele_exp",
      let v1 = (* "@selector" *) token env v1 in
      let v2 = R.List (List.map (token env (* "(" *)) v2) in
      let v3 =
        (match v3 with
        | `Id tok -> R.Case ("Id",
            (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
          )
        | `Meth_id x -> R.Case ("Meth_id",
            map_method_identifier env x
          )
        | `Pat_cbe6e28 x -> R.Case ("Pat_cbe6e28",
            map_pat_cbe6e28 env x
          )
        )
      in
      let v4 = R.List (List.map (token env (* ")" *)) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Avai_exp (v1, v2, v3, v4, v5) -> R.Case ("Avai_exp",
      let v1 =
        (match v1 with
        | `ATav tok -> R.Case ("ATav",
            (* "@available" *) token env tok
          )
        | `X___buil_avai tok -> R.Case ("X___buil_avai",
            (* "__builtin_available" *) token env tok
          )
        )
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_anon_choice_type_id_f6d2043 env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_anon_choice_type_id_f6d2043 env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Range_exp (v1, v2, v3) -> R.Case ("Range_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "..." *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Blk_lit (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Blk_lit",
      let v1 = (* "^" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_name env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_compound_statement env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Dict_lit (v1, v2, v3, v4) -> R.Case ("Dict_lit",
      let v1 = (* "@" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_dictionary_pair env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_dictionary_pair env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Array_lit (v1, v2, v3, v4) -> R.Case ("Array_lit",
      let v1 = (* "@" *) token env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_expression env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* "," *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `At_exp (v1, v2) -> R.Case ("At_exp",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Encode_exp (v1, v2, v3, v4) -> R.Case ("Encode_exp",
      let v1 = (* "@encode" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_name env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Va_arg_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Va_arg_exp",
      let v1 = (* "va_arg" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 = map_type_descriptor env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_keyword_identifier env x
    )
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_extension_expression (env : env) ((v1, v2) : CST.extension_expression) =
  let v1 = (* "__extension__" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_field_declaration_list_item env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_field_declaration_list_item (env : env) (x : CST.field_declaration_list_item) =
  (match x with
  | `Field_decl (v1, v2, v3, v4, v5) -> R.Case ("Field_decl",
      let v1 = map_declaration_specifiers env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_anon_choice_field_decl_ac513ea env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_bitfield_clause env x
                ))
              | None -> R.Option None)
            in
            let v3 =
              R.List (List.map (fun (v1, v2, v3) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_field_decl_ac513ea env v2 in
                let v3 =
                  (match v3 with
                  | Some x -> R.Option (Some (
                      map_bitfield_clause env x
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2; v3]
              ) v3)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_bitfield_clause env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Prep_def x -> R.Case ("Prep_def",
      map_preproc_def env x
    )
  | `Prep_func_def x -> R.Case ("Prep_func_def",
      map_preproc_function_def env x
    )
  | `Prep_call x -> R.Case ("Prep_call",
      map_preproc_call env x
    )
  | `Prep_if_in_field_decl_list (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if_in_field_decl_list",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_field_declaration_list_item env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_field_decl_list_1fef6b2 env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Prep_ifdef_in_field_decl_list (v1, v2, v3, v4, v5) -> R.Case ("Prep_ifdef_in_field_decl_list",
      let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        R.List (List.map (map_field_declaration_list_item env) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_field_decl_list_1fef6b2 env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_pat_c46d1b2 env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Atdef_field (v1, v2, v3, v4) -> R.Case ("Atdef_field",
      let v1 = (* "@defs" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_field_declarator (env : env) (x : CST.field_declarator) =
  (match x with
  | `Choice_attr_field_decl x -> R.Case ("Choice_attr_field_decl",
      (match x with
      | `Attr_field_decl x -> R.Case ("Attr_field_decl",
          map_attributed_field_declarator env x
        )
      | `Poin_field_decl x -> R.Case ("Poin_field_decl",
          map_pointer_field_declarator env x
        )
      | `Func_field_decl x -> R.Case ("Func_field_decl",
          map_function_field_declarator env x
        )
      | `Array_field_decl x -> R.Case ("Array_field_decl",
          map_array_field_declarator env x
        )
      | `Paren_field_decl x -> R.Case ("Paren_field_decl",
          map_parenthesized_field_declarator env x
        )
      | `Choice_id x -> R.Case ("Choice_id",
          map_field_identifier env x
        )
      )
    )
  | `Blk_poin_field_decl (v1, v2, v3) -> R.Case ("Blk_poin_field_decl",
      let v1 = (* "^" *) token env v1 in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      let v3 = map_field_declarator env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> R.Case ("DOT",
        (* "." *) token env tok
      )
    | `DASHGT tok -> R.Case ("DASHGT",
        (* "->" *) token env tok
      )
    )
  in
  let v3 = map_field_identifier env v3 in
  R.Tuple [v1; v2; v3]

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 =
    (match v1 with
    | `ATfi tok -> R.Case ("ATfi",
        (* "@finally" *) token env tok
      )
    | `X___fina tok -> R.Case ("X___fina",
        (* "__finally" *) token env tok
      )
    )
  in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

and map_for_statement (env : env) (x : CST.for_statement) =
  (match x with
  | `For_LPAR_for_stmt_body_RPAR_stmt (v1, v2, v3, v4, v5) -> R.Case ("For_LPAR_for_stmt_body_RPAR_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_for_statement_body env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For_LPAR_choice_decl_specis_decl_in_exp_RPAR_choice_attr_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("For_LPAR_choice_decl_specis_decl_in_exp_RPAR_choice_attr_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Decl_specis_decl (v1, v2) -> R.Case ("Decl_specis_decl",
            let v1 = map_declaration_specifiers env v1 in
            let v2 = map_declarator env v2 in
            R.Tuple [v1; v2]
          )
        | `Id tok -> R.Case ("Id",
            (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
          )
        )
      in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_non_case_statement env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

and map_for_statement_body (env : env) ((v1, v2, v3, v4) : CST.for_statement_body) =
  let v1 =
    (match v1 with
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    | `Opt_choice_exp_SEMI x -> R.Case ("Opt_choice_exp_SEMI",
        map_expression_statement env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_function_definition (env : env) ((v1, v2, v3, v4) : CST.function_definition) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_ms_call_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_declarator env v3 in
  let v4 = map_compound_statement env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_function_field_declarator (env : env) ((v1, v2, v3) : CST.function_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = map_parameter_list env v2 in
  let v3 =
    R.List (List.map (map_anon_choice_attr_spec_73f6bce env) v3)
  in
  R.Tuple [v1; v2; v3]

and map_generic_expression (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.generic_expression) =
  let v1 = (* "_Generic" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 = map_type_descriptor env v5 in
  let v6 = (* ":" *) token env v6 in
  let v7 = map_expression env v7 in
  let v8 =
    R.List (List.map (fun (v1, v2, v3, v4) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_descriptor env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    ) v8)
  in
  let v9 = (* ")" *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_generic_specifier (env : env) ((v1, v2) : CST.generic_specifier) =
  let v1 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2, v3, v4) ->
      let v1 = (* "<" *) token env v1 in
      let v2 = map_type_name env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_name env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ">" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_gnu_asm_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.gnu_asm_expression) =
  let v1 =
    (match v1 with
    | `Asm tok -> R.Case ("Asm",
        (* "asm" *) token env tok
      )
    | `X___asm__ tok -> R.Case ("X___asm__",
        (* "__asm__" *) token env tok
      )
    | `X___asm tok -> R.Case ("X___asm",
        (* "__asm" *) token env tok
      )
    )
  in
  let v2 = R.List (List.map (map_gnu_asm_qualifier env) v2) in
  let v3 = (* "(" *) token env v3 in
  let v4 = map_string_ env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_gnu_asm_output_operand_list env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_gnu_asm_input_operand_list env v1 in
              let v2 =
                (match v2 with
                | Some (v1, v2) -> R.Option (Some (
                    let v1 = map_gnu_asm_clobber_list env v1 in
                    let v2 =
                      (match v2 with
                      | Some x -> R.Option (Some (
                          map_gnu_asm_goto_list env x
                        ))
                      | None -> R.Option None)
                    in
                    R.Tuple [v1; v2]
                  ))
                | None -> R.Option None)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_gnu_asm_input_operand (env : env) ((v1, v2, v3, v4, v5) : CST.gnu_asm_input_operand) =
  let v1 =
    (match v1 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "[" *) token env v1 in
        let v2 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v2 = map_string_literal env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_gnu_asm_input_operand_list (env : env) ((v1, v2) : CST.gnu_asm_input_operand_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_gnu_asm_input_operand env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_gnu_asm_input_operand env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_gnu_asm_output_operand (env : env) ((v1, v2, v3, v4, v5) : CST.gnu_asm_output_operand) =
  let v1 =
    (match v1 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "[" *) token env v1 in
        let v2 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v2 = map_string_literal env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_gnu_asm_output_operand_list (env : env) ((v1, v2) : CST.gnu_asm_output_operand_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_gnu_asm_output_operand env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_gnu_asm_output_operand env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_else_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_implementation_definition (env : env) (x : CST.implementation_definition) =
  (match x with
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Prop_impl x -> R.Case ("Prop_impl",
      map_property_implementation env x
    )
  | `Struct_spec_SEMI (v1, v2) -> R.Case ("Struct_spec_SEMI",
      let v1 = map_struct_specifier env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Meth_defi (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("Meth_defi",
      let v1 = map_anon_choice_PLUS_da42005 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_method_type env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | `Choice_id_opt_meth_param_rep_opt_choice_choice_id_meth_param (v1, v2) -> R.Case ("Choice_id_opt_meth_param_rep_opt_choice_choice_id_meth_param",
            let v1 = map_method_selector_no_list env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = map_method_parameter env v1 in
                  let v2 =
                    map_anon_rep_opt_meth_sele_meth_param_f14b947 env v2
                  in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Meth_param_rep_opt_choice_choice_id_meth_param (v1, v2) -> R.Case ("Meth_param_rep_opt_choice_choice_id_meth_param",
            let v1 = map_method_parameter env v1 in
            let v2 =
              map_anon_rep_opt_meth_sele_meth_param_f14b947 env v2
            in
            R.Tuple [v1; v2]
          )
        )
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_DOTDOTDOT_18da608 env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 = R.List (List.map (map_declaration env) v6) in
      let v7 =
        R.List (List.map (map_declaration_modifiers env) v7)
      in
      let v8 =
        (match v8 with
        | Some tok -> R.Option (Some (
            (* ";" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v9 = map_compound_statement env v9 in
      let v10 =
        (match v10 with
        | Some tok -> R.Option (Some (
            (* ";" *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  | `Prep_func_def x -> R.Case ("Prep_func_def",
      map_preproc_function_def env x
    )
  | `Macro_type_spec x -> R.Case ("Macro_type_spec",
      map_macro_type_specifier env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Prep_if_in_impl_defi (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if_in_impl_defi",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_implementation_definition env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_impl_defi_a30bcf7 env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Prep_ifdef x -> R.Case ("Prep_ifdef",
      map_preproc_ifdef env x
    )
  | `Prep_undef x -> R.Case ("Prep_undef",
      map_preproc_undef env x
    )
  | `Prep_def x -> R.Case ("Prep_def",
      map_preproc_def env x
    )
  | `Prep_call x -> R.Case ("Prep_call",
      map_preproc_call env x
    )
  )

and map_init_declarator (env : env) ((v1, v2, v3, v4) : CST.init_declarator) =
  let v1 = map_declarator env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=" *) token env v3 in
  let v4 =
    (match v4 with
    | `Init_list x -> R.Case ("Init_list",
        map_initializer_list env x
      )
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_initializer_list (env : env) ((v1, v2, v3, v4) : CST.initializer_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_init_pair_1a6981e env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_init_pair_1a6981e env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_initializer_pair (env : env) (x : CST.initializer_pair) =
  (match x with
  | `Rep1_choice_subs_desi_EQ_choice_exp (v1, v2, v3) -> R.Case ("Rep1_choice_subs_desi_EQ_choice_exp",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Subs_desi x -> R.Case ("Subs_desi",
              map_subscript_designator env x
            )
          | `Field_desi x -> R.Case ("Field_desi",
              map_field_designator env x
            )
          | `Subs_range_desi x -> R.Case ("Subs_range_desi",
              map_subscript_range_designator env x
            )
          )
        ) v1)
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_anon_choice_exp_3078596 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_id_COLON_choice_exp (v1, v2, v3) -> R.Case ("Choice_id_COLON_choice_exp",
      let v1 = map_field_identifier env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_anon_choice_exp_3078596 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_instance_variable (env : env) (x : CST.instance_variable) =
  (match x with
  | `Visi_spec x -> R.Case ("Visi_spec",
      map_visibility_specification env x
    )
  | `Struct_decl x -> R.Case ("Struct_decl",
      map_struct_declaration env x
    )
  | `Atomic_decl x -> R.Case ("Atomic_decl",
      map_atomic_declaration env x
    )
  | `Prep_ifdef x -> R.Case ("Prep_ifdef",
      map_preproc_ifdef env x
    )
  | `Prep_if x -> R.Case ("Prep_if",
      map_preproc_if env x
    )
  )

and map_instance_variables (env : env) ((v1, v2, v3, v4) : CST.instance_variables) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_anon_choice_attr_spec_c763cd2 env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_instance_variable env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_interface_declaration (env : env) (x : CST.interface_declaration) =
  (match x with
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Prop_decl (v1, v2, v3, v4, v5) -> R.Case ("Prop_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_declaration_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "@property" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_property_attributes_declaration env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_attr_spec_c763cd2 env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | `Struct_decl x -> R.Case ("Struct_decl",
            map_struct_declaration env x
          )
        | `Atomic_decl x -> R.Case ("Atomic_decl",
            map_atomic_declaration env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Meth_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Meth_decl",
      let v1 = map_anon_choice_PLUS_da42005 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_method_type env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_attr_spec_c763cd2 env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        R.List (List.map (fun x ->
          (match x with
          | `Choice_choice_id_opt_attr_spec_opt_meth_param (v1, v2, v3) -> R.Case ("Choice_choice_id_opt_attr_spec_opt_meth_param",
              let v1 = map_method_selector env v1 in
              let v2 =
                (match v2 with
                | Some x -> R.Option (Some (
                    map_attribute_specifier env x
                  ))
                | None -> R.Option None)
              in
              let v3 =
                (match v3 with
                | Some x -> R.Option (Some (
                    map_method_parameter env x
                  ))
                | None -> R.Option None)
              in
              R.Tuple [v1; v2; v3]
            )
          | `Meth_param x -> R.Case ("Meth_param",
              map_method_parameter env x
            )
          )
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_DOTDOTDOT_18da608 env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 =
        R.List (List.map (map_declaration_modifiers env) v6)
      in
      let v7 = R.List (List.map (token env (* ";" *)) v7) in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Prep_if_in_inte_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if_in_inte_decl",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_interface_declaration env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_inte_decl_eefbd14 env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Prep_def x -> R.Case ("Prep_def",
      map_preproc_def env x
    )
  | `Prep_ifdef x -> R.Case ("Prep_ifdef",
      map_preproc_ifdef env x
    )
  | `Prep_undef x -> R.Case ("Prep_undef",
      map_preproc_undef env x
    )
  | `Prep_call x -> R.Case ("Prep_call",
      map_preproc_call env x
    )
  | `Struct_spec_SEMI (v1, v2) -> R.Case ("Struct_spec_SEMI",
      let v1 = map_struct_specifier env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_keyword_declarator (env : env) ((v1, v2, v3, v4) : CST.keyword_declarator) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* ";" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_method_type env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v4
  in
  R.Tuple [v1; v2; v3; v4]

and map_keyword_selector (env : env) (xs : CST.keyword_selector) =
  R.List (List.map (map_keyword_declarator env) xs)

and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    (match v3 with
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    | `Stmt x -> R.Case ("Stmt",
        map_statement env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_linkage_specification (env : env) ((v1, v2, v3) : CST.linkage_specification) =
  let v1 = (* "extern" *) token env v1 in
  let v2 = map_string_literal env v2 in
  let v3 =
    (match v3 with
    | `Func_defi x -> R.Case ("Func_defi",
        map_function_definition env x
      )
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    | `Decl_list x -> R.Case ("Decl_list",
        map_declaration_list env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_macro_type_specifier (env : env) ((v1, v2, v3, v4) : CST.macro_type_specifier) =
  let v1 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_descriptor env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_method_parameter (env : env) ((v1, v2, v3, v4, v5) : CST.method_parameter) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_method_type env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_declaration_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | `Id tok -> R.Case ("Id",
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    | `Choice_id x -> R.Case ("Choice_id",
        map_keyword_identifier env x
      )
    )
  in
  let v5 =
    R.List (List.map (map_declaration_modifiers env) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_method_selector (env : env) (x : CST.method_selector) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_method_selector_no_list env x
    )
  | `Rep1_kw_decl_COMMA (v1, v2) -> R.Case ("Rep1_kw_decl_COMMA",
      let v1 = map_keyword_selector env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_method_selector_no_list (env : env) (x : CST.method_selector_no_list) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Rep1_kw_decl x -> R.Case ("Rep1_kw_decl",
      map_keyword_selector env x
    )
  | `Rep1_kw_decl_COMMA_DOTDOTDOT (v1, v2, v3) -> R.Case ("Rep1_kw_decl_COMMA_DOTDOTDOT",
      let v1 = map_keyword_selector env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_method_type (env : env) ((v1, v2, v3, v4, v5) : CST.method_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_choice_type_name_2868ede env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_anon_choice_type_name_2868ede env v3 in
      R.Tuple [v1; v2; v3]
    ) v4)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let v1 = (* "__based" *) token env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_non_case_statement (env : env) (x : CST.non_case_statement) =
  (match x with
  | `Attr_stmt x -> R.Case ("Attr_stmt",
      map_attributed_statement env x
    )
  | `Labe_stmt x -> R.Case ("Labe_stmt",
      map_labeled_statement env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `If_stmt x -> R.Case ("If_stmt",
      map_if_statement env x
    )
  | `Switch_stmt x -> R.Case ("Switch_stmt",
      map_switch_statement env x
    )
  | `Do_stmt x -> R.Case ("Do_stmt",
      map_do_statement env x
    )
  | `While_stmt x -> R.Case ("While_stmt",
      map_while_statement env x
    )
  | `For_stmt x -> R.Case ("For_stmt",
      map_for_statement env x
    )
  | `Ret_stmt x -> R.Case ("Ret_stmt",
      map_return_statement env x
    )
  | `Brk_stmt x -> R.Case ("Brk_stmt",
      map_break_statement env x
    )
  | `Cont_stmt x -> R.Case ("Cont_stmt",
      map_continue_statement env x
    )
  | `Goto_stmt x -> R.Case ("Goto_stmt",
      map_goto_statement env x
    )
  | `Try_stmt (v1, v2, v3) -> R.Case ("Try_stmt",
      let v1 =
        (match v1 with
        | `ATtry tok -> R.Case ("ATtry",
            (* "@try" *) token env tok
          )
        | `X___try tok -> R.Case ("X___try",
            (* "__try" *) token env tok
          )
        )
      in
      let v2 = map_compound_statement env v2 in
      let v3 =
        (match v3 with
        | `Rep1_catch_clause_opt_fina_clause (v1, v2) -> R.Case ("Rep1_catch_clause_opt_fina_clause",
            let v1 = R.List (List.map (map_catch_clause env) v1) in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_finally_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Fina_clause x -> R.Case ("Fina_clause",
            map_finally_clause env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Throw_stmt (v1, v2, v3) -> R.Case ("Throw_stmt",
      let v1 = (* "@throw" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Sync_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("Sync_stmt",
      let v1 = (* "@synchronized" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = map_compound_statement env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Ms_asm_blk (v1, v2, v3, v4) -> R.Case ("Ms_asm_blk",
      let v1 = (* "__asm" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_pat_7a545ba env v3 in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_objc_bridge (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.objc_bridge) =
  let v1 = (* "objc_bridge_related" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression env v1 in
        let v2 = (* ":" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 = (* "," *) token env v6 in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v8 = (* ")" *) token env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_offsetof_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.offsetof_expression) =
  let v1 = (* "offsetof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_descriptor env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 = map_field_identifier env v5 in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_old_style_function_declarator (env : env) ((v1, v2) : CST.old_style_function_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = map_old_style_parameter_list env v2 in
  R.Tuple [v1; v2]

and map_old_style_function_definition (env : env) ((v1, v2, v3, v4, v5) : CST.old_style_function_definition) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_ms_call_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_old_style_function_declarator env v3 in
  let v4 = R.List (List.map (map_declaration env) v4) in
  let v5 = map_compound_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Opt_choice_param_decl_rep_COMMA_choice_param_decl opt -> R.Case ("Opt_choice_param_decl_rep_COMMA_choice_param_decl",
        (match opt with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anon_choice_param_decl_4ac2852 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_param_decl_4ac2852 env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      )
    | `Comp_stmt x -> R.Case ("Comp_stmt",
        map_compound_statement env x
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parameterized_arguments (env : env) ((v1, v2, v3) : CST.parameterized_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | `Opt_choice___cova_id_rep_COMMA_opt_choice___cova_id_opt_COLON_type_name_rep_COMMA_opt_choice___cova_id_rep_COMMA_opt_choice___cova_id_opt_COLON_type_name (v1, v2, v3, v4, v5) -> R.Case ("Opt_choice___cova_id_rep_COMMA_opt_choice___cova_id_opt_COLON_type_name_rep_COMMA_opt_choice___cova_id_rep_COMMA_opt_choice___cova_id_opt_COLON_type_name",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_anon_choice___cova_4925ac1 env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        let v3 =
          map_anon_rep_COMMA_opt_choice___cova_type_id_0fed85a env v3
        in
        let v4 =
          (match v4 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* ":" *) token env v1 in
              let v2 = map_type_name env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v5 =
          R.List (List.map (fun (v1, v2, v3, v4, v5) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_anon_choice___cova_4925ac1 env x
                ))
              | None -> R.Option None)
            in
            let v3 =
              (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
            in
            let v4 =
              map_anon_rep_COMMA_opt_choice___cova_type_id_0fed85a env v4
            in
            let v5 =
              (match v5 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* ":" *) token env v1 in
                  let v2 = map_type_name env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3; v4; v5]
          ) v5)
        in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `Type_name_rep_COMMA_type_name (v1, v2) -> R.Case ("Type_name_rep_COMMA_type_name",
        let v1 = map_type_name env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_name env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_declarator (env : env) ((v1, v2, v3, v4) : CST.parenthesized_declarator) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (map_declaration_modifiers env) v2)
  in
  let v3 = map_declarator env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_parenthesized_expression (env : env) (x : CST.parenthesized_expression) =
  (match x with
  | `LPAR_choice_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Comma_exp x -> R.Case ("Comma_exp",
            map_comma_expression env x
          )
        | `Comp_stmt x -> R.Case ("Comp_stmt",
            map_compound_statement env x
          )
        )
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LPAR_semg_typed_meta_RPAR (v1, v2, v3) -> R.Case ("LPAR_semg_typed_meta_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_semgrep_typed_metavar env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_parenthesized_field_declarator (env : env) ((v1, v2, v3, v4) : CST.parenthesized_field_declarator) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    R.List (List.map (map_declaration_modifiers env) v2)
  in
  let v3 = map_field_declarator env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_pointer_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.pointer_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_ms_based_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "*" *) token env v2 in
  let v3 =
    R.List (List.map (map_ms_pointer_modifier env) v3)
  in
  let v4 =
    R.List (List.map (map_declaration_modifiers env) v4)
  in
  let v5 = map_declarator env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_pointer_expression (env : env) ((v1, v2) : CST.pointer_expression) =
  let v1 =
    (match v1 with
    | `STAR tok -> R.Case ("STAR",
        (* "*" *) token env tok
      )
    | `AMP tok -> R.Case ("AMP",
        (* "&" *) token env tok
      )
    )
  in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_pointer_field_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.pointer_field_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_ms_based_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "*" *) token env v2 in
  let v3 =
    R.List (List.map (map_ms_pointer_modifier env) v3)
  in
  let v4 =
    R.List (List.map (map_declaration_modifiers env) v4)
  in
  let v5 = map_field_declarator env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_preproc_elifdef (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (map_anon_choice_blk_item_e6161e0 env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_8b52b0f env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_if (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* "\n" *) token env v3 in
  let v4 =
    R.List (List.map (map_anon_choice_blk_item_e6161e0 env) v4)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_8b52b0f env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_ifdef (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (map_anon_choice_blk_item_e6161e0 env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_prep_else x -> R.Case ("Choice_prep_else",
            map_anon_choice_prep_else_8b52b0f env x
          )
        | `Prep_elif x -> R.Case ("Prep_elif",
            map_preproc_elifdef env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_c46d1b2 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_protocol_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.protocol_declaration) =
  let v1 =
    R.List (List.map (map_declaration_modifiers env) v1)
  in
  let v2 = (* "@protocol" *) token env v2 in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_protocol_reference_list env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    R.List (List.map (map_interface_declaration env) v5)
  in
  let v6 =
    R.List (List.map (map_qualified_protocol_interface_declaration env) v6)
  in
  let v7 = (* "@end" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_protocol_forward_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.protocol_forward_declaration) =
  let v1 =
    R.List (List.map (map_declaration_modifiers env) v1)
  in
  let v2 = (* "@protocol" *) token env v2 in
  let v3 =
    (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_qualified_protocol_interface_declaration (env : env) (x : CST.qualified_protocol_interface_declaration) =
  (match x with
  | `ATop_rep_choice_decl (v1, v2) -> R.Case ("ATop_rep_choice_decl",
      let v1 = (* "@optional" *) token env v1 in
      let v2 =
        R.List (List.map (map_interface_declaration env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `ATre_rep_choice_decl (v1, v2) -> R.Case ("ATre_rep_choice_decl",
      let v1 = (* "@required" *) token env v1 in
      let v2 =
        R.List (List.map (map_interface_declaration env) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_semgrep_typed_metavar (env : env) ((v1, v2) : CST.semgrep_typed_metavar) =
  let v1 = map_type_descriptor env v1 in
  let v2 = (* pattern \$[A-Z_][A-Z_0-9]* *) token env v2 in
  R.Tuple [v1; v2]

and map_sized_type_specifier (env : env) (x : CST.sized_type_specifier) =
  (match x with
  | `Rep_choice_signed_opt_choice_id_rep1_choice_signed (v1, v2, v3) -> R.Case ("Rep_choice_signed_opt_choice_id_rep1_choice_signed",
      let v1 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_type_id_1a79fc3 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Rep1_choice_signed_rep_type_qual_opt_choice_id_rep_choice_signed (v1, v2, v3, v4) -> R.Case ("Rep1_choice_signed_rep_type_qual_opt_choice_id_rep_choice_signed",
      let v1 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v1)
      in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_type_id_1a79fc3 env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_sizeof_expression (env : env) ((v1, v2) : CST.sizeof_expression) =
  let v1 = (* "sizeof" *) token env v1 in
  let v2 =
    (match v2 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `LPAR_type_desc_RPAR (v1, v2, v3) -> R.Case ("LPAR_type_desc_RPAR",
        let v1 = (* "(" *) token env v1 in
        let v2 = map_type_descriptor env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  R.Tuple [v1; v2]

and map_specifier_qualifier (env : env) (x : CST.specifier_qualifier) =
  (match x with
  | `Type_spec x -> R.Case ("Type_spec",
      map_type_specifier env x
    )
  | `Type_qual x -> R.Case ("Type_qual",
      map_type_qualifier env x
    )
  | `Prot_qual x -> R.Case ("Prot_qual",
      map_protocol_qualifier env x
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Case_stmt x -> R.Case ("Case_stmt",
      map_case_statement env x
    )
  | `Choice_attr_stmt x -> R.Case ("Choice_attr_stmt",
      map_non_case_statement env x
    )
  )

and map_struct_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.struct_declaration) =
  let v1 =
    R.List (List.map (map_specifier_qualifier env) v1)
  in
  let v2 = map_struct_declarator env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_struct_declarator env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_declaration_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_struct_declarator (env : env) (x : CST.struct_declarator) =
  (match x with
  | `Decl x -> R.Case ("Decl",
      map_declarator env x
    )
  | `Opt_decl_COLON_exp (v1, v2, v3) -> R.Case ("Opt_decl_COLON_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_declarator env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_struct_specifier (env : env) ((v1, v2, v3, v4, v5) : CST.struct_specifier) =
  let v1 = (* "struct" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_ms_declspec_modifier env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v4
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_subscript_designator (env : env) ((v1, v2, v3) : CST.subscript_designator) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_subscript_expression (env : env) ((v1, v2, v3, v4) : CST.subscript_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_subscript_range_designator (env : env) ((v1, v2, v3, v4, v5) : CST.subscript_range_designator) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "..." *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_switch_statement (env : env) ((v1, v2, v3) : CST.switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_type_declarator (env : env) (x : CST.type_declarator) =
  (match x with
  | `Poin_type_decl (v1, v2, v3, v4, v5) -> R.Case ("Poin_type_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_ms_based_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "*" *) token env v2 in
      let v3 =
        R.List (List.map (map_ms_pointer_modifier env) v3)
      in
      let v4 =
        R.List (List.map (map_declaration_modifiers env) v4)
      in
      let v5 = map_type_declarator env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Func_type_decl (v1, v2, v3) -> R.Case ("Func_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        R.List (List.map (map_anon_choice_attr_spec_73f6bce env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Array_type_decl (v1, v2, v3, v4, v5) -> R.Case ("Array_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        R.List (List.map (map_anon_choice_type_qual_b00a56a env) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_exp_508611b env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "]" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Paren_type_decl (v1, v2, v3, v4) -> R.Case ("Paren_type_decl",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        R.List (List.map (map_declaration_modifiers env) v2)
      in
      let v3 = map_type_declarator env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Blk_poin_type_decl (v1, v2, v3) -> R.Case ("Blk_poin_type_decl",
      let v1 = (* "^" *) token env v1 in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      let v3 = map_type_declarator env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Choice_signed x -> R.Case ("Choice_signed",
      map_anon_choice_signed_a0bfc19 env x
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  )

and map_type_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.type_definition) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "__extension__" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_ms_declspec_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "typedef" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_ms_declspec_modifier env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_type_definition_type env v6 in
  let v7 = map_type_definition_declarators env v7 in
  let v8 = (* ";" *) token env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_type_definition_declarators (env : env) ((v1, v2, v3) : CST.type_definition_declarators) =
  let v1 = map_type_declarator env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_declarator env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_declaration_modifiers env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_type_definition_type (env : env) ((v1, v2, v3, v4, v5) : CST.type_definition_type) =
  let v1 = R.List (List.map (map_type_qualifier env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_type_specifier env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_ms_declspec_modifier env x
      ))
    | None -> R.Option None)
  in
  let v5 = R.List (List.map (map_type_qualifier env) v5) in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) =
  let v1 = R.List (List.map (map_type_qualifier env) v1) in
  let v2 = map_type_specifier env v2 in
  let v3 = R.List (List.map (map_type_qualifier env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_type_name (env : env) ((v1, v2, v3) : CST.type_name) =
  let v1 =
    R.List (List.map (fun x ->
      (match x with
      | `Spec_qual x -> R.Case ("Spec_qual",
          map_specifier_qualifier env x
        )
      | `Attr_spec x -> R.Case ("Attr_spec",
          map_attribute_specifier env x
        )
      | `Decl x -> R.Case ("Decl",
          map_declarator env x
        )
      )
    ) v1)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_protocol_reference_list env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_type_params (env : env) (x : CST.type_params) =
  (match x with
  | `Gene_args_opt_para_args (v1, v2) -> R.Case ("Gene_args_opt_para_args",
      let v1 = map_generic_arguments env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_parameterized_arguments env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Para_args x -> R.Case ("Para_args",
      map_parameterized_arguments env x
    )
  )

and map_type_qualifier (env : env) (x : CST.type_qualifier) =
  (match x with
  | `Choice_const x -> R.Case ("Choice_const",
      (match x with
      | `Const tok -> R.Case ("Const",
          (* "const" *) token env tok
        )
      | `Cons tok -> R.Case ("Cons",
          (* "constexpr" *) token env tok
        )
      | `Vola tok -> R.Case ("Vola",
          (* "volatile" *) token env tok
        )
      | `Rest tok -> R.Case ("Rest",
          (* "restrict" *) token env tok
        )
      | `X___rest__ tok -> R.Case ("X___rest__",
          (* "__restrict__" *) token env tok
        )
      | `X___exte__ tok -> R.Case ("X___exte__",
          (* "__extension__" *) token env tok
        )
      | `X__Atomic tok -> R.Case ("X__Atomic",
          (* "_Atomic" *) token env tok
        )
      | `X__Nore tok -> R.Case ("X__Nore",
          (* "_Noreturn" *) token env tok
        )
      | `Nore tok -> R.Case ("Nore",
          (* "noreturn" *) token env tok
        )
      | `X__Nonn tok -> R.Case ("X__Nonn",
          (* "_Nonnull" *) token env tok
        )
      | `Alignas_qual x -> R.Case ("Alignas_qual",
          map_alignas_qualifier env x
        )
      )
    )
  | `Null tok -> R.Case ("Null",
      (* "nullable" *) token env tok
    )
  | `X__Comp tok -> R.Case ("X__Comp",
      (* "_Complex" *) token env tok
    )
  | `X__Nonn tok -> R.Case ("X__Nonn",
      (* "_Nonnull" *) token env tok
    )
  | `X__Null tok -> R.Case ("X__Null",
      (* "_Nullable" *) token env tok
    )
  | `X__Null_result tok -> R.Case ("X__Null_result",
      (* "_Nullable_result" *) token env tok
    )
  | `X__Null_unsp tok -> R.Case ("X__Null_unsp",
      (* "_Null_unspecified" *) token env tok
    )
  | `X___auto tok -> R.Case ("X___auto",
      (* "__autoreleasing" *) token env tok
    )
  | `X___blk tok -> R.Case ("X___blk",
      (* "__block" *) token env tok
    )
  | `X___bridge tok -> R.Case ("X___bridge",
      (* "__bridge" *) token env tok
    )
  | `X___bridge_reta tok -> R.Case ("X___bridge_reta",
      (* "__bridge_retained" *) token env tok
    )
  | `X___bridge_tran tok -> R.Case ("X___bridge_tran",
      (* "__bridge_transfer" *) token env tok
    )
  | `X___comp tok -> R.Case ("X___comp",
      (* "__complex" *) token env tok
    )
  | `X___const tok -> R.Case ("X___const",
      (* "__const" *) token env tok
    )
  | `X___imag tok -> R.Case ("X___imag",
      (* "__imag" *) token env tok
    )
  | `X___kindof tok -> R.Case ("X___kindof",
      (* "__kindof" *) token env tok
    )
  | `X___nonn tok -> R.Case ("X___nonn",
      (* "__nonnull" *) token env tok
    )
  | `X___null tok -> R.Case ("X___null",
      (* "__nullable" *) token env tok
    )
  | `X___ptra_objc_class_ro tok -> R.Case ("X___ptra_objc_class_ro",
      (* "__ptrauth_objc_class_ro" *) token env tok
    )
  | `X___ptra_objc_isa_poin tok -> R.Case ("X___ptra_objc_isa_poin",
      (* "__ptrauth_objc_isa_pointer" *) token env tok
    )
  | `X___ptra_objc_super_poin tok -> R.Case ("X___ptra_objc_super_poin",
      (* "__ptrauth_objc_super_pointer" *) token env tok
    )
  | `X___real tok -> R.Case ("X___real",
      (* "__real" *) token env tok
    )
  | `X___strong tok -> R.Case ("X___strong",
      (* "__strong" *) token env tok
    )
  | `X___unsafe_unre tok -> R.Case ("X___unsafe_unre",
      (* "__unsafe_unretained" *) token env tok
    )
  | `X___unused tok -> R.Case ("X___unused",
      (* "__unused" *) token env tok
    )
  | `X___weak tok -> R.Case ("X___weak",
      (* "__weak" *) token env tok
    )
  )

and map_type_specifier (env : env) (x : CST.type_specifier) =
  (match x with
  | `Choice_struct_spec x -> R.Case ("Choice_struct_spec",
      (match x with
      | `Struct_spec x -> R.Case ("Struct_spec",
          map_struct_specifier env x
        )
      | `Union_spec x -> R.Case ("Union_spec",
          map_union_specifier env x
        )
      | `Enum_spec x -> R.Case ("Enum_spec",
          map_enum_specifier env x
        )
      | `Macro_type_spec x -> R.Case ("Macro_type_spec",
          map_macro_type_specifier env x
        )
      | `Sized_type_spec x -> R.Case ("Sized_type_spec",
          map_sized_type_specifier env x
        )
      | `Prim_type tok -> R.Case ("Prim_type",
          (* primitive_type *) token env tok
        )
      | `Id tok -> R.Case ("Id",
          (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      )
    )
  | `Type_spec (v1, v2) -> R.Case ("Type_spec",
      let v1 = map_typedefed_identifier env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_protocol_reference_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Gene_spec x -> R.Case ("Gene_spec",
      map_generic_specifier env x
    )
  | `Typeof_spec x -> R.Case ("Typeof_spec",
      map_typeof_specifier env x
    )
  | `Array_type_spec (v1, v2, v3, v4) -> R.Case ("Array_type_spec",
      let v1 = map_type_specifier env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = R.List (List.map (map_type_qualifier env) v1) in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_typeof_specifier (env : env) ((v1, v2, v3, v4) : CST.typeof_specifier) =
  let v1 =
    (match v1 with
    | `X___typeof__ tok -> R.Case ("X___typeof__",
        (* "__typeof__" *) token env tok
      )
    | `X___typeof tok -> R.Case ("X___typeof",
        (* "__typeof" *) token env tok
      )
    | `Typeof tok -> R.Case ("Typeof",
        (* "typeof" *) token env tok
      )
    )
  in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_anon_choice_exp_86a4e82 env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v1 = map_anon_choice_BANG_67174d6 env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_union_specifier (env : env) ((v1, v2, v3, v4, v5) : CST.union_specifier) =
  let v1 = (* "union" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_ms_declspec_modifier env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v4
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Choice_DASHDASH_exp (v1, v2) -> R.Case ("Choice_DASHDASH_exp",
      let v1 = map_anon_choice_DASHDASH_d11def2 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_choice_DASHDASH (v1, v2) -> R.Case ("Exp_choice_DASHDASH",
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DASHDASH_d11def2 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

let map_seh_except_clause (env : env) ((v1, v2, v3) : CST.seh_except_clause) =
  let v1 = (* "__except" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

let map_seh_finally_clause (env : env) ((v1, v2) : CST.seh_finally_clause) =
  let v1 = (* "__finally" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

let map_top_level_expression_statement (env : env) ((v1, v2) : CST.top_level_expression_statement) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_expression_not_binary env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let rec map_anon_choice_prep_else_in_enum_list_no_comma_aaec454 (env : env) (x : CST.anon_choice_prep_else_in_enum_list_no_comma_aaec454) =
  (match x with
  | `Prep_else_in_enum_list_no_comma (v1, v2) -> R.Case ("Prep_else_in_enum_list_no_comma",
      let v1 = map_pat_56631e5 env v1 in
      let v2 = R.List (List.map (map_enumerator env) v2) in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_enum_list_no_comma_0776021 (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum_list_no_comma_0776021",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 = R.List (List.map (map_enumerator env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_list_no_comma_aaec454 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Prep_elif_in_enum_list_no_comma_b3ccc22 (v1, v2, v3, v4) -> R.Case ("Prep_elif_in_enum_list_no_comma_b3ccc22",
      let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 = R.List (List.map (map_enumerator env) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_list_no_comma_aaec454 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_top_level_statement (env : env) (x : CST.top_level_statement) =
  (match x with
  | `Choice_case_stmt x -> R.Case ("Choice_case_stmt",
      (match x with
      | `Case_stmt x -> R.Case ("Case_stmt",
          map_case_statement env x
        )
      | `Attr_stmt x -> R.Case ("Attr_stmt",
          map_attributed_statement env x
        )
      | `Labe_stmt x -> R.Case ("Labe_stmt",
          map_labeled_statement env x
        )
      | `Comp_stmt x -> R.Case ("Comp_stmt",
          map_compound_statement env x
        )
      | `Top_level_exp_stmt x -> R.Case ("Top_level_exp_stmt",
          map_top_level_expression_statement env x
        )
      | `If_stmt x -> R.Case ("If_stmt",
          map_if_statement env x
        )
      | `Switch_stmt x -> R.Case ("Switch_stmt",
          map_switch_statement env x
        )
      | `Do_stmt x -> R.Case ("Do_stmt",
          map_do_statement env x
        )
      | `While_stmt x -> R.Case ("While_stmt",
          map_while_statement env x
        )
      | `For_stmt x -> R.Case ("For_stmt",
          map_for_statement env x
        )
      | `Ret_stmt x -> R.Case ("Ret_stmt",
          map_return_statement env x
        )
      | `Brk_stmt x -> R.Case ("Brk_stmt",
          map_break_statement env x
        )
      | `Cont_stmt x -> R.Case ("Cont_stmt",
          map_continue_statement env x
        )
      | `Goto_stmt x -> R.Case ("Goto_stmt",
          map_goto_statement env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

let rec map_anon_choice_prep_else_in_enum_list_a31466c (env : env) (x : CST.anon_choice_prep_else_in_enum_list_a31466c) =
  (match x with
  | `Prep_else_in_enum_list (v1, v2) -> R.Case ("Prep_else_in_enum_list",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_enum_list_1680e39 (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum_list_1680e39",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_list_a31466c env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Prep_elif_in_enum_list_8fab4d0 (v1, v2, v3, v4) -> R.Case ("Prep_elif_in_enum_list_8fab4d0",
      let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
      let v2 =
        (* pattern (\$|\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\$|\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_list_a31466c env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_top_level_item (env : env) (x : CST.top_level_item) =
  (match x with
  | `Choice_func_defi x -> R.Case ("Choice_func_defi",
      (match x with
      | `Func_defi x -> R.Case ("Func_defi",
          map_function_definition env x
        )
      | `Old_style_func_defi x -> R.Case ("Old_style_func_defi",
          map_old_style_function_definition env x
        )
      | `Link_spec x -> R.Case ("Link_spec",
          map_linkage_specification env x
        )
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Top_level_stmt x -> R.Case ("Top_level_stmt",
          map_top_level_statement env x
        )
      | `Attr_stmt x -> R.Case ("Attr_stmt",
          map_attributed_statement env x
        )
      | `Type_defi x -> R.Case ("Type_defi",
          map_type_definition env x
        )
      | `Empty_decl x -> R.Case ("Empty_decl",
          map_empty_declaration env x
        )
      | `Prep_if x -> R.Case ("Prep_if",
          map_preproc_if env x
        )
      | `Prep_ifdef x -> R.Case ("Prep_ifdef",
          map_preproc_ifdef env x
        )
      | `Prep_incl x -> R.Case ("Prep_incl",
          map_preproc_include env x
        )
      | `Prep_def x -> R.Case ("Prep_def",
          map_preproc_def env x
        )
      | `Prep_func_def x -> R.Case ("Prep_func_def",
          map_preproc_function_def env x
        )
      | `Prep_call x -> R.Case ("Prep_call",
          map_preproc_call env x
        )
      )
    )
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Class_inte x -> R.Case ("Class_inte",
      map_class_interface env x
    )
  | `Class_impl x -> R.Case ("Class_impl",
      map_class_implementation env x
    )
  | `Prot_decl x -> R.Case ("Prot_decl",
      map_protocol_declaration env x
    )
  | `Prot_forw_decl x -> R.Case ("Prot_forw_decl",
      map_protocol_forward_declaration env x
    )
  | `Module_import x -> R.Case ("Module_import",
      map_module_import env x
    )
  | `Comp_alias_decl x -> R.Case ("Comp_alias_decl",
      map_compatibility_alias_declaration env x
    )
  | `Prep_undef x -> R.Case ("Prep_undef",
      map_preproc_undef env x
    )
  | `Prep_line x -> R.Case ("Prep_line",
      map_preproc_linemarker env x
    )
  )

let map_translation_unit (env : env) (x : CST.translation_unit) =
  (match x with
  | `Rep_top_level_item xs -> R.Case ("Rep_top_level_item",
      R.List (List.map (map_top_level_item env) xs)
    )
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_translation_unit () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
