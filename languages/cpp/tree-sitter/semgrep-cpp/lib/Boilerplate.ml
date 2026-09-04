(**
   Boilerplate to be used as a template when mapping the cpp CST
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

let map_true_ (env : env) (tok : CST.true_) =
  (* true *) token env tok

let map_anon_choice_EQ_6389fc4 (env : env) (x : CST.anon_choice_EQ_6389fc4) =
  (match x with
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
  | `And_eq tok -> R.Case ("And_eq",
      (* "and_eq" *) token env tok
    )
  | `Or_eq tok -> R.Case ("Or_eq",
      (* "or_eq" *) token env tok
    )
  | `Xor_eq tok -> R.Case ("Xor_eq",
      (* "xor_eq" *) token env tok
    )
  )

let map_anon_choice_type_a2fe5d4 (env : env) (x : CST.anon_choice_type_a2fe5d4) =
  (match x with
  | `Type tok -> R.Case ("Type",
      (* "typename" *) token env tok
    )
  | `Class tok -> R.Case ("Class",
      (* "class" *) token env tok
    )
  )

let map_primitive_type (env : env) (tok : CST.primitive_type) =
  (* primitive_type *) token env tok

let map_fold_operator (env : env) (x : CST.fold_operator) =
  (match x with
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `SLASH tok -> R.Case ("SLASH",
      (* "/" *) token env tok
    )
  | `PERC tok -> R.Case ("PERC",
      (* "%" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  | `PLUSEQ tok -> R.Case ("PLUSEQ",
      (* "+=" *) token env tok
    )
  | `DASHEQ tok -> R.Case ("DASHEQ",
      (* "-=" *) token env tok
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
  | `HATEQ tok -> R.Case ("HATEQ",
      (* "^=" *) token env tok
    )
  | `AMPEQ tok -> R.Case ("AMPEQ",
      (* "&=" *) token env tok
    )
  | `BAREQ tok -> R.Case ("BAREQ",
      (* "|=" *) token env tok
    )
  | `GTGTEQ tok -> R.Case ("GTGTEQ",
      (* ">>=" *) token env tok
    )
  | `LTLTEQ tok -> R.Case ("LTLTEQ",
      (* "<<=" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  | `AMPAMP tok -> R.Case ("AMPAMP",
      (* "&&" *) token env tok
    )
  | `BARBAR tok -> R.Case ("BARBAR",
      (* "||" *) token env tok
    )
  | `COMMA tok -> R.Case ("COMMA",
      (* "," *) token env tok
    )
  | `DOTSTAR tok -> R.Case ("DOTSTAR",
      (* ".*" *) token env tok
    )
  | `DASHGTSTAR tok -> R.Case ("DASHGTSTAR",
      (* "->*" *) token env tok
    )
  | `Or tok -> R.Case ("Or",
      (* "or" *) token env tok
    )
  | `And tok -> R.Case ("And",
      (* "and" *) token env tok
    )
  | `Bitor tok -> R.Case ("Bitor",
      (* "bitor" *) token env tok
    )
  | `Xor tok -> R.Case ("Xor",
      (* "xor" *) token env tok
    )
  | `Bitand tok -> R.Case ("Bitand",
      (* "bitand" *) token env tok
    )
  | `Not_eq tok -> R.Case ("Not_eq",
      (* "not_eq" *) token env tok
    )
  )

let map_pat_bfeb4bb (env : env) (tok : CST.pat_bfeb4bb) =
  (* pattern #[ 	]*elif *) token env tok

let map_preproc_directive (env : env) (tok : CST.preproc_directive) =
  (* pattern #[ \t]*[a-zA-Z0-9]\w* *) token env tok

let map_access_specifier (env : env) (x : CST.access_specifier) =
  (match x with
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  | `Prot tok -> R.Case ("Prot",
      (* "protected" *) token env tok
    )
  )

let map_default_method_clause (env : env) ((v1, v2, v3) : CST.default_method_clause) =
  let v1 = (* "=" *) token env v1 in
  let v2 = (* "default" *) token env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_pat_c3ea183 (env : env) (tok : CST.pat_c3ea183) =
  (* pattern #[ 	]*define *) token env tok

let map_anon_choice_DASHDASH_d11def2 (env : env) (x : CST.anon_choice_DASHDASH_d11def2) =
  (match x with
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  )

let map_delete_method_clause (env : env) ((v1, v2, v3) : CST.delete_method_clause) =
  let v1 = (* "=" *) token env v1 in
  let v2 = (* "delete" *) token env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_continue_statement (env : env) ((v1, v2) : CST.continue_statement) =
  let v1 = (* "continue" *) token env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_storage_class_specifier (env : env) (x : CST.storage_class_specifier) =
  (match x with
  | `Extern tok -> R.Case ("Extern",
      (* "extern" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
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

let map_gnu_asm_qualifier (env : env) (x : CST.gnu_asm_qualifier) =
  (match x with
  | `Vola tok -> R.Case ("Vola",
      (* "volatile" *) token env tok
    )
  | `Inline tok -> R.Case ("Inline",
      (* "inline" *) token env tok
    )
  | `Goto tok -> R.Case ("Goto",
      (* "goto" *) token env tok
    )
  )

let map_null (env : env) (x : CST.null) =
  (match x with
  | `NULL tok -> R.Case ("NULL",
      (* "NULL" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "nullptr" *) token env tok
    )
  )

let map_pat_a6d4183 (env : env) (tok : CST.pat_a6d4183) =
  (* pattern #[ 	]*elifndef *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

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

let map_ms_unaligned_ptr_modifier (env : env) (x : CST.ms_unaligned_ptr_modifier) =
  (match x with
  | `X__unal tok -> R.Case ("X__unal",
      (* "_unaligned" *) token env tok
    )
  | `X___unal tok -> R.Case ("X___unal",
      (* "__unaligned" *) token env tok
    )
  )

let map_type_qualifier (env : env) (x : CST.type_qualifier) =
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
      )
    )
  | `Muta tok -> R.Case ("Muta",
      (* "mutable" *) token env tok
    )
  | `Cons_36fe86c tok -> R.Case ("Cons_36fe86c",
      (* "constinit" *) token env tok
    )
  | `Cons_a25342f tok -> R.Case ("Cons_a25342f",
      (* "consteval" *) token env tok
    )
  )

let map_break_statement (env : env) ((v1, v2) : CST.break_statement) =
  let v1 = (* "break" *) token env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_semgrep_metavar (env : env) (tok : CST.semgrep_metavar) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_imm_tok_pat_36637e2 (env : env) (tok : CST.imm_tok_pat_36637e2) =
  (* pattern "[^\\n']" *) token env tok

let map_pat_9d92f6a (env : env) (tok : CST.pat_9d92f6a) =
  (* pattern #[ 	]*ifndef *) token env tok

let map_false_ (env : env) (tok : CST.false_) =
  (* false *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok

let map_seh_leave_statement (env : env) ((v1, v2) : CST.seh_leave_statement) =
  let v1 = (* "__leave" *) token env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_virtual_ (env : env) (x : CST.virtual_) =
  (match x with
  | `Virt tok -> R.Case ("Virt",
      (* "virtual" *) token env tok
    )
  )

let map_number_literal (env : env) (tok : CST.number_literal) =
  (* number_literal *) token env tok

let map_raw_string_delimiter (env : env) (tok : CST.raw_string_delimiter) =
  (* raw_string_delimiter *) token env tok

let map_pat_c46d1b2 (env : env) (tok : CST.pat_c46d1b2) =
  (* pattern #[ 	]*endif *) token env tok

let map_pat_3df6e71 (env : env) (tok : CST.pat_3df6e71) =
  (* pattern #[ 	]*if *) token env tok

let map_preproc_arg (env : env) (tok : CST.preproc_arg) =
  (* preproc_arg *) token env tok

let map_literal_suffix (env : env) (tok : CST.literal_suffix) =
  (* pattern [a-zA-Z_]\w* *) token env tok

let map_semgrep_named_ellipsis (env : env) (tok : CST.semgrep_named_ellipsis) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_system_lib_string (env : env) (tok : CST.system_lib_string) =
  (* system_lib_string *) token env tok

let map_pat_ca8830e (env : env) (tok : CST.pat_ca8830e) =
  (* pattern #[ 	]*include *) token env tok

let map_tok_prec_p1_gt (env : env) (tok : CST.tok_prec_p1_gt) =
  (* tok_prec_p1_gt *) token env tok

let map_virtual_specifier (env : env) (x : CST.virtual_specifier) =
  (match x with
  | `Final tok -> R.Case ("Final",
      (* "final" *) token env tok
    )
  | `Over tok -> R.Case ("Over",
      (* "override" *) token env tok
    )
  )

let map_pat_25b90ba (env : env) (tok : CST.pat_25b90ba) =
  (* pattern #[ 	]*ifdef *) token env tok

let map_imm_tok_pat_509ec78 (env : env) (tok : CST.imm_tok_pat_509ec78) =
  (* pattern \r?\n *) token env tok

let map_imm_tok_prec_p1_pat_c7f65b4 (env : env) (tok : CST.imm_tok_prec_p1_pat_c7f65b4) =
  (* pattern "[^\\\\\"\\n]+" *) token env tok

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

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_pat_56631e5 (env : env) (tok : CST.pat_56631e5) =
  (* pattern #[ 	]*else *) token env tok

let map_raw_string_content (env : env) (tok : CST.raw_string_content) =
  (* raw_string_content *) token env tok

let map_lambda_default_capture (env : env) (x : CST.lambda_default_capture) =
  (match x with
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  )

let map_ref_qualifier (env : env) (x : CST.ref_qualifier) =
  (match x with
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `AMPAMP tok -> R.Case ("AMPAMP",
      (* "&&" *) token env tok
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

let map_decltype_auto (env : env) ((v1, v2, v3, v4) : CST.decltype_auto) =
  let v1 = (* "decltype" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* "auto" *) token env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_binary_fold_operator (env : env) ((v1, v2, v3) : CST.binary_fold_operator) =
  let v1 = map_fold_operator env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 = map_fold_operator env v3 in
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

let map_anon_choice_access_spec_23a010c (env : env) (x : CST.anon_choice_access_spec_23a010c) =
  (match x with
  | `Access_spec x -> R.Case ("Access_spec",
      map_access_specifier env x
    )
  | `Access_spec_virt (v1, v2) -> R.Case ("Access_spec_virt",
      let v1 = map_access_specifier env v1 in
      let v2 = map_virtual_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Virt_access_spec (v1, v2) -> R.Case ("Virt_access_spec",
      let v1 = map_virtual_ env v1 in
      let v2 = map_access_specifier env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_anon_choice_pat_25b90ba_4a37f8c (env : env) (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  (match x with
  | `Pat_25b90ba x -> R.Case ("Pat_25b90ba",
      map_pat_25b90ba env x
    )
  | `Pat_9d92f6a x -> R.Case ("Pat_9d92f6a",
      map_pat_9d92f6a env x
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

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 =
    (match v1 with
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

let map_raw_string_literal (env : env) ((v1, v2, v3) : CST.raw_string_literal) =
  let v1 =
    (match v1 with
    | `RDQUOT tok -> R.Case ("RDQUOT",
        (* "R\"" *) token env tok
      )
    | `LRDQUOT tok -> R.Case ("LRDQUOT",
        (* "LR\"" *) token env tok
      )
    | `URDQ tok -> R.Case ("URDQ",
        (* "uR\"" *) token env tok
      )
    | `URDQUOT tok -> R.Case ("URDQUOT",
        (* "UR\"" *) token env tok
      )
    | `U8RDQUOT tok -> R.Case ("U8RDQUOT",
        (* "u8R\"" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | `Raw_str_deli_LPAR_raw_str_content_RPAR_raw_str_deli (v1, v2, v3, v4, v5) -> R.Case ("Raw_str_deli_LPAR_raw_str_content_RPAR_raw_str_deli",
        let v1 = (* raw_string_delimiter *) token env v1 in
        let v2 = (* "(" *) token env v2 in
        let v3 = (* raw_string_content *) token env v3 in
        let v4 = (* ")" *) token env v4 in
        let v5 = (* raw_string_delimiter *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `LPAR_raw_str_content_RPAR (v1, v2, v3) -> R.Case ("LPAR_raw_str_content_RPAR",
        let v1 = (* "(" *) token env v1 in
        let v2 = (* raw_string_content *) token env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_destructor_name (env : env) ((v1, v2) : CST.destructor_name) =
  let v1 = (* "~" *) token env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  R.Tuple [v1; v2]

let map_field_identifier (env : env) (x : CST.field_identifier) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

let map_operator_name (env : env) ((v1, v2) : CST.operator_name) =
  let v1 = (* "operator" *) token env v1 in
  let v2 =
    (match v2 with
    | `Co_await tok -> R.Case ("Co_await",
        (* "co_await" *) token env tok
      )
    | `PLUS tok -> R.Case ("PLUS",
        (* "+" *) token env tok
      )
    | `DASH tok -> R.Case ("DASH",
        (* "-" *) token env tok
      )
    | `STAR tok -> R.Case ("STAR",
        (* "*" *) token env tok
      )
    | `SLASH tok -> R.Case ("SLASH",
        (* "/" *) token env tok
      )
    | `PERC tok -> R.Case ("PERC",
        (* "%" *) token env tok
      )
    | `HAT tok -> R.Case ("HAT",
        (* "^" *) token env tok
      )
    | `AMP tok -> R.Case ("AMP",
        (* "&" *) token env tok
      )
    | `BAR tok -> R.Case ("BAR",
        (* "|" *) token env tok
      )
    | `TILDE tok -> R.Case ("TILDE",
        (* "~" *) token env tok
      )
    | `BANG tok -> R.Case ("BANG",
        (* "!" *) token env tok
      )
    | `EQ tok -> R.Case ("EQ",
        (* "=" *) token env tok
      )
    | `LT tok -> R.Case ("LT",
        (* "<" *) token env tok
      )
    | `GT tok -> R.Case ("GT",
        (* ">" *) token env tok
      )
    | `PLUSEQ tok -> R.Case ("PLUSEQ",
        (* "+=" *) token env tok
      )
    | `DASHEQ tok -> R.Case ("DASHEQ",
        (* "-=" *) token env tok
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
    | `HATEQ tok -> R.Case ("HATEQ",
        (* "^=" *) token env tok
      )
    | `AMPEQ tok -> R.Case ("AMPEQ",
        (* "&=" *) token env tok
      )
    | `BAREQ tok -> R.Case ("BAREQ",
        (* "|=" *) token env tok
      )
    | `LTLT tok -> R.Case ("LTLT",
        (* "<<" *) token env tok
      )
    | `GTGT tok -> R.Case ("GTGT",
        (* ">>" *) token env tok
      )
    | `GTGTEQ tok -> R.Case ("GTGTEQ",
        (* ">>=" *) token env tok
      )
    | `LTLTEQ tok -> R.Case ("LTLTEQ",
        (* "<<=" *) token env tok
      )
    | `EQEQ tok -> R.Case ("EQEQ",
        (* "==" *) token env tok
      )
    | `BANGEQ tok -> R.Case ("BANGEQ",
        (* "!=" *) token env tok
      )
    | `LTEQ tok -> R.Case ("LTEQ",
        (* "<=" *) token env tok
      )
    | `GTEQ tok -> R.Case ("GTEQ",
        (* ">=" *) token env tok
      )
    | `LTEQGT tok -> R.Case ("LTEQGT",
        (* "<=>" *) token env tok
      )
    | `AMPAMP tok -> R.Case ("AMPAMP",
        (* "&&" *) token env tok
      )
    | `BARBAR tok -> R.Case ("BARBAR",
        (* "||" *) token env tok
      )
    | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
        (* "++" *) token env tok
      )
    | `DASHDASH tok -> R.Case ("DASHDASH",
        (* "--" *) token env tok
      )
    | `COMMA tok -> R.Case ("COMMA",
        (* "," *) token env tok
      )
    | `DASHGTSTAR tok -> R.Case ("DASHGTSTAR",
        (* "->*" *) token env tok
      )
    | `DASHGT tok -> R.Case ("DASHGT",
        (* "->" *) token env tok
      )
    | `LPARRPAR tok -> R.Case ("LPARRPAR",
        (* "()" *) token env tok
      )
    | `LBRACKRBRACK tok -> R.Case ("LBRACKRBRACK",
        (* "[]" *) token env tok
      )
    | `Xor tok -> R.Case ("Xor",
        (* "xor" *) token env tok
      )
    | `Bitand tok -> R.Case ("Bitand",
        (* "bitand" *) token env tok
      )
    | `Bitor tok -> R.Case ("Bitor",
        (* "bitor" *) token env tok
      )
    | `Compl tok -> R.Case ("Compl",
        (* "compl" *) token env tok
      )
    | `Not tok -> R.Case ("Not",
        (* "not" *) token env tok
      )
    | `Xor_eq tok -> R.Case ("Xor_eq",
        (* "xor_eq" *) token env tok
      )
    | `And_eq tok -> R.Case ("And_eq",
        (* "and_eq" *) token env tok
      )
    | `Or_eq tok -> R.Case ("Or_eq",
        (* "or_eq" *) token env tok
      )
    | `Not_eq tok -> R.Case ("Not_eq",
        (* "not_eq" *) token env tok
      )
    | `And tok -> R.Case ("And",
        (* "and" *) token env tok
      )
    | `Or tok -> R.Case ("Or",
        (* "or" *) token env tok
      )
    | `Choice_new_opt_LBRACKRBRACK (v1, v2) -> R.Case ("Choice_new_opt_LBRACKRBRACK",
        let v1 =
          (match v1 with
          | `New tok -> R.Case ("New",
              (* "new" *) token env tok
            )
          | `Delete tok -> R.Case ("Delete",
              (* "delete" *) token env tok
            )
          )
        in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "[]" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `DQUOTDQUOT_id (v1, v2) -> R.Case ("DQUOTDQUOT_id",
        let v1 = (* "\"\"" *) token env v1 in
        let v2 =
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

let map_type_parameter_declaration (env : env) ((v1, v2) : CST.type_parameter_declaration) =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_anon_choice_name_id_d3c4b5f (env : env) (x : CST.anon_choice_name_id_d3c4b5f) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

let map_preproc_defined (env : env) (x : CST.preproc_defined) =
  (match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) -> R.Case ("Defi_LPAR_id_RPAR",
      let v1 = (* "defined" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Defi_id (v1, v2) -> R.Case ("Defi_id",
      let v1 = (* "defined" *) token env v1 in
      let v2 =
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  )

let map_variadic_declarator (env : env) ((v1, v2) : CST.variadic_declarator) =
  let v1 = (* "..." *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
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

let map_namespace_specifier (env : env) ((v1, v2) : CST.namespace_specifier) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "inline" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  R.Tuple [v1; v2]

let map_anon_choice_name_id_1a79fc3 (env : env) (x : CST.anon_choice_name_id_1a79fc3) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  )

let map_variadic_type_parameter_declaration (env : env) ((v1, v2, v3) : CST.variadic_type_parameter_declaration) =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_ms_declspec_modifier (env : env) ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let v1 = (* "__declspec" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_gnu_asm_goto_list (env : env) ((v1, v2) : CST.gnu_asm_goto_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
            in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_goto_statement (env : env) ((v1, v2, v3) : CST.goto_statement) =
  let v1 = (* "goto" *) token env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_name_id_fe6e1ce (env : env) (x : CST.anon_choice_name_id_fe6e1ce) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Vari_param v1 -> R.Case ("Vari_param",
      (* "..." *) token env v1
    )
  )

let map_gnu_asm_clobber_list (env : env) ((v1, v2) : CST.gnu_asm_clobber_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_string_literal env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_string_literal env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_gnu_asm_output_operand (env : env) ((v1, v2, v3, v4, v5) : CST.gnu_asm_output_operand) =
  let v1 =
    (match v1 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "[" *) token env v1 in
        let v2 =
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v2 = map_string_literal env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v4
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_anon_choice_name_id_dd8d494 (env : env) (x : CST.anon_choice_name_id_dd8d494) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Raw_str_lit x -> R.Case ("Raw_str_lit",
      map_raw_string_literal env x
    )
  )

let map_field_designator (env : env) ((v1, v2) : CST.field_designator) =
  let v1 = (* "." *) token env v1 in
  let v2 = map_field_identifier env v2 in
  R.Tuple [v1; v2]

let map_preproc_params (env : env) ((v1, v2, v3) : CST.preproc_params) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_name_id_d3c4b5f env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_name_id_d3c4b5f env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

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
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = map_preproc_argument_list env v2 in
  R.Tuple [v1; v2]

and map_preproc_expression (env : env) (x : CST.preproc_expression) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
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
  | `Prep_un_exp (v1, v2) -> R.Case ("Prep_un_exp",
      let v1 = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_preproc_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Prep_bin_exp x -> R.Case ("Prep_bin_exp",
      map_preproc_binary_expression env x
    )
  | `Prep_paren_exp (v1, v2, v3) -> R.Case ("Prep_paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_variadic_reference_declarator (env : env) ((v1, v2) : CST.variadic_reference_declarator) =
  let v1 =
    (match v1 with
    | `AMPAMP tok -> R.Case ("AMPAMP",
        (* "&&" *) token env tok
      )
    | `AMP tok -> R.Case ("AMP",
        (* "&" *) token env tok
      )
    )
  in
  let v2 = map_variadic_declarator env v2 in
  R.Tuple [v1; v2]

let rec map_nested_namespace_specifier (env : env) ((v1, v2, v3) : CST.nested_namespace_specifier) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_namespace_specifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | `Nested_name_spec x -> R.Case ("Nested_name_spec",
        map_nested_namespace_specifier env x
      )
    | `Name_spec x -> R.Case ("Name_spec",
        map_namespace_specifier env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_sized_type_specifier (env : env) (x : CST.sized_type_specifier) =
  (match x with
  | `Rep_choice_signed_opt_choice_id_rep1_choice_signed (v1, v2, v3) -> R.Case ("Rep_choice_signed_opt_choice_id_rep1_choice_signed",
      let v1 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_name_id_1a79fc3 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Rep1_choice_signed_opt_choice_id_rep_choice_signed (v1, v2, v3) -> R.Case ("Rep1_choice_signed_opt_choice_id_rep_choice_signed",
      let v1 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_name_id_1a79fc3 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_old_style_parameter_list (env : env) ((v1, v2, v3) : CST.old_style_parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_name_id_fe6e1ce env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_name_id_fe6e1ce env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_gnu_asm_output_operand_list (env : env) ((v1, v2) : CST.gnu_asm_output_operand_list) =
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

let map_concatenated_string (env : env) ((v1, v2, v3) : CST.concatenated_string) =
  let v1 = map_anon_choice_name_id_dd8d494 env v1 in
  let v2 =
    (match v2 with
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    | `Raw_str_lit x -> R.Case ("Raw_str_lit",
        map_raw_string_literal env x
      )
    )
  in
  let v3 =
    R.List (List.map (map_anon_choice_name_id_dd8d494 env) v3)
  in
  R.Tuple [v1; v2; v3]

let map_preproc_function_def (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_function_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
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

let map_preproc_include (env : env) ((v1, v2, v3) : CST.preproc_include) =
  let v1 = map_pat_ca8830e env v1 in
  let v2 =
    (match v2 with
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    | `System_lib_str tok -> R.Case ("System_lib_str",
        (* system_lib_string *) token env tok
      )
    | `Id tok -> R.Case ("Id",
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    | `Prep_call_exp x -> R.Case ("Prep_call_exp",
        map_preproc_call_expression env x
      )
    )
  in
  let v3 = map_imm_tok_pat_509ec78 env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_name_id_7bae85c (env : env) (x : CST.anon_choice_name_id_7bae85c) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Nested_name_spec x -> R.Case ("Nested_name_spec",
      map_nested_namespace_specifier env x
    )
  )

let map_user_defined_literal (env : env) ((v1, v2) : CST.user_defined_literal) =
  let v1 =
    (match v1 with
    | `Num_lit tok -> R.Case ("Num_lit",
        (* number_literal *) token env tok
      )
    | `Char_lit x -> R.Case ("Char_lit",
        map_char_literal env x
      )
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    | `Raw_str_lit x -> R.Case ("Raw_str_lit",
        map_raw_string_literal env x
      )
    | `Conc_str x -> R.Case ("Conc_str",
        map_concatenated_string env x
      )
    )
  in
  let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
  R.Tuple [v1; v2]

let map_namespace_alias_definition (env : env) ((v1, v2, v3, v4, v5) : CST.namespace_alias_definition) =
  let v1 = (* "namespace" *) token env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_anon_choice_name_id_7bae85c env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_abstract_array_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.abstract_array_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "[" *) token env v2 in
  let v3 = R.List (List.map (map_type_qualifier env) v3) in
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
  | `Abst_ref_decl x -> R.Case ("Abst_ref_decl",
      map_abstract_reference_declarator env x
    )
  )

and map_abstract_function_declarator (env : env) ((v1, v2) : CST.abstract_function_declarator) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_function_declarator_seq env v2 in
  R.Tuple [v1; v2]

and map_abstract_parenthesized_declarator (env : env) ((v1, v2, v3) : CST.abstract_parenthesized_declarator) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_abstract_declarator env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_abstract_pointer_declarator (env : env) ((v1, v2, v3) : CST.abstract_pointer_declarator) =
  let v1 = (* "*" *) token env v1 in
  let v2 = R.List (List.map (map_type_qualifier env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_abstract_reference_declarator (env : env) ((v1, v2) : CST.abstract_reference_declarator) =
  let v1 = map_ref_qualifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_alias_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.alias_declaration) =
  let v1 = (* "using" *) token env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (map_attribute_declaration env) v3)
  in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_type_descriptor env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_alignas_specifier (env : env) ((v1, v2, v3, v4) : CST.alignas_specifier) =
  let v1 = (* "alignas" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Prim_type tok -> R.Case ("Prim_type",
        (* primitive_type *) token env tok
      )
    )
  in
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

and map_anon_choice_arg_list_e4b6f8f (env : env) (x : CST.anon_choice_arg_list_e4b6f8f) =
  (match x with
  | `Arg_list x -> R.Case ("Arg_list",
      map_argument_list env x
    )
  | `Init_list x -> R.Case ("Init_list",
      map_initializer_list env x
    )
  )

and map_anon_choice_bitf_clause_6707c09 (env : env) (x : CST.anon_choice_bitf_clause_6707c09) =
  (match x with
  | `Bitf_clause x -> R.Case ("Bitf_clause",
      map_bitfield_clause env x
    )
  | `Init_list x -> R.Case ("Init_list",
      map_initializer_list env x
    )
  | `EQ_choice_exp (v1, v2) -> R.Case ("EQ_choice_exp",
      let v1 = (* "=" *) token env v1 in
      let v2 = map_anon_choice_exp_3078596 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_comp_stmt_e6a11e2 (env : env) (x : CST.anon_choice_comp_stmt_e6a11e2) =
  (match x with
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Try_stmt x -> R.Case ("Try_stmt",
      map_try_statement env x
    )
  )

and map_anon_choice_decl_opt_gnu_asm_exp_2c80446 (env : env) (x : CST.anon_choice_decl_opt_gnu_asm_exp_2c80446) =
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

and map_anon_choice_op_cast_b108b62 (env : env) (x : CST.anon_choice_op_cast_b108b62) =
  (match x with
  | `Op_cast x -> R.Case ("Op_cast",
      map_operator_cast env x
    )
  | `Qual_op_cast_id x -> R.Case ("Qual_op_cast_id",
      map_qualified_operator_cast_identifier env x
    )
  )

and map_anon_choice_opt___exte___exp_16c9151 (env : env) (x : CST.anon_choice_opt___exte___exp_16c9151) =
  (match x with
  | `Opt___exte___exp (v1, v2) -> R.Case ("Opt___exte___exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "__extension__" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Init_list x -> R.Case ("Init_list",
      map_initializer_list env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  )

and map_anon_choice_param_decl_13b5913 (env : env) (x : CST.anon_choice_param_decl_13b5913) =
  (match x with
  | `Param_decl x -> R.Case ("Param_decl",
      map_parameter_declaration env x
    )
  | `Opt_param_decl x -> R.Case ("Opt_param_decl",
      map_optional_parameter_declaration env x
    )
  | `Type_param_decl x -> R.Case ("Type_param_decl",
      map_type_parameter_declaration env x
    )
  | `Vari_param_decl x -> R.Case ("Vari_param_decl",
      map_variadic_parameter_declaration env x
    )
  | `Vari_type_param_decl x -> R.Case ("Vari_type_param_decl",
      map_variadic_type_parameter_declaration env x
    )
  | `Opt_type_param_decl x -> R.Case ("Opt_type_param_decl",
      map_optional_type_parameter_declaration env x
    )
  | `Temp_temp_param_decl (v1, v2, v3) -> R.Case ("Temp_temp_param_decl",
      let v1 = (* "template" *) token env v1 in
      let v2 = map_template_parameter_list env v2 in
      let v3 =
        (match v3 with
        | `Type_param_decl x -> R.Case ("Type_param_decl",
            map_type_parameter_declaration env x
          )
        | `Vari_type_param_decl x -> R.Case ("Vari_type_param_decl",
            map_variadic_type_parameter_declaration env x
          )
        | `Opt_type_param_decl x -> R.Case ("Opt_type_param_decl",
            map_optional_type_parameter_declaration env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_param_decl_1a61eef (env : env) (x : CST.anon_choice_param_decl_1a61eef) =
  (match x with
  | `Param_decl x -> R.Case ("Param_decl",
      map_parameter_declaration env x
    )
  | `Opt_param_decl x -> R.Case ("Opt_param_decl",
      map_optional_parameter_declaration env x
    )
  | `Vari_param_decl x -> R.Case ("Vari_param_decl",
      map_variadic_parameter_declaration env x
    )
  )

and map_anon_choice_param_decl_d9083af (env : env) (x : CST.anon_choice_param_decl_d9083af) =
  (match x with
  | `Param_decl x -> R.Case ("Param_decl",
      map_parameter_declaration env x
    )
  | `Opt_param_decl x -> R.Case ("Opt_param_decl",
      map_optional_parameter_declaration env x
    )
  | `Vari_param_decl x -> R.Case ("Vari_param_decl",
      map_variadic_parameter_declaration env x
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

and map_anon_choice_prep_else_8b52b0f (env : env) (x : CST.anon_choice_prep_else_8b52b0f) =
  (match x with
  | `Prep_else (v1, v2) -> R.Case ("Prep_else",
      let v1 = map_pat_56631e5 env v1 in
      let v2 = R.List (List.map (map_block_item env) v2) in
      R.Tuple [v1; v2]
    )
  | `Prep_elif (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 = R.List (List.map (map_block_item env) v4) in
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

and map_anon_choice_prep_else_in_enum_list_8258275 (env : env) (x : CST.anon_choice_prep_else_in_enum_list_8258275) =
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
  | `Prep_elif_in_enum_list (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum_list",
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
            map_anon_choice_prep_else_in_enum_list_8258275 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 (env : env) (x : CST.anon_choice_prep_else_in_enum_list_no_comma_04fd5a5) =
  (match x with
  | `Prep_else_in_enum_list_no_comma (v1, v2) -> R.Case ("Prep_else_in_enum_list_no_comma",
      let v1 = map_pat_56631e5 env v1 in
      let v2 = R.List (List.map (map_enumerator env) v2) in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_enum_list_no_comma (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum_list_no_comma",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 = R.List (List.map (map_enumerator env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_field_decl_list_97ea65e (env : env) (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) =
  (match x with
  | `Prep_else_in_field_decl_list (v1, v2) -> R.Case ("Prep_else_in_field_decl_list",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (map_field_declaration_list_item env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_field_decl_list",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_field_declaration_list_item env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_type_desc_4d9cafa (env : env) (x : CST.anon_choice_type_desc_4d9cafa) =
  (match x with
  | `Type_desc x -> R.Case ("Type_desc",
      map_type_descriptor env x
    )
  | `Type_param_pack_expa (v1, v2) -> R.Case ("Type_param_pack_expa",
      let v1 = map_type_descriptor env v1 in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_opt___exte___exp_16c9151 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_opt___exte___exp_16c9151 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.array_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = R.List (List.map (map_type_qualifier env) v3) in
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
  let v3 = R.List (List.map (map_type_qualifier env) v3) in
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
  let v2 = map_anon_choice_EQ_6389fc4 env v2 in
  let v3 = map_anon_choice_exp_3078596 env v3 in
  R.Tuple [v1; v2; v3]

and map_assignment_expression_lhs_expression (env : env) ((v1, v2, v3) : CST.assignment_expression_lhs_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_anon_choice_EQ_6389fc4 env v2 in
  let v3 = map_anon_choice_exp_3078596 env v3 in
  R.Tuple [v1; v2; v3]

and map_assignment_left_expression (env : env) (x : CST.assignment_left_expression) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      (match x with
      | `Id tok -> R.Case ("Id",
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
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
    )
  | `Qual_id x -> R.Case ("Qual_id",
      map_qualified_identifier env x
    )
  | `User_defi_lit x -> R.Case ("User_defi_lit",
      map_user_defined_literal env x
    )
  )

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 = (* "::" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_attribute_declaration (env : env) ((v1, v2, v3, v4) : CST.attribute_declaration) =
  let v1 = (* "[[" *) token env v1 in
  let v2 = map_attribute env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_attribute_specifier (env : env) ((v1, v2, v3, v4) : CST.attribute_specifier) =
  let v1 = (* "__attribute__" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_argument_list env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_attributed_declarator (env : env) ((v1, v2) : CST.attributed_declarator) =
  let v1 = map_declarator env v1 in
  let v2 =
    R.List (List.map (map_attribute_declaration env) v2)
  in
  R.Tuple [v1; v2]

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

and map_base_class_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.base_class_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    R.List (List.map (map_attribute_declaration env) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_access_spec_23a010c env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_class_name env v4 in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "..." *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 =
    R.List (List.map (fun (v1, v2, v3, v4, v5) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        R.List (List.map (map_attribute_declaration env) v2)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_choice_access_spec_23a010c env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_class_name env v4 in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "..." *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    ) v6)
  in
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
  | `Exp_LTEQGT_exp (v1, v2, v3) -> R.Case ("Exp_LTEQGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_or_exp (v1, v2, v3) -> R.Case ("Exp_or_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_and_exp (v1, v2, v3) -> R.Case ("Exp_and_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_bitor_exp (v1, v2, v3) -> R.Case ("Exp_bitor_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "bitor" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_xor_exp (v1, v2, v3) -> R.Case ("Exp_xor_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "xor" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_bitand_exp (v1, v2, v3) -> R.Case ("Exp_bitand_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "bitand" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_not_eq_exp (v1, v2, v3) -> R.Case ("Exp_not_eq_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "not_eq" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_binary_fold (env : env) ((v1, v2, v3) : CST.binary_fold) =
  let v1 = map_expression env v1 in
  let v2 = map_binary_fold_operator env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_block_item (env : env) (x : CST.block_item) =
  (match x with
  | `Choice_func_defi x -> R.Case ("Choice_func_defi",
      (match x with
      | `Func_defi x -> R.Case ("Func_defi",
          map_function_definition env x
        )
      | `Link_spec x -> R.Case ("Link_spec",
          map_linkage_specification env x
        )
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Choice_case_stmt x -> R.Case ("Choice_case_stmt",
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
      | `Name_defi x -> R.Case ("Name_defi",
          map_namespace_definition env x
        )
      | `Conc_defi x -> R.Case ("Conc_defi",
          map_concept_definition env x
        )
      | `Name_alias_defi x -> R.Case ("Name_alias_defi",
          map_namespace_alias_definition env x
        )
      | `Using_decl x -> R.Case ("Using_decl",
          map_using_declaration env x
        )
      | `Alias_decl x -> R.Case ("Alias_decl",
          map_alias_declaration env x
        )
      | `Static_assert_decl x -> R.Case ("Static_assert_decl",
          map_static_assert_declaration env x
        )
      | `Temp_decl x -> R.Case ("Temp_decl",
          map_template_declaration env x
        )
      | `Temp_inst x -> R.Case ("Temp_inst",
          map_template_instantiation env x
        )
      | `Cons_or_dest_defi x -> R.Case ("Cons_or_dest_defi",
          map_constructor_or_destructor_definition env x
        )
      | `Op_cast_defi x -> R.Case ("Op_cast_defi",
          map_operator_cast_definition env x
        )
      | `Op_cast_decl x -> R.Case ("Op_cast_decl",
          map_operator_cast_declaration env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Exp_arg_list (v1, v2) -> R.Case ("Exp_arg_list",
      let v1 = map_expression env v1 in
      let v2 = map_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Prim_type_arg_list (v1, v2) -> R.Case ("Prim_type_arg_list",
      let v1 = (* primitive_type *) token env v1 in
      let v2 = map_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  )

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
      | `Choice_choice_attr_stmt x -> R.Case ("Choice_choice_attr_stmt",
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

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_descriptor env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = map_parameter_list env v2 in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_class_declaration (env : env) ((v1, v2, v3, v4) : CST.class_declaration) =
  let v1 =
    R.List (List.map (fun x ->
      (match x with
      | `Attr_spec x -> R.Case ("Attr_spec",
          map_attribute_specifier env x
        )
      | `Alignas_spec x -> R.Case ("Alignas_spec",
          map_alignas_specifier env x
        )
      )
    ) v1)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_ms_declspec_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    R.List (List.map (map_attribute_declaration env) v3)
  in
  let v4 = map_class_declaration_item env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_class_declaration_item (env : env) ((v1, v2) : CST.class_declaration_item) =
  let v1 =
    (match v1 with
    | `Class_name x -> R.Case ("Class_name",
        map_class_name env x
      )
    | `Opt_class_name_opt_virt_spec_opt_base_class_clause_field_decl_list (v1, v2, v3, v4) -> R.Case ("Opt_class_name_opt_virt_spec_opt_base_class_clause_field_decl_list",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_class_name env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_virtual_specifier env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_base_class_clause env x
            ))
          | None -> R.Option None)
        in
        let v4 = map_field_declaration_list env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_class_name (env : env) (x : CST.class_name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Temp_type x -> R.Case ("Temp_type",
      map_template_type env x
    )
  | `Qual_type_id x -> R.Case ("Qual_type_id",
      map_qualified_type_identifier env x
    )
  )

and map_co_return_statement (env : env) ((v1, v2, v3) : CST.co_return_statement) =
  let v1 = (* "co_return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_co_yield_statement (env : env) ((v1, v2, v3) : CST.co_yield_statement) =
  let v1 = (* "co_yield" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_comma_expression (env : env) ((v1, v2, v3) : CST.comma_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 = map_anon_choice_exp_55b4dba env v3 in
  R.Tuple [v1; v2; v3]

and map_compound_literal_expression (env : env) (x : CST.compound_literal_expression) =
  (match x with
  | `LPAR_type_desc_RPAR_init_list (v1, v2, v3, v4) -> R.Case ("LPAR_type_desc_RPAR_init_list",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_descriptor env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_initializer_list env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Choice_class_name_init_list (v1, v2) -> R.Case ("Choice_class_name_init_list",
      let v1 =
        (match v1 with
        | `Class_name x -> R.Case ("Class_name",
            map_class_name env x
          )
        | `Prim_type tok -> R.Case ("Prim_type",
            (* primitive_type *) token env tok
          )
        )
      in
      let v2 = map_initializer_list env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_block_item env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_concept_definition (env : env) ((v1, v2, v3, v4, v5) : CST.concept_definition) =
  let v1 = (* "concept" *) token env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_condition_clause (env : env) ((v1, v2, v3, v4) : CST.condition_clause) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_init_statement env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    | `Comma_exp x -> R.Case ("Comma_exp",
        map_comma_expression env x
      )
    | `Cond_decl x -> R.Case ("Cond_decl",
        map_condition_declaration env x
      )
    )
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_condition_declaration (env : env) ((v1, v2, v3) : CST.condition_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 = map_declarator env v2 in
  let v3 =
    (match v3 with
    | `EQ_exp (v1, v2) -> R.Case ("EQ_exp",
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      )
    | `Init_list x -> R.Case ("Init_list",
        map_initializer_list env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_conditional_expression (env : env) ((v1, v2, v3, v4, v5) : CST.conditional_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_constructor_or_destructor_declaration (env : env) ((v1, v2, v3) : CST.constructor_or_destructor_declaration) =
  let v1 =
    R.List (List.map (map_constructor_specifiers env) v1)
  in
  let v2 = map_function_declarator env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_constructor_or_destructor_definition (env : env) ((v1, v2, v3) : CST.constructor_or_destructor_definition) =
  let v1 =
    R.List (List.map (map_constructor_specifiers env) v1)
  in
  let v2 = map_function_declarator env v2 in
  let v3 =
    (match v3 with
    | `Opt_field_init_list_comp_stmt (v1, v2) -> R.Case ("Opt_field_init_list_comp_stmt",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_field_initializer_list env x
            ))
          | None -> R.Option None)
        in
        let v2 = map_compound_statement env v2 in
        R.Tuple [v1; v2]
      )
    | `Cons_try_stmt x -> R.Case ("Cons_try_stmt",
        map_constructor_try_statement env x
      )
    | `Defa_meth_clause x -> R.Case ("Defa_meth_clause",
        map_default_method_clause env x
      )
    | `Delete_meth_clause x -> R.Case ("Delete_meth_clause",
        map_delete_method_clause env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_constructor_specifiers (env : env) (x : CST.constructor_specifiers) =
  (match x with
  | `Decl_modifs x -> R.Case ("Decl_modifs",
      map_declaration_modifiers env x
    )
  | `Expl_func_spec x -> R.Case ("Expl_func_spec",
      map_explicit_function_specifier env x
    )
  )

and map_constructor_try_statement (env : env) ((v1, v2, v3, v4) : CST.constructor_try_statement) =
  let v1 = (* "try" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_field_initializer_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_compound_statement env v3 in
  let v4 = R.List (List.map (map_catch_clause env) v4) in
  R.Tuple [v1; v2; v3; v4]

and map_declaration (env : env) ((v1, v2, v3) : CST.declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 = map_declaration_declarator env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_declarator (env : env) ((v1, v2) : CST.declaration_declarator) =
  let v1 =
    map_anon_choice_decl_opt_gnu_asm_exp_2c80446 env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        map_anon_choice_decl_opt_gnu_asm_exp_2c80446 env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

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
  | `Virt x -> R.Case ("Virt",
      map_virtual_ env x
    )
  | `Alignas_spec x -> R.Case ("Alignas_spec",
      map_alignas_specifier env x
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
  | `Choice_attr_decl x -> R.Case ("Choice_attr_decl",
      (match x with
      | `Attr_decl x -> R.Case ("Attr_decl",
          map_attributed_declarator env x
        )
      | `Poin_decl x -> R.Case ("Poin_decl",
          map_pointer_declarator env x
        )
      | `Func_decl x -> R.Case ("Func_decl",
          map_function_declarator env x
        )
      | `Array_decl x -> R.Case ("Array_decl",
          map_array_declarator env x
        )
      | `Paren_decl x -> R.Case ("Paren_decl",
          map_parenthesized_declarator env x
        )
      | `Id tok -> R.Case ("Id",
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      )
    )
  | `Ref_decl (v1, v2) -> R.Case ("Ref_decl",
      let v1 = map_ref_qualifier env v1 in
      let v2 = map_declarator env v2 in
      R.Tuple [v1; v2]
    )
  | `Qual_id x -> R.Case ("Qual_id",
      map_qualified_identifier env x
    )
  | `Temp_func x -> R.Case ("Temp_func",
      map_template_function env x
    )
  | `Op_name x -> R.Case ("Op_name",
      map_operator_name env x
    )
  | `Dest_name x -> R.Case ("Dest_name",
      map_destructor_name env x
    )
  | `Stru_bind_decl (v1, v2, v3, v4) -> R.Case ("Stru_bind_decl",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
          in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_decltype (env : env) ((v1, v2, v3, v4) : CST.decltype) =
  let v1 = (* "decltype" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_dependent_field_identifier (env : env) ((v1, v2) : CST.dependent_field_identifier) =
  let v1 = (* "template" *) token env v1 in
  let v2 = map_template_method env v2 in
  R.Tuple [v1; v2]

and map_dependent_identifier (env : env) ((v1, v2) : CST.dependent_identifier) =
  let v1 = (* "template" *) token env v1 in
  let v2 = map_template_function env v2 in
  R.Tuple [v1; v2]

and map_dependent_type_identifier (env : env) ((v1, v2) : CST.dependent_type_identifier) =
  let v1 = (* "template" *) token env v1 in
  let v2 = map_template_type env v2 in
  R.Tuple [v1; v2]

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

and map_enum_base_clause (env : env) ((v1, v2) : CST.enum_base_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | `Qual_type_id x -> R.Case ("Qual_type_id",
        map_qualified_type_identifier env x
      )
    | `Id tok -> R.Case ("Id",
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    | `Prim_type tok -> R.Case ("Prim_type",
        (* primitive_type *) token env tok
      )
    | `Sized_type_spec x -> R.Case ("Sized_type_spec",
        map_sized_type_specifier env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_enumerator (env : env) ((v1, v2) : CST.enumerator) =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
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
    R.List (List.map (fun x ->
      (match x with
      | `Enum_COMMA (v1, v2) -> R.Case ("Enum_COMMA",
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        )
      | `Prep_if_in_enum_list x -> R.Case ("Prep_if_in_enum_list",
          map_preproc_if_in_enumerator_list env x
        )
      | `Prep_ifdef_in_enum_list x -> R.Case ("Prep_ifdef_in_enum_list",
          map_preproc_ifdef_in_enumerator_list env x
        )
      | `Prep_call_COMMA (v1, v2) -> R.Case ("Prep_call_COMMA",
          let v1 = map_preproc_call env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        )
      )
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some v1 -> R.Option (Some (
        (match v1 with
        | `Enum x -> R.Case ("Enum",
            map_enumerator env x
          )
        | `Prep_if_in_enum_list_no_comma x -> R.Case ("Prep_if_in_enum_list_no_comma",
            map_preproc_if_in_enumerator_list_no_comma env x
          )
        | `Prep_ifdef_in_enum_list_no_comma x -> R.Case ("Prep_ifdef_in_enum_list_no_comma",
            map_preproc_ifdef_in_enumerator_list_no_comma env x
          )
        | `Prep_call x -> R.Case ("Prep_call",
            map_preproc_call env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_explicit_function_specifier (env : env) (x : CST.explicit_function_specifier) =
  (match x with
  | `Expl tok -> R.Case ("Expl",
      (* "explicit" *) token env tok
    )
  | `Expl_LPAR_exp_RPAR (v1, v2, v3, v4) -> R.Case ("Expl_LPAR_exp_RPAR",
      let v1 = (* "explicit" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_exp_not_bin x -> R.Case ("Choice_exp_not_bin",
      (match x with
      | `Exp_not_bin x -> R.Case ("Exp_not_bin",
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
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      | `Num_lit tok -> R.Case ("Num_lit",
          (* number_literal *) token env tok
        )
      | `Str_lit x -> R.Case ("Str_lit",
          map_string_literal env x
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
      | `Conc_str x -> R.Case ("Conc_str",
          map_concatenated_string env x
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
      )
    )
  | `Co_await_exp (v1, v2) -> R.Case ("Co_await_exp",
      let v1 = (* "co_await" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Requis_exp x -> R.Case ("Requis_exp",
      map_requires_expression env x
    )
  | `Requis_clause x -> R.Case ("Requis_clause",
      map_requires_clause env x
    )
  | `Temp_func x -> R.Case ("Temp_func",
      map_template_function env x
    )
  | `Qual_id x -> R.Case ("Qual_id",
      map_qualified_identifier env x
    )
  | `New_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("New_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "::" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "new" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_type_specifier env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_new_declarator env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_anon_choice_arg_list_e4b6f8f env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Delete_exp (v1, v2, v3, v4) -> R.Case ("Delete_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "::" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "delete" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "[" *) token env v1 in
            let v2 = (* "]" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Lambda_exp x -> R.Case ("Lambda_exp",
      map_lambda_expression env x
    )
  | `Param_pack_expa (v1, v2) -> R.Case ("Param_pack_expa",
      let v1 = map_expression env v1 in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `This tok -> R.Case ("This",
      (* "this" *) token env tok
    )
  | `Raw_str_lit x -> R.Case ("Raw_str_lit",
      map_raw_string_literal env x
    )
  | `User_defi_lit x -> R.Case ("User_defi_lit",
      map_user_defined_literal env x
    )
  | `Fold_exp x -> R.Case ("Fold_exp",
      map_fold_expression env x
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

and map_field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_field_declarator env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_anon_choice_bitf_clause_6707c09 env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          R.List (List.map (fun (v1, v2, v3) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_field_declarator env v2 in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_anon_choice_bitf_clause_6707c09 env x
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
        map_attribute_specifier env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_field_declaration_list_item env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_field_declaration_list_item (env : env) (x : CST.field_declaration_list_item) =
  (match x with
  | `Choice_field_decl x -> R.Case ("Choice_field_decl",
      (match x with
      | `Field_decl x -> R.Case ("Field_decl",
          map_field_declaration env x
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
      | `Prep_if_in_field_decl_list x -> R.Case ("Prep_if_in_field_decl_list",
          map_preproc_if_in_field_declaration_list env x
        )
      | `Prep_ifdef_in_field_decl_list x -> R.Case ("Prep_ifdef_in_field_decl_list",
          map_preproc_ifdef_in_field_declaration_list env x
        )
      )
    )
  | `Temp_decl x -> R.Case ("Temp_decl",
      map_template_declaration env x
    )
  | `Inline_meth_defi (v1, v2, v3) -> R.Case ("Inline_meth_defi",
      let v1 = map_declaration_specifiers env v1 in
      let v2 = map_field_declarator env v2 in
      let v3 =
        (match v3 with
        | `Choice_comp_stmt x -> R.Case ("Choice_comp_stmt",
            map_anon_choice_comp_stmt_e6a11e2 env x
          )
        | `Defa_meth_clause x -> R.Case ("Defa_meth_clause",
            map_default_method_clause env x
          )
        | `Delete_meth_clause x -> R.Case ("Delete_meth_clause",
            map_delete_method_clause env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_or_dest_defi x -> R.Case ("Cons_or_dest_defi",
      map_constructor_or_destructor_definition env x
    )
  | `Cons_or_dest_decl x -> R.Case ("Cons_or_dest_decl",
      map_constructor_or_destructor_declaration env x
    )
  | `Op_cast_defi x -> R.Case ("Op_cast_defi",
      map_operator_cast_definition env x
    )
  | `Op_cast_decl x -> R.Case ("Op_cast_decl",
      map_operator_cast_declaration env x
    )
  | `Friend_decl x -> R.Case ("Friend_decl",
      map_friend_declaration env x
    )
  | `Access_spec_COLON (v1, v2) -> R.Case ("Access_spec_COLON",
      let v1 = map_access_specifier env v1 in
      let v2 = (* ":" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Alias_decl x -> R.Case ("Alias_decl",
      map_alias_declaration env x
    )
  | `Using_decl x -> R.Case ("Using_decl",
      map_using_declaration env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Static_assert_decl x -> R.Case ("Static_assert_decl",
      map_static_assert_declaration env x
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
  | `Ref_field_decl (v1, v2) -> R.Case ("Ref_field_decl",
      let v1 = map_ref_qualifier env v1 in
      let v2 = map_field_declarator env v2 in
      R.Tuple [v1; v2]
    )
  | `Temp_meth x -> R.Case ("Temp_meth",
      map_template_method env x
    )
  | `Op_name x -> R.Case ("Op_name",
      map_operator_name env x
    )
  )

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> R.Case ("DOT",
        (* "." *) token env tok
      )
    | `DOTSTAR tok -> R.Case ("DOTSTAR",
        (* ".*" *) token env tok
      )
    | `DASHGT tok -> R.Case ("DASHGT",
        (* "->" *) token env tok
      )
    )
  in
  let v3 =
    (match v3 with
    | `Choice_id x -> R.Case ("Choice_id",
        map_field_identifier env x
      )
    | `Dest_name x -> R.Case ("Dest_name",
        map_destructor_name env x
      )
    | `Temp_meth x -> R.Case ("Temp_meth",
        map_template_method env x
      )
    | `Depe_field_id x -> R.Case ("Depe_field_id",
        map_dependent_field_identifier env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 =
    (match v1 with
    | `Choice_id x -> R.Case ("Choice_id",
        map_field_identifier env x
      )
    | `Temp_meth x -> R.Case ("Temp_meth",
        map_template_method env x
      )
    | `Qual_field_id x -> R.Case ("Qual_field_id",
        map_qualified_field_identifier env x
      )
    )
  in
  let v2 =
    (match v2 with
    | `Init_list x -> R.Case ("Init_list",
        map_initializer_list env x
      )
    | `Arg_list x -> R.Case ("Arg_list",
        map_argument_list env x
      )
    )
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "..." *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_field_initializer_list (env : env) ((v1, v2, v3) : CST.field_initializer_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_field_initializer env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_field_initializer env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_fold_expression (env : env) ((v1, v2, v3) : CST.fold_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Un_right_fold x -> R.Case ("Un_right_fold",
        map_unary_right_fold env x
      )
    | `Un_left_fold x -> R.Case ("Un_left_fold",
        map_unary_left_fold env x
      )
    | `Bin_fold x -> R.Case ("Bin_fold",
        map_binary_fold env x
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_for_range_loop (env : env) ((v1, v2, v3, v4, v5) : CST.for_range_loop) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_for_range_loop_body env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_for_range_loop_body (env : env) ((v1, v2, v3, v4, v5) : CST.for_range_loop_body) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_init_statement env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_declarator env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_anon_choice_exp_3078596 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_for_statement_body env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

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

and map_friend_declaration (env : env) ((v1, v2) : CST.friend_declaration) =
  let v1 = (* "friend" *) token env v1 in
  let v2 =
    (match v2 with
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    | `Func_defi x -> R.Case ("Func_defi",
        map_function_definition env x
      )
    | `Opt_choice_class_class_name_SEMI (v1, v2, v3) -> R.Case ("Opt_choice_class_class_name_SEMI",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              (match x with
              | `Class tok -> R.Case ("Class",
                  (* "class" *) token env tok
                )
              | `Struct tok -> R.Case ("Struct",
                  (* "struct" *) token env tok
                )
              | `Union tok -> R.Case ("Union",
                  (* "union" *) token env tok
                )
              )
            ))
          | None -> R.Option None)
        in
        let v2 = map_class_name env v2 in
        let v3 = (* ";" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  R.Tuple [v1; v2]

and map_function_attributes_end (env : env) ((v1, v2) : CST.function_attributes_end) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_gnu_asm_expression env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Rep1_attr_spec_rep_attr_decl (v1, v2) -> R.Case ("Rep1_attr_spec_rep_attr_decl",
        let v1 =
          R.List (List.map (map_attribute_specifier env) v1)
        in
        let v2 =
          R.List (List.map (map_attribute_declaration env) v2)
        in
        R.Tuple [v1; v2]
      )
    | `Rep_attr_spec_rep1_attr_decl (v1, v2) -> R.Case ("Rep_attr_spec_rep1_attr_decl",
        let v1 =
          R.List (List.map (map_attribute_specifier env) v1)
        in
        let v2 =
          R.List (List.map (map_attribute_declaration env) v2)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

and map_function_attributes_start (env : env) (x : CST.function_attributes_start) =
  (match x with
  | `Rep1_attr_spec_rep_type_qual (v1, v2) -> R.Case ("Rep1_attr_spec_rep_type_qual",
      let v1 =
        R.List (List.map (map_attribute_specifier env) v1)
      in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      R.Tuple [v1; v2]
    )
  | `Rep_attr_spec_rep1_type_qual (v1, v2) -> R.Case ("Rep_attr_spec_rep1_type_qual",
      let v1 =
        R.List (List.map (map_attribute_specifier env) v1)
      in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      R.Tuple [v1; v2]
    )
  )

and map_function_declarator (env : env) ((v1, v2) : CST.function_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = map_function_declarator_seq env v2 in
  R.Tuple [v1; v2]

and map_function_declarator_seq (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_declarator_seq) =
  let v1 = map_parameter_list env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_function_attributes_start env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_ref_qualifier env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_function_exception_specification env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_function_attributes_end env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_trailing_return_type env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_function_postfix env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

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
  let v4 = map_anon_choice_comp_stmt_e6a11e2 env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_function_exception_specification (env : env) (x : CST.function_exception_specification) =
  (match x with
  | `Noex (v1, v2) -> R.Case ("Noex",
      let v1 = (* "noexcept" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_expression env x
                ))
              | None -> R.Option None)
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Throw_spec (v1, v2, v3, v4) -> R.Case ("Throw_spec",
      let v1 = (* "throw" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_type_descriptor env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_type_descriptor env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_function_field_declarator (env : env) ((v1, v2) : CST.function_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = map_function_declarator_seq env v2 in
  R.Tuple [v1; v2]

and map_function_postfix (env : env) (x : CST.function_postfix) =
  (match x with
  | `Rep1_virt_spec xs -> R.Case ("Rep1_virt_spec",
      R.List (List.map (map_virtual_specifier env) xs)
    )
  | `Requis_clause x -> R.Case ("Requis_clause",
      map_requires_clause env x
    )
  )

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

and map_gnu_asm_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.gnu_asm_expression) =
  let v1 =
    (match v1 with
    | `Asm tok -> R.Case ("Asm",
        (* "asm" *) token env tok
      )
    | `X___asm__ tok -> R.Case ("X___asm__",
        (* "__asm__" *) token env tok
      )
    )
  in
  let v2 = R.List (List.map (map_gnu_asm_qualifier env) v2) in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (match v4 with
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    | `Conc_str x -> R.Case ("Conc_str",
        map_concatenated_string env x
      )
    )
  in
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
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
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

and map_if_statement (env : env) ((v1, v2, v3, v4, v5) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "constexpr" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_condition_clause env v3 in
  let v4 = map_statement env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_else_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_init_declarator (env : env) (x : CST.init_declarator) =
  (match x with
  | `Decl_EQ_choice_init_list (v1, v2, v3) -> R.Case ("Decl_EQ_choice_init_list",
      let v1 = map_declarator env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Init_list x -> R.Case ("Init_list",
            map_initializer_list env x
          )
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Decl_choice_arg_list (v1, v2) -> R.Case ("Decl_choice_arg_list",
      let v1 = map_declarator env v1 in
      let v2 = map_anon_choice_arg_list_e4b6f8f env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_init_statement (env : env) (x : CST.init_statement) =
  (match x with
  | `Alias_decl x -> R.Case ("Alias_decl",
      map_alias_declaration env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  )

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

and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_lambda_capture_specifier (env : env) ((v1, v2, v3) : CST.lambda_capture_specifier) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | `Lambda_defa_capt x -> R.Case ("Lambda_defa_capt",
        map_lambda_default_capture env x
      )
    | `Opt_exp_rep_COMMA_exp opt -> R.Case ("Opt_exp_rep_COMMA_exp",
        (match opt with
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
      )
    | `Lambda_defa_capt_COMMA_exp_rep_COMMA_exp (v1, v2, v3, v4) -> R.Case ("Lambda_defa_capt_COMMA_exp_rep_COMMA_exp",
        let v1 = map_lambda_default_capture env v1 in
        let v2 = (* "," *) token env v2 in
        let v3 = map_expression env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_lambda_expression (env : env) ((v1, v2, v3, v4) : CST.lambda_expression) =
  let v1 = map_lambda_capture_specifier env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_template_parameter_list env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_requires_clause env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_abstract_function_declarator env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_compound_statement env v4 in
  R.Tuple [v1; v2; v3; v4]

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

and map_ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let v1 = (* "__based" *) token env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_namespace_definition (env : env) ((v1, v2, v3, v4) : CST.namespace_definition) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "inline" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "namespace" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_name_id_7bae85c env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_declaration_list env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_new_declarator (env : env) (x : CST.new_declarator) =
  (match x with
  | `Rectype (v1, v2, v3, v4) -> R.Case ("Rectype",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_new_declarator env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_non_case_statement (env : env) (x : CST.non_case_statement) =
  (match x with
  | `Choice_attr_stmt x -> R.Case ("Choice_attr_stmt",
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
      | `Seh_try_stmt x -> R.Case ("Seh_try_stmt",
          map_seh_try_statement env x
        )
      | `Seh_leave_stmt x -> R.Case ("Seh_leave_stmt",
          map_seh_leave_statement env x
        )
      )
    )
  | `Co_ret_stmt x -> R.Case ("Co_ret_stmt",
      map_co_return_statement env x
    )
  | `Co_yield_stmt x -> R.Case ("Co_yield_stmt",
      map_co_yield_statement env x
    )
  | `For_range_loop x -> R.Case ("For_range_loop",
      map_for_range_loop env x
    )
  | `Try_stmt x -> R.Case ("Try_stmt",
      map_try_statement env x
    )
  | `Throw_stmt x -> R.Case ("Throw_stmt",
      map_throw_statement env x
    )
  )

and map_offsetof_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.offsetof_expression) =
  let v1 = (* "offsetof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_descriptor env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 = map_field_identifier env v5 in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_operator_cast (env : env) ((v1, v2, v3) : CST.operator_cast) =
  let v1 = (* "operator" *) token env v1 in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_abstract_declarator env v3 in
  R.Tuple [v1; v2; v3]

and map_operator_cast_declaration (env : env) ((v1, v2, v3, v4) : CST.operator_cast_declaration) =
  let v1 =
    R.List (List.map (map_constructor_specifiers env) v1)
  in
  let v2 = map_anon_choice_op_cast_b108b62 env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_operator_cast_definition (env : env) ((v1, v2, v3) : CST.operator_cast_definition) =
  let v1 =
    R.List (List.map (map_constructor_specifiers env) v1)
  in
  let v2 = map_anon_choice_op_cast_b108b62 env v2 in
  let v3 = map_anon_choice_comp_stmt_e6a11e2 env v3 in
  R.Tuple [v1; v2; v3]

and map_optional_parameter_declaration (env : env) ((v1, v2, v3, v4) : CST.optional_parameter_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Decl x -> R.Case ("Decl",
            map_declarator env x
          )
        | `Abst_ref_decl x -> R.Case ("Abst_ref_decl",
            map_abstract_reference_declarator env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_optional_type_parameter_declaration (env : env) ((v1, v2, v3, v4) : CST.optional_type_parameter_declaration) =
  let v1 = map_anon_choice_type_a2fe5d4 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_type_specifier env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_parameter_declaration (env : env) ((v1, v2) : CST.parameter_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Decl x -> R.Case ("Decl",
            map_declarator env x
          )
        | `Abst_decl x -> R.Case ("Abst_decl",
            map_abstract_declarator env x
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_param_decl_d9083af env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_param_decl_d9083af env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_declarator (env : env) ((v1, v2, v3) : CST.parenthesized_declarator) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_declarator env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) (x : CST.parenthesized_expression) =
  (match x with
  | `Choice_LPAR_choice_exp_RPAR x -> R.Case ("Choice_LPAR_choice_exp_RPAR",
      (match x with
      | `LPAR_choice_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_exp_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_anon_choice_exp_55b4dba env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `LPAR_assign_exp_lhs_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_assign_exp_lhs_exp_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_assignment_expression_lhs_expression env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `LPAR_semg_typed_meta_RPAR (v1, v2, v3) -> R.Case ("LPAR_semg_typed_meta_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_semgrep_typed_metavar env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_parenthesized_field_declarator (env : env) ((v1, v2, v3) : CST.parenthesized_field_declarator) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_field_declarator env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

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
  let v4 = R.List (List.map (map_type_qualifier env) v4) in
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
  let v4 = R.List (List.map (map_type_qualifier env) v4) in
  let v5 = map_field_declarator env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_pointer_type_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.pointer_type_declarator) =
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
  let v4 = R.List (List.map (map_type_qualifier env) v4) in
  let v5 = map_type_declarator env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_preproc_elifdef (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_block_item env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_8b52b0f env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_elifdef_in_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef_in_enumerator_list) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
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
        map_anon_choice_prep_else_in_enum_list_8258275 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_elifdef_in_enumerator_list_no_comma (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef_in_enumerator_list_no_comma) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_enumerator env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_elifdef_in_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef_in_field_declaration_list) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (map_field_declaration_list_item env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_if (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* "\n" *) token env v3 in
  let v4 = R.List (List.map (map_block_item env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_8b52b0f env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_enumerator_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_enumerator_list) =
  let v1 = map_pat_3df6e71 env v1 in
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
        map_anon_choice_prep_else_in_enum_list_8258275 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_enumerator_list_no_comma (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_enumerator_list_no_comma) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* "\n" *) token env v3 in
  let v4 = R.List (List.map (map_enumerator env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_field_declaration_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_field_declaration_list) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* "\n" *) token env v3 in
  let v4 =
    R.List (List.map (map_field_declaration_list_item env) v4)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_ifdef (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_block_item env) v3) in
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

and map_preproc_ifdef_in_enumerator_list (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_enumerator_list) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
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
        | `Choice_prep_else_in_enum_list x -> R.Case ("Choice_prep_else_in_enum_list",
            map_anon_choice_prep_else_in_enum_list_8258275 env x
          )
        | `Prep_elif_in_enum_list x -> R.Case ("Prep_elif_in_enum_list",
            map_preproc_elifdef_in_enumerator_list env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_c46d1b2 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_preproc_ifdef_in_enumerator_list_no_comma (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_enumerator_list_no_comma) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_enumerator env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_prep_else_in_enum_list_no_comma x -> R.Case ("Choice_prep_else_in_enum_list_no_comma",
            map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
          )
        | `Prep_elif_in_enum_list_no_comma x -> R.Case ("Prep_elif_in_enum_list_no_comma",
            map_preproc_elifdef_in_enumerator_list_no_comma env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_c46d1b2 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_preproc_ifdef_in_field_declaration_list (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_field_declaration_list) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (map_field_declaration_list_item env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_prep_else_in_field_decl_list x -> R.Case ("Choice_prep_else_in_field_decl_list",
            map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
          )
        | `Prep_elif_in_field_decl_list x -> R.Case ("Prep_elif_in_field_decl_list",
            map_preproc_elifdef_in_field_declaration_list env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_c46d1b2 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_qualified_field_identifier (env : env) ((v1, v2) : CST.qualified_field_identifier) =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    (match v2 with
    | `Depe_field_id x -> R.Case ("Depe_field_id",
        map_dependent_field_identifier env x
      )
    | `Qual_field_id x -> R.Case ("Qual_field_id",
        map_qualified_field_identifier env x
      )
    | `Temp_meth x -> R.Case ("Temp_meth",
        map_template_method env x
      )
    | `Choice_id x -> R.Case ("Choice_id",
        map_field_identifier env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_qualified_identifier (env : env) ((v1, v2) : CST.qualified_identifier) =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    (match v2 with
    | `Depe_id x -> R.Case ("Depe_id",
        map_dependent_identifier env x
      )
    | `Qual_id x -> R.Case ("Qual_id",
        map_qualified_identifier env x
      )
    | `Temp_func x -> R.Case ("Temp_func",
        map_template_function env x
      )
    | `Opt_temp_id (v1, v2) -> R.Case ("Opt_temp_id",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "template" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Op_name x -> R.Case ("Op_name",
        map_operator_name env x
      )
    | `Dest_name x -> R.Case ("Dest_name",
        map_destructor_name env x
      )
    | `Poin_type_decl x -> R.Case ("Poin_type_decl",
        map_pointer_type_declarator env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_qualified_operator_cast_identifier (env : env) ((v1, v2) : CST.qualified_operator_cast_identifier) =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    (match v2 with
    | `Qual_op_cast_id x -> R.Case ("Qual_op_cast_id",
        map_qualified_operator_cast_identifier env x
      )
    | `Op_cast x -> R.Case ("Op_cast",
        map_operator_cast env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_qualified_type_identifier (env : env) ((v1, v2) : CST.qualified_type_identifier) =
  let v1 = map_scope_resolution env v1 in
  let v2 =
    (match v2 with
    | `Depe_type_id x -> R.Case ("Depe_type_id",
        map_dependent_type_identifier env x
      )
    | `Qual_type_id x -> R.Case ("Qual_type_id",
        map_qualified_type_identifier env x
      )
    | `Temp_type x -> R.Case ("Temp_type",
        map_template_type env x
      )
    | `Id tok -> R.Case ("Id",
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

and map_requirement (env : env) (x : CST.requirement) =
  (match x with
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Type_requ (v1, v2) -> R.Case ("Type_requ",
      let v1 = (* "typename" *) token env v1 in
      let v2 = map_class_name env v2 in
      R.Tuple [v1; v2]
    )
  | `Comp_requ (v1, v2, v3, v4, v5, v6) -> R.Case ("Comp_requ",
      let v1 = (* "{" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "}" *) token env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "noexcept" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_trailing_return_type env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ";" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_requirement_clause_constraint (env : env) (x : CST.requirement_clause_constraint) =
  (match x with
  | `True tok -> R.Case ("True",
      (* true *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* false *) token env tok
    )
  | `Class_name x -> R.Case ("Class_name",
      map_class_name env x
    )
  | `Fold_exp x -> R.Case ("Fold_exp",
      map_fold_expression env x
    )
  | `Lambda_exp x -> R.Case ("Lambda_exp",
      map_lambda_expression env x
    )
  | `Requis_exp x -> R.Case ("Requis_exp",
      map_requires_expression env x
    )
  | `LPAR_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_conj (v1, v2, v3) -> R.Case ("Cons_conj",
      let v1 = map_requirement_clause_constraint env v1 in
      let v2 =
        (match v2 with
        | `AMPAMP tok -> R.Case ("AMPAMP",
            (* "&&" *) token env tok
          )
        | `And tok -> R.Case ("And",
            (* "and" *) token env tok
          )
        )
      in
      let v3 = map_requirement_clause_constraint env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_disj (v1, v2, v3) -> R.Case ("Cons_disj",
      let v1 = map_requirement_clause_constraint env v1 in
      let v2 =
        (match v2 with
        | `BARBAR tok -> R.Case ("BARBAR",
            (* "||" *) token env tok
          )
        | `Or tok -> R.Case ("Or",
            (* "or" *) token env tok
          )
        )
      in
      let v3 = map_requirement_clause_constraint env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_requirement_seq (env : env) ((v1, v2, v3) : CST.requirement_seq) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_requirement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_requires_clause (env : env) ((v1, v2) : CST.requires_clause) =
  let v1 = (* "requires" *) token env v1 in
  let v2 = map_requirement_clause_constraint env v2 in
  R.Tuple [v1; v2]

and map_requires_expression (env : env) ((v1, v2, v3) : CST.requires_expression) =
  let v1 = (* "requires" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_requires_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_requirement_seq env v3 in
  R.Tuple [v1; v2; v3]

and map_requires_parameter_list (env : env) ((v1, v2, v3) : CST.requires_parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_param_decl_1a61eef env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_param_decl_1a61eef env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_return_statement (env : env) (v1 : CST.return_statement) =
  (match v1 with
  | `Ret_opt_choice_exp_SEMI (v1, v2, v3) -> R.Case ("Ret_opt_choice_exp_SEMI",
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
    )
  | `Ret_init_list_SEMI (v1, v2, v3) -> R.Case ("Ret_init_list_SEMI",
      let v1 = (* "return" *) token env v1 in
      let v2 = map_initializer_list env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_scope_resolution (env : env) ((v1, v2) : CST.scope_resolution) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Id tok -> R.Case ("Id",
            (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
          )
        | `Temp_type x -> R.Case ("Temp_type",
            map_template_type env x
          )
        | `Decl x -> R.Case ("Decl",
            map_decltype env x
          )
        | `Depe_type_id x -> R.Case ("Depe_type_id",
            map_dependent_type_identifier env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = (* "::" *) token env v2 in
  R.Tuple [v1; v2]

and map_seh_except_clause (env : env) ((v1, v2, v3) : CST.seh_except_clause) =
  let v1 = (* "__except" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_seh_finally_clause (env : env) ((v1, v2) : CST.seh_finally_clause) =
  let v1 = (* "__finally" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

and map_seh_try_statement (env : env) ((v1, v2, v3) : CST.seh_try_statement) =
  let v1 = (* "__try" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  let v3 =
    (match v3 with
    | `Seh_except_clause x -> R.Case ("Seh_except_clause",
        map_seh_except_clause env x
      )
    | `Seh_fina_clause x -> R.Case ("Seh_fina_clause",
        map_seh_finally_clause env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_semgrep_typed_metavar (env : env) ((v1, v2) : CST.semgrep_typed_metavar) =
  let v1 = map_type_descriptor env v1 in
  let v2 = (* pattern \$[A-Z_][A-Z_0-9]* *) token env v2 in
  R.Tuple [v1; v2]

and map_sizeof_expression (env : env) (x : CST.sizeof_expression) =
  (match x with
  | `Sizeof_choice_exp (v1, v2) -> R.Case ("Sizeof_choice_exp",
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
    )
  | `Sizeof_DOTDOTDOT_LPAR_id_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Sizeof_DOTDOTDOT_LPAR_id_RPAR",
      let v1 = (* "sizeof" *) token env v1 in
      let v2 = (* "..." *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v4
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Case_stmt x -> R.Case ("Case_stmt",
      map_case_statement env x
    )
  | `Choice_choice_attr_stmt x -> R.Case ("Choice_choice_attr_stmt",
      map_non_case_statement env x
    )
  )

and map_static_assert_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.static_assert_declaration) =
  let v1 = (* "static_assert" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "," *) token env v1 in
        let v2 =
          (match v2 with
          | `Str_lit x -> R.Case ("Str_lit",
              map_string_literal env x
            )
          | `Raw_str_lit x -> R.Case ("Raw_str_lit",
              map_raw_string_literal env x
            )
          | `Conc_str x -> R.Case ("Conc_str",
              map_concatenated_string env x
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_subscript_argument_list (env : env) ((v1, v2, v3) : CST.subscript_argument_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_exp_3078596 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_exp_3078596 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_subscript_designator (env : env) ((v1, v2, v3) : CST.subscript_designator) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_subscript_expression (env : env) ((v1, v2) : CST.subscript_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_subscript_argument_list env v2 in
  R.Tuple [v1; v2]

and map_subscript_range_designator (env : env) ((v1, v2, v3, v4, v5) : CST.subscript_range_designator) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "..." *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_switch_statement (env : env) ((v1, v2, v3) : CST.switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_condition_clause env v2 in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_template_argument_list (env : env) ((v1, v2, v3) : CST.template_argument_list) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_type_desc_4d9cafa env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_type_desc_4d9cafa env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_tok_prec_p1_gt env v3 in
  R.Tuple [v1; v2; v3]

and map_template_declaration (env : env) ((v1, v2, v3, v4) : CST.template_declaration) =
  let v1 = (* "template" *) token env v1 in
  let v2 = map_template_parameter_list env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_requires_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | `Empty_decl x -> R.Case ("Empty_decl",
        map_empty_declaration env x
      )
    | `Alias_decl x -> R.Case ("Alias_decl",
        map_alias_declaration env x
      )
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    | `Temp_decl x -> R.Case ("Temp_decl",
        map_template_declaration env x
      )
    | `Func_defi x -> R.Case ("Func_defi",
        map_function_definition env x
      )
    | `Conc_defi x -> R.Case ("Conc_defi",
        map_concept_definition env x
      )
    | `Friend_decl x -> R.Case ("Friend_decl",
        map_friend_declaration env x
      )
    | `Cons_or_dest_decl x -> R.Case ("Cons_or_dest_decl",
        map_constructor_or_destructor_declaration env x
      )
    | `Cons_or_dest_defi x -> R.Case ("Cons_or_dest_defi",
        map_constructor_or_destructor_definition env x
      )
    | `Op_cast_decl x -> R.Case ("Op_cast_decl",
        map_operator_cast_declaration env x
      )
    | `Op_cast_defi x -> R.Case ("Op_cast_defi",
        map_operator_cast_definition env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_template_function (env : env) ((v1, v2) : CST.template_function) =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = map_template_argument_list env v2 in
  R.Tuple [v1; v2]

and map_template_instantiation (env : env) ((v1, v2, v3, v4) : CST.template_instantiation) =
  let v1 = (* "template" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_declaration_specifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_declarator env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_template_method (env : env) ((v1, v2) : CST.template_method) =
  let v1 =
    (match v1 with
    | `Choice_id x -> R.Case ("Choice_id",
        map_field_identifier env x
      )
    | `Op_name x -> R.Case ("Op_name",
        map_operator_name env x
      )
    )
  in
  let v2 = map_template_argument_list env v2 in
  R.Tuple [v1; v2]

and map_template_parameter_list (env : env) ((v1, v2, v3) : CST.template_parameter_list) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_param_decl_13b5913 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_param_decl_13b5913 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_tok_prec_p1_gt env v3 in
  R.Tuple [v1; v2; v3]

and map_template_type (env : env) ((v1, v2) : CST.template_type) =
  let v1 =
    (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = map_template_argument_list env v2 in
  R.Tuple [v1; v2]

and map_throw_statement (env : env) ((v1, v2, v3) : CST.throw_statement) =
  let v1 = (* "throw" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_trailing_return_type (env : env) ((v1, v2) : CST.trailing_return_type) =
  let v1 = (* "->" *) token env v1 in
  let v2 = map_type_descriptor env v2 in
  R.Tuple [v1; v2]

and map_try_statement (env : env) ((v1, v2, v3) : CST.try_statement) =
  let v1 = (* "try" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  let v3 = R.List (List.map (map_catch_clause env) v3) in
  R.Tuple [v1; v2; v3]

and map_type_declarator (env : env) (x : CST.type_declarator) =
  (match x with
  | `Attr_type_decl (v1, v2) -> R.Case ("Attr_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 =
        R.List (List.map (map_attribute_declaration env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Poin_type_decl x -> R.Case ("Poin_type_decl",
      map_pointer_type_declarator env x
    )
  | `Func_type_decl (v1, v2) -> R.Case ("Func_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Array_type_decl (v1, v2, v3, v4, v5) -> R.Case ("Array_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = R.List (List.map (map_type_qualifier env) v3) in
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
  | `Paren_type_decl (v1, v2, v3) -> R.Case ("Paren_type_decl",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Choice_signed x -> R.Case ("Choice_signed",
      map_anon_choice_signed_a0bfc19 env x
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  )

and map_type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_definition) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "__extension__" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "typedef" *) token env v2 in
  let v3 = map_type_definition_type env v3 in
  let v4 = map_type_definition_declarators env v4 in
  let v5 =
    R.List (List.map (map_attribute_specifier env) v5)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_type_definition_declarators (env : env) ((v1, v2) : CST.type_definition_declarators) =
  let v1 = map_type_declarator env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_declarator env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_type_definition_type (env : env) ((v1, v2, v3) : CST.type_definition_type) =
  let v1 = R.List (List.map (map_type_qualifier env) v1) in
  let v2 = map_type_specifier env v2 in
  let v3 = R.List (List.map (map_type_qualifier env) v3) in
  R.Tuple [v1; v2; v3]

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

and map_type_specifier (env : env) (x : CST.type_specifier) =
  (match x with
  | `Struct_spec (v1, v2) -> R.Case ("Struct_spec",
      let v1 = (* "struct" *) token env v1 in
      let v2 = map_class_declaration env v2 in
      R.Tuple [v1; v2]
    )
  | `Union_spec (v1, v2) -> R.Case ("Union_spec",
      let v1 = (* "union" *) token env v1 in
      let v2 = map_class_declaration env v2 in
      R.Tuple [v1; v2]
    )
  | `Enum_spec (v1, v2, v3, v4) -> R.Case ("Enum_spec",
      let v1 = (* "enum" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Class tok -> R.Case ("Class",
                (* "class" *) token env tok
              )
            | `Struct tok -> R.Case ("Struct",
                (* "struct" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Class_name_opt_enum_base_clause_opt_enum_list (v1, v2, v3) -> R.Case ("Class_name_opt_enum_base_clause_opt_enum_list",
            let v1 = map_class_name env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_enum_base_clause env x
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
        | `Enum_list x -> R.Case ("Enum_list",
            map_enumerator_list env x
          )
        )
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Class_spec (v1, v2) -> R.Case ("Class_spec",
      let v1 = (* "class" *) token env v1 in
      let v2 = map_class_declaration env v2 in
      R.Tuple [v1; v2]
    )
  | `Sized_type_spec x -> R.Case ("Sized_type_spec",
      map_sized_type_specifier env x
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  | `Temp_type x -> R.Case ("Temp_type",
      map_template_type env x
    )
  | `Depe_type (v1, v2) -> R.Case ("Depe_type",
      let v1 = (* "typename" *) token env v1 in
      let v2 = map_type_specifier env v2 in
      R.Tuple [v1; v2]
    )
  | `Plac_type_spec (v1, v2) -> R.Case ("Plac_type_spec",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_specifier env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Auto tok -> R.Case ("Auto",
            (* "auto" *) token env tok
          )
        | `Decl_auto x -> R.Case ("Decl_auto",
            map_decltype_auto env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Decl x -> R.Case ("Decl",
      map_decltype env x
    )
  | `Choice_qual_type_id x -> R.Case ("Choice_qual_type_id",
      (match x with
      | `Qual_type_id x -> R.Case ("Qual_type_id",
          map_qualified_type_identifier env x
        )
      | `Id tok -> R.Case ("Id",
          (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      )
    )
  )

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Choice_BANG_exp (v1, v2) -> R.Case ("Choice_BANG_exp",
      let v1 = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_not_exp (v1, v2) -> R.Case ("Choice_not_exp",
      let v1 =
        (match v1 with
        | `Not tok -> R.Case ("Not",
            (* "not" *) token env tok
          )
        | `Compl tok -> R.Case ("Compl",
            (* "compl" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_unary_left_fold (env : env) ((v1, v2, v3) : CST.unary_left_fold) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_fold_operator env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_unary_right_fold (env : env) ((v1, v2, v3) : CST.unary_right_fold) =
  let v1 = map_expression env v1 in
  let v2 = map_fold_operator env v2 in
  let v3 = (* "..." *) token env v3 in
  R.Tuple [v1; v2; v3]

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

and map_using_declaration (env : env) ((v1, v2, v3, v4) : CST.using_declaration) =
  let v1 = (* "using" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Name tok -> R.Case ("Name",
            (* "namespace" *) token env tok
          )
        | `Enum tok -> R.Case ("Enum",
            (* "enum" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Id tok -> R.Case ("Id",
        (* pattern \$?(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    | `Qual_id x -> R.Case ("Qual_id",
        map_qualified_identifier env x
      )
    )
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_variadic_parameter_declaration (env : env) ((v1, v2) : CST.variadic_parameter_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 =
    (match v2 with
    | `Vari_decl x -> R.Case ("Vari_decl",
        map_variadic_declarator env x
      )
    | `Vari_ref_decl x -> R.Case ("Vari_ref_decl",
        map_variadic_reference_declarator env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_condition_clause env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

let map_top_level_expression_statement (env : env) ((v1, v2) : CST.top_level_expression_statement) =
  let v1 = map_expression_not_binary env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_old_style_function_declarator (env : env) ((v1, v2) : CST.old_style_function_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = map_old_style_parameter_list env v2 in
  R.Tuple [v1; v2]

let map_top_level_statement (env : env) (x : CST.top_level_statement) =
  (match x with
  | `Choice_choice_case_stmt x -> R.Case ("Choice_choice_case_stmt",
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
      | `Co_ret_stmt x -> R.Case ("Co_ret_stmt",
          map_co_return_statement env x
        )
      | `Co_yield_stmt x -> R.Case ("Co_yield_stmt",
          map_co_yield_statement env x
        )
      | `For_range_loop x -> R.Case ("For_range_loop",
          map_for_range_loop env x
        )
      | `Try_stmt x -> R.Case ("Try_stmt",
          map_try_statement env x
        )
      | `Throw_stmt x -> R.Case ("Throw_stmt",
          map_throw_statement env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  )

let map_top_level_item (env : env) (x : CST.top_level_item) =
  (match x with
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Link_spec x -> R.Case ("Link_spec",
      map_linkage_specification env x
    )
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Choice_choice_choice_case_stmt x -> R.Case ("Choice_choice_choice_case_stmt",
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
  | `Name_defi x -> R.Case ("Name_defi",
      map_namespace_definition env x
    )
  | `Conc_defi x -> R.Case ("Conc_defi",
      map_concept_definition env x
    )
  | `Name_alias_defi x -> R.Case ("Name_alias_defi",
      map_namespace_alias_definition env x
    )
  | `Using_decl x -> R.Case ("Using_decl",
      map_using_declaration env x
    )
  | `Alias_decl x -> R.Case ("Alias_decl",
      map_alias_declaration env x
    )
  | `Static_assert_decl x -> R.Case ("Static_assert_decl",
      map_static_assert_declaration env x
    )
  | `Temp_decl x -> R.Case ("Temp_decl",
      map_template_declaration env x
    )
  | `Temp_inst x -> R.Case ("Temp_inst",
      map_template_instantiation env x
    )
  | `Cons_or_dest_defi x -> R.Case ("Cons_or_dest_defi",
      map_constructor_or_destructor_definition env x
    )
  | `Op_cast_defi x -> R.Case ("Op_cast_defi",
      map_operator_cast_definition env x
    )
  | `Op_cast_decl x -> R.Case ("Op_cast_decl",
      map_operator_cast_declaration env x
    )
  )

let map_translation_unit (env : env) (x : CST.translation_unit) =
  (match x with
  | `Rep_choice_func_defi xs -> R.Case ("Rep_choice_func_defi",
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
