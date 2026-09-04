(**
   Boilerplate to be used as a template when mapping the php CST
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

let map_encapsed_string_chars_heredoc (env : env) (tok : CST.encapsed_string_chars_heredoc) =
  (* encapsed_string_chars_heredoc *) token env tok

let map_pat_throw (env : env) (tok : CST.pat_throw) =
  (* pattern throw *) token env tok

let map_null (env : env) (tok : CST.null) =
  (* pattern null *) token env tok

let map_pat_endfor (env : env) (tok : CST.pat_endfor) =
  (* pattern endfor *) token env tok

let map_pat_match (env : env) (tok : CST.pat_match) =
  (* pattern match *) token env tok

let map_pat_int_ (env : env) (tok : CST.pat_int_) =
  (* pattern integer *) token env tok

let map_pat_trait (env : env) (tok : CST.pat_trait) =
  (* pattern trait *) token env tok

let map_nowdoc_string (env : env) (tok : CST.nowdoc_string) =
  (* nowdoc_string *) token env tok

let map_pat_13043a2 (env : env) (tok : CST.pat_13043a2) =
  (* pattern yield from *) token env tok

let map_imm_tok_prec_p1_pat_b5d7a99 (env : env) (tok : CST.imm_tok_prec_p1_pat_b5d7a99) =
  (* pattern "\\\\?[^'\\\\]+" *) token env tok

let map_pat_brk (env : env) (tok : CST.pat_brk) =
  (* pattern break *) token env tok

let map_pat_int (env : env) (tok : CST.pat_int) =
  (* pattern int *) token env tok

let map_pat_inst_ (env : env) (tok : CST.pat_inst_) =
  (* pattern instanceof *) token env tok

let map_pat_iter (env : env) (tok : CST.pat_iter) =
  (* pattern iterable *) token env tok

let map_pat_new (env : env) (tok : CST.pat_new) =
  (* pattern new *) token env tok

let map_encapsed_string_chars (env : env) (tok : CST.encapsed_string_chars) =
  (* encapsed_string_chars *) token env tok

let map_pat_ends (env : env) (tok : CST.pat_ends) =
  (* pattern endswitch *) token env tok

let map_pat_unset (env : env) (tok : CST.pat_unset) =
  (* pattern unset *) token env tok

let map_pat_final (env : env) (tok : CST.pat_final) =
  (* pattern final *) token env tok

let map_pat_and (env : env) (tok : CST.pat_and) =
  (* pattern and *) token env tok

let map_pat_endf (env : env) (tok : CST.pat_endf) =
  (* pattern endforeach *) token env tok

let map_tok_prec_p1_pat_b91d208 (env : env) (tok : CST.tok_prec_p1_pat_b91d208) =
  (* tok_prec_p1_pat_b91d208 *) token env tok

let map_imm_tok_rpar (env : env) (tok : CST.imm_tok_rpar) =
  (* ")" *) token env tok

let map_pat_real (env : env) (tok : CST.pat_real) =
  (* pattern real *) token env tok

let map_pat_inst (env : env) (tok : CST.pat_inst) =
  (* pattern insteadof *) token env tok

let map_pat_cont (env : env) (tok : CST.pat_cont) =
  (* pattern continue *) token env tok

let map_boolean (env : env) (tok : CST.boolean) =
  (* pattern true|false *) token env tok

let map_tok_amp (env : env) (tok : CST.tok_amp) =
  (* tok_amp *) token env tok

let map_pat_goto (env : env) (tok : CST.pat_goto) =
  (* pattern goto *) token env tok

let map_pat_8694eac (env : env) (tok : CST.pat_8694eac) =
  (* pattern "[bB]\"" *) token env tok

let map_pat_endd (env : env) (tok : CST.pat_endd) =
  (* pattern enddeclare *) token env tok

let map_pat_ret (env : env) (tok : CST.pat_ret) =
  (* pattern return *) token env tok

let map_pat_prot (env : env) (tok : CST.pat_prot) =
  (* pattern protected *) token env tok

let map_php_tag (env : env) (tok : CST.php_tag) =
  (* pattern <\?([pP][hH][pP]|=)? *) token env tok

let map_pat_endif (env : env) (tok : CST.pat_endif) =
  (* pattern endif *) token env tok

let map_pat_switch (env : env) (tok : CST.pat_switch) =
  (* pattern switch *) token env tok

let map_pat_elseif (env : env) (tok : CST.pat_elseif) =
  (* pattern elseif *) token env tok

let map_pat_decl (env : env) (tok : CST.pat_decl) =
  (* pattern declare *) token env tok

let map_anon_choice_DASHDASH_d11def2 (env : env) (x : CST.anon_choice_DASHDASH_d11def2) =
  (match x with
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  )

let map_pat_use (env : env) (tok : CST.pat_use) =
  (* pattern use *) token env tok

let map_encapsed_string_chars_after_variable (env : env) (tok : CST.encapsed_string_chars_after_variable) =
  (* encapsed_string_chars_after_variable *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* pattern \d*(_\d+)*((\.\d*(_\d+)*\
  )?([eE][\+-]?\d+(_\d+)*\
  )|(\.\d*(_\d+)*\
  )([eE][\+-]?\d+(_\d+)*\
  )?) *) token env tok

let map_execution_string_chars (env : env) (tok : CST.execution_string_chars) =
  (* execution_string_chars *) token env tok

let map_pat_func (env : env) (tok : CST.pat_func) =
  (* pattern function *) token env tok

let map_pat_mixed (env : env) (tok : CST.pat_mixed) =
  (* pattern mixed *) token env tok

let map_name (env : env) (tok : CST.name) =
  (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok

let map_pat_else (env : env) (tok : CST.pat_else) =
  (* pattern else *) token env tok

let map_pat_true (env : env) (tok : CST.pat_true) =
  (* pattern true *) token env tok

let map_pat_array (env : env) (tok : CST.pat_array) =
  (* pattern array *) token env tok

let map_pat_const (env : env) (tok : CST.pat_const) =
  (* pattern const *) token env tok

let map_pat_requ (env : env) (tok : CST.pat_requ) =
  (* pattern require *) token env tok

let map_pat_exit (env : env) (tok : CST.pat_exit) =
  (* pattern exit *) token env tok

let map_pat_incl_once (env : env) (tok : CST.pat_incl_once) =
  (* pattern include_once *) token env tok

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let map_pat_imples (env : env) (tok : CST.pat_imples) =
  (* pattern implements *) token env tok

let map_pat_inte (env : env) (tok : CST.pat_inte) =
  (* pattern interface *) token env tok

let map_bottom_type (env : env) (tok : CST.bottom_type) =
  (* pattern never *) token env tok

let map_pat_void (env : env) (tok : CST.pat_void) =
  (* pattern void *) token env tok

let map_pat_call (env : env) (tok : CST.pat_call) =
  (* pattern callable *) token env tok

let map_pat_list (env : env) (tok : CST.pat_list) =
  (* pattern list *) token env tok

let map_pat_self (env : env) (tok : CST.pat_self) =
  (* pattern self *) token env tok

let map_eof (env : env) (tok : CST.eof) =
  (* eof *) token env tok

let map_imm_tok_squot (env : env) (tok : CST.imm_tok_squot) =
  (* "'" *) token env tok

let map_encapsed_string_chars_after_variable_heredoc (env : env) (tok : CST.encapsed_string_chars_after_variable_heredoc) =
  (* encapsed_string_chars_after_variable_heredoc *) token env tok

let map_pat_float (env : env) (tok : CST.pat_float) =
  (* pattern float *) token env tok

let map_pat_clone (env : env) (tok : CST.pat_clone) =
  (* pattern clone *) token env tok

let map_pat_abst (env : env) (tok : CST.pat_abst) =
  (* pattern abstract *) token env tok

let map_pat_priv (env : env) (tok : CST.pat_priv) =
  (* pattern private *) token env tok

let map_pat_try (env : env) (tok : CST.pat_try) =
  (* pattern try *) token env tok

let map_pat_name (env : env) (tok : CST.pat_name) =
  (* pattern namespace *) token env tok

let map_pat_bool (env : env) (tok : CST.pat_bool) =
  (* pattern bool *) token env tok

let map_pat_bin (env : env) (tok : CST.pat_bin) =
  (* pattern binary *) token env tok

let map_tok_choice_bslashbslash (env : env) (tok : CST.tok_choice_bslashbslash) =
  (* tok_choice_bslashbslash *) token env tok

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_pat_print (env : env) (tok : CST.pat_print) =
  (* pattern print *) token env tok

let map_pat_xor (env : env) (tok : CST.pat_xor) =
  (* pattern xor *) token env tok

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  (* heredoc_start *) token env tok

let map_pat_bool_ (env : env) (tok : CST.pat_bool_) =
  (* pattern boolean *) token env tok

let map_pat_false (env : env) (tok : CST.pat_false) =
  (* pattern false *) token env tok

let map_pat_obj (env : env) (tok : CST.pat_obj) =
  (* pattern object *) token env tok

let map_pat_yield (env : env) (tok : CST.pat_yield) =
  (* pattern yield *) token env tok

let map_pat_as (env : env) (tok : CST.pat_as) =
  (* pattern as *) token env tok

let map_pat_enum (env : env) (tok : CST.pat_enum) =
  (* pattern enum *) token env tok

let map_pat_e816325 (env : env) (tok : CST.pat_e816325) =
  (* pattern "[bB]'" *) token env tok

let map_pat_null (env : env) (tok : CST.pat_null) =
  (* pattern null *) token env tok

let map_pat_echo (env : env) (tok : CST.pat_echo) =
  (* pattern echo *) token env tok

let map_pat_requ_once (env : env) (tok : CST.pat_requ_once) =
  (* pattern require_once *) token env tok

let map_var_modifier (env : env) (tok : CST.var_modifier) =
  (* pattern var *) token env tok

let map_pat_str (env : env) (tok : CST.pat_str) =
  (* pattern string *) token env tok

let map_pat_defa (env : env) (tok : CST.pat_defa) =
  (* pattern default *) token env tok

let map_execution_string_chars_after_variable (env : env) (tok : CST.execution_string_chars_after_variable) =
  (* execution_string_chars_after_variable *) token env tok

let map_imm_tok_dquot (env : env) (tok : CST.imm_tok_dquot) =
  (* "\"" *) token env tok

let map_pat_215c2d4 (env : env) (tok : CST.pat_215c2d4) =
  (* pattern true|false *) token env tok

let map_pat_catch (env : env) (tok : CST.pat_catch) =
  (* pattern catch *) token env tok

let map_pat_double (env : env) (tok : CST.pat_double) =
  (* pattern double *) token env tok

let map_pat_fn (env : env) (tok : CST.pat_fn) =
  (* pattern fn *) token env tok

let map_pat_read (env : env) (tok : CST.pat_read) =
  (* pattern readonly *) token env tok

let map_pat_if (env : env) (tok : CST.pat_if) =
  (* pattern if *) token env tok

let map_tok_ltltlt (env : env) (tok : CST.tok_ltltlt) =
  (* tok_ltltlt *) token env tok

let map_tok_prec_n1_pat_524a507 (env : env) (tok : CST.tok_prec_n1_pat_524a507) =
  (* tok_prec_n1_pat_524a507 *) token env tok

let map_pat_do (env : env) (tok : CST.pat_do) =
  (* pattern do *) token env tok

let map_pat_incl (env : env) (tok : CST.pat_incl) =
  (* pattern include *) token env tok

let map_pat_or (env : env) (tok : CST.pat_or) =
  (* pattern or *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok

let map_pat_case (env : env) (tok : CST.pat_case) =
  (* pattern case *) token env tok

let map_pat_fore (env : env) (tok : CST.pat_fore) =
  (* pattern foreach *) token env tok

let map_pat_public (env : env) (tok : CST.pat_public) =
  (* pattern public *) token env tok

let map_pat_global (env : env) (tok : CST.pat_global) =
  (* pattern global *) token env tok

let map_pat_for (env : env) (tok : CST.pat_for) =
  (* pattern for *) token env tok

let map_pat_parent (env : env) (tok : CST.pat_parent) =
  (* pattern parent *) token env tok

let map_pat_while (env : env) (tok : CST.pat_while) =
  (* pattern while *) token env tok

let map_new_line (env : env) (tok : CST.new_line) =
  (* pattern \r?\n|\r *) token env tok

let map_pat_class (env : env) (tok : CST.pat_class) =
  (* pattern class *) token env tok

let map_pat_endw (env : env) (tok : CST.pat_endw) =
  (* pattern endwhile *) token env tok

let map_pat_fina (env : env) (tok : CST.pat_fina) =
  (* pattern finally *) token env tok

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_pat_static (env : env) (tok : CST.pat_static) =
  (* pattern static *) token env tok

let map_pat_extends (env : env) (tok : CST.pat_extends) =
  (* pattern extends *) token env tok

let map_anon_choice_COLON_5102e09 (env : env) (x : CST.anon_choice_COLON_5102e09) =
  (match x with
  | `COLON tok -> R.Case ("COLON",
      (* ":" *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_string_content (env : env) (xs : CST.string_content) =
  R.List (List.map (fun x ->
    map_imm_tok_prec_p1_pat_b5d7a99 env x
  ) xs)

let map_final_modifier (env : env) (x : CST.final_modifier) =
  map_pat_final env x

let map_namespace_name (env : env) ((v1, v2) : CST.namespace_name) =
  let v1 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "\\" *) token env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_variable_name (env : env) ((v1, v2) : CST.variable_name) =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v2
  in
  R.Tuple [v1; v2]

let map_namespace_use_type (env : env) (x : CST.namespace_use_type) =
  (match x with
  | `Pat_func x -> R.Case ("Pat_func",
      map_pat_func env x
    )
  | `Pat_const x -> R.Case ("Pat_const",
      map_pat_const env x
    )
  )

let map_abstract_modifier (env : env) (x : CST.abstract_modifier) =
  map_pat_abst env x

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `Auto_semi tok -> R.Case ("Auto_semi",
      (* automatic_semicolon *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Array tok -> R.Case ("Array",
      (* "array" *) token env tok
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Pat_call x -> R.Case ("Pat_call",
      map_pat_call env x
    )
  | `Pat_false x -> R.Case ("Pat_false",
      map_pat_false env x
    )
  | `Float tok -> R.Case ("Float",
      (* "float" *) token env tok
    )
  | `Int tok -> R.Case ("Int",
      (* "int" *) token env tok
    )
  | `Pat_iter x -> R.Case ("Pat_iter",
      map_pat_iter env x
    )
  | `Pat_mixed x -> R.Case ("Pat_mixed",
      map_pat_mixed env x
    )
  | `Null tok -> R.Case ("Null",
      (* "null" *) token env tok
    )
  | `Obj tok -> R.Case ("Obj",
      (* "object" *) token env tok
    )
  | `Str tok -> R.Case ("Str",
      (* "string" *) token env tok
    )
  | `Pat_true x -> R.Case ("Pat_true",
      map_pat_true env x
    )
  | `Pat_void x -> R.Case ("Pat_void",
      map_pat_void env x
    )
  )

let map_cast_type (env : env) (x : CST.cast_type) =
  (match x with
  | `Pat_array x -> R.Case ("Pat_array",
      map_pat_array env x
    )
  | `Pat_bin x -> R.Case ("Pat_bin",
      map_pat_bin env x
    )
  | `Pat_bool x -> R.Case ("Pat_bool",
      map_pat_bool env x
    )
  | `Pat_bool_ x -> R.Case ("Pat_bool_",
      map_pat_bool_ env x
    )
  | `Pat_double x -> R.Case ("Pat_double",
      map_pat_double env x
    )
  | `Pat_float x -> R.Case ("Pat_float",
      map_pat_float env x
    )
  | `Pat_int x -> R.Case ("Pat_int",
      map_pat_int env x
    )
  | `Pat_int_ x -> R.Case ("Pat_int_",
      map_pat_int_ env x
    )
  | `Pat_obj x -> R.Case ("Pat_obj",
      map_pat_obj env x
    )
  | `Pat_real x -> R.Case ("Pat_real",
      map_pat_real env x
    )
  | `Pat_str x -> R.Case ("Pat_str",
      map_pat_str env x
    )
  | `Pat_unset x -> R.Case ("Pat_unset",
      map_pat_unset env x
    )
  )

let map_readonly_modifier (env : env) (x : CST.readonly_modifier) =
  map_pat_read env x

let map_text (env : env) (xs : CST.text) =
  R.List (List.map (fun x ->
    (match x with
    | `Tok_prec_n1_pat_524a507 x -> R.Case ("Tok_prec_n1_pat_524a507",
        map_tok_prec_n1_pat_524a507 env x
      )
    | `Tok_prec_p1_pat_b91d208 x -> R.Case ("Tok_prec_p1_pat_b91d208",
        map_tok_prec_p1_pat_b91d208 env x
      )
    )
  ) xs)

let map_nowdoc_body (env : env) ((v1, v2) : CST.nowdoc_body) =
  let v1 = (* pattern \r?\n|\r *) token env v1 in
  let v2 =
    R.List (List.map (token env (* nowdoc_string *)) v2)
  in
  R.Tuple [v1; v2]

let map_visibility_modifier (env : env) ((v1, v2) : CST.visibility_modifier) =
  let v1 =
    (match v1 with
    | `Pat_public x -> R.Case ("Pat_public",
        map_pat_public env x
      )
    | `Pat_prot x -> R.Case ("Pat_prot",
        map_pat_prot env x
      )
    | `Pat_priv x -> R.Case ("Pat_priv",
        map_pat_priv env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_imm_tok_lpar env v1 in
        let v2 =
          (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v2
        in
        let v3 = map_imm_tok_rpar env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_static_modifier (env : env) (x : CST.static_modifier) =
  map_pat_static env x

let map_relative_name (env : env) ((v1, v2, v3, v4) : CST.relative_name) =
  let v1 = map_pat_name env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "\\" *) token env v1 in
        let v2 = map_namespace_name env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "\\" *) token env v3 in
  let v4 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v4
  in
  R.Tuple [v1; v2; v3; v4]

let map_qualified_name (env : env) ((v1, v2, v3, v4) : CST.qualified_name) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "\\" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_namespace_name env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "\\" *) token env v3 in
  let v4 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v4
  in
  R.Tuple [v1; v2; v3; v4]

let map_simple_string_array_access_argument (env : env) (x : CST.simple_string_array_access_argument) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Simple_str_subs_un_exp (v1, v2) -> R.Case ("Simple_str_subs_un_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = (* integer *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
    )
  | `Var_name x -> R.Case ("Var_name",
      map_variable_name env x
    )
  )

let map_relative_scope (env : env) (x : CST.relative_scope) =
  (match x with
  | `Pat_self x -> R.Case ("Pat_self",
      map_pat_self env x
    )
  | `Pat_parent x -> R.Case ("Pat_parent",
      map_pat_parent env x
    )
  | `Pat_static x -> R.Case ("Pat_static",
      map_static_modifier env x
    )
  )

let map_argument_name (env : env) ((v1, v2) : CST.argument_name) =
  let v1 =
    (match v1 with
    | `Name tok -> R.Case ("Name",
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
      )
    | `Pat_array x -> R.Case ("Pat_array",
        map_pat_array env x
      )
    | `Pat_fn x -> R.Case ("Pat_fn",
        map_pat_fn env x
      )
    | `Pat_func x -> R.Case ("Pat_func",
        map_pat_func env x
      )
    | `Pat_match x -> R.Case ("Pat_match",
        map_pat_match env x
      )
    | `Pat_name x -> R.Case ("Pat_name",
        map_pat_name env x
      )
    | `Pat_null x -> R.Case ("Pat_null",
        map_pat_null env x
      )
    | `Pat_static x -> R.Case ("Pat_static",
        map_static_modifier env x
      )
    | `Pat_throw x -> R.Case ("Pat_throw",
        map_pat_throw env x
      )
    | `Pat_parent x -> R.Case ("Pat_parent",
        map_pat_parent env x
      )
    | `Pat_self x -> R.Case ("Pat_self",
        map_pat_self env x
      )
    | `Pat_215c2d4 x -> R.Case ("Pat_215c2d4",
        map_pat_215c2d4 env x
      )
    )
  in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Var_modi tok -> R.Case ("Var_modi",
      (* pattern var *) token env tok
    )
  | `Visi_modi x -> R.Case ("Visi_modi",
      map_visibility_modifier env x
    )
  | `Static_modi x -> R.Case ("Static_modi",
      map_static_modifier env x
    )
  | `Final_modi x -> R.Case ("Final_modi",
      map_final_modifier env x
    )
  | `Abst_modi x -> R.Case ("Abst_modi",
      map_abstract_modifier env x
    )
  | `Read_modi x -> R.Case ("Read_modi",
      map_readonly_modifier env x
    )
  )

let map_name_ (env : env) (x : CST.name_) =
  (match x with
  | `Pat_static x -> R.Case ("Pat_static",
      map_static_modifier env x
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Rela_name x -> R.Case ("Rela_name",
      map_relative_name env x
    )
  )

let map_namespace_use_clause (env : env) ((v1, v2, v3) : CST.namespace_use_clause) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_namespace_use_type env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Name tok -> R.Case ("Name",
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
      )
    | `Qual_name x -> R.Case ("Qual_name",
        map_qualified_name env x
      )
    )
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_pat_as env v1 in
        let v2 =
          (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v2
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_named_type (env : env) (x : CST.named_type) =
  (match x with
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Rela_name x -> R.Case ("Rela_name",
      map_relative_name env x
    )
  )

let map_class_interface_clause (env : env) ((v1, v2, v3) : CST.class_interface_clause) =
  let v1 = map_pat_imples env v1 in
  let v2 = map_name_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_name_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_base_clause (env : env) ((v1, v2, v3) : CST.base_clause) =
  let v1 = map_pat_extends env v1 in
  let v2 = map_name_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_name_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_namespace_use_group_ (env : env) ((v1, v2, v3, v4) : CST.namespace_use_group_) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_namespace_use_clause env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_namespace_use_clause env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_type_list (env : env) ((v1, v2) : CST.type_list) =
  let v1 = map_named_type env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_named_type env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_namespace_use_group (env : env) ((v1, v2, v3, v4) : CST.namespace_use_group) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_namespace_use_type env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_namespace_name env v2 in
  let v3 = (* "\\" *) token env v3 in
  let v4 = map_namespace_use_group_ env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_types (env : env) (x : CST.types) =
  (match x with
  | `Opt_type (v1, v2) -> R.Case ("Opt_type",
      let v1 = (* "?" *) token env v1 in
      let v2 =
        (match v2 with
        | `Named_type x -> R.Case ("Named_type",
            map_named_type env x
          )
        | `Prim_type x -> R.Case ("Prim_type",
            map_primitive_type env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Named_type x -> R.Case ("Named_type",
      map_named_type env x
    )
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  )

let map_intersection_type (env : env) ((v1, v2) : CST.intersection_type) =
  let v1 = map_types env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_tok_amp env v1 in
      let v2 = map_types env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_anon_choice_LPAR_inte_type_RPAR_d169dd3 (env : env) (x : CST.anon_choice_LPAR_inte_type_RPAR_d169dd3) =
  (match x with
  | `LPAR_inte_type_RPAR (v1, v2, v3) -> R.Case ("LPAR_inte_type_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_intersection_type env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Types x -> R.Case ("Types",
      map_types env x
    )
  )

let map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Types x -> R.Case ("Types",
      map_types env x
    )
  | `Union_type (v1, v2) -> R.Case ("Union_type",
      let v1 = map_types env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "|" *) token env v1 in
          let v2 = map_types env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Inte_type x -> R.Case ("Inte_type",
      map_intersection_type env x
    )
  | `Disj_normal_form_type (v1, v2) -> R.Case ("Disj_normal_form_type",
      let v1 =
        map_anon_choice_LPAR_inte_type_RPAR_d169dd3 env v1
      in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "|" *) token env v1 in
          let v2 =
            map_anon_choice_LPAR_inte_type_RPAR_d169dd3 env v2
          in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

let map_return_type (env : env) ((v1, v2) : CST.return_type) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | `Type x -> R.Case ("Type",
        map_type_ env x
      )
    | `Bottom_type tok -> R.Case ("Bottom_type",
        (* pattern never *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let rec map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 (env : env) ((v1, v2) : CST.anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4) =
  let v1 = map_array_element_initializer env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_array_element_initializer env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_choice_array_dest_4a8a962 (env : env) (x : CST.anon_choice_array_dest_4a8a962) =
  (match x with
  | `Array_dest x -> R.Case ("Array_dest",
      map_array_destructing env x
    )
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `By_ref x -> R.Case ("By_ref",
      map_by_ref env x
    )
  )

and map_anon_choice_by_ref_06f912a (env : env) (x : CST.anon_choice_by_ref_06f912a) =
  (match x with
  | `By_ref x -> R.Case ("By_ref",
      map_by_ref env x
    )
  | `Var_name x -> R.Case ("Var_name",
      map_variable_name env x
    )
  )

and map_anon_choice_by_ref_2379e10 (env : env) (x : CST.anon_choice_by_ref_2379e10) =
  (match x with
  | `By_ref x -> R.Case ("By_ref",
      map_by_ref env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_anon_choice_case_stmt_f1b35bc (env : env) (x : CST.anon_choice_case_stmt_f1b35bc) =
  (match x with
  | `Case_stmt (v1, v2, v3, v4) -> R.Case ("Case_stmt",
      let v1 = map_pat_case env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_anon_choice_COLON_5102e09 env v3 in
      let v4 = R.List (List.map (map_statement env) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Defa_stmt (v1, v2, v3) -> R.Case ("Defa_stmt",
      let v1 = map_pat_defa env v1 in
      let v2 = map_anon_choice_COLON_5102e09 env v2 in
      let v3 = R.List (List.map (map_statement env) v3) in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_list_dest_284bbd6 (env : env) (x : CST.anon_choice_list_dest_284bbd6) =
  (match x with
  | `List_dest x -> R.Case ("List_dest",
      map_list_destructing env x
    )
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `By_ref x -> R.Case ("By_ref",
      map_by_ref env x
    )
  | `Exp_EQGT_choice_list_dest (v1, v2, v3) -> R.Case ("Exp_EQGT_choice_list_dest",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_list_dest_8617a9f env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_list_dest_8617a9f (env : env) (x : CST.anon_choice_list_dest_8617a9f) =
  (match x with
  | `List_dest x -> R.Case ("List_dest",
      map_list_destructing env x
    )
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `By_ref x -> R.Case ("By_ref",
      map_by_ref env x
    )
  )

and map_anon_choice_match_cond_exp_d891119 (env : env) (x : CST.anon_choice_match_cond_exp_d891119) =
  (match x with
  | `Match_cond_exp (v1, v2, v3) -> R.Case ("Match_cond_exp",
      let v1 = map_match_condition_list env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Match_defa_exp (v1, v2, v3) -> R.Case ("Match_defa_exp",
      let v1 = map_pat_defa env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_simple_param_5af5eb3 (env : env) (x : CST.anon_choice_simple_param_5af5eb3) =
  (match x with
  | `Simple_param (v1, v2, v3, v4, v5) -> R.Case ("Simple_param",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_variable_name env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Vari_param (v1, v2, v3, v4, v5) -> R.Case ("Vari_param",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "..." *) token env v4 in
      let v5 = map_variable_name env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Prop_prom_param (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Prop_prom_param",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_visibility_modifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_readonly_modifier env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_anon_choice_by_ref_06f912a env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_property_hook_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

and map_anon_choice_var_7d2dd93 (env : env) (x : CST.anon_choice_var_7d2dd93) =
  (match x with
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `List_lit x -> R.Case ("List_lit",
      map_list_literal env x
    )
  )

and map_anonymous_class (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.anonymous_class) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_list env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = map_pat_class env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_base_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_class_interface_clause env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_declaration_list env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_anonymous_function_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.anonymous_function_header) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_list env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_static_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_pat_func env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "&" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = map_formal_parameters env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_anonymous_function_use_clause env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_return_type env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_anonymous_function_use_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_use_clause) =
  let v1 = map_pat_use env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_anon_choice_by_ref_06f912a env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_by_ref_06f912a env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_argument (env : env) ((v1, v2, v3) : CST.argument) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_argument_name env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "&" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Rela_scope x -> R.Case ("Rela_scope",
        map_relative_scope env x
      )
    | `Vari_unpa x -> R.Case ("Vari_unpa",
        map_variadic_unpacking env x
      )
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Arg_rep_COMMA_arg_opt_COMMA (v1, v2, v3) -> R.Case ("Arg_rep_COMMA_arg_opt_COMMA",
            let v1 = map_argument env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_argument env v2 in
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
          )
        | `Vari_plac tok -> R.Case ("Vari_plac",
            (* "..." *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_creation_expression (env : env) (x : CST.array_creation_expression) =
  (match x with
  | `Pat_array_LPAR_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Pat_array_LPAR_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RPAR",
      let v1 = map_pat_array env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `LBRACK_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RBRACK (v1, v2, v3, v4) -> R.Case ("LBRACK_opt_array_elem_init_rep_COMMA_array_elem_init_opt_COMMA_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_array_elem_init_rep_COMMA_array_elem_init_1dad3d4 env x
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
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_array_destructing (env : env) ((v1, v2, v3, v4) : CST.array_destructing) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_array_destructing_element env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_array_destructing_element env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_array_destructing_element (env : env) (x : CST.array_destructing_element) =
  (match x with
  | `Choice_array_dest x -> R.Case ("Choice_array_dest",
      map_anon_choice_array_dest_4a8a962 env x
    )
  | `Exp_EQGT_choice_array_dest (v1, v2, v3) -> R.Case ("Exp_EQGT_choice_array_dest",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_array_dest_4a8a962 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_array_element_initializer (env : env) (x : CST.array_element_initializer) =
  (match x with
  | `Choice_by_ref x -> R.Case ("Choice_by_ref",
      map_anon_choice_by_ref_2379e10 env x
    )
  | `Exp_EQGT_choice_by_ref (v1, v2, v3) -> R.Case ("Exp_EQGT_choice_by_ref",
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_by_ref_2379e10 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Vari_unpa x -> R.Case ("Vari_unpa",
      map_variadic_unpacking env x
    )
  )

and map_arrow_function_header (env : env) ((v1, v2, v3, v4, v5, v6) : CST.arrow_function_header) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_list env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_static_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_pat_fn env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "&" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = map_formal_parameters env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_return_type env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = map_name_ env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_attribute_group (env : env) ((v1, v2, v3, v4, v5) : CST.attribute_group) =
  let v1 = (* "#[" *) token env v1 in
  let v2 = map_attribute env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_attribute_list (env : env) (xs : CST.attribute_list) =
  R.List (List.map (map_attribute_group env) xs)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Un_exp_pat_inst__class_name_ref (v1, v2, v3) -> R.Case ("Un_exp_pat_inst__class_name_ref",
      let v1 = map_unary_expression env v1 in
      let v2 = map_pat_inst_ env v2 in
      let v3 = map_class_name_reference env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_QMARKQMARK_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARK_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STARSTAR_exp (v1, v2, v3) -> R.Case ("Exp_STARSTAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pat_and_exp (v1, v2, v3) -> R.Case ("Exp_pat_and_exp",
      let v1 = map_expression env v1 in
      let v2 = map_pat_and env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pat_or_exp (v1, v2, v3) -> R.Case ("Exp_pat_or_exp",
      let v1 = map_expression env v1 in
      let v2 = map_pat_or env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pat_xor_exp (v1, v2, v3) -> R.Case ("Exp_pat_xor_exp",
      let v1 = map_expression env v1 in
      let v2 = map_pat_xor env v2 in
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
  | `Exp_LTGT_exp (v1, v2, v3) -> R.Case ("Exp_LTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQGT_exp (v1, v2, v3) -> R.Case ("Exp_LTEQGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARGT_exp (v1, v2, v3) -> R.Case ("Exp_BARGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DOT_exp (v1, v2, v3) -> R.Case ("Exp_DOT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
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
  )

and map_by_ref (env : env) ((v1, v2) : CST.by_ref) =
  let v1 = (* "&" *) token env v1 in
  let v2 = map_variable env v2 in
  R.Tuple [v1; v2]

and map_callable_expression (env : env) (x : CST.callable_expression) =
  (match x with
  | `Call_var x -> R.Case ("Call_var",
      map_callable_variable env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Dere_scalar x -> R.Case ("Dere_scalar",
      map_dereferencable_scalar env x
    )
  | `New_dere_exp x -> R.Case ("New_dere_exp",
      map_new_dereferencable_expression env x
    )
  )

and map_callable_variable (env : env) (x : CST.callable_variable) =
  (match x with
  | `Simple_var x -> R.Case ("Simple_var",
      map_simple_variable env x
    )
  | `Dere_subs_exp (v1, v2, v3, v4) -> R.Case ("Dere_subs_exp",
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Member_call_exp (v1, v2, v3, v4) -> R.Case ("Member_call_exp",
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Null_member_call_exp (v1, v2, v3, v4) -> R.Case ("Null_member_call_exp",
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "?->" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_call_exp (v1, v2) -> R.Case ("Func_call_exp",
      let v1 =
        (match v1 with
        | `Name_ x -> R.Case ("Name_",
            map_name_ env x
          )
        | `Call_exp x -> R.Case ("Call_exp",
            map_callable_expression env x
          )
        )
      in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Scoped_call_exp (v1, v2, v3, v4) -> R.Case ("Scoped_call_exp",
      let v1 = map_scope_resolution_qualifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_member_name env v3 in
      let v4 = map_arguments env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = map_pat_catch env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_list env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_variable_name env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_compound_statement env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.class_const_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_list env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_final_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_modifier env) v3) in
  let v4 = map_pat_const env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_ env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_class_const_element env v6 in
  let v7 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_class_const_element env v2 in
      R.Tuple [v1; v2]
    ) v7)
  in
  let v8 = map_semicolon env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_class_const_element (env : env) ((v1, v2, v3) : CST.class_const_element) =
  let v1 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v1
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_class_constant_access_expression (env : env) ((v1, v2, v3) : CST.class_constant_access_expression) =
  let v1 = map_scope_resolution_qualifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | `Name tok -> R.Case ("Name",
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
      )
    | `LCURL_exp_RCURL x -> R.Case ("LCURL_exp_RCURL",
        map_complex_string_part env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_class_name_reference (env : env) (x : CST.class_name_reference) =
  (match x with
  | `Name_ x -> R.Case ("Name_",
      map_name_ env x
    )
  | `New_var x -> R.Case ("New_var",
      map_new_variable env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_colon_block (env : env) ((v1, v2) : CST.colon_block) =
  let v1 = (* ":" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  R.Tuple [v1; v2]

and map_complex_string_part (env : env) ((v1, v2, v3) : CST.complex_string_part) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_const_element (env : env) ((v1, v2, v3) : CST.const_element) =
  let v1 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v1
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_member_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declare_directive (env : env) ((v1, v2, v3) : CST.declare_directive) =
  let v1 =
    (match v1 with
    | `Ticks tok -> R.Case ("Ticks",
        (* "ticks" *) token env tok
      )
    | `Enco tok -> R.Case ("Enco",
        (* "encoding" *) token env tok
      )
    | `Strict_types tok -> R.Case ("Strict_types",
        (* "strict_types" *) token env tok
      )
    )
  in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_literal env v3 in
  R.Tuple [v1; v2; v3]

and map_dereferencable_expression (env : env) (x : CST.dereferencable_expression) =
  (match x with
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `New_dere_exp x -> R.Case ("New_dere_exp",
      map_new_dereferencable_expression env x
    )
  | `Class_cst_access_exp x -> R.Case ("Class_cst_access_exp",
      map_class_constant_access_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Dere_scalar x -> R.Case ("Dere_scalar",
      map_dereferencable_scalar env x
    )
  | `Name_ x -> R.Case ("Name_",
      map_name_ env x
    )
  )

and map_dereferencable_scalar (env : env) (x : CST.dereferencable_scalar) =
  (match x with
  | `Array_crea_exp x -> R.Case ("Array_crea_exp",
      map_array_creation_expression env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  )

and map_dynamic_variable_name (env : env) (x : CST.dynamic_variable_name) =
  (match x with
  | `DOLLAR_simple_var (v1, v2) -> R.Case ("DOLLAR_simple_var",
      let v1 = (* "$" *) token env v1 in
      let v2 = map_simple_variable env v2 in
      R.Tuple [v1; v2]
    )
  | `DOLLAR_LCURL_exp_RCURL (v1, v2, v3, v4) -> R.Case ("DOLLAR_LCURL_exp_RCURL",
      let v1 = (* "$" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = map_pat_else env v1 in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_else_clause_2 (env : env) ((v1, v2) : CST.else_clause_2) =
  let v1 = map_pat_else env v1 in
  let v2 = map_colon_block env v2 in
  R.Tuple [v1; v2]

and map_else_if_clause (env : env) ((v1, v2, v3) : CST.else_if_clause) =
  let v1 = map_pat_elseif env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_else_if_clause_2 (env : env) ((v1, v2, v3) : CST.else_if_clause_2) =
  let v1 = map_pat_elseif env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_colon_block env v3 in
  R.Tuple [v1; v2; v3]

and map_enum_declaration_list (env : env) ((v1, v2, v3) : CST.enum_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_enum_member_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_enum_member_declaration (env : env) (x : CST.enum_member_declaration) =
  (match x with
  | `Class_const_decl x -> R.Case ("Class_const_decl",
      map_class_const_declaration env x
    )
  | `Enum_case (v1, v2, v3, v4, v5) -> R.Case ("Enum_case",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_case env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Use_decl x -> R.Case ("Use_decl",
      map_use_declaration env x
    )
  )

and map_error_suppression_expression (env : env) ((v1, v2) : CST.error_suppression_expression) =
  let v1 = (* "@" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Cond_exp (v1, v2, v3, v4, v5) -> R.Case ("Cond_exp",
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
    )
  | `Match_exp (v1, v2, v3) -> R.Case ("Match_exp",
      let v1 = map_pat_match env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_match_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Augm_assign_exp (v1, v2, v3) -> R.Case ("Augm_assign_exp",
      let v1 = map_variable env v1 in
      let v2 =
        (match v2 with
        | `STARSTAREQ tok -> R.Case ("STARSTAREQ",
            (* "**=" *) token env tok
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
        | `DOTEQ tok -> R.Case ("DOTEQ",
            (* ".=" *) token env tok
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
        | `QMARKQMARKEQ tok -> R.Case ("QMARKQMARKEQ",
            (* "??=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 = map_anon_choice_var_7d2dd93 env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ref_assign_exp (v1, v2, v3, v4) -> R.Case ("Ref_assign_exp",
      let v1 = map_anon_choice_var_7d2dd93 env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = (* "&" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Yield_exp x -> R.Case ("Yield_exp",
      map_yield_expression env x
    )
  | `Un_exp x -> R.Case ("Un_exp",
      map_unary_expression env x
    )
  | `Error_supp_exp x -> R.Case ("Error_supp_exp",
      map_error_suppression_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Incl_exp x -> R.Case ("Incl_exp",
      map_include_expression env x
    )
  | `Incl_once_exp x -> R.Case ("Incl_once_exp",
      map_include_once_expression env x
    )
  | `Requ_exp (v1, v2) -> R.Case ("Requ_exp",
      let v1 = map_pat_requ env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Requ_once_exp (v1, v2) -> R.Case ("Requ_once_exp",
      let v1 = map_pat_requ_once env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_expressions (env : env) (x : CST.expressions) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Seq_exp x -> R.Case ("Seq_exp",
      map_sequence_expression env x
    )
  )

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = map_pat_fina env v1 in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

and map_foreach_pair (env : env) ((v1, v2, v3) : CST.foreach_pair) =
  let v1 = map_expression env v1 in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_foreach_value env v3 in
  R.Tuple [v1; v2; v3]

and map_foreach_value (env : env) (x : CST.foreach_value) =
  (match x with
  | `By_ref x -> R.Case ("By_ref",
      map_by_ref env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `List_lit x -> R.Case ("List_lit",
      map_list_literal env x
    )
  )

and map_formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_simple_param_5af5eb3 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_simple_param_5af5eb3 env v2 in
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
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_heredoc_body (env : env) ((v1, v2) : CST.heredoc_body) =
  let v1 = (* pattern \r?\n|\r *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* pattern \r?\n|\r *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_interpolated_string_body_heredoc env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_include_expression (env : env) ((v1, v2) : CST.include_expression) =
  let v1 = map_pat_incl env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_include_once_expression (env : env) ((v1, v2) : CST.include_once_expression) =
  let v1 = map_pat_incl_once env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_interpolated_execution_operator_body (env : env) (xs : CST.interpolated_execution_operator_body) =
  R.List (List.map (fun x ->
    (match x with
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    | `Var_name_exec_str_chars_after_var (v1, v2) -> R.Case ("Var_name_exec_str_chars_after_var",
        let v1 = map_variable_name env v1 in
        let v2 =
          (* execution_string_chars_after_variable *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Exec_str_chars tok -> R.Case ("Exec_str_chars",
        (* execution_string_chars *) token env tok
      )
    | `Simple_str_part x -> R.Case ("Simple_str_part",
        map_simple_string_part env x
      )
    | `Comp_str_part x -> R.Case ("Comp_str_part",
        map_complex_string_part env x
      )
    | `BSLA tok -> R.Case ("BSLA",
        (* "\\u" *) token env tok
      )
    )
  ) xs)

and map_interpolated_string_body (env : env) (xs : CST.interpolated_string_body) =
  R.List (List.map (fun x ->
    (match x with
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    | `Var_name_enca_str_chars_after_var (v1, v2) -> R.Case ("Var_name_enca_str_chars_after_var",
        let v1 = map_variable_name env v1 in
        let v2 =
          (* encapsed_string_chars_after_variable *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Enca_str_chars tok -> R.Case ("Enca_str_chars",
        (* encapsed_string_chars *) token env tok
      )
    | `Simple_str_part x -> R.Case ("Simple_str_part",
        map_simple_string_part env x
      )
    | `Comp_str_part x -> R.Case ("Comp_str_part",
        map_complex_string_part env x
      )
    | `BSLA tok -> R.Case ("BSLA",
        (* "\\u" *) token env tok
      )
    )
  ) xs)

and map_interpolated_string_body_heredoc (env : env) (xs : CST.interpolated_string_body_heredoc) =
  R.List (List.map (fun x ->
    (match x with
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    | `Var_name_enca_str_chars_after_var_here (v1, v2) -> R.Case ("Var_name_enca_str_chars_after_var_here",
        let v1 = map_variable_name env v1 in
        let v2 =
          (* encapsed_string_chars_after_variable_heredoc *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Enca_str_chars_here tok -> R.Case ("Enca_str_chars_here",
        (* encapsed_string_chars_heredoc *) token env tok
      )
    | `Simple_str_part x -> R.Case ("Simple_str_part",
        map_simple_string_part env x
      )
    | `Comp_str_part x -> R.Case ("Comp_str_part",
        map_complex_string_part env x
      )
    | `BSLA tok -> R.Case ("BSLA",
        (* "\\u" *) token env tok
      )
    )
  ) xs)

and map_list_destructing (env : env) ((v1, v2, v3, v4, v5) : CST.list_destructing) =
  let v1 = map_pat_list env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_list_dest_284bbd6 env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_list_dest_284bbd6 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_list_literal (env : env) (x : CST.list_literal) =
  (match x with
  | `List_dest x -> R.Case ("List_dest",
      map_list_destructing env x
    )
  | `Array_dest x -> R.Case ("Array_dest",
      map_array_destructing env x
    )
  )

and map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Int tok -> R.Case ("Int",
      (* integer *) token env tok
    )
  | `Float tok -> R.Case ("Float",
      (* pattern \d*(_\d+)*((\.\d*(_\d+)*\
  )?([eE][\+-]?\d+(_\d+)*\
  )|(\.\d*(_\d+)*\
  )([eE][\+-]?\d+(_\d+)*\
  )?) *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Bool tok -> R.Case ("Bool",
      (* pattern true|false *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* pattern null *) token env tok
    )
  )

and map_match_block (env : env) ((v1, v2, v3, v4) : CST.match_block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_match_cond_exp_d891119 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_match_cond_exp_d891119 env v2 in
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

and map_match_condition_list (env : env) ((v1, v2, v3) : CST.match_condition_list) =
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

and map_member_declaration (env : env) (x : CST.member_declaration) =
  (match x with
  | `Class_const_decl x -> R.Case ("Class_const_decl",
      map_class_const_declaration env x
    )
  | `Prop_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Prop_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_property_element env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_property_element env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 =
        (match v6 with
        | `Semi x -> R.Case ("Semi",
            map_semicolon env x
          )
        | `Prop_hook_list x -> R.Case ("Prop_hook_list",
            map_property_hook_list env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Use_decl x -> R.Case ("Use_decl",
      map_use_declaration env x
    )
  )

and map_member_name (env : env) (x : CST.member_name) =
  (match x with
  | `Choice_name x -> R.Case ("Choice_name",
      (match x with
      | `Name tok -> R.Case ("Name",
          (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
        )
      | `Simple_var x -> R.Case ("Simple_var",
          map_simple_variable env x
        )
      )
    )
  | `LCURL_exp_RCURL x -> R.Case ("LCURL_exp_RCURL",
      map_complex_string_part env x
    )
  )

and map_method_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_list env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = map_pat_func env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "&" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v5
  in
  let v6 = map_formal_parameters env v6 in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_return_type env x
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | `Comp_stmt x -> R.Case ("Comp_stmt",
        map_compound_statement env x
      )
    | `Semi x -> R.Case ("Semi",
        map_semicolon env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_new_dereferencable_expression (env : env) ((v1, v2) : CST.new_dereferencable_expression) =
  let v1 = map_pat_new env v1 in
  let v2 =
    (match v2 with
    | `Class_name_ref_args (v1, v2) -> R.Case ("Class_name_ref_args",
        let v1 = map_class_name_reference env v1 in
        let v2 = map_arguments env v2 in
        R.Tuple [v1; v2]
      )
    | `Anon_class x -> R.Case ("Anon_class",
        map_anonymous_class env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_new_variable (env : env) (x : CST.new_variable) =
  (match x with
  | `Simple_var x -> R.Case ("Simple_var",
      map_simple_variable env x
    )
  | `Var_subs_exp (v1, v2, v3, v4) -> R.Case ("Var_subs_exp",
      let v1 = map_new_variable env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Var_member_access_exp (v1, v2, v3) -> R.Case ("Var_member_access_exp",
      let v1 = map_new_variable env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_member_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Var_null_member_access_exp (v1, v2, v3) -> R.Case ("Var_null_member_access_exp",
      let v1 = map_new_variable env v1 in
      let v2 = (* "?->" *) token env v2 in
      let v3 = map_member_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Var_scoped_prop_access_exp (v1, v2, v3) -> R.Case ("Var_scoped_prop_access_exp",
      let v1 =
        (match v1 with
        | `Name_ x -> R.Case ("Name_",
            map_name_ env x
          )
        | `New_var x -> R.Case ("New_var",
            map_new_variable env x
          )
        )
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_simple_variable env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_object_creation_expression (env : env) (x : CST.object_creation_expression) =
  (match x with
  | `New_dere_exp x -> R.Case ("New_dere_exp",
      map_new_dereferencable_expression env x
    )
  | `New_non_dere_exp (v1, v2) -> R.Case ("New_non_dere_exp",
      let v1 = map_pat_new env v1 in
      let v2 = map_class_name_reference env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Choice_cast_var x -> R.Case ("Choice_cast_var",
      map_variable env x
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Class_cst_access_exp x -> R.Case ("Class_cst_access_exp",
      map_class_constant_access_expression env x
    )
  | `Qual_name x -> R.Case ("Qual_name",
      map_qualified_name env x
    )
  | `Rela_name x -> R.Case ("Rela_name",
      map_relative_name env x
    )
  | `Name tok -> R.Case ("Name",
      (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
    )
  | `Array_crea_exp x -> R.Case ("Array_crea_exp",
      map_array_creation_expression env x
    )
  | `Print_intr (v1, v2) -> R.Case ("Print_intr",
      let v1 = map_pat_print env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Anon_func (v1, v2) -> R.Case ("Anon_func",
      let v1 = map_anonymous_function_header env v1 in
      let v2 = map_compound_statement env v2 in
      R.Tuple [v1; v2]
    )
  | `Arrow_func (v1, v2, v3) -> R.Case ("Arrow_func",
      let v1 = map_arrow_function_header env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Obj_crea_exp x -> R.Case ("Obj_crea_exp",
      map_object_creation_expression env x
    )
  | `Update_exp x -> R.Case ("Update_exp",
      map_update_expression env x
    )
  | `Shell_cmd_exp (v1, v2, v3) -> R.Case ("Shell_cmd_exp",
      let v1 = (* "`" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_interpolated_execution_operator_body env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Throw_exp (v1, v2) -> R.Case ("Throw_exp",
      let v1 = map_pat_throw env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_property_element (env : env) ((v1, v2) : CST.property_element) =
  let v1 = map_variable_name env v1 in
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

and map_property_hook (env : env) ((v1, v2, v3, v4, v5, v6) : CST.property_hook) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attribute_list env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_final_modifier env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "&" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v4
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_formal_parameters env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_property_hook_body env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_property_hook_body (env : env) (x : CST.property_hook_body) =
  (match x with
  | `EQGT_exp_semi (v1, v2, v3) -> R.Case ("EQGT_exp_semi",
      let v1 = (* "=>" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Semi x -> R.Case ("Semi",
      map_semicolon env x
    )
  )

and map_property_hook_list (env : env) ((v1, v2, v3) : CST.property_hook_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_property_hook env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_scope_resolution_qualifier (env : env) (x : CST.scope_resolution_qualifier) =
  (match x with
  | `Rela_scope x -> R.Case ("Rela_scope",
      map_relative_scope env x
    )
  | `Name_ x -> R.Case ("Name_",
      map_name_ env x
    )
  | `Dere_exp x -> R.Case ("Dere_exp",
      map_dereferencable_expression env x
    )
  )

and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "," *) token env v2 in
  let v3 =
    (match v3 with
    | `Seq_exp x -> R.Case ("Seq_exp",
        map_sequence_expression env x
      )
    | `Exp x -> R.Case ("Exp",
        map_expression env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_simple_string_part (env : env) (x : CST.simple_string_part) =
  (match x with
  | `Simple_str_member_access_exp (v1, v2, v3) -> R.Case ("Simple_str_member_access_exp",
      let v1 = map_variable_name env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `Simple_var x -> R.Case ("Simple_var",
      map_simple_variable env x
    )
  | `Simple_str_subs_exp (v1, v2, v3, v4) -> R.Case ("Simple_str_subs_exp",
      let v1 = map_variable_name env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_simple_string_array_access_argument env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_simple_variable (env : env) (x : CST.simple_variable) =
  (match x with
  | `Var_name x -> R.Case ("Var_name",
      map_variable_name env x
    )
  | `Dyna_var_name x -> R.Case ("Dyna_var_name",
      map_dynamic_variable_name env x
    )
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Empty_stmt tok -> R.Case ("Empty_stmt",
      (* ";" *) token env tok
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Named_label_stmt (v1, v2) -> R.Case ("Named_label_stmt",
      let v1 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v1
      in
      let v2 = (* ":" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_stmt (v1, v2) -> R.Case ("Exp_stmt",
      let v1 = map_expression env v1 in
      let v2 = map_semicolon env v2 in
      R.Tuple [v1; v2]
    )
  | `If_stmt (v1, v2, v3) -> R.Case ("If_stmt",
      let v1 = map_pat_if env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        (match v3 with
        | `Stmt_rep_else_if_clause_opt_else_clause (v1, v2, v3) -> R.Case ("Stmt_rep_else_if_clause_opt_else_clause",
            let v1 = map_statement env v1 in
            let v2 = R.List (List.map (map_else_if_clause env) v2) in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_else_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Colon_blk_rep_else_if_clause_2_opt_else_clause_2_pat_endif_semi (v1, v2, v3, v4, v5) -> R.Case ("Colon_blk_rep_else_if_clause_2_opt_else_clause_2_pat_endif_semi",
            let v1 = map_colon_block env v1 in
            let v2 = R.List (List.map (map_else_if_clause_2 env) v2) in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_else_clause_2 env x
                ))
              | None -> R.Option None)
            in
            let v4 = map_pat_endif env v4 in
            let v5 = map_semicolon env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Switch_stmt (v1, v2, v3) -> R.Case ("Switch_stmt",
      let v1 = map_pat_switch env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_block env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While_stmt (v1, v2, v3) -> R.Case ("While_stmt",
      let v1 = map_pat_while env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 =
        (match v3 with
        | `Stmt x -> R.Case ("Stmt",
            map_statement env x
          )
        | `Colon_blk_pat_endw_semi (v1, v2, v3) -> R.Case ("Colon_blk_pat_endw_semi",
            let v1 = map_colon_block env v1 in
            let v2 = map_pat_endw env v2 in
            let v3 = map_semicolon env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Do_stmt (v1, v2, v3, v4, v5) -> R.Case ("Do_stmt",
      let v1 = map_pat_do env v1 in
      let v2 = map_statement env v2 in
      let v3 = map_pat_while env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("For_stmt",
      let v1 = map_pat_for env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_expressions env x
          ))
        | None -> R.Option None)
      in
      let v8 = (* ")" *) token env v8 in
      let v9 =
        (match v9 with
        | `Semi x -> R.Case ("Semi",
            map_semicolon env x
          )
        | `Stmt x -> R.Case ("Stmt",
            map_statement env x
          )
        | `COLON_rep_stmt_pat_endfor_semi (v1, v2, v3, v4) -> R.Case ("COLON_rep_stmt_pat_endfor_semi",
            let v1 = (* ":" *) token env v1 in
            let v2 = R.List (List.map (map_statement env) v2) in
            let v3 = map_pat_endfor env v3 in
            let v4 = map_semicolon env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Fore_stmt",
      let v1 = map_pat_fore env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = map_pat_as env v4 in
      let v5 =
        (match v5 with
        | `Fore_pair x -> R.Case ("Fore_pair",
            map_foreach_pair env x
          )
        | `Fore_value x -> R.Case ("Fore_value",
            map_foreach_value env x
          )
        )
      in
      let v6 = (* ")" *) token env v6 in
      let v7 =
        (match v7 with
        | `Semi x -> R.Case ("Semi",
            map_semicolon env x
          )
        | `Stmt x -> R.Case ("Stmt",
            map_statement env x
          )
        | `Colon_blk_pat_endf_semi (v1, v2, v3) -> R.Case ("Colon_blk_pat_endf_semi",
            let v1 = map_colon_block env v1 in
            let v2 = map_pat_endf env v2 in
            let v3 = map_semicolon env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Goto_stmt (v1, v2, v3) -> R.Case ("Goto_stmt",
      let v1 = map_pat_goto env v1 in
      let v2 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v2
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cont_stmt (v1, v2, v3) -> R.Case ("Cont_stmt",
      let v1 = map_pat_cont env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Brk_stmt (v1, v2, v3) -> R.Case ("Brk_stmt",
      let v1 = map_pat_brk env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = map_pat_ret env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Try_stmt (v1, v2, v3) -> R.Case ("Try_stmt",
      let v1 = map_pat_try env v1 in
      let v2 = map_compound_statement env v2 in
      let v3 =
        R.List (List.map (fun x ->
          (match x with
          | `Catch_clause x -> R.Case ("Catch_clause",
              map_catch_clause env x
            )
          | `Fina_clause x -> R.Case ("Fina_clause",
              map_finally_clause env x
            )
          )
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Decl_stmt (v1, v2, v3, v4, v5) -> R.Case ("Decl_stmt",
      let v1 = map_pat_decl env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_declare_directive env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | `Stmt x -> R.Case ("Stmt",
            map_statement env x
          )
        | `Semi x -> R.Case ("Semi",
            map_semicolon env x
          )
        | `COLON_rep_stmt_pat_endd_semi (v1, v2, v3, v4) -> R.Case ("COLON_rep_stmt_pat_endd_semi",
            let v1 = (* ":" *) token env v1 in
            let v2 = R.List (List.map (map_statement env) v2) in
            let v3 = map_pat_endd env v3 in
            let v4 = map_semicolon env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Echo_stmt (v1, v2, v3) -> R.Case ("Echo_stmt",
      let v1 = map_pat_echo env v1 in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exit_stmt (v1, v2, v3) -> R.Case ("Exit_stmt",
      let v1 = map_pat_exit env v1 in
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
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Unset_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Unset_stmt",
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_variable env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_variable env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* ")" *) token env v6 in
      let v7 = map_semicolon env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Const_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Const_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_pat_const env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_ env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_const_element env v5 in
      let v6 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_const_element env v2 in
          R.Tuple [v1; v2]
        ) v6)
      in
      let v7 = map_semicolon env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Func_defi (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Func_defi",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_func env v2 in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "&" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v4
      in
      let v5 = map_formal_parameters env v5 in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_return_type env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_compound_statement env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Class_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_pat_class env v3 in
      let v4 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v4
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_base_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_class_interface_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_declaration_list env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Inte_decl (v1, v2, v3, v4, v5) -> R.Case ("Inte_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_inte env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_base_clause env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_declaration_list env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Trait_decl (v1, v2, v3, v4) -> R.Case ("Trait_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_trait env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v3
      in
      let v4 = map_declaration_list env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Enum_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Enum_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_pat_enum env v2 in
      let v3 =
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v3
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 =
              (match v2 with
              | `Str tok -> R.Case ("Str",
                  (* "string" *) token env tok
                )
              | `Int tok -> R.Case ("Int",
                  (* "int" *) token env tok
                )
              )
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_class_interface_clause env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_enum_declaration_list env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Name_defi (v1, v2) -> R.Case ("Name_defi",
      let v1 = map_pat_name env v1 in
      let v2 =
        (match v2 with
        | `Name_name_semi (v1, v2) -> R.Case ("Name_name_semi",
            let v1 = map_namespace_name env v1 in
            let v2 = map_semicolon env v2 in
            R.Tuple [v1; v2]
          )
        | `Opt_name_name_comp_stmt (v1, v2) -> R.Case ("Opt_name_name_comp_stmt",
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_namespace_name env x
                ))
              | None -> R.Option None)
            in
            let v2 = map_compound_statement env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Name_use_decl (v1, v2, v3) -> R.Case ("Name_use_decl",
      let v1 = map_pat_use env v1 in
      let v2 =
        (match v2 with
        | `Name_use_clause_rep_COMMA_name_use_clause (v1, v2) -> R.Case ("Name_use_clause_rep_COMMA_name_use_clause",
            let v1 = map_namespace_use_clause env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_namespace_use_clause env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          )
        | `Name_use_group x -> R.Case ("Name_use_group",
            map_namespace_use_group env x
          )
        )
      in
      let v3 = map_semicolon env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Global_decl (v1, v2, v3, v4) -> R.Case ("Global_decl",
      let v1 = map_pat_global env v1 in
      let v2 = map_simple_variable env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_simple_variable env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_static_decl (v1, v2, v3, v4) -> R.Case ("Func_static_decl",
      let v1 = map_static_modifier env v1 in
      let v2 = map_static_variable_declaration env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_static_variable_declaration env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_static_variable_declaration (env : env) ((v1, v2) : CST.static_variable_declaration) =
  let v1 = map_variable_name env v1 in
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

and map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Enca_str (v1, v2, v3) -> R.Case ("Enca_str",
      let v1 =
        (match v1 with
        | `Pat_8694eac x -> R.Case ("Pat_8694eac",
            map_pat_8694eac env x
          )
        | `DQUOT tok -> R.Case ("DQUOT",
            (* "\"" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_interpolated_string_body env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Str_ (v1, v2, v3) -> R.Case ("Str_",
      let v1 =
        (match v1 with
        | `Pat_e816325 x -> R.Case ("Pat_e816325",
            map_pat_e816325 env x
          )
        | `SQUOT tok -> R.Case ("SQUOT",
            (* "'" *) token env tok
          )
        )
      in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Tok_choice_bsla x -> R.Case ("Tok_choice_bsla",
              map_tok_choice_bslashbslash env x
            )
          | `Str_content x -> R.Case ("Str_content",
              map_string_content env x
            )
          )
        ) v2)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Here (v1, v2, v3, v4, v5, v6) -> R.Case ("Here",
      let v1 = map_tok_ltltlt env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "\"" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* heredoc_start *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_imm_tok_dquot env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | `Here_body_new_line (v1, v2) -> R.Case ("Here_body_new_line",
            let v1 = map_heredoc_body env v1 in
            let v2 = (* pattern \r?\n|\r *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Opt_here_body opt -> R.Case ("Opt_here_body",
            (match opt with
            | Some x -> R.Option (Some (
                map_heredoc_body env x
              ))
            | None -> R.Option None)
          )
        )
      in
      let v6 = (* heredoc_end *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Nowdoc (v1, v2, v3, v4, v5, v6) -> R.Case ("Nowdoc",
      let v1 = map_tok_ltltlt env v1 in
      let v2 = (* "'" *) token env v2 in
      let v3 = (* heredoc_start *) token env v3 in
      let v4 = map_imm_tok_squot env v4 in
      let v5 =
        (match v5 with
        | `Nowdoc_body_new_line (v1, v2) -> R.Case ("Nowdoc_body_new_line",
            let v1 = map_nowdoc_body env v1 in
            let v2 = (* pattern \r?\n|\r *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Opt_nowdoc_body opt -> R.Case ("Opt_nowdoc_body",
            (match opt with
            | Some x -> R.Option (Some (
                map_nowdoc_body env x
              ))
            | None -> R.Option None)
          )
        )
      in
      let v6 = (* heredoc_end *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_switch_block (env : env) (x : CST.switch_block) =
  (match x with
  | `LCURL_rep_choice_case_stmt_RCURL (v1, v2, v3) -> R.Case ("LCURL_rep_choice_case_stmt_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_case_stmt_f1b35bc env) v2)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `COLON_rep_choice_case_stmt_pat_ends_semi (v1, v2, v3, v4) -> R.Case ("COLON_rep_choice_case_stmt_pat_ends_semi",
      let v1 = (* ":" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_case_stmt_f1b35bc env) v2)
      in
      let v3 = map_pat_ends env v3 in
      let v4 = map_semicolon env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Clone_exp (v1, v2) -> R.Case ("Clone_exp",
      let v1 = map_pat_clone env v1 in
      let v2 = map_primary_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Un_op_exp (v1, v2) -> R.Case ("Un_op_exp",
      let v1 =
        (match v1 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `TILDE tok -> R.Case ("TILDE",
            (* "~" *) token env tok
          )
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Cast_exp (v1, v2, v3, v4) -> R.Case ("Cast_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 =
        (match v4 with
        | `Un_exp x -> R.Case ("Un_exp",
            map_unary_expression env x
          )
        | `Incl_exp x -> R.Case ("Incl_exp",
            map_include_expression env x
          )
        | `Incl_once_exp x -> R.Case ("Incl_once_exp",
            map_include_once_expression env x
          )
        | `Error_supp_exp x -> R.Case ("Error_supp_exp",
            map_error_suppression_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Choice_DASHDASH_choice_cast_var (v1, v2) -> R.Case ("Choice_DASHDASH_choice_cast_var",
      let v1 = map_anon_choice_DASHDASH_d11def2 env v1 in
      let v2 = map_variable env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_cast_var_choice_DASHDASH (v1, v2) -> R.Case ("Choice_cast_var_choice_DASHDASH",
      let v1 = map_variable env v1 in
      let v2 = map_anon_choice_DASHDASH_d11def2 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_use_as_clause (env : env) ((v1, v2, v3) : CST.use_as_clause) =
  let v1 =
    (match v1 with
    | `Class_cst_access_exp x -> R.Case ("Class_cst_access_exp",
        map_class_constant_access_expression env x
      )
    | `Name tok -> R.Case ("Name",
        (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
      )
    )
  in
  let v2 = map_pat_as env v2 in
  let v3 =
    (match v3 with
    | `Opt_visi_modi_name (v1, v2) -> R.Case ("Opt_visi_modi_name",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_visibility_modifier env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v2
        in
        R.Tuple [v1; v2]
      )
    | `Visi_modi_opt_name (v1, v2) -> R.Case ("Visi_modi_opt_name",
        let v1 = map_visibility_modifier env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) =
  let v1 = map_pat_use env v1 in
  let v2 = map_name_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_name_ env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | `Use_list x -> R.Case ("Use_list",
        map_use_list env x
      )
    | `Semi x -> R.Case ("Semi",
        map_semicolon env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_use_instead_of_clause (env : env) ((v1, v2, v3) : CST.use_instead_of_clause) =
  let v1 = map_class_constant_access_expression env v1 in
  let v2 = map_pat_inst env v2 in
  let v3 =
    (* pattern [_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff][_a-zA-Z\u0080-\u009f\u00a1-\u200a\u200c-\u205f\u2061-\ufefe\uff00-\uffff\d]* *) token env v3
  in
  R.Tuple [v1; v2; v3]

and map_use_list (env : env) ((v1, v2, v3) : CST.use_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Use_inst_of_clause x -> R.Case ("Use_inst_of_clause",
            map_use_instead_of_clause env x
          )
        | `Use_as_clause x -> R.Case ("Use_as_clause",
            map_use_as_clause env x
          )
        )
      in
      let v2 = map_semicolon env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Cast_var (v1, v2, v3, v4) -> R.Case ("Cast_var",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_cast_type env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_variable env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `New_var x -> R.Case ("New_var",
      map_new_variable env x
    )
  | `Call_var x -> R.Case ("Call_var",
      map_callable_variable env x
    )
  | `Scoped_prop_access_exp (v1, v2, v3) -> R.Case ("Scoped_prop_access_exp",
      let v1 = map_scope_resolution_qualifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_simple_variable env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Member_access_exp (v1, v2, v3) -> R.Case ("Member_access_exp",
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_member_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Null_member_access_exp (v1, v2, v3) -> R.Case ("Null_member_access_exp",
      let v1 = map_dereferencable_expression env v1 in
      let v2 = (* "?->" *) token env v2 in
      let v3 = map_member_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_variadic_unpacking (env : env) ((v1, v2) : CST.variadic_unpacking) =
  let v1 = (* "..." *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_yield_expression (env : env) (x : CST.yield_expression) =
  (match x with
  | `Pat_yield_opt_array_elem_init (v1, v2) -> R.Case ("Pat_yield_opt_array_elem_init",
      let v1 = map_pat_yield env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_array_element_initializer env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Pat_13043a2_exp (v1, v2) -> R.Case ("Pat_13043a2_exp",
      let v1 = map_pat_13043a2 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_text env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* pattern <\?([pP][hH][pP]|=)? *) token env v1 in
        let v2 = R.List (List.map (map_statement env) v2) in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_text_interpolation (env : env) ((v1, v2, v3) : CST.text_interpolation) =
  let v1 = (* "?>" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_text env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Php_tag tok -> R.Case ("Php_tag",
        (* pattern <\?([pP][hH][pP]|=)? *) token env tok
      )
    | `Eof tok -> R.Case ("Eof",
        (* eof *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

let dump_tree root =
  map_program () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Text_interpolation (_loc, x) -> ("text_interpolation", "text_interpolation", map_text_interpolation env x)

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
