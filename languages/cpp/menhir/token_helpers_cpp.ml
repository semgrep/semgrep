(* tokens *)
open Parser_cpp
module PI = Lib_ast_fuzzy

(*****************************************************************************)
(* Is_xxx, categories *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

(* ---------------------------------------------------------------------- *)
let is_space = function
  | TCommentSpace _
  | TCommentNewline _ ->
      true
  | _ -> false

let is_comment_or_space = function
  | TCommentSpace _
  | TCommentNewline _
  | TComment _ ->
      true
  | _ -> false

let is_just_comment = function
  | TComment _ -> true
  | _ -> false

let is_comment = function
  | TCommentSpace _
  | TCommentNewline _
  | TComment _
  | TComment_Pp _
  | TComment_Cpp _ ->
      true
  | _ -> false

let is_real_comment = function
  | TComment _
  | TCommentSpace _
  | TCommentNewline _ ->
      true
  | _ -> false

let is_fake_comment = function
  | TComment_Pp _
  | TComment_Cpp _ ->
      true
  | _ -> false

let is_not_comment x = not (is_comment x)

(* ---------------------------------------------------------------------- *)
(*
let is_gcc_token = function
  | Tasm _ | Tinline _  | Tattribute _  | Ttypeof _
      -> true
  | _ -> false
*)

let is_pp_instruction = function
  | TInclude _
  | TDefine _
  | TIfdef _
  | TIfdefelse _
  | TIfdefelif _
  | TEndif _
  | TIfdefBool _
  | TIfdefMisc _
  | TIfdefVersion _
  | TUndef _
  | TCppDirectiveOther _ ->
      true
  | _ -> false

let is_opar = function
  | TOPar _
  | TOPar_Define _
  | TOPar_CplusplusInit _ ->
      true
  | _ -> false

let is_cpar = function
  | TCPar _
  | TCPar_EOL _ ->
      true
  | _ -> false

let is_obrace = function
  | TOBrace _
  | TOBrace_DefineInit _ ->
      true
  | _ -> false

let is_cbrace = function
  | TCBrace _ -> true
  | _ -> false

let is_statement = function
  | Tfor _
  | Tdo _
  | Tif _
  | Twhile _
  | Treturn _
  | Tbreak _
  | Telse _
  | Tswitch _
  | Tcase _
  | Tcontinue _
  | Tgoto _
  | TPtVirg _
  | TIdent_MacroIterator _ ->
      true
  | _ -> false

(* is_start_of_something is used in parse_c for error recovery, to find
 * a synchronisation token.
 *
 * Would like to put TIdent or TDefine, TIfdef but they can be in the
 * middle of a function, for instance with label:.
 *
 * Could put Typedefident but fired ? it would work in error recovery
 * on the already_passed tokens, which has been already gone in the
 * Parsing_hacks.lookahead machinery, but it will not work on the
 * "next" tokens. But because the namespace for labels is different
 * from namespace for ident/typedef, we can use the name for a typedef
 * for a label and so dangerous to put Typedefident at true here.
 *
 * Can look in parser_c.output to know what can be at toplevel
 * at the very beginning.
 *)

let is_start_of_something = function
  | Tchar _
  | Tshort _
  | Tint _
  | Tdouble _
  | Tfloat _
  | Tlong _
  | Tunsigned _
  | Tsigned _
  | Tvoid _
  | Tauto _
  | Tregister _
  | Textern _
  | Tstatic _
  | Tconst _
  | Tvolatile _
  | Ttypedef _
  | Tstruct _
  | Tunion _
  | Tenum _
  (* c++ext: *)
  | Tclass _
  | Tbool _
  | Twchar_t _ ->
      true
  | _ -> false

let is_binary_operator = function
  | TOrLog _
  | TAndLog _
  | TOr _
  | TXor _
  | TAnd _
  | TEqEq _
  | TNotEq _
  | TInf _
  | TSup _
  | TInfEq _
  | TSupEq _
  | TShl _
  | TShr _
  | TPlus _
  | TMinus _
  | TMul _
  | TDiv _
  | TMod _ ->
      true
  | _ -> false

let is_binary_operator_except_star = function
  (* | TAnd _ *)
  (*|  TMul _*)
  (* | TAndLog _ *)
  | TOrLog _
  | TOr _
  | TXor _
  | TEqEq _
  | TNotEq _
  | TInf _
  | TSup _
  | TInfEq _
  | TSupEq _
  | TShl _
  | TShr _
  | TPlus _
  | TMinus _
  | TDiv _
  | TMod _ ->
      true
  | _ -> false

let is_stuff_taking_parenthized = function
  | Tif _
  | Twhile _
  | Tswitch _
  | Ttypeof _
  | TIdent_MacroIterator _ ->
      true
  | _ -> false

let is_static_cast_like = function
  | Tconst_cast _
  | Tdynamic_cast _
  | Tstatic_cast _
  | Treinterpret_cast _ ->
      true
  | _ -> false

let is_basic_type = function
  | Tchar _
  | Tshort _
  | Tint _
  | Tdouble _
  | Tfloat _
  | Tlong _
  | Tbool _
  | Twchar_t _
  | Tunsigned _
  | Tsigned _
  | Tvoid _ ->
      true
  | _ -> false

let is_struct_like_keyword = function
  | Tstruct _
  | Tunion _
  | Tenum _ ->
      true
  (* c++ext: *)
  | Tclass _ -> true
  | _ -> false

let is_classkey_keyword = function
  | Tstruct _
  | Tunion _
  | Tclass _ ->
      true
  | _ -> false

let is_cpp_keyword = function
  | Tclass _
  | Tthis _
  | Tnew _
  | Tdelete _
  | Ttemplate _
  | Ttypeid _
  | Ttypename _
  | Tcatch _
  | Ttry _
  | Tthrow _
  | Toperator _
  | Tpublic _
  | Tprivate _
  | Tprotected _
  | Tfriend _
  | Tvirtual _
  | Tnamespace _
  | Tusing _
  | Tbool _
  | Tfalse _
  | Ttrue _
  | Twchar_t _
  | Tconst_cast _
  | Tdynamic_cast _
  | Tstatic_cast _
  | Treinterpret_cast _
  | Texplicit _
  | Tmutable _ ->
      true
  | _ -> false

let is_really_cpp_keyword = function
  | Tconst_cast _
  | Tdynamic_cast _
  | Tstatic_cast _
  | Treinterpret_cast _ ->
      true
  (* when have some asm volatile, can have some ::
     | TColCol  _
        -> true
  *)
  | _ -> false

(* some false positive on some C file like sqlite3.c *)
let is_maybenot_cpp_keyword = function
  | Tpublic _
  | Tprivate _
  | Tprotected _
  | Ttemplate _
  | Tnew _
  | Ttypename _
  | Tnamespace _ ->
      true
  | _ -> false

(* used in the algorithm for "10 most problematic tokens". C-s for TIdent
 * in parser_cpp.mly
 *)
let is_ident_like = function
  | TIdent _
  | TIdent_Typedef _
  | TIdent_Define _ (*  | TDefParamVariadic _*)
  | TUnknown _
  | TIdent_MacroStmt _
  | TIdent_MacroString _
  | TIdent_MacroIterator _
  | TIdent_MacroDecl _
  (*  | TIdent_MacroDeclConst _ *)
  (*
  | TIdent_MacroAttr _
  | TIdent_MacroAttrStorage _
*)
  | TIdent_ClassnameInQualifier _
  | TIdent_ClassnameInQualifier_BeforeTypedef _
  | TIdent_Templatename _
  | TIdent_TemplatenameInQualifier _
  | TIdent_TemplatenameInQualifier_BeforeTypedef _
  | TIdent_Constructor _
  | TIdent_TypedefConstr _ ->
      true
  | _ -> false

let is_privacy_keyword = function
  | Tpublic _
  | Tprivate _
  | Tprotected _ ->
      true
  | _ -> false

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

let token_kind_of_tok t =
  match t with
  | TComment _
  | TComment_Pp _
  | TComment_Cpp _ ->
      PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline
  | TOPar _
  | TOPar_Define _
  | TOPar_CplusplusInit _ ->
      PI.LPar
  | TCPar _
  | TCPar_EOL _ ->
      PI.RPar
  | TOBrace _
  | TOBrace_DefineInit _ ->
      PI.LBrace
  | TCBrace _ -> PI.RBrace
  | TOCro _
  | TOCro_Lambda _ ->
      PI.LBracket
  | TCCro _ -> PI.RBracket
  (* not always balanced: TInf and TSup! *)
  | TInf_Template _ -> PI.LAngle
  | TSup_Template _ -> PI.RAngle
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

(* Because ocamlyacc force us to do it that way. The ocamlyacc token
 * cant be a pair of a sum type, it must be directly a sum type.
 *)

(* used by tokens to complete the parse_info with filename, line, col infos *)
let visitor_info_of_tok f = function
  | Tnull ii -> Tnull (f ii)
  | LDots ii -> LDots (f ii)
  | RDots ii -> RDots (f ii)
  | Tdecltype i -> Tdecltype (f i)
  | Tthread_local i -> Tthread_local (f i)
  | Tnullptr i -> Tnullptr (f i)
  | Tconstexpr i -> Tconstexpr (f i)
  | TString (s, i) -> TString (s, f i)
  | TChar (s, i) -> TChar (s, f i)
  | TFloat (s, i) -> TFloat (s, f i)
  | TAssign (x, i) -> TAssign (x, f i)
  | TIdent (s, i) -> TIdent (s, f i)
  | TIdent_Typedef (s, i) -> TIdent_Typedef (s, f i)
  | TInt pi -> TInt (Parsed_int.map_tok f pi)
  (* cppext: *)
  | TDefine i1 -> TDefine (f i1)
  | TUndef (s, i1) -> TUndef (s, f i1)
  | TCppDirectiveOther i1 -> TCppDirectiveOther (f i1)
  | TInclude (includes, filename, i1) -> TInclude (includes, filename, f i1)
  | TCppEscapedNewline i1 -> TCppEscapedNewline (f i1)
  | TCommentNewline_DefineEndOfMacro i1 ->
      TCommentNewline_DefineEndOfMacro (f i1)
  | TOPar_Define i1 -> TOPar_Define (f i1)
  | TIdent_Define (s, i) -> TIdent_Define (s, f i)
  | TDefParamVariadic (s, i1) -> TDefParamVariadic (s, f i1)
  | TOBrace_DefineInit i1 -> TOBrace_DefineInit (f i1)
  | TUnknown i -> TUnknown (f i)
  | TIdent_MacroStmt i -> TIdent_MacroStmt (f i)
  | TIdent_MacroString i -> TIdent_MacroString (f i)
  | TIdent_MacroIterator (s, i) -> TIdent_MacroIterator (s, f i)
  | TIdent_MacroDecl (s, i) -> TIdent_MacroDecl (s, f i)
  | Tconst_MacroDeclConst i -> Tconst_MacroDeclConst (f i)
  | Tfinal i -> Tfinal (f i)
  | Toverride i -> Toverride (f i)
  (*  | TMacroTop          (s,i) -> TMacroTop             (s,f i) *)
  | TCPar_EOL i -> TCPar_EOL (f i)
  | TAny_Action i -> TAny_Action (f i)
  | TComment i -> TComment (f i)
  | TCommentSpace i -> TCommentSpace (f i)
  | TCommentNewline i -> TCommentNewline (f i)
  | TComment_Pp (cppkind, i) -> TComment_Pp (cppkind, f i)
  | TComment_Cpp (cppkind, i) -> TComment_Cpp (cppkind, f i)
  | TIfdef i -> TIfdef (f i)
  | TIfdefelse i -> TIfdefelse (f i)
  | TIfdefelif i -> TIfdefelif (f i)
  | TEndif i -> TEndif (f i)
  | TIfdefBool (b, i) -> TIfdefBool (b, f i)
  | TIfdefMisc (b, i) -> TIfdefMisc (b, f i)
  | TIfdefVersion (b, i) -> TIfdefVersion (b, f i)
  | TOPar i -> TOPar (f i)
  | TOPar_CplusplusInit i -> TOPar_CplusplusInit (f i)
  | TCPar i -> TCPar (f i)
  | TOBrace i -> TOBrace (f i)
  | TCBrace i -> TCBrace (f i)
  | TOCro i -> TOCro (f i)
  | TCCro i -> TCCro (f i)
  | TDot i -> TDot (f i)
  | TComma i -> TComma (f i)
  | TPtrOp i -> TPtrOp (f i)
  | TInc i -> TInc (f i)
  | TDec i -> TDec (f i)
  | TEq i -> TEq (f i)
  | TWhy i -> TWhy (f i)
  | TTilde i -> TTilde (f i)
  | TBang i -> TBang (f i)
  | TEllipsis i -> TEllipsis (f i)
  | TCol i -> TCol (f i)
  | TPtVirg i -> TPtVirg (f i)
  | TOrLog i -> TOrLog (f i)
  | TAndLog i -> TAndLog (f i)
  | TOr i -> TOr (f i)
  | TXor i -> TXor (f i)
  | TAnd i -> TAnd (f i)
  | TEqEq i -> TEqEq (f i)
  | TNotEq i -> TNotEq (f i)
  | TInf i -> TInf (f i)
  | TSup i -> TSup (f i)
  | TInfEq i -> TInfEq (f i)
  | TSupEq i -> TSupEq (f i)
  | TShl i -> TShl (f i)
  | TShr i -> TShr (f i)
  | TPlus i -> TPlus (f i)
  | TMinus i -> TMinus (f i)
  | TMul i -> TMul (f i)
  | TDiv i -> TDiv (f i)
  | TMod i -> TMod (f i)
  | Tchar i -> Tchar (f i)
  | Tshort i -> Tshort (f i)
  | Tint i -> Tint (f i)
  | Tdouble i -> Tdouble (f i)
  | Tfloat i -> Tfloat (f i)
  | Tlong i -> Tlong (f i)
  | Tunsigned i -> Tunsigned (f i)
  | Tsigned i -> Tsigned (f i)
  | Tvoid i -> Tvoid (f i)
  | Tauto i -> Tauto (f i)
  | Tregister i -> Tregister (f i)
  | Textern i -> Textern (f i)
  | Tstatic i -> Tstatic (f i)
  | Tconst i -> Tconst (f i)
  | Tvolatile i -> Tvolatile (f i)
  | Trestrict i -> Trestrict (f i)
  | Tstruct i -> Tstruct (f i)
  | Tenum i -> Tenum (f i)
  | Ttypedef i -> Ttypedef (f i)
  | Tunion i -> Tunion (f i)
  | Tbreak i -> Tbreak (f i)
  | Telse i -> Telse (f i)
  | Tswitch i -> Tswitch (f i)
  | Tcase i -> Tcase (f i)
  | Tcontinue i -> Tcontinue (f i)
  | Tfor i -> Tfor (f i)
  | Tdo i -> Tdo (f i)
  | Tif i -> Tif (f i)
  | Twhile i -> Twhile (f i)
  | Treturn i -> Treturn (f i)
  | Tgoto i -> Tgoto (f i)
  | Tdefault i -> Tdefault (f i)
  | Tsizeof i -> Tsizeof (f i)
  | Tasm i -> Tasm (f i)
  | Tattribute i -> Tattribute (f i)
  | Tinline i -> Tinline (f i)
  | Ttypeof i -> Ttypeof (f i)
  | Tclass i -> Tclass (f i)
  | Tthis i -> Tthis (f i)
  | Tnew i -> Tnew (f i)
  | Tdelete i -> Tdelete (f i)
  | Ttemplate i -> Ttemplate (f i)
  | Ttypeid i -> Ttypeid (f i)
  | Ttypename i -> Ttypename (f i)
  | Tcatch i -> Tcatch (f i)
  | Ttry i -> Ttry (f i)
  | Tthrow i -> Tthrow (f i)
  | Toperator i -> Toperator (f i)
  | Tpublic i -> Tpublic (f i)
  | Tprivate i -> Tprivate (f i)
  | Tprotected i -> Tprotected (f i)
  | Tfriend i -> Tfriend (f i)
  | Tvirtual i -> Tvirtual (f i)
  | Tnamespace i -> Tnamespace (f i)
  | Tusing i -> Tusing (f i)
  | Tbool i -> Tbool (f i)
  | Ttrue i -> Ttrue (f i)
  | Tfalse i -> Tfalse (f i)
  | Twchar_t i -> Twchar_t (f i)
  | Tconst_cast i -> Tconst_cast (f i)
  | Tdynamic_cast i -> Tdynamic_cast (f i)
  | Tstatic_cast i -> Tstatic_cast (f i)
  | Treinterpret_cast i -> Treinterpret_cast (f i)
  | Texplicit i -> Texplicit (f i)
  | Tmutable i -> Tmutable (f i)
  | TColCol i -> TColCol (f i)
  | TColCol_BeforeTypedef i -> TColCol_BeforeTypedef (f i)
  | TPtrOpStar i -> TPtrOpStar (f i)
  | TDotStar i -> TDotStar (f i)
  | TIdent_ClassnameInQualifier (s, i) -> TIdent_ClassnameInQualifier (s, f i)
  | TIdent_ClassnameInQualifier_BeforeTypedef (s, i) ->
      TIdent_ClassnameInQualifier_BeforeTypedef (s, f i)
  | TIdent_Templatename (s, i) -> TIdent_Templatename (s, f i)
  | TIdent_Constructor (s, i) -> TIdent_Constructor (s, f i)
  | TIdent_TypedefConstr (s, i) -> TIdent_TypedefConstr (s, f i)
  | TIdent_TemplatenameInQualifier (s, i) ->
      TIdent_TemplatenameInQualifier (s, f i)
  | TIdent_TemplatenameInQualifier_BeforeTypedef (s, i) ->
      TIdent_TemplatenameInQualifier_BeforeTypedef (s, f i)
  | TInf_Template i -> TInf_Template (f i)
  | TSup_Template i -> TSup_Template (f i)
  | TOCro_new i -> TOCro_new (f i)
  | TCCro_new i -> TCCro_new (f i)
  | TOCro_Lambda i -> TOCro_Lambda (f i)
  | TInt_ZeroVirtual i -> TInt_ZeroVirtual (f i)
  | Tchar_Constr i -> Tchar_Constr (f i)
  | Tint_Constr i -> Tint_Constr (f i)
  | Tfloat_Constr i -> Tfloat_Constr (f i)
  | Tdouble_Constr i -> Tdouble_Constr (f i)
  | Twchar_t_Constr i -> Twchar_t_Constr (f i)
  | Tshort_Constr i -> Tshort_Constr (f i)
  | Tlong_Constr i -> Tlong_Constr (f i)
  | Tbool_Constr i -> Tbool_Constr (f i)
  | Tsigned_Constr i -> Tsigned_Constr (f i)
  | Tunsigned_Constr i -> Tunsigned_Constr (f i)
  | EOF i -> EOF (f i)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  match !res with
  | Some x -> x
  | None -> Tok.unsafe_fake_tok "NOTOK"

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let line_of_tok tok =
  let info = info_of_tok tok in
  Tok.line_of_tok info
