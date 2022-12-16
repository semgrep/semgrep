/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The parser definition */

%{

open Asttypes
open Longident
open Parsetree
open Ast_helper
open Docstrings
open Docstrings.WithMenhir

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let ghost_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = true;
}

let mktyp ~loc ?attrs d = Typ.mk ~loc:(make_loc loc) ?attrs d
let mkpat ~loc d = Pat.mk ~loc:(make_loc loc) d
let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d
let mkmty ~loc ?attrs d = Mty.mk ~loc:(make_loc loc) ?attrs d
let mksig ~loc d = Sig.mk ~loc:(make_loc loc) d
let mkmod ~loc ?attrs d = Mod.mk ~loc:(make_loc loc) ?attrs d
let mkstr ~loc d = Str.mk ~loc:(make_loc loc) d
let mkclass ~loc ?attrs d = Cl.mk ~loc:(make_loc loc) ?attrs d
let mkcty ~loc ?attrs d = Cty.mk ~loc:(make_loc loc) ?attrs d

let pstr_typext (te, ext) =
  (Pstr_typext te, ext)
let pstr_primitive (vd, ext) =
  (Pstr_primitive vd, ext)
let pstr_type ((nr, ext), tys) =
  (Pstr_type (nr, tys), ext)
let pstr_exception (te, ext) =
  (Pstr_exception te, ext)
let pstr_include (body, ext) =
  (Pstr_include body, ext)
let pstr_recmodule (ext, bindings) =
  (Pstr_recmodule bindings, ext)

let psig_typext (te, ext) =
  (Psig_typext te, ext)
let psig_value (vd, ext) =
  (Psig_value vd, ext)
let psig_type ((nr, ext), tys) =
  (Psig_type (nr, tys), ext)
let psig_typesubst ((nr, ext), tys) =
  assert (nr = Recursive); (* see [no_nonrec_flag] *)
  (Psig_typesubst tys, ext)
let psig_exception (te, ext) =
  (Psig_exception te, ext)
let psig_include (body, ext) =
  (Psig_include body, ext)

let mkctf ~loc ?attrs ?docs d =
  Ctf.mk ~loc:(make_loc loc) ?attrs ?docs d
let mkcf ~loc ?attrs ?docs d =
  Cf.mk ~loc:(make_loc loc) ?attrs ?docs d

let mkrhs rhs loc = mkloc rhs (make_loc loc)
let ghrhs rhs loc = mkloc rhs (ghost_loc loc)

let push_loc x acc =
  if x.Location.loc_ghost
  then acc
  else x :: acc

let reloc_pat ~loc x =
  { x with ppat_loc = make_loc loc;
           ppat_loc_stack = push_loc x.ppat_loc x.ppat_loc_stack };;
let reloc_exp ~loc x =
  { x with pexp_loc = make_loc loc;
           pexp_loc_stack = push_loc x.pexp_loc x.pexp_loc_stack };;
let reloc_typ ~loc x =
  { x with ptyp_loc = make_loc loc;
           ptyp_loc_stack = push_loc x.ptyp_loc x.ptyp_loc_stack };;

let mkexpvar ~loc (name : string) =
  mkexp ~loc (Pexp_ident(mkrhs (Lident name) loc))

let mkoperator =
  mkexpvar

let mkpatvar ~loc name =
  mkpat ~loc (Ppat_var (mkrhs name loc))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp ~loc d = Exp.mk ~loc:(ghost_loc loc) d
let ghpat ~loc d = Pat.mk ~loc:(ghost_loc loc) d
let ghtyp ~loc d = Typ.mk ~loc:(ghost_loc loc) d
let ghloc ~loc d = { txt = d; loc = ghost_loc loc }
let ghstr ~loc d = Str.mk ~loc:(ghost_loc loc) d
let ghsig ~loc d = Sig.mk ~loc:(ghost_loc loc) d

let mkinfix arg1 op arg2 =
  Pexp_apply(op, [Nolabel, arg1; Nolabel, arg2])

let neg_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus ~oploc name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Pconst_integer (n,m)) ->
      Pexp_constant(Pconst_integer(neg_string n,m))
  | ("-" | "-."), Pexp_constant(Pconst_float (f, m)) ->
      Pexp_constant(Pconst_float(neg_string f, m))
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc ("~" ^ name), [Nolabel, arg])

let mkuplus ~oploc name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Pconst_integer _)
  | ("+" | "+."), Pexp_constant(Pconst_float _) -> desc
  | _ ->
      Pexp_apply(mkoperator ~loc:oploc ("~" ^ name), [Nolabel, arg])

(* TODO define an abstraction boundary between locations-as-pairs
   and locations-as-Location.t; it should be clear when we move from
   one world to the other *)

let mkexp_cons_desc consloc args =
  Pexp_construct(mkrhs (Lident "::") consloc, Some args)
let mkexp_cons ~loc consloc args =
  mkexp ~loc (mkexp_cons_desc consloc args)

let mkpat_cons_desc consloc args =
  Ppat_construct(mkrhs (Lident "::") consloc, Some args)
let mkpat_cons ~loc consloc args =
  mkpat ~loc (mkpat_cons_desc consloc args)

let ghexp_cons_desc consloc args =
  Pexp_construct(ghrhs (Lident "::") consloc, Some args)
let ghpat_cons_desc consloc args =
  Ppat_construct(ghrhs (Lident "::") consloc, Some args)

let rec mktailexp nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (Lident "[]") in
      Pexp_construct (nil, None), nilloc
  | e1 :: el ->
      let exp_el, el_loc = mktailexp nilloc el in
      let loc = (e1.pexp_loc.loc_start, snd el_loc) in
      let arg = ghexp ~loc (Pexp_tuple [e1; ghexp ~loc:el_loc exp_el]) in
      ghexp_cons_desc loc arg, loc

let rec mktailpat nilloc = let open Location in function
    [] ->
      let nil = ghloc ~loc:nilloc (Lident "[]") in
      Ppat_construct (nil, None), nilloc
  | p1 :: pl ->
      let pat_pl, el_loc = mktailpat nilloc pl in
      let loc = (p1.ppat_loc.loc_start, snd el_loc) in
      let arg = ghpat ~loc (Ppat_tuple [p1; ghpat ~loc:el_loc pat_pl]) in
      ghpat_cons_desc loc arg, loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint ~loc e (t1, t2) =
  match t1, t2 with
  | Some t, None -> ghexp ~loc (Pexp_constraint(e, t))
  | _, Some t -> ghexp ~loc (Pexp_coerce(e, t1, t))
  | None, None -> assert false

let mkexp_opt_constraint ~loc e = function
  | None -> e
  | Some constraint_ -> mkexp_constraint ~loc e constraint_

let mkpat_opt_constraint ~loc p = function
  | None -> p
  | Some typ -> mkpat ~loc (Ppat_constraint(p, typ))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_loc closing_name closing_loc =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(make_loc opening_loc, opening_name,
                                           make_loc closing_loc, closing_name)))

let expecting loc nonterm =
    raise Syntaxerr.(Error(Expecting(make_loc loc, nonterm)))

let not_expecting loc nonterm =
    raise Syntaxerr.(Error(Not_expecting(make_loc loc, nonterm)))

let dotop ~left ~right ~assign ~ext ~multi =
  let assign = if assign then "<-" else "" in
  let mid = if multi then ";.." else "" in
  String.concat "" ["."; ext; left; mid; right; assign]
let paren = "(",")"
let brace = "{", "}"
let bracket = "[", "]"
let lident x =  Lident x
let ldot x y = Ldot(x,y)
let dotop_fun ~loc dotop =
  (* We could use ghexp here, but sticking to mkexp for parser.mly
     compatibility. TODO improve parser.mly *)
  mkexp ~loc (Pexp_ident (ghloc ~loc dotop))

let array_function ~loc str name =
  ghloc ~loc (Ldot(Lident str,
                   (if !Clflags.unsafe then "unsafe_" ^ name else name)))

let array_get_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "Array" "get"))
let string_get_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "String" "get"))

let array_set_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "Array" "set"))
let string_set_fun ~loc =
  ghexp ~loc (Pexp_ident(array_function ~loc "String" "set"))

let multi_indices ~loc = function
  | [a] -> false, a
  | l -> true, mkexp ~loc (Pexp_array l)

let index_get ~loc get_fun array index =
  let args = [Nolabel, array; Nolabel, index] in
   mkexp ~loc (Pexp_apply(get_fun, args))

let index_set ~loc set_fun array index value =
  let args = [Nolabel, array; Nolabel, index; Nolabel, value] in
   mkexp ~loc (Pexp_apply(set_fun, args))

let array_get ~loc = index_get ~loc (array_get_fun ~loc)
let string_get ~loc = index_get ~loc (string_get_fun ~loc)
let dotop_get ~loc path (left,right) ext array index =
  let multi, index = multi_indices ~loc index in
  index_get ~loc
    (dotop_fun ~loc (path @@ dotop ~left ~right ~ext ~multi ~assign:false))
    array index

let array_set ~loc = index_set ~loc (array_set_fun ~loc)
let string_set ~loc = index_set ~loc (string_set_fun ~loc)
let dotop_set ~loc path (left,right) ext array index value=
  let multi, index = multi_indices ~loc index in
  index_set ~loc
    (dotop_fun ~loc (path @@ dotop ~left ~right ~ext ~multi ~assign:true))
    array index value


let bigarray_function ~loc str name =
  ghloc ~loc (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get ~loc arr arg =
  let mkexp, ghexp = mkexp ~loc, ghexp ~loc in
  let bigarray_function = bigarray_function ~loc in
  let get = if !Clflags.unsafe then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       [Nolabel, arr; Nolabel, c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, c2; Nolabel, c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       [Nolabel, arr; Nolabel, ghexp(Pexp_array coords)]))

let bigarray_set ~loc arr arg newval =
  let mkexp, ghexp = mkexp ~loc, ghexp ~loc in
  let bigarray_function = bigarray_function ~loc in
  let set = if !Clflags.unsafe then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       [Nolabel, arr; Nolabel, c1; Nolabel, newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       [Nolabel, arr; Nolabel, c1;
                        Nolabel, c2; Nolabel, newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       [Nolabel, arr; Nolabel, c1;
                        Nolabel, c2; Nolabel, c3; Nolabel, newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       [Nolabel, arr;
                        Nolabel, ghexp(Pexp_array coords);
                        Nolabel, newval]))

let lapply ~loc p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(
                  Syntaxerr.Applicative_path (make_loc loc)))

let exp_of_longident ~loc lid =
  mkexp ~loc (Pexp_ident {lid with txt = Lident(Longident.last lid.txt)})

(* [loc_map] could be [Location.map]. *)
let loc_map (f : 'a -> 'b) (x : 'a Location.loc) : 'b Location.loc =
  { x with txt = f x.txt }

let loc_last (id : Longident.t Location.loc) : string Location.loc =
  loc_map Longident.last id

let loc_lident (id : string Location.loc) : Longident.t Location.loc =
  loc_map (fun x -> Lident x) id

let exp_of_label ~loc lbl =
  mkexp ~loc (Pexp_ident (loc_lident lbl))

let pat_of_label ~loc lbl =
  mkpat ~loc (Ppat_var (loc_last lbl))

let mk_newtypes ~loc newtypes exp =
  let mkexp = mkexp ~loc in
  List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
    newtypes exp

let wrap_type_annotation ~loc newtypes core_type body =
  let mkexp, ghtyp = mkexp ~loc, ghtyp ~loc in
  let mk_newtypes = mk_newtypes ~loc in
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp = mk_newtypes newtypes exp in
  (exp, ghtyp(Ptyp_poly(newtypes, Typ.varify_constructors newtypes core_type)))

let wrap_exp_attrs ~loc body (ext, attrs) =
  let ghexp = ghexp ~loc in
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs ~loc d attrs =
  wrap_exp_attrs ~loc (mkexp ~loc d) attrs

let wrap_typ_attrs ~loc typ (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let typ = {typ with ptyp_attributes = attrs @ typ.ptyp_attributes} in
  match ext with
  | None -> typ
  | Some id -> ghtyp ~loc (Ptyp_extension (id, PTyp typ))

let wrap_pat_attrs ~loc pat (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let pat = {pat with ppat_attributes = attrs @ pat.ppat_attributes} in
  match ext with
  | None -> pat
  | Some id -> ghpat ~loc (Ppat_extension (id, PPat (pat, None)))

let mkpat_attrs ~loc d attrs =
  wrap_pat_attrs ~loc (mkpat ~loc d) attrs

let wrap_class_attrs ~loc:_ body attrs =
  {body with pcl_attributes = attrs @ body.pcl_attributes}
let wrap_mod_attrs ~loc:_ attrs body =
  {body with pmod_attributes = attrs @ body.pmod_attributes}
let wrap_mty_attrs ~loc:_ attrs body =
  {body with pmty_attributes = attrs @ body.pmty_attributes}

let wrap_str_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghstr ~loc (Pstr_extension ((id, PStr [body]), []))

let wrap_mkstr_ext ~loc (item, ext) =
  wrap_str_ext ~loc (mkstr ~loc item) ext

let wrap_sig_ext ~loc body ext =
  match ext with
  | None -> body
  | Some id -> ghsig ~loc (Psig_extension ((id, PSig [body]), []))

let wrap_mksig_ext ~loc (item, ext) =
  wrap_sig_ext ~loc (mksig ~loc item) ext

let mk_quotedext ~loc (id, idloc, str, strloc, delim) =
  let exp_id = mkloc id idloc in
  let e = ghexp ~loc (Pexp_constant (Pconst_string (str, strloc, delim))) in
  (exp_id, PStr [mkstrexp e []])

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)
let text_def pos = [Ptop_def (Str.text (rhs_text pos))]

let extra_text startpos endpos text items =
  match items with
  | [] ->
      let post = rhs_post_text endpos in
      let post_extras = rhs_post_extra_text endpos in
      text post @ text post_extras
  | _ :: _ ->
      let pre_extras = rhs_pre_extra_text startpos in
      let post_extras = rhs_post_extra_text endpos in
        text pre_extras @ items @ text post_extras

let extra_str p1 p2 items = extra_text p1 p2 Str.text items
let extra_sig p1 p2 items = extra_text p1 p2 Sig.text items
let extra_cstr p1 p2 items = extra_text p1 p2 Cf.text items
let extra_csig p1 p2 items = extra_text p1 p2 Ctf.text  items
let extra_def p1 p2 items =
  extra_text p1 p2 (fun txt -> [Ptop_def (Str.text txt)]) items

let extra_rhs_core_type ct ~pos =
  let docs = rhs_info pos in
  { ct with ptyp_attributes = add_info_attrs docs ct.ptyp_attributes }

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option;
    lbs_loc: Location.t }

let mklb first ~loc (p, e) attrs =
  {
    lb_pattern = p;
    lb_expression = e;
    lb_attributes = attrs;
    lb_docs = symbol_docs_lazy loc;
    lb_text = (if first then empty_text_lazy
               else symbol_text_lazy (fst loc));
    lb_loc = make_loc loc;
  }

let mklbs ~loc ext rf lb =
  {
    lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = ext ;
    lbs_loc = make_loc loc;
  }

let addlb lbs lb =
  { lbs with lbs_bindings = lb :: lbs.lbs_bindings }

let val_of_let_bindings ~loc lbs =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
  let str = mkstr ~loc (Pstr_value(lbs.lbs_rec, List.rev bindings)) in
  match lbs.lbs_extension with
  | None -> str
  | Some id -> ghstr ~loc (Pstr_extension((id, PStr [str]), []))

let expr_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    mkexp_attrs ~loc (Pexp_let(lbs.lbs_rec, List.rev bindings, body))
      (lbs.lbs_extension, [])

let class_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    (* Our use of let_bindings(no_ext) guarantees the following: *)
    assert (lbs.lbs_extension = None);
    mkclass ~loc (Pcl_let (lbs.lbs_rec, List.rev bindings, body))

(* Alternatively, we could keep the generic module type in the Parsetree
   and extract the package type during type-checking. In that case,
   the assertions below should be turned into explicit checks. *)
let package_type_of_module_type pmty =
  let err loc s =
    raise (Syntaxerr.Error (Syntaxerr.Invalid_package_type (loc, s)))
  in
  let map_cstr = function
    | Pwith_type (lid, ptyp) ->
        let loc = ptyp.ptype_loc in
        if ptyp.ptype_params <> [] then
          err loc "parametrized types are not supported";
        if ptyp.ptype_cstrs <> [] then
          err loc "constrained types are not supported";
        if ptyp.ptype_private <> Public then
          err loc "private types are not supported";

        (* restrictions below are checked by the 'with_constraint' rule *)
        assert (ptyp.ptype_kind = Ptype_abstract);
        assert (ptyp.ptype_attributes = []);
        let ty =
          match ptyp.ptype_manifest with
          | Some ty -> ty
          | None -> assert false
        in
        (lid, ty)
    | _ ->
        err pmty.pmty_loc "only 'with type t =' constraints are supported"
  in
  match pmty with
  | {pmty_desc = Pmty_ident lid} -> (lid, [], pmty.pmty_attributes)
  | {pmty_desc = Pmty_with({pmty_desc = Pmty_ident lid}, cstrs)} ->
      (lid, List.map map_cstr cstrs, pmty.pmty_attributes)
  | _ ->
      err pmty.pmty_loc
        "only module type identifier and 'with type' constraints are supported"

let mk_directive_arg ~loc k =
  { pdira_desc = k;
    pdira_loc = make_loc loc;
  }

let mk_directive ~loc name arg =
  Ptop_dir {
      pdir_name = name;
      pdir_arg = arg;
      pdir_loc = make_loc loc;
    }

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string * char option> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <string> DOTOP
%token <string> LETOP
%token <string> ANDOP
%token INHERIT
%token INITIALIZER
%token <string * char option> INT
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token NEW
%token NONREC
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token HASH
%token <string> HASHOP
%token SIG
%token STAR
%token <string * Location.t * string option> STRING
%token
  <string * Location.t * string * Location.t * string option> QUOTED_STRING_EXPR
%token
  <string * Location.t * string * Location.t * string option> QUOTED_STRING_ITEM
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT
%token <Docstrings.docstring> DOCSTRING

%token EOL

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* function_type (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_HASH
%nonassoc HASH                         /* simple_expr/toplevel_directive */
%left     HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTOP
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT QUOTED_STRING_EXPR


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%start parse_constr_longident
%type <Longident.t> parse_constr_longident
%start parse_val_longident
%type <Longident.t> parse_val_longident
%start parse_mty_longident
%type <Longident.t> parse_mty_longident
%start parse_mod_ext_longident
%type <Longident.t> parse_mod_ext_longident
%start parse_mod_longident
%type <Longident.t> parse_mod_longident
%start parse_any_longident
%type <Longident.t> parse_any_longident
%%

/* macros */
%inline extra_str(symb): symb { extra_str $startpos $endpos $1 };
%inline extra_sig(symb): symb { extra_sig $startpos $endpos $1 };
%inline extra_cstr(symb): symb { extra_cstr $startpos $endpos $1 };
%inline extra_csig(symb): symb { extra_csig $startpos $endpos $1 };
%inline extra_def(symb): symb { extra_def $startpos $endpos $1 };
%inline extra_text(symb): symb { extra_text $startpos $endpos $1 };
%inline extra_rhs(symb): symb { extra_rhs_core_type $1 ~pos:$endpos($1) };
%inline mkrhs(symb): symb
    { mkrhs $1 $sloc }
;

%inline text_str(symb): symb
  { text_str $startpos @ [$1] }
%inline text_str_SEMISEMI: SEMISEMI
  { text_str $startpos }
%inline text_sig(symb): symb
  { text_sig $startpos @ [$1] }
%inline text_sig_SEMISEMI: SEMISEMI
  { text_sig $startpos }
%inline text_def(symb): symb
  { text_def $startpos @ [$1] }
%inline top_def(symb): symb
  { Ptop_def [$1] }
%inline text_cstr(symb): symb
  { text_cstr $startpos @ [$1] }
%inline text_csig(symb): symb
  { text_csig $startpos @ [$1] }

(* Using this %inline definition means that we do not control precisely
   when [mark_rhs_docs] is called, but I don't think this matters. *)
%inline mark_rhs_docs(symb): symb
  { mark_rhs_docs $startpos $endpos;
    $1 }

%inline op(symb): symb
   { mkoperator ~loc:$sloc $1 }

%inline mkloc(symb): symb
    { mkloc $1 (make_loc $sloc) }

%inline mkexp(symb): symb
    { mkexp ~loc:$sloc $1 }
%inline mkpat(symb): symb
    { mkpat ~loc:$sloc $1 }
%inline mktyp(symb): symb
    { mktyp ~loc:$sloc $1 }
%inline mkstr(symb): symb
    { mkstr ~loc:$sloc $1 }
%inline mksig(symb): symb
    { mksig ~loc:$sloc $1 }
%inline mkmod(symb): symb
    { mkmod ~loc:$sloc $1 }
%inline mkmty(symb): symb
    { mkmty ~loc:$sloc $1 }
%inline mkcty(symb): symb
    { mkcty ~loc:$sloc $1 }
%inline mkctf(symb): symb
    { mkctf ~loc:$sloc $1 }
%inline mkcf(symb): symb
    { mkcf ~loc:$sloc $1 }
%inline mkclass(symb): symb
    { mkclass ~loc:$sloc $1 }

%inline wrap_mkstr_ext(symb): symb
    { wrap_mkstr_ext ~loc:$sloc $1 }
%inline wrap_mksig_ext(symb): symb
    { wrap_mksig_ext ~loc:$sloc $1 }

%inline mk_directive_arg(symb): symb
    { mk_directive_arg ~loc:$sloc $1 }

/* Generic definitions */

(* [iloption(X)] recognizes either nothing or [X]. Assuming [X] produces
   an OCaml list, it produces an OCaml list, too. *)

%inline iloption(X):
  /* nothing */
    { [] }
| x = X
    { x }

(* [llist(X)] recognizes a possibly empty list of [X]s. It is left-recursive. *)

reversed_llist(X):
  /* empty */
    { [] }
| xs = reversed_llist(X) x = X
    { x :: xs }

%inline llist(X):
  xs = rev(reversed_llist(X))
    { xs }

(* [reversed_nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces
   an OCaml list in reverse order -- that is, the last element in the input text
   appears first in this list. Its definition is left-recursive. *)

reversed_nonempty_llist(X):
  x = X
    { [ x ] }
| xs = reversed_nonempty_llist(X) x = X
    { x :: xs }

(* [nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces an OCaml
   list in direct order -- that is, the first element in the input text appears
   first in this list. *)

%inline nonempty_llist(X):
  xs = rev(reversed_nonempty_llist(X))
    { xs }

(* [reversed_separated_nonempty_llist(separator, X)] recognizes a nonempty list
   of [X]s, separated with [separator]s, and produces an OCaml list in reverse
   order -- that is, the last element in the input text appears first in this
   list. Its definition is left-recursive. *)

(* [inline_reversed_separated_nonempty_llist(separator, X)] is semantically
   equivalent to [reversed_separated_nonempty_llist(separator, X)], but is
   marked %inline, which means that the case of a list of length one and
   the case of a list of length more than one will be distinguished at the
   use site, and will give rise there to two productions. This can be used
   to avoid certain conflicts. *)

%inline inline_reversed_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reversed_separated_nonempty_llist(separator, X)
  separator
  x = X
    { x :: xs }

reversed_separated_nonempty_llist(separator, X):
  xs = inline_reversed_separated_nonempty_llist(separator, X)
    { xs }

(* [separated_nonempty_llist(separator, X)] recognizes a nonempty list of [X]s,
   separated with [separator]s, and produces an OCaml list in direct order --
   that is, the first element in the input text appears first in this list. *)

%inline separated_nonempty_llist(separator, X):
  xs = rev(reversed_separated_nonempty_llist(separator, X))
    { xs }

%inline inline_separated_nonempty_llist(separator, X):
  xs = rev(inline_reversed_separated_nonempty_llist(separator, X))
    { xs }

(* [reversed_separated_nontrivial_llist(separator, X)] recognizes a list of at
   least two [X]s, separated with [separator]s, and produces an OCaml list in
   reverse order -- that is, the last element in the input text appears first
   in this list. Its definition is left-recursive. *)

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

(* [separated_nontrivial_llist(separator, X)] recognizes a list of at least
   two [X]s, separated with [separator]s, and produces an OCaml list in direct
   order -- that is, the first element in the input text appears first in this
   list. *)

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }

(* [separated_or_terminated_nonempty_list(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally terminated with a
   final [delimiter]. Its definition is right-recursive. *)

separated_or_terminated_nonempty_list(delimiter, X):
  x = X ioption(delimiter)
    { [x] }
| x = X
  delimiter
  xs = separated_or_terminated_nonempty_list(delimiter, X)
    { x :: xs }

(* [reversed_preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a
   nonempty list of [X]s, separated with [delimiter]s, and optionally preceded
   with a leading [delimiter]. It produces an OCaml list in reverse order. Its
   definition is left-recursive. *)

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
  ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

(* [preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

(* [bar_llist(X)] recognizes a nonempty list of [X]'s, separated with BARs,
   with an optional leading BAR. We assume that [X] is itself parameterized
   with an opening symbol, which can be [epsilon] or [BAR]. *)

(* This construction may seem needlessly complicated: one might think that
   using [preceded_or_separated_nonempty_llist(BAR, X)], where [X] is *not*
   itself parameterized, would be sufficient. Indeed, this simpler approach
   would recognize the same language. However, the two approaches differ in
   the footprint of [X]. We want the start location of [X] to include [BAR]
   when present. In the future, we might consider switching to the simpler
   definition, at the cost of producing slightly different locations. TODO *)

reversed_bar_llist(X):
    (* An [X] without a leading BAR. *)
    x = X(epsilon)
      { [x] }
  | (* An [X] with a leading BAR. *)
    x = X(BAR)
      { [x] }
  | (* An initial list, followed with a BAR and an [X]. *)
    xs = reversed_bar_llist(X)
    x = X(BAR)
      { x :: xs }

%inline bar_llist(X):
  xs = reversed_bar_llist(X)
    { List.rev xs }

(* [xlist(A, B)] recognizes [AB*]. We assume that the semantic value for [A]
   is a pair [x, b], while the semantic value for [B*] is a list [bs].
   We return the pair [x, b :: bs]. *)

%inline xlist(A, B):
  a = A bs = B*
    { let (x, b) = a in x, b :: bs }

(* [listx(delimiter, X, Y)] recognizes a nonempty list of [X]s, optionally
   followed with a [Y], separated-or-terminated with [delimiter]s. The
   semantic value is a pair of a list of [X]s and an optional [Y]. *)

listx(delimiter, X, Y):
| x = X ioption(delimiter)
    { [x], None }
| x = X delimiter y = Y delimiter?
    { [x], Some y }
| x = X
  delimiter
  tail = listx(delimiter, X, Y)
    { let xs, y = tail in
      x :: xs, y }

(* -------------------------------------------------------------------------- *)

(* Entry points. *)

(* An .ml file. *)
implementation:
  structure EOF
    { $1 }
;

(* An .mli file. *)
interface:
  signature EOF
    { $1 }
;

(* A toplevel phrase. *)
toplevel_phrase:
  (* An expression with attributes, ended by a double semicolon. *)
  extra_str(text_str(str_exp))
  SEMISEMI
    { Ptop_def $1 }
| (* A list of structure items, ended by a double semicolon. *)
  extra_str(flatten(text_str(structure_item)*))
  SEMISEMI
    { Ptop_def $1 }
| (* A directive, ended by a double semicolon. *)
  toplevel_directive
  SEMISEMI
    { $1 }
| (* End of input. *)
  EOF
    { raise End_of_file }
;

(* An .ml file that is read by #use. *)
use_file:
  (* An optional standalone expression,
     followed with a series of elements,
     followed with EOF. *)
  extra_def(append(
    optional_use_file_standalone_expression,
    flatten(use_file_element*)
  ))
  EOF
    { $1 }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_use_file_standalone_expression:
  iloption(text_def(top_def(str_exp)))
    { $1 }
;

(* An element in a #used file is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item;
   - a toplevel directive.
 *)
%inline use_file_element:
  preceded(SEMISEMI, optional_use_file_standalone_expression)
| text_def(top_def(structure_item))
| text_def(mark_rhs_docs(toplevel_directive))
      { $1 }
;

parse_core_type:
  core_type EOF
    { $1 }
;

parse_expression:
  seq_expr EOF
    { $1 }
;

parse_pattern:
  pattern EOF
    { $1 }
;

parse_mty_longident:
  mty_longident EOF
    { $1 }
;

parse_val_longident:
  val_longident EOF
    { $1 }
;

parse_constr_longident:
  constr_longident EOF
    { $1 }
;

parse_mod_ext_longident:
  mod_ext_longident EOF
    { $1 }
;

parse_mod_longident:
  mod_longident EOF
    { $1 }
;

parse_any_longident:
  any_longident EOF
    { $1 }
;
(* -------------------------------------------------------------------------- *)

(* Functor arguments appear in module expressions and module types. *)

%inline functor_args:
  reversed_nonempty_llist(functor_arg)
    { $1 }
    (* Produce a reversed list on purpose;
       later processed using [fold_left]. *)
;

functor_arg:
    (* An anonymous and untyped argument. *)
    LPAREN RPAREN
      { Unit }
  | (* An argument accompanied with an explicit type. *)
    LPAREN x = mkrhs(module_name) COLON mty = module_type RPAREN
      { Named (x, mty) }
;

module_name:
    (* A named argument. *)
    x = UIDENT
      { Some x }
  | (* An anonymous argument. *)
    UNDERSCORE
      { None }
;

(* -------------------------------------------------------------------------- *)

(* Module expressions. *)

(* The syntax of module expressions is not properly stratified. The cases of
   functors, functor applications, and attributes interact and cause conflicts,
   which are resolved by precedence declarations. This is concise but fragile.
   Perhaps in the future an explicit stratification could be used. *)

module_expr:
  | STRUCT attrs = attributes s = structure END
      { mkmod ~loc:$sloc ~attrs (Pmod_structure s) }
  | STRUCT attributes structure error
      { unclosed "struct" $loc($1) "end" $loc($4) }
  | FUNCTOR attrs = attributes args = functor_args MINUSGREATER me = module_expr
      { wrap_mod_attrs ~loc:$sloc attrs (
          List.fold_left (fun acc arg ->
            mkmod ~loc:$sloc (Pmod_functor (arg, acc))
          ) me args
        ) }
  | me = paren_module_expr
      { me }
  | me = module_expr attr = attribute
      { Mod.attr me attr }
  | mkmod(
      (* A module identifier. *)
      x = mkrhs(mod_longident)
        { Pmod_ident x }
    | (* In a functor application, the actual argument must be parenthesized. *)
      me1 = module_expr me2 = paren_module_expr
        { Pmod_apply(me1, me2) }
    | (* Application to unit is sugar for application to an empty structure. *)
      me1 = module_expr LPAREN RPAREN
        { (* TODO review mkmod location *)
          Pmod_apply(me1, mkmod ~loc:$sloc (Pmod_structure [])) }
    | (* An extension. *)
      ex = extension
        { Pmod_extension ex }
    )
    { $1 }
;

(* A parenthesized module expression is a module expression that begins
   and ends with parentheses. *)

paren_module_expr:
    (* A module expression annotated with a module type. *)
    LPAREN me = module_expr COLON mty = module_type RPAREN
      { mkmod ~loc:$sloc (Pmod_constraint(me, mty)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | (* A module expression within parentheses. *)
    LPAREN me = module_expr RPAREN
      { me (* TODO consider reloc *) }
  | LPAREN module_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | (* A core language expression that produces a first-class module.
       This expression can be annotated in various ways. *)
    LPAREN VAL attrs = attributes e = expr_colon_package_type RPAREN
      { mkmod ~loc:$sloc ~attrs (Pmod_unpack e) }
  | LPAREN VAL attributes expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr COLONGREATER error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr error
      { unclosed "(" $loc($1) ")" $loc($5) }
;

(* The various ways of annotating a core language expression that
   produces a first-class module that we wish to unpack. *)
%inline expr_colon_package_type:
    e = expr
      { e }
  | e = expr COLON ty = package_type
      { ghexp ~loc:$loc (Pexp_constraint (e, ty)) }
  | e = expr COLON ty1 = package_type COLONGREATER ty2 = package_type
      { ghexp ~loc:$loc (Pexp_coerce (e, Some ty1, ty2)) }
  | e = expr COLONGREATER ty2 = package_type
      { ghexp ~loc:$loc (Pexp_coerce (e, None, ty2)) }
;

(* A structure, which appears between STRUCT and END (among other places),
   begins with an optional standalone expression, and continues with a list
   of structure elements. *)
structure:
  extra_str(append(
    optional_structure_standalone_expression,
    flatten(structure_element*)
  ))
  { $1 }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_structure_standalone_expression:
  items = iloption(mark_rhs_docs(text_str(str_exp)))
    { items }
;

(* An expression with attributes, wrapped as a structure item. *)
%inline str_exp:
  e = seq_expr
  attrs = post_item_attributes
    { mkstrexp e attrs }
;

(* A structure element is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item. *)
%inline structure_element:
    append(text_str_SEMISEMI, optional_structure_standalone_expression)
  | text_str(structure_item)
      { $1 }
;

(* A structure item. *)
structure_item:
    let_bindings(ext)
      { val_of_let_bindings ~loc:$sloc $1 }
  | mkstr(
      item_extension post_item_attributes
        { let docs = symbol_docs $sloc in
          Pstr_extension ($1, add_docs_attrs docs $2) }
    | floating_attribute
        { Pstr_attribute $1 }
    )
  | wrap_mkstr_ext(
      primitive_declaration
        { pstr_primitive $1 }
    | value_description
        { pstr_primitive $1 }
    | type_declarations
        { pstr_type $1 }
    | str_type_extension
        { pstr_typext $1 }
    | str_exception_declaration
        { pstr_exception $1 }
    | module_binding
        { $1 }
    | rec_module_bindings
        { pstr_recmodule $1 }
    | module_type_declaration
        { let (body, ext) = $1 in (Pstr_modtype body, ext) }
    | open_declaration
        { let (body, ext) = $1 in (Pstr_open body, ext) }
    | class_declarations
        { let (ext, l) = $1 in (Pstr_class l, ext) }
    | class_type_declarations
        { let (ext, l) = $1 in (Pstr_class_type l, ext) }
    | include_statement(module_expr)
        { pstr_include $1 }
    )
    { $1 }
;

(* A single module binding. *)
%inline module_binding:
  MODULE
  ext = ext attrs1 = attributes
  name = mkrhs(module_name)
  body = module_binding_body
  attrs2 = post_item_attributes
    { let docs = symbol_docs $sloc in
      let loc = make_loc $sloc in
      let attrs = attrs1 @ attrs2 in
      let body = Mb.mk name body ~attrs ~loc ~docs in
      Pstr_module body, ext }
;

(* The body (right-hand side) of a module binding. *)
module_binding_body:
    EQUAL me = module_expr
      { me }
  | mkmod(
      COLON mty = module_type EQUAL me = module_expr
        { Pmod_constraint(me, mty) }
    | arg = functor_arg body = module_binding_body
        { Pmod_functor(arg, body) }
  ) { $1 }
;

(* A group of recursive module bindings. *)
%inline rec_module_bindings:
  xlist(rec_module_binding, and_module_binding)
    { $1 }
;

(* The first binding in a group of recursive module bindings. *)
%inline rec_module_binding:
  MODULE
  ext = ext
  attrs1 = attributes
  REC
  name = mkrhs(module_name)
  body = module_binding_body
  attrs2 = post_item_attributes
  {
    let loc = make_loc $sloc in
    let attrs = attrs1 @ attrs2 in
    let docs = symbol_docs $sloc in
    ext,
    Mb.mk name body ~attrs ~loc ~docs
  }
;

(* The following bindings in a group of recursive module bindings. *)
%inline and_module_binding:
  AND
  attrs1 = attributes
  name = mkrhs(module_name)
  body = module_binding_body
  attrs2 = post_item_attributes
  {
    let loc = make_loc $sloc in
    let attrs = attrs1 @ attrs2 in
    let docs = symbol_docs $sloc in
    let text = symbol_text $symbolstartpos in
    Mb.mk name body ~attrs ~loc ~text ~docs
  }
;

(* -------------------------------------------------------------------------- *)

(* Shared material between structures and signatures. *)

(* An [include] statement can appear in a structure or in a signature,
   which is why this definition is parameterized. *)
%inline include_statement(thing):
  INCLUDE
  ext = ext
  attrs1 = attributes
  thing = thing
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Incl.mk thing ~attrs ~loc ~docs, ext
  }
;

(* A module type declaration. *)
module_type_declaration:
  MODULE TYPE
  ext = ext
  attrs1 = attributes
  id = mkrhs(ident)
  typ = preceded(EQUAL, module_type)?
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Mtd.mk id ?typ ~attrs ~loc ~docs, ext
  }
;

(* -------------------------------------------------------------------------- *)

(* Opens. *)

open_declaration:
  OPEN
  override = override_flag
  ext = ext
  attrs1 = attributes
  me = module_expr
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Opn.mk me ~override ~attrs ~loc ~docs, ext
  }
;

open_description:
  OPEN
  override = override_flag
  ext = ext
  attrs1 = attributes
  id = mkrhs(mod_ext_longident)
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Opn.mk id ~override ~attrs ~loc ~docs, ext
  }
;

%inline open_dot_declaration: mkrhs(mod_longident)
  { let loc = make_loc $loc($1) in
    let me = Mod.ident ~loc $1 in
    Opn.mk ~loc me }
;

(* -------------------------------------------------------------------------- *)

/* Module types */

module_type:
  | SIG attrs = attributes s = signature END
      { mkmty ~loc:$sloc ~attrs (Pmty_signature s) }
  | SIG attributes signature error
      { unclosed "sig" $loc($1) "end" $loc($4) }
  | FUNCTOR attrs = attributes args = functor_args
    MINUSGREATER mty = module_type
      %prec below_WITH
      { wrap_mty_attrs ~loc:$sloc attrs (
          List.fold_left (fun acc arg ->
            mkmty ~loc:$sloc (Pmty_functor (arg, acc))
          ) mty args
        ) }
  | MODULE TYPE OF attributes module_expr %prec below_LBRACKETAT
      { mkmty ~loc:$sloc ~attrs:$4 (Pmty_typeof $5) }
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | module_type attribute
      { Mty.attr $1 $2 }
  | mkmty(
      mkrhs(mty_longident)
        { Pmty_ident $1 }
    | module_type MINUSGREATER module_type
        %prec below_WITH
        { Pmty_functor(Named (mknoloc None, $1), $3) }
    | module_type WITH separated_nonempty_llist(AND, with_constraint)
        { Pmty_with($1, $3) }
/*  | LPAREN MODULE mkrhs(mod_longident) RPAREN
        { Pmty_alias $3 } */
    | extension
        { Pmty_extension $1 }
    )
    { $1 }
;
(* A signature, which appears between SIG and END (among other places),
   is a list of signature elements. *)
signature:
  extra_sig(flatten(signature_element*))
    { $1 }
;

(* A signature element is one of the following:
   - a double semicolon;
   - a signature item. *)
%inline signature_element:
    text_sig_SEMISEMI
  | text_sig(signature_item)
      { $1 }
;

(* A signature item. *)
signature_item:
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mksig ~loc:$sloc (Psig_extension ($1, (add_docs_attrs docs $2))) }
  | mksig(
      floating_attribute
        { Psig_attribute $1 }
    )
    { $1 }
  | wrap_mksig_ext(
      value_description
        { psig_value $1 }
    | primitive_declaration
        { psig_value $1 }
    | type_declarations
        { psig_type $1 }
    | type_subst_declarations
        { psig_typesubst $1 }
    | sig_type_extension
        { psig_typext $1 }
    | sig_exception_declaration
        { psig_exception $1 }
    | module_declaration
        { let (body, ext) = $1 in (Psig_module body, ext) }
    | module_alias
        { let (body, ext) = $1 in (Psig_module body, ext) }
    | module_subst
        { let (body, ext) = $1 in (Psig_modsubst body, ext) }
    | rec_module_declarations
        { let (ext, l) = $1 in (Psig_recmodule l, ext) }
    | module_type_declaration
        { let (body, ext) = $1 in (Psig_modtype body, ext) }
    | open_description
        { let (body, ext) = $1 in (Psig_open body, ext) }
    | include_statement(module_type)
        { psig_include $1 }
    | class_descriptions
        { let (ext, l) = $1 in (Psig_class l, ext) }
    | class_type_declarations
        { let (ext, l) = $1 in (Psig_class_type l, ext) }
    )
    { $1 }

(* A module declaration. *)
%inline module_declaration:
  MODULE
  ext = ext attrs1 = attributes
  name = mkrhs(module_name)
  body = module_declaration_body
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Md.mk name body ~attrs ~loc ~docs, ext
  }
;

(* The body (right-hand side) of a module declaration. *)
module_declaration_body:
    COLON mty = module_type
      { mty }
  | mkmty(
      arg = functor_arg body = module_declaration_body
        { Pmty_functor(arg, body) }
    )
    { $1 }
;

(* A module alias declaration (in a signature). *)
%inline module_alias:
  MODULE
  ext = ext attrs1 = attributes
  name = mkrhs(module_name)
  EQUAL
  body = module_expr_alias
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Md.mk name body ~attrs ~loc ~docs, ext
  }
;
%inline module_expr_alias:
  id = mkrhs(mod_longident)
    { Mty.alias ~loc:(make_loc $sloc) id }
;
(* A module substitution (in a signature). *)
module_subst:
  MODULE
  ext = ext attrs1 = attributes
  uid = mkrhs(UIDENT)
  COLONEQUAL
  body = mkrhs(mod_ext_longident)
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Ms.mk uid body ~attrs ~loc ~docs, ext
  }
| MODULE ext attributes mkrhs(UIDENT) COLONEQUAL error
    { expecting $loc($6) "module path" }
;

(* A group of recursive module declarations. *)
%inline rec_module_declarations:
  xlist(rec_module_declaration, and_module_declaration)
    { $1 }
;
%inline rec_module_declaration:
  MODULE
  ext = ext
  attrs1 = attributes
  REC
  name = mkrhs(module_name)
  COLON
  mty = module_type
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    ext, Md.mk name mty ~attrs ~loc ~docs
  }
;
%inline and_module_declaration:
  AND
  attrs1 = attributes
  name = mkrhs(module_name)
  COLON
  mty = module_type
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let docs = symbol_docs $sloc in
    let loc = make_loc $sloc in
    let text = symbol_text $symbolstartpos in
    Md.mk name mty ~attrs ~loc ~text ~docs
  }
;

(* -------------------------------------------------------------------------- *)

(* Class declarations. *)

%inline class_declarations:
  xlist(class_declaration, and_class_declaration)
    { $1 }
;
%inline class_declaration:
  CLASS
  ext = ext
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  body = class_fun_binding
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    ext,
    Ci.mk id body ~virt ~params ~attrs ~loc ~docs
  }
;
%inline and_class_declaration:
  AND
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  body = class_fun_binding
  attrs2 = post_item_attributes
  {
    let attrs = attrs1 @ attrs2 in
    let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    let text = symbol_text $symbolstartpos in
    Ci.mk id body ~virt ~params ~attrs ~loc ~text ~docs
  }
;

class_fun_binding:
    EQUAL class_expr
      { $2 }
  | mkclass(
      COLON class_type EQUAL class_expr
        { Pcl_constraint($4, $2) }
    | labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in Pcl_fun(l, o, p, $2) }
    ) { $1 }
;

formal_class_parameters:
  params = class_parameters(type_parameter)
    { params }
;

(* -------------------------------------------------------------------------- *)

(* Class expressions. *)

class_expr:
    class_simple_expr
      { $1 }
  | FUN attributes class_fun_def
      { wrap_class_attrs ~loc:$sloc $3 $2 }
  | let_bindings(no_ext) IN class_expr
      { class_of_let_bindings ~loc:$sloc $1 $3 }
  | LET OPEN override_flag attributes mkrhs(mod_longident) IN class_expr
      { let loc = ($startpos($2), $endpos($4)) in
        let od = Opn.mk ~override:$3 ~loc:(make_loc loc) $5 in
        mkclass ~loc:$sloc ~attrs:$4 (Pcl_open(od, $7)) }
  | class_expr attribute
      { Cl.attr $1 $2 }
  | mkclass(
      class_simple_expr nonempty_llist(labeled_simple_expr)
        { Pcl_apply($1, $2) }
    | extension
        { Pcl_extension $1 }
    ) { $1 }
;
class_simple_expr:
  | LPAREN class_expr RPAREN
      { $2 }
  | LPAREN class_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | mkclass(
      tys = actual_class_parameters cid = mkrhs(class_longident)
        { Pcl_constr(cid, tys) }
    | OBJECT attributes class_structure error
        { unclosed "object" $loc($1) "end" $loc($4) }
    | LPAREN class_expr COLON class_type RPAREN
        { Pcl_constraint($2, $4) }
    | LPAREN class_expr COLON class_type error
        { unclosed "(" $loc($1) ")" $loc($5) }
    ) { $1 }
  | OBJECT attributes class_structure END
    { mkclass ~loc:$sloc ~attrs:$2 (Pcl_structure $3) }
;

class_fun_def:
  mkclass(
    labeled_simple_pattern MINUSGREATER e = class_expr
  | labeled_simple_pattern e = class_fun_def
      { let (l,o,p) = $1 in Pcl_fun(l, o, p, e) }
  ) { $1 }
;
%inline class_structure:
  |  class_self_pattern extra_cstr(class_fields)
       { Cstr.mk $1 $2 }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { reloc_pat ~loc:$sloc $2 }
  | mkpat(LPAREN pattern COLON core_type RPAREN
      { Ppat_constraint($2, $4) })
      { $1 }
  | /* empty */
      { ghpat ~loc:$sloc Ppat_any }
;
%inline class_fields:
  flatten(text_cstr(class_field)*)
    { $1 }
;
class_field:
  | INHERIT override_flag attributes class_expr
    self = preceded(AS, mkrhs(LIDENT))?
    post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_inherit ($2, $4, self)) ~attrs:($3@$6) ~docs }
  | VAL value post_item_attributes
      { let v, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_val v) ~attrs:(attrs@$3) ~docs }
  | METHOD method_ post_item_attributes
      { let meth, attrs = $2 in
        let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_method meth) ~attrs:(attrs@$3) ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_constraint $3) ~attrs:($2@$4) ~docs }
  | INITIALIZER attributes seq_expr post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_initializer $3) ~attrs:($2@$4) ~docs }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mkcf ~loc:$sloc (Pcf_extension $1) ~attrs:$2 ~docs }
  | mkcf(floating_attribute
      { Pcf_attribute $1 })
      { $1 }
;
value:
    no_override_flag
    attrs = attributes
    mutable_ = virtual_with_mutable_flag
    label = mkrhs(label) COLON ty = core_type
      { (label, mutable_, Cfk_virtual ty), attrs }
  | override_flag attributes mutable_flag mkrhs(label) EQUAL seq_expr
      { ($4, $3, Cfk_concrete ($1, $6)), $2 }
  | override_flag attributes mutable_flag mkrhs(label) type_constraint
    EQUAL seq_expr
      { let e = mkexp_constraint ~loc:$sloc $7 $5 in
        ($4, $3, Cfk_concrete ($1, e)), $2
      }
;
method_:
    no_override_flag
    attrs = attributes
    private_ = virtual_with_private_flag
    label = mkrhs(label) COLON ty = poly_type
      { (label, private_, Cfk_virtual ty), attrs }
  | override_flag attributes private_flag mkrhs(label) strict_binding
      { let e = $5 in
        let loc = Location.(e.pexp_loc.loc_start, e.pexp_loc.loc_end) in
        ($4, $3,
        Cfk_concrete ($1, ghexp ~loc (Pexp_poly (e, None)))), $2 }
  | override_flag attributes private_flag mkrhs(label)
    COLON poly_type EQUAL seq_expr
      { let poly_exp =
          let loc = ($startpos($6), $endpos($8)) in
          ghexp ~loc (Pexp_poly($8, Some $6)) in
        ($4, $3, Cfk_concrete ($1, poly_exp)), $2 }
  | override_flag attributes private_flag mkrhs(label) COLON TYPE lident_list
    DOT core_type EQUAL seq_expr
      { let poly_exp_loc = ($startpos($7), $endpos($11)) in
        let poly_exp =
          let exp, poly =
            (* it seems odd to use the global ~loc here while poly_exp_loc
               is tighter, but this is what ocamlyacc does;
               TODO improve parser.mly *)
            wrap_type_annotation ~loc:$sloc $7 $9 $11 in
          ghexp ~loc:poly_exp_loc (Pexp_poly(exp, Some poly)) in
        ($4, $3,
        Cfk_concrete ($1, poly_exp)), $2 }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | mkcty(
      label = arg_label
      domain = tuple_type
      MINUSGREATER
      codomain = class_type
        { Pcty_arrow(label, domain, codomain) }
    ) { $1 }
 ;
class_signature:
    mkcty(
      tys = actual_class_parameters cid = mkrhs(clty_longident)
        { Pcty_constr (cid, tys) }
    | extension
        { Pcty_extension $1 }
    ) { $1 }
  | OBJECT attributes class_sig_body END
      { mkcty ~loc:$sloc ~attrs:$2 (Pcty_signature $3) }
  | OBJECT attributes class_sig_body error
      { unclosed "object" $loc($1) "end" $loc($4) }
  | class_signature attribute
      { Cty.attr $1 $2 }
  | LET OPEN override_flag attributes mkrhs(mod_longident) IN class_signature
      { let loc = ($startpos($2), $endpos($4)) in
        let od = Opn.mk ~override:$3 ~loc:(make_loc loc) $5 in
        mkcty ~loc:$sloc ~attrs:$4 (Pcty_open(od, $7)) }
;
%inline class_parameters(parameter):
  | /* empty */
      { [] }
  | LBRACKET params = separated_nonempty_llist(COMMA, parameter) RBRACKET
      { params }
;
%inline actual_class_parameters:
  tys = class_parameters(core_type)
    { tys }
;
%inline class_sig_body:
    class_self_type extra_csig(class_sig_fields)
      { Csig.mk $1 $2 }
;
class_self_type:
    LPAREN core_type RPAREN
      { $2 }
  | mktyp((* empty *) { Ptyp_any })
      { $1 }
;
%inline class_sig_fields:
  flatten(text_csig(class_sig_field)*)
    { $1 }
;
class_sig_field:
    INHERIT attributes class_signature post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_inherit $3) ~attrs:($2@$4) ~docs }
  | VAL attributes value_type post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_val $3) ~attrs:($2@$4) ~docs }
  | METHOD attributes private_virtual_flags mkrhs(label) COLON poly_type
    post_item_attributes
      { let (p, v) = $3 in
        let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_method ($4, p, v, $6)) ~attrs:($2@$7) ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_constraint $3) ~attrs:($2@$4) ~docs }
  | item_extension post_item_attributes
      { let docs = symbol_docs $sloc in
        mkctf ~loc:$sloc (Pctf_extension $1) ~attrs:$2 ~docs }
  | mkctf(floating_attribute
      { Pctf_attribute $1 })
      { $1 }
;
%inline value_type:
  flags = mutable_virtual_flags
  label = mkrhs(label)
  COLON
  ty = core_type
  {
    let mut, virt = flags in
    label, mut, virt, ty
  }
;
%inline constrain:
    core_type EQUAL core_type
    { $1, $3, make_loc $sloc }
;
constrain_field:
  core_type EQUAL core_type
    { $1, $3 }
;
(* A group of class descriptions. *)
%inline class_descriptions:
  xlist(class_description, and_class_description)
    { $1 }
;
%inline class_description:
  CLASS
  ext = ext
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  COLON
  cty = class_type
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      ext,
      Ci.mk id cty ~virt ~params ~attrs ~loc ~docs
    }
;
%inline and_class_description:
  AND
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  COLON
  cty = class_type
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id cty ~virt ~params ~attrs ~loc ~text ~docs
    }
;
class_type_declarations:
  xlist(class_type_declaration, and_class_type_declaration)
    { $1 }
;
%inline class_type_declaration:
  CLASS TYPE
  ext = ext
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  EQUAL
  csig = class_signature
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      ext,
      Ci.mk id csig ~virt ~params ~attrs ~loc ~docs
    }
;
%inline and_class_type_declaration:
  AND
  attrs1 = attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  EQUAL
  csig = class_signature
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id csig ~virt ~params ~attrs ~loc ~text ~docs
    }
;

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { $1 }
  | mkexp(expr SEMI seq_expr
    { Pexp_sequence($1, $3) })
    { $1 }
  | expr SEMI PERCENT attr_id seq_expr
    { let seq = mkexp ~loc:$sloc (Pexp_sequence ($1, $5)) in
      let payload = PStr [mkstrexp seq []] in
      mkexp ~loc:$sloc (Pexp_extension ($4, payload)) }
;
labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern opt_default RPAREN
      { (Optional (fst $3), $4, snd $3) }
  | QUESTION label_var
      { (Optional (fst $2), None, snd $2) }
  | OPTLABEL LPAREN let_pattern opt_default RPAREN
      { (Optional $1, $4, $3) }
  | OPTLABEL pattern_var
      { (Optional $1, None, $2) }
  | TILDE LPAREN label_let_pattern RPAREN
      { (Labelled (fst $3), None, snd $3) }
  | TILDE label_var
      { (Labelled (fst $2), None, snd $2) }
  | LABEL simple_pattern
      { (Labelled $1, None, $2) }
  | simple_pattern
      { (Nolabel, None, $1) }
;

pattern_var:
  mkpat(
      mkrhs(LIDENT)     { Ppat_var $1 }
    | UNDERSCORE        { Ppat_any }
  ) { $1 }
;

%inline opt_default:
  preceded(EQUAL, seq_expr)?
    { $1 }
;
label_let_pattern:
    x = label_var
      { x }
  | x = label_var COLON cty = core_type
      { let lab, pat = x in
        lab,
        mkpat ~loc:$sloc (Ppat_constraint (pat, cty)) }
;
%inline label_var:
    mkrhs(LIDENT)
      { ($1.Location.txt, mkpat ~loc:$sloc (Ppat_var $1)) }
;
let_pattern:
    pattern
      { $1 }
  | mkpat(pattern COLON core_type
      { Ppat_constraint($1, $3) })
      { $1 }
;

expr:
    simple_expr %prec below_HASH
      { $1 }
  | expr_attrs
      { let desc, attrs = $1 in
        mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(expr_)
      { $1 }
  | let_bindings(ext) IN seq_expr
      { expr_of_let_bindings ~loc:$sloc $1 $3 }
  | pbop_op = mkrhs(LETOP) bindings = letop_bindings IN body = seq_expr
      { let (pbop_pat, pbop_exp, rev_ands) = bindings in
        let ands = List.rev rev_ands in
        let pbop_loc = make_loc $sloc in
        let let_ = {pbop_op; pbop_pat; pbop_exp; pbop_loc} in
        mkexp ~loc:$sloc (Pexp_letop{ let_; ands; body}) }
  | expr COLONCOLON expr
      { mkexp_cons ~loc:$sloc $loc($2) (ghexp ~loc:$sloc (Pexp_tuple[$1;$3])) }
  | mkrhs(label) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setinstvar($1, $3)) }
  | simple_expr DOT mkrhs(label_longident) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { array_set ~loc:$sloc $1 $4 $7 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { string_set ~loc:$sloc $1 $4 $7 }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { bigarray_set ~loc:$sloc $1 $4 $7 }
  | simple_expr DOTOP LBRACKET expr_semi_list RBRACKET LESSMINUS expr
      { dotop_set ~loc:$sloc lident bracket $2 $1 $4 $7 }
  | simple_expr DOTOP LPAREN expr_semi_list RPAREN LESSMINUS expr
      { dotop_set ~loc:$sloc lident paren $2 $1 $4 $7 }
  | simple_expr DOTOP LBRACE expr_semi_list RBRACE LESSMINUS expr
      { dotop_set ~loc:$sloc lident brace $2 $1 $4 $7 }
  | simple_expr DOT mod_longident DOTOP LBRACKET expr_semi_list RBRACKET
      LESSMINUS expr
      { dotop_set ~loc:$sloc (ldot $3) bracket $4 $1 $6 $9 }
  | simple_expr DOT mod_longident DOTOP LPAREN expr_semi_list RPAREN
      LESSMINUS expr
      { dotop_set ~loc:$sloc (ldot $3) paren $4 $1 $6 $9  }
  | simple_expr DOT mod_longident DOTOP LBRACE expr_semi_list RBRACE
      LESSMINUS expr
      { dotop_set ~loc:$sloc (ldot $3) brace $4 $1 $6 $9 }
  | expr attribute
      { Exp.attr $1 $2 }
  | UNDERSCORE
     { not_expecting $loc($1) "wildcard \"_\"" }
;
%inline expr_attrs:
  | LET MODULE ext_attributes mkrhs(module_name) module_binding_body IN seq_expr
      { Pexp_letmodule($4, $5, $7), $3 }
  | LET EXCEPTION ext_attributes let_exception_declaration IN seq_expr
      { Pexp_letexception($4, $6), $3 }
  | LET OPEN override_flag ext_attributes module_expr IN seq_expr
      { let open_loc = make_loc ($startpos($2), $endpos($5)) in
        let od = Opn.mk $5 ~override:$3 ~loc:open_loc in
        Pexp_open(od, $7), $4 }
  | FUNCTION ext_attributes match_cases
      { Pexp_function $3, $2 }
  | FUN ext_attributes labeled_simple_pattern fun_def
      { let (l,o,p) = $3 in
        Pexp_fun(l, o, p, $4), $2 }
  | FUN ext_attributes LPAREN TYPE lident_list RPAREN fun_def
      { (mk_newtypes ~loc:$sloc $5 $7).pexp_desc, $2 }
  | MATCH ext_attributes seq_expr WITH match_cases
      { Pexp_match($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH match_cases
      { Pexp_try($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH error
      { syntax_error() }
  | IF ext_attributes seq_expr THEN expr ELSE expr
      { Pexp_ifthenelse($3, $5, Some $7), $2 }
  | IF ext_attributes seq_expr THEN expr
      { Pexp_ifthenelse($3, $5, None), $2 }
  | WHILE ext_attributes seq_expr DO seq_expr DONE
      { Pexp_while($3, $5), $2 }
  | FOR ext_attributes pattern EQUAL seq_expr direction_flag seq_expr DO
    seq_expr DONE
      { Pexp_for($3, $5, $7, $6, $9), $2 }
  | ASSERT ext_attributes simple_expr %prec below_HASH
      { Pexp_assert $3, $2 }
  | LAZY ext_attributes simple_expr %prec below_HASH
      { Pexp_lazy $3, $2 }
  | OBJECT ext_attributes class_structure END
      { Pexp_object $3, $2 }
  | OBJECT ext_attributes class_structure error
      { unclosed "object" $loc($1) "end" $loc($4) }
;
%inline expr_:
  | simple_expr nonempty_llist(labeled_simple_expr)
      { Pexp_apply($1, $2) }
  | expr_comma_list %prec below_COMMA
      { Pexp_tuple($1) }
  | mkrhs(constr_longident) simple_expr %prec below_HASH
      { Pexp_construct($1, Some $2) }
  | name_tag simple_expr %prec below_HASH
      { Pexp_variant($1, Some $2) }
  | e1 = expr op = op(infix_operator) e2 = expr
      { mkinfix e1 op e2 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus ~oploc:$loc($1) $1 $2 }
  | additive expr %prec prec_unary_plus
      { mkuplus ~oploc:$loc($1) $1 $2 }
;

simple_expr:
  | LPAREN seq_expr RPAREN
      { reloc_exp ~loc:$sloc $2 }
  | LPAREN seq_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN seq_expr type_constraint RPAREN
      { mkexp_constraint ~loc:$sloc $2 $3 }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { array_get ~loc:$sloc $1 $4 }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { string_get ~loc:$sloc $1 $4 }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | simple_expr DOTOP LBRACKET expr_semi_list RBRACKET
      { dotop_get ~loc:$sloc lident bracket $2 $1 $4 }
  | simple_expr DOTOP LBRACKET expr_semi_list error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | simple_expr DOTOP LPAREN expr_semi_list RPAREN
      { dotop_get ~loc:$sloc lident paren $2 $1 $4  }
  | simple_expr DOTOP LPAREN expr_semi_list error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | simple_expr DOTOP LBRACE expr_semi_list RBRACE
      { dotop_get ~loc:$sloc lident brace $2 $1 $4 }
  | simple_expr DOTOP LBRACE expr error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | simple_expr DOT mod_longident DOTOP LBRACKET expr_semi_list RBRACKET
      { dotop_get ~loc:$sloc (ldot $3) bracket $4 $1 $6  }
  | simple_expr DOT
    mod_longident DOTOP LBRACKET expr_semi_list error
      { unclosed "[" $loc($5) "]" $loc($7) }
  | simple_expr DOT mod_longident DOTOP LPAREN expr_semi_list RPAREN
      { dotop_get ~loc:$sloc (ldot $3) paren $4 $1 $6 }
  | simple_expr DOT
    mod_longident DOTOP LPAREN expr_semi_list error
      { unclosed "(" $loc($5) ")" $loc($7) }
  | simple_expr DOT mod_longident DOTOP LBRACE expr_semi_list RBRACE
      { dotop_get ~loc:$sloc (ldot $3) brace $4 $1 $6  }
  | simple_expr DOT
    mod_longident DOTOP LBRACE expr_semi_list error
      { unclosed "{" $loc($5) "}" $loc($7) }
  | simple_expr DOT LBRACE expr RBRACE
      { bigarray_get ~loc:$sloc $1 $4 }
  | simple_expr DOT LBRACE expr error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | simple_expr_attrs
    { let desc, attrs = $1 in
      mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(simple_expr_)
      { $1 }
;
%inline simple_expr_attrs:
  | BEGIN ext = ext attrs = attributes e = seq_expr END
      { e.pexp_desc, (ext, attrs @ e.pexp_attributes) }
  | BEGIN ext_attributes END
      { Pexp_construct (mkloc (Lident "()") (make_loc $sloc), None), $2 }
  | BEGIN ext_attributes seq_expr error
      { unclosed "begin" $loc($1) "end" $loc($4) }
  | NEW ext_attributes mkrhs(class_longident)
      { Pexp_new($3), $2 }
  | LPAREN MODULE ext_attributes module_expr RPAREN
      { Pexp_pack $4, $3 }
  | LPAREN MODULE ext_attributes module_expr COLON package_type RPAREN
      { Pexp_constraint (ghexp ~loc:$sloc (Pexp_pack $4), $6), $3 }
  | LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
;
%inline simple_expr_:
  | mkrhs(val_longident)
      { Pexp_ident ($1) }
  | constant
      { Pexp_constant $1 }
  | mkrhs(constr_longident) %prec prec_constant_constructor
      { Pexp_construct($1, None) }
  | name_tag %prec prec_constant_constructor
      { Pexp_variant($1, None) }
  | op(PREFIXOP) simple_expr
      { Pexp_apply($1, [Nolabel,$2]) }
  | op(BANG {"!"}) simple_expr
      { Pexp_apply($1, [Nolabel,$2]) }
  | LBRACELESS object_expr_content GREATERRBRACE
      { Pexp_override $2 }
  | LBRACELESS object_expr_content error
      { unclosed "{<" $loc($1) ">}" $loc($3) }
  | LBRACELESS GREATERRBRACE
      { Pexp_override [] }
  | simple_expr DOT mkrhs(label_longident)
      { Pexp_field($1, $3) }
  | od=open_dot_declaration DOT LPAREN seq_expr RPAREN
      { Pexp_open(od, $4) }
  | od=open_dot_declaration DOT LBRACELESS object_expr_content GREATERRBRACE
      { (* TODO: review the location of Pexp_override *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_override $4)) }
  | mod_longident DOT LBRACELESS object_expr_content error
      { unclosed "{<" $loc($3) ">}" $loc($5) }
  | simple_expr HASH mkrhs(label)
      { Pexp_send($1, $3) }
  | simple_expr op(HASHOP) simple_expr
      { mkinfix $1 $2 $3 }
  | extension
      { Pexp_extension $1 }
  | od=open_dot_declaration DOT mkrhs(LPAREN RPAREN {Lident "()"})
      { (* TODO: review the location of Pexp_construct *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_construct($3, None))) }
  | mod_longident DOT LPAREN seq_expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | LBRACE record_expr_content RBRACE
      { let (exten, fields) = $2 in
        Pexp_record(fields, exten) }
  | LBRACE record_expr_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
  | od=open_dot_declaration DOT LBRACE record_expr_content RBRACE
      { let (exten, fields) = $4 in
        (* TODO: review the location of Pexp_construct *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_record(fields, exten))) }
  | mod_longident DOT LBRACE record_expr_content error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | LBRACKETBAR expr_semi_list BARRBRACKET
      { Pexp_array($2) }
  | LBRACKETBAR expr_semi_list error
      { unclosed "[|" $loc($1) "|]" $loc($3) }
  | LBRACKETBAR BARRBRACKET
      { Pexp_array [] }
  | od=open_dot_declaration DOT LBRACKETBAR expr_semi_list BARRBRACKET
      { (* TODO: review the location of Pexp_array *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_array($4))) }
  | od=open_dot_declaration DOT LBRACKETBAR BARRBRACKET
      { (* TODO: review the location of Pexp_array *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_array [])) }
  | mod_longident DOT
    LBRACKETBAR expr_semi_list error
      { unclosed "[|" $loc($3) "|]" $loc($5) }
  | LBRACKET expr_semi_list RBRACKET
      { fst (mktailexp $loc($3) $2) }
  | LBRACKET expr_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
  | od=open_dot_declaration DOT LBRACKET expr_semi_list RBRACKET
      { let list_exp =
          (* TODO: review the location of list_exp *)
          let tail_exp, _tail_loc = mktailexp $loc($5) $4 in
          mkexp ~loc:$sloc tail_exp in
        Pexp_open(od, list_exp) }
  | od=open_dot_declaration DOT mkrhs(LBRACKET RBRACKET {Lident "[]"})
      { (* TODO: review the location of Pexp_construct *)
        Pexp_open(od, mkexp ~loc:$sloc (Pexp_construct($3, None))) }
  | mod_longident DOT
    LBRACKET expr_semi_list error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | od=open_dot_declaration DOT LPAREN MODULE ext_attributes module_expr COLON
    package_type RPAREN
      { (* TODO: review the location of Pexp_constraint *)
        let modexp =
          mkexp_attrs ~loc:$sloc
            (Pexp_constraint (ghexp ~loc:$sloc (Pexp_pack $6), $8)) $5 in
        Pexp_open(od, modexp) }
  | mod_longident DOT
    LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($3) ")" $loc($8) }
;
labeled_simple_expr:
    simple_expr %prec below_HASH
      { (Nolabel, $1) }
  | LABEL simple_expr %prec below_HASH
      { (Labelled $1, $2) }
  | TILDE label = LIDENT
      { let loc = $loc(label) in
        (Labelled label, mkexpvar ~loc label) }
  | QUESTION label = LIDENT
      { let loc = $loc(label) in
        (Optional label, mkexpvar ~loc label) }
  | OPTLABEL simple_expr %prec below_HASH
      { (Optional $1, $2) }
;
%inline lident_list:
  xs = mkrhs(LIDENT)+
    { xs }
;
%inline let_ident:
    val_ident { mkpatvar ~loc:$sloc $1 }
;
let_binding_body:
    let_ident strict_binding
      { ($1, $2) }
  | let_ident type_constraint EQUAL seq_expr
      { let v = $1 in (* PR#7344 *)
        let t =
          match $2 with
            Some t, None -> t
          | _, Some t -> t
          | _ -> assert false
        in
        let loc = Location.(t.ptyp_loc.loc_start, t.ptyp_loc.loc_end) in
        let typ = ghtyp ~loc (Ptyp_poly([],t)) in
        let patloc = ($startpos($1), $endpos($2)) in
        (ghpat ~loc:patloc (Ppat_constraint(v, typ)),
         mkexp_constraint ~loc:$sloc $4 $2) }
  | let_ident COLON typevar_list DOT core_type EQUAL seq_expr
      (* TODO: could replace [typevar_list DOT core_type]
               with [mktyp(poly(core_type))]
               and simplify the semantic action? *)
      { let typloc = ($startpos($3), $endpos($5)) in
        let patloc = ($startpos($1), $endpos($5)) in
        (ghpat ~loc:patloc
           (Ppat_constraint($1, ghtyp ~loc:typloc (Ptyp_poly($3,$5)))),
         $7) }
  | let_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
      { let exp, poly =
          wrap_type_annotation ~loc:$sloc $4 $6 $8 in
        let loc = ($startpos($1), $endpos($6)) in
        (ghpat ~loc (Ppat_constraint($1, poly)), exp) }
  | pattern_no_exn EQUAL seq_expr
      { ($1, $3) }
  | simple_pattern_not_ident COLON core_type EQUAL seq_expr
      { let loc = ($startpos($1), $endpos($3)) in
        (ghpat ~loc (Ppat_constraint($1, $3)), $5) }
;
(* The formal parameter EXT can be instantiated with ext or no_ext
   so as to indicate whether an extension is allowed or disallowed. *)
let_bindings(EXT):
    let_binding(EXT)                            { $1 }
  | let_bindings(EXT) and_let_binding           { addlb $1 $2 }
;
%inline let_binding(EXT):
  LET
  ext = EXT
  attrs1 = attributes
  rec_flag = rec_flag
  body = let_binding_body
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      mklbs ~loc:$sloc ext rec_flag (mklb ~loc:$sloc true body attrs)
    }
;
and_let_binding:
  AND
  attrs1 = attributes
  body = let_binding_body
  attrs2 = post_item_attributes
    {
      let attrs = attrs1 @ attrs2 in
      mklb ~loc:$sloc false body attrs
    }
;
letop_binding_body:
    pat = let_ident exp = strict_binding
      { (pat, exp) }
  | pat = simple_pattern COLON typ = core_type EQUAL exp = seq_expr
      { let loc = ($startpos(pat), $endpos(typ)) in
        (ghpat ~loc (Ppat_constraint(pat, typ)), exp) }
  | pat = pattern_no_exn EQUAL exp = seq_expr
      { (pat, exp) }
;
letop_bindings:
    body = letop_binding_body
      { let let_pat, let_exp = body in
        let_pat, let_exp, [] }
  | bindings = letop_bindings pbop_op = mkrhs(ANDOP) body = let_binding_body
      { let let_pat, let_exp, rev_ands = bindings in
        let pbop_pat, pbop_exp = body in
        let pbop_loc = make_loc $sloc in
        let and_ = {pbop_op; pbop_pat; pbop_exp; pbop_loc} in
        let_pat, let_exp, and_ :: rev_ands }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { mkexp_constraint ~loc:$sloc $3 $1 }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | labeled_simple_pattern fun_binding
      { let (l, o, p) = $1 in ghexp ~loc:$sloc (Pexp_fun(l, o, p, $2)) }
  | LPAREN TYPE lident_list RPAREN fun_binding
      { mk_newtypes ~loc:$sloc $3 $5 }
;
%inline match_cases:
  xs = preceded_or_separated_nonempty_llist(BAR, match_case)
    { xs }
;
match_case:
    pattern MINUSGREATER seq_expr
      { Exp.case $1 $3 }
  | pattern WHEN seq_expr MINUSGREATER seq_expr
      { Exp.case $1 ~guard:$3 $5 }
  | pattern MINUSGREATER DOT
      { Exp.case $1 (Exp.unreachable ~loc:(make_loc $loc($3)) ()) }
;
fun_def:
    MINUSGREATER seq_expr
      { $2 }
  | mkexp(COLON atomic_type MINUSGREATER seq_expr
      { Pexp_constraint ($4, $2) })
      { $1 }
/* Cf #5939: we used to accept (fun p when e0 -> e) */
  | labeled_simple_pattern fun_def
      {
       let (l,o,p) = $1 in
       ghexp ~loc:$sloc (Pexp_fun(l, o, p, $2))
      }
  | LPAREN TYPE lident_list RPAREN fun_def
      { mk_newtypes ~loc:$sloc $3 $5 }
;
%inline expr_comma_list:
  es = separated_nontrivial_llist(COMMA, expr)
    { es }
;
record_expr_content:
  eo = ioption(terminated(simple_expr, WITH))
  fields = separated_or_terminated_nonempty_list(SEMI, record_expr_field)
    { eo, fields }
;
%inline record_expr_field:
  | label = mkrhs(label_longident)
    c = type_constraint?
    eo = preceded(EQUAL, expr)?
      { let e =
          match eo with
          | None ->
              (* No pattern; this is a pun. Desugar it. *)
              exp_of_longident ~loc:$sloc label
          | Some e ->
              e
        in
        label, mkexp_opt_constraint ~loc:$sloc e c }
;
%inline object_expr_content:
  xs = separated_or_terminated_nonempty_list(SEMI, object_expr_field)
    { xs }
;
%inline object_expr_field:
    label = mkrhs(label)
    oe = preceded(EQUAL, expr)?
      { let e =
          match oe with
          | None ->
              (* No expression; this is a pun. Desugar it. *)
              exp_of_label ~loc:$sloc label
          | Some e ->
              e
        in
        label, e }
;
%inline expr_semi_list:
  es = separated_or_terminated_nonempty_list(SEMI, expr)
    { es }
;
type_constraint:
    COLON core_type                             { (Some $2, None) }
  | COLON core_type COLONGREATER core_type      { (Some $2, Some $4) }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

/* Patterns */

(* Whereas [pattern] is an arbitrary pattern, [pattern_no_exn] is a pattern
   that does not begin with the [EXCEPTION] keyword. Thus, [pattern_no_exn]
   is the intersection of the context-free language [pattern] with the
   regular language [^EXCEPTION .*].

   Ideally, we would like to use [pattern] everywhere and check in a later
   phase that EXCEPTION patterns are used only where they are allowed (there
   is code in typing/typecore.ml to this end). Unfortunately, in the
   definition of [let_binding_body], we cannot allow [pattern]. That would
   create a shift/reduce conflict: upon seeing LET EXCEPTION ..., the parser
   wouldn't know whether this is the beginning of a LET EXCEPTION construct or
   the beginning of a LET construct whose pattern happens to begin with
   EXCEPTION. The conflict is avoided there by using [pattern_no_exn] in the
   definition of [let_binding_body].

   In order to avoid duplication between the definitions of [pattern] and
   [pattern_no_exn], we create a parameterized definition [pattern_(self)]
   and instantiate it twice. *)

pattern:
    pattern_(pattern)
      { $1 }
  | EXCEPTION ext_attributes pattern %prec prec_constr_appl
      { mkpat_attrs ~loc:$sloc (Ppat_exception $3) $2}
;

pattern_no_exn:
    pattern_(pattern_no_exn)
      { $1 }
;

%inline pattern_(self):
  | self COLONCOLON pattern
      { mkpat_cons ~loc:$sloc $loc($2) (ghpat ~loc:$sloc (Ppat_tuple[$1;$3])) }
  | self attribute
      { Pat.attr $1 $2 }
  | pattern_gen
      { $1 }
  | mkpat(
      self AS mkrhs(val_ident)
        { Ppat_alias($1, $3) }
    | self AS error
        { expecting $loc($3) "identifier" }
    | pattern_comma_list(self) %prec below_COMMA
        { Ppat_tuple(List.rev $1) }
    | self COLONCOLON error
        { expecting $loc($3) "pattern" }
    | self BAR pattern
        { Ppat_or($1, $3) }
    | self BAR error
        { expecting $loc($3) "pattern" }
  ) { $1 }
;

pattern_gen:
    simple_pattern
      { $1 }
  | mkpat(
      mkrhs(constr_longident) pattern %prec prec_constr_appl
        { Ppat_construct($1, Some $2) }
    | name_tag pattern %prec prec_constr_appl
        { Ppat_variant($1, Some $2) }
    ) { $1 }
  | LAZY ext_attributes simple_pattern
      { mkpat_attrs ~loc:$sloc (Ppat_lazy $3) $2}
;
simple_pattern:
    mkpat(mkrhs(val_ident) %prec below_EQUAL
      { Ppat_var ($1) })
      { $1 }
  | simple_pattern_not_ident { $1 }
;

simple_pattern_not_ident:
  | LPAREN pattern RPAREN
      { reloc_pat ~loc:$sloc $2 }
  | simple_delimited_pattern
      { $1 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) RPAREN
      { mkpat_attrs ~loc:$sloc (Ppat_unpack $4) $3 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) COLON package_type RPAREN
      { mkpat_attrs ~loc:$sloc
          (Ppat_constraint(mkpat ~loc:$sloc (Ppat_unpack $4), $6))
          $3 }
  | mkpat(simple_pattern_not_ident_)
      { $1 }
;
%inline simple_pattern_not_ident_:
  | UNDERSCORE
      { Ppat_any }
  | signed_constant
      { Ppat_constant $1 }
  | signed_constant DOTDOT signed_constant
      { Ppat_interval ($1, $3) }
  | mkrhs(constr_longident)
      { Ppat_construct($1, None) }
  | name_tag
      { Ppat_variant($1, None) }
  | HASH mkrhs(type_longident)
      { Ppat_type ($2) }
  | mkrhs(mod_longident) DOT simple_delimited_pattern
      { Ppat_open($1, $3) }
  | mkrhs(mod_longident) DOT mkrhs(LBRACKET RBRACKET {Lident "[]"})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT mkrhs(LPAREN RPAREN {Lident "()"})
    { Ppat_open($1, mkpat ~loc:$sloc (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT LPAREN pattern RPAREN
      { Ppat_open ($1, $4) }
  | mod_longident DOT LPAREN pattern error
      { unclosed "(" $loc($3) ")" $loc($5)  }
  | mod_longident DOT LPAREN error
      { expecting $loc($4) "pattern" }
  | LPAREN pattern error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN pattern COLON core_type RPAREN
      { Ppat_constraint($2, $4) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN pattern COLON error
      { expecting $loc($4) "type" }
  | LPAREN MODULE ext_attributes module_name COLON package_type
    error
      { unclosed "(" $loc($1) ")" $loc($7) }
  | extension
      { Ppat_extension $1 }
;

simple_delimited_pattern:
  mkpat(
      LBRACE record_pat_content RBRACE
      { let (fields, closed) = $2 in
        Ppat_record(fields, closed) }
    | LBRACE record_pat_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
    | LBRACKET pattern_semi_list RBRACKET
      { fst (mktailpat $loc($3) $2) }
    | LBRACKET pattern_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
    | LBRACKETBAR pattern_semi_list BARRBRACKET
      { Ppat_array $2 }
    | LBRACKETBAR BARRBRACKET
      { Ppat_array [] }
    | LBRACKETBAR pattern_semi_list error
      { unclosed "[|" $loc($1) "|]" $loc($3) }
  ) { $1 }

pattern_comma_list(self):
    pattern_comma_list(self) COMMA pattern      { $3 :: $1 }
  | self COMMA pattern                          { [$3; $1] }
  | self COMMA error                            { expecting $loc($3) "pattern" }
;
%inline pattern_semi_list:
  ps = separated_or_terminated_nonempty_list(SEMI, pattern)
    { ps }
;
(* A label-pattern list is a nonempty list of label-pattern pairs, optionally
   followed with an UNDERSCORE, separated-or-terminated with semicolons. *)
%inline record_pat_content:
  listx(SEMI, record_pat_field, UNDERSCORE)
    { let fields, closed = $1 in
      let closed = match closed with Some () -> Open | None -> Closed in
      fields, closed }
;
%inline record_pat_field:
  label = mkrhs(label_longident)
  octy = preceded(COLON, core_type)?
  opat = preceded(EQUAL, pattern)?
    { let pat =
        match opat with
        | None ->
            (* No pattern; this is a pun. Desugar it. *)
            pat_of_label ~loc:$sloc label
        | Some pat ->
            pat
      in
      label, mkpat_opt_constraint ~loc:$sloc pat octy
    }
;

/* Value descriptions */

value_description:
  VAL
  ext = ext
  attrs1 = attributes
  id = mkrhs(val_ident)
  COLON
  ty = core_type
  attrs2 = post_item_attributes
    { let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      Val.mk id ty ~attrs ~loc ~docs,
      ext }
;

/* Primitive declarations */

primitive_declaration:
  EXTERNAL
  ext = ext
  attrs1 = attributes
  id = mkrhs(val_ident)
  COLON
  ty = core_type
  EQUAL
  prim = raw_string+
  attrs2 = post_item_attributes
    { let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      Val.mk id ty ~prim ~attrs ~loc ~docs,
      ext }
;

(* Type declarations and type substitutions. *)

(* Type declarations [type t = u] and type substitutions [type t := u] are very
   similar, so we view them as instances of [generic_type_declarations]. In the
   case of a type declaration, the use of [nonrec_flag] means that [NONREC] may
   be absent or present, whereas in the case of a type substitution, the use of
   [no_nonrec_flag] means that [NONREC] must be absent. The use of [type_kind]
   versus [type_subst_kind] means that in the first case, we expect an [EQUAL]
   sign, whereas in the second case, we expect [COLONEQUAL]. *)

%inline type_declarations:
  generic_type_declarations(nonrec_flag, type_kind)
    { $1 }
;

%inline type_subst_declarations:
  generic_type_declarations(no_nonrec_flag, type_subst_kind)
    { $1 }
;

(* A set of type declarations or substitutions begins with a
   [generic_type_declaration] and continues with a possibly empty list of
   [generic_and_type_declaration]s. *)

%inline generic_type_declarations(flag, kind):
  xlist(
    generic_type_declaration(flag, kind),
    generic_and_type_declaration(kind)
  )
  { $1 }
;

(* [generic_type_declaration] and [generic_and_type_declaration] look similar,
   but are in reality different enough that it is difficult to share anything
   between them. *)

generic_type_declaration(flag, kind):
  TYPE
  ext = ext
  attrs1 = attributes
  flag = flag
  params = type_parameters
  id = mkrhs(LIDENT)
  kind_priv_manifest = kind
  cstrs = constraints
  attrs2 = post_item_attributes
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs $sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      (flag, ext),
      Type.mk id ~params ~cstrs ~kind ~priv ?manifest ~attrs ~loc ~docs
    }
;
%inline generic_and_type_declaration(kind):
  AND
  attrs1 = attributes
  params = type_parameters
  id = mkrhs(LIDENT)
  kind_priv_manifest = kind
  cstrs = constraints
  attrs2 = post_item_attributes
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs = symbol_docs $sloc in
      let attrs = attrs1 @ attrs2 in
      let loc = make_loc $sloc in
      let text = symbol_text $symbolstartpos in
      Type.mk id ~params ~cstrs ~kind ~priv ?manifest ~attrs ~loc ~docs ~text
    }
;
%inline constraints:
  llist(preceded(CONSTRAINT, constrain))
    { $1 }
;
(* Lots of %inline expansion are required for [nonempty_type_kind] to be
   LR(1). At the cost of some manual expansion, it would be possible to give a
   definition that leads to a smaller grammar (after expansion) and therefore
   a smaller automaton. *)
nonempty_type_kind:
  | priv = inline_private_flag
    ty = core_type
      { (Ptype_abstract, priv, Some ty) }
  | oty = type_synonym
    priv = inline_private_flag
    cs = constructor_declarations
      { (Ptype_variant cs, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    DOTDOT
      { (Ptype_open, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    LBRACE ls = label_declarations RBRACE
      { (Ptype_record ls, priv, oty) }
;
%inline type_synonym:
  ioption(terminated(core_type, EQUAL))
    { $1 }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, Public, None) }
  | EQUAL nonempty_type_kind
      { $2 }
;
%inline type_subst_kind:
    COLONEQUAL nonempty_type_kind
      { $2 }
;
type_parameters:
    /* empty */
      { [] }
  | p = type_parameter
      { [p] }
  | LPAREN ps = separated_nonempty_llist(COMMA, type_parameter) RPAREN
      { ps }
;
type_parameter:
    type_variance type_variable        { $2, $1 }
;
type_variable:
  mktyp(
    QUOTE tyvar = ident
      { Ptyp_var tyvar }
  | UNDERSCORE
      { Ptyp_any }
  ) { $1 }
;

type_variance:
    /* empty */                                 { Invariant }
  | PLUS                                        { Covariant }
  | MINUS                                       { Contravariant }
;

(* A sequence of constructor declarations is either a single BAR, which
   means that the list is empty, or a nonempty BAR-separated list of
   declarations, with an optional leading BAR. *)
constructor_declarations:
  | BAR
      { [] }
  | cs = bar_llist(constructor_declaration)
      { cs }
;
(* A constructor declaration begins with an opening symbol, which can
   be either epsilon or BAR. Note that this opening symbol is included
   in the footprint $sloc. *)
(* Because [constructor_declaration] and [extension_constructor_declaration]
   are identical except for their semantic actions, we introduce the symbol
   [generic_constructor_declaration], whose semantic action is neutral -- it
   merely returns a tuple. *)
generic_constructor_declaration(opening):
  opening
  cid = mkrhs(constr_ident)
  args_res = generalized_constructor_arguments
  attrs = attributes
    {
      let args, res = args_res in
      let info = symbol_info $endpos in
      let loc = make_loc $sloc in
      cid, args, res, attrs, loc, info
    }
;
%inline constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let cid, args, res, attrs, loc, info = d in
      Type.constructor cid ~args ?res ~attrs ~loc ~info
    }
;
str_exception_declaration:
  sig_exception_declaration
    { $1 }
| EXCEPTION
  ext = ext
  attrs1 = attributes
  id = mkrhs(constr_ident)
  EQUAL
  lid = mkrhs(constr_longident)
  attrs2 = attributes
  attrs = post_item_attributes
  { let loc = make_loc $sloc in
    let docs = symbol_docs $sloc in
    Te.mk_exception ~attrs
      (Te.rebind id lid ~attrs:(attrs1 @ attrs2) ~loc ~docs)
    , ext }
;
sig_exception_declaration:
  EXCEPTION
  ext = ext
  attrs1 = attributes
  id = mkrhs(constr_ident)
  args_res = generalized_constructor_arguments
  attrs2 = attributes
  attrs = post_item_attributes
    { let args, res = args_res in
      let loc = make_loc $sloc in
      let docs = symbol_docs $sloc in
      Te.mk_exception ~attrs
        (Te.decl id ~args ?res ~attrs:(attrs1 @ attrs2) ~loc ~docs)
      , ext }
;
%inline let_exception_declaration:
    mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let args, res = $2 in
        Te.decl $1 ~args ?res ~attrs:$3 ~loc:(make_loc $sloc) }
;
generalized_constructor_arguments:
    /*empty*/                     { (Pcstr_tuple [],None) }
  | OF constructor_arguments      { ($2,None) }
  | COLON constructor_arguments MINUSGREATER atomic_type %prec below_HASH
                                  { ($2,Some $4) }
  | COLON atomic_type %prec below_HASH
                                  { (Pcstr_tuple [],Some $2) }
;

constructor_arguments:
  | tys = inline_separated_nonempty_llist(STAR, atomic_type)
    %prec below_HASH
      { Pcstr_tuple tys }
  | LBRACE label_declarations RBRACE
      { Pcstr_record $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declaration_semi                      { [$1] }
  | label_declaration_semi label_declarations   { $1 :: $2 }
;
label_declaration:
    mutable_flag mkrhs(label) COLON poly_type_no_attr attributes
      { let info = symbol_info $endpos in
        Type.field $2 $4 ~mut:$1 ~attrs:$5 ~loc:(make_loc $sloc) ~info }
;
label_declaration_semi:
    mutable_flag mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
      { let info =
          match rhs_info $endpos($5) with
          | Some _ as info_before_semi -> info_before_semi
          | None -> symbol_info $endpos
       in
       Type.field $2 $4 ~mut:$1 ~attrs:($5 @ $7) ~loc:(make_loc $sloc) ~info }
;

/* Type Extensions */

%inline str_type_extension:
  type_extension(extension_constructor)
    { $1 }
;
%inline sig_type_extension:
  type_extension(extension_constructor_declaration)
    { $1 }
;
%inline type_extension(declaration):
  TYPE
  ext = ext
  attrs1 = attributes
  no_nonrec_flag
  params = type_parameters
  tid = mkrhs(type_longident)
  PLUSEQ
  priv = private_flag
  cs = bar_llist(declaration)
  attrs2 = post_item_attributes
    { let docs = symbol_docs $sloc in
      let attrs = attrs1 @ attrs2 in
      Te.mk tid cs ~params ~priv ~attrs ~docs,
      ext }
;
%inline extension_constructor(opening):
    extension_constructor_declaration(opening)
      { $1 }
  | extension_constructor_rebind(opening)
      { $1 }
;
%inline extension_constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let cid, args, res, attrs, loc, info = d in
      Te.decl cid ~args ?res ~attrs ~loc ~info
    }
;
extension_constructor_rebind(opening):
  opening
  cid = mkrhs(constr_ident)
  EQUAL
  lid = mkrhs(constr_longident)
  attrs = attributes
      { let info = symbol_info $endpos in
        Te.rebind cid lid ~attrs ~loc:(make_loc $sloc) ~info }
;

/* "with" constraints (additional type equations over signature components) */

with_constraint:
    TYPE type_parameters mkrhs(label_longident) with_type_binder
    core_type_no_attr constraints
      { let lident = loc_last $3 in
        Pwith_type
          ($3,
           (Type.mk lident
              ~params:$2
              ~cstrs:$6
              ~manifest:$5
              ~priv:$4
              ~loc:(make_loc $sloc))) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE type_parameters mkrhs(label_longident)
    COLONEQUAL core_type_no_attr
      { let lident = loc_last $3 in
        Pwith_typesubst
         ($3,
           (Type.mk lident
              ~params:$2
              ~manifest:$5
              ~loc:(make_loc $sloc))) }
  | MODULE mkrhs(mod_longident) EQUAL mkrhs(mod_ext_longident)
      { Pwith_module ($2, $4) }
  | MODULE mkrhs(mod_longident) COLONEQUAL mkrhs(mod_ext_longident)
      { Pwith_modsubst ($2, $4) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private }
;

/* Polymorphic types */

%inline typevar:
  QUOTE mkrhs(ident)
    { $2 }
;
%inline typevar_list:
  nonempty_llist(typevar)
    { $1 }
;
%inline poly(X):
  typevar_list DOT X
    { Ptyp_poly($1, $3) }
;
possibly_poly(X):
  X
    { $1 }
| mktyp(poly(X))
    { $1 }
;
%inline poly_type:
  possibly_poly(core_type)
    { $1 }
;
%inline poly_type_no_attr:
  possibly_poly(core_type_no_attr)
    { $1 }
;

(* -------------------------------------------------------------------------- *)

(* Core language types. *)

(* A core type (core_type) is a core type without attributes (core_type_no_attr)
   followed with a list of attributes. *)
core_type:
    core_type_no_attr
      { $1 }
  | core_type attribute
      { Typ.attr $1 $2 }
;

(* A core type without attributes is currently defined as an alias type, but
   this could change in the future if new forms of types are introduced. From
   the outside, one should use core_type_no_attr. *)
%inline core_type_no_attr:
  alias_type
    { $1 }
;

(* Alias types include:
   - function types (see below);
   - proper alias types:                  'a -> int as 'a
 *)
alias_type:
    function_type
      { $1 }
  | mktyp(
      ty = alias_type AS QUOTE tyvar = ident
        { Ptyp_alias(ty, tyvar) }
    )
    { $1 }
;

(* Function types include:
   - tuple types (see below);
   - proper function types:               int -> int
                                          foo: int -> int
                                          ?foo: int -> int
 *)
function_type:
  | ty = tuple_type
    %prec MINUSGREATER
      { ty }
  | mktyp(
      label = arg_label
      domain = extra_rhs(tuple_type)
      MINUSGREATER
      codomain = function_type
        { Ptyp_arrow(label, domain, codomain) }
    )
    { $1 }
;
%inline arg_label:
  | label = optlabel
      { Optional label }
  | label = LIDENT COLON
      { Labelled label }
  | /* empty */
      { Nolabel }
;
(* Tuple types include:
   - atomic types (see below);
   - proper tuple types:                  int * int * int list
   A proper tuple type is a star-separated list of at least two atomic types.
 *)
tuple_type:
  | ty = atomic_type
    %prec below_HASH
      { ty }
  | mktyp(
      tys = separated_nontrivial_llist(STAR, atomic_type)
        { Ptyp_tuple tys }
    )
    { $1 }
;

(* Atomic types are the most basic level in the syntax of types.
   Atomic types include:
   - types between parentheses:           (int -> int)
   - first-class module types:            (module S)
   - type variables:                      'a
   - applications of type constructors:   int, int list, int option list
   - variant types:                       [`A]
 *)
atomic_type:
  | LPAREN core_type RPAREN
      { $2 }
  | LPAREN MODULE ext_attributes package_type RPAREN
      { wrap_typ_attrs ~loc:$sloc (reloc_typ ~loc:$sloc $4) $3 }
  | mktyp( /* begin mktyp group */
      QUOTE ident
        { Ptyp_var $2 }
    | UNDERSCORE
        { Ptyp_any }
    | tys = actual_type_parameters
      tid = mkrhs(type_longident)
        { Ptyp_constr(tid, tys) }
    | LESS meth_list GREATER
        { let (f, c) = $2 in Ptyp_object (f, c) }
    | LESS GREATER
        { Ptyp_object ([], Closed) }
    | tys = actual_type_parameters
      HASH
      cid = mkrhs(clty_longident)
        { Ptyp_class(cid, tys) }
    | LBRACKET tag_field RBRACKET
        (* not row_field; see CONFLICTS *)
        { Ptyp_variant([$2], Closed, None) }
    | LBRACKET BAR row_field_list RBRACKET
        { Ptyp_variant($3, Closed, None) }
    | LBRACKET row_field BAR row_field_list RBRACKET
        { Ptyp_variant($2 :: $4, Closed, None) }
    | LBRACKETGREATER BAR? row_field_list RBRACKET
        { Ptyp_variant($3, Open, None) }
    | LBRACKETGREATER RBRACKET
        { Ptyp_variant([], Open, None) }
    | LBRACKETLESS BAR? row_field_list RBRACKET
        { Ptyp_variant($3, Closed, Some []) }
    | LBRACKETLESS BAR? row_field_list GREATER name_tag_list RBRACKET
        { Ptyp_variant($3, Closed, Some $5) }
    | extension
        { Ptyp_extension $1 }
  )
  { $1 } /* end mktyp group */
;

(* This is the syntax of the actual type parameters in an application of
   a type constructor, such as int, int list, or (int, bool) Hashtbl.t.
   We allow one of the following:
   - zero parameters;
   - one parameter:
     an atomic type;
     among other things, this can be an arbitrary type between parentheses;
   - two or more parameters:
     arbitrary types, between parentheses, separated with commas.
 *)
%inline actual_type_parameters:
  | /* empty */
      { [] }
  | ty = atomic_type
      { [ty] }
  | LPAREN tys = separated_nontrivial_llist(COMMA, core_type) RPAREN
      { tys }
;

%inline package_type: module_type
      { let (lid, cstrs, attrs) = package_type_of_module_type $1 in
        let descr = Ptyp_package (lid, cstrs) in
        mktyp ~loc:$sloc ~attrs descr }
;
%inline row_field_list:
  separated_nonempty_llist(BAR, row_field)
    { $1 }
;
row_field:
    tag_field
      { $1 }
  | core_type
      { Rf.inherit_ ~loc:(make_loc $sloc) $1 }
;
tag_field:
    mkrhs(name_tag) OF opt_ampersand amper_type_list attributes
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $5 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 $3 $4 }
  | mkrhs(name_tag) attributes
      { let info = symbol_info $endpos in
        let attrs = add_info_attrs info $2 in
        Rf.tag ~loc:(make_loc $sloc) ~attrs $1 true [] }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
%inline amper_type_list:
  separated_nonempty_llist(AMPERSAND, core_type_no_attr)
    { $1 }
;
%inline name_tag_list:
  nonempty_llist(name_tag)
    { $1 }
;
(* A method list (in an object type). *)
meth_list:
    head = field_semi         tail = meth_list
  | head = inherit_field SEMI tail = meth_list
      { let (f, c) = tail in (head :: f, c) }
  | head = field_semi
  | head = inherit_field SEMI
      { [head], Closed }
  | head = field
  | head = inherit_field
      { [head], Closed }
  | DOTDOT
      { [], Open }
;
%inline field:
  mkrhs(label) COLON poly_type_no_attr attributes
    { let info = symbol_info $endpos in
      let attrs = add_info_attrs info $4 in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

%inline field_semi:
  mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
    { let info =
        match rhs_info $endpos($4) with
        | Some _ as info_before_semi -> info_before_semi
        | None -> symbol_info $endpos
      in
      let attrs = add_info_attrs info ($4 @ $6) in
      Of.tag ~loc:(make_loc $sloc) ~attrs $1 $3 }
;

%inline inherit_field:
  ty = atomic_type
    { Of.inherit_ ~loc:(make_loc $sloc) ty }
;

%inline label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
  | INT          { let (n, m) = $1 in Pconst_integer (n, m) }
  | CHAR         { Pconst_char $1 }
  | STRING       { let (s, strloc, d) = $1 in Pconst_string (s, strloc, d) }
  | FLOAT        { let (f, m) = $1 in Pconst_float (f, m) }
;
signed_constant:
    constant     { $1 }
  | MINUS INT    { let (n, m) = $2 in Pconst_integer("-" ^ n, m) }
  | MINUS FLOAT  { let (f, m) = $2 in Pconst_float("-" ^ f, m) }
  | PLUS INT     { let (n, m) = $2 in Pconst_integer (n, m) }
  | PLUS FLOAT   { let (f, m) = $2 in Pconst_float(f, m) }
;

/* Identifiers and long identifiers */

ident:
    UIDENT                    { $1 }
  | LIDENT                    { $1 }
;
val_extra_ident:
  | LPAREN operator RPAREN    { $2 }
  | LPAREN operator error     { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN error              { expecting $loc($2) "operator" }
  | LPAREN MODULE error       { expecting $loc($3) "module-expr" }
;
val_ident:
    LIDENT                    { $1 }
  | val_extra_ident           { $1 }
;
operator:
    PREFIXOP                                    { $1 }
  | LETOP                                       { $1 }
  | ANDOP                                       { $1 }
  | DOTOP LPAREN index_mod RPAREN               { "."^ $1 ^"(" ^ $3 ^ ")" }
  | DOTOP LPAREN index_mod RPAREN LESSMINUS     { "."^ $1 ^ "(" ^ $3 ^ ")<-" }
  | DOTOP LBRACKET index_mod RBRACKET           { "."^ $1 ^"[" ^ $3 ^ "]" }
  | DOTOP LBRACKET index_mod RBRACKET LESSMINUS { "."^ $1 ^ "[" ^ $3 ^ "]<-" }
  | DOTOP LBRACE index_mod RBRACE               { "."^ $1 ^"{" ^ $3 ^ "}" }
  | DOTOP LBRACE index_mod RBRACE LESSMINUS     { "."^ $1 ^ "{" ^ $3 ^ "}<-" }
  | HASHOP                                      { $1 }
  | BANG                                        { "!" }
  | infix_operator                              { $1 }
;
%inline infix_operator:
  | op = INFIXOP0 { op }
  | op = INFIXOP1 { op }
  | op = INFIXOP2 { op }
  | op = INFIXOP3 { op }
  | op = INFIXOP4 { op }
  | PLUS           {"+"}
  | PLUSDOT       {"+."}
  | PLUSEQ        {"+="}
  | MINUS          {"-"}
  | MINUSDOT      {"-."}
  | STAR           {"*"}
  | PERCENT        {"%"}
  | EQUAL          {"="}
  | LESS           {"<"}
  | GREATER        {">"}
  | OR            {"or"}
  | BARBAR        {"||"}
  | AMPERSAND      {"&"}
  | AMPERAMPER    {"&&"}
  | COLONEQUAL    {":="}
;
index_mod:
| { "" }
| SEMI DOTDOT { ";.." }
;

%inline constr_extra_ident:
  | LPAREN COLONCOLON RPAREN                    { "::" }
;
constr_extra_nonprefix_ident:
  | LBRACKET RBRACKET                           { "[]" }
  | LPAREN RPAREN                               { "()" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;
constr_ident:
    UIDENT                                      { $1 }
  | constr_extra_ident                          { $1 }
  | constr_extra_nonprefix_ident                { $1 }
;
constr_longident:
    mod_longident       %prec below_DOT  { $1 } /* A.B.x vs (A).B.x */
  | mod_longident DOT constr_extra_ident { Ldot($1,$3) }
  | constr_extra_ident                   { Lident $1 }
  | constr_extra_nonprefix_ident         { Lident $1 }
;
mk_longident(prefix,final):
   | final            { Lident $1 }
   | prefix DOT final { Ldot($1,$3) }
;
val_longident:
    mk_longident(mod_longident, val_ident) { $1 }
;
label_longident:
    mk_longident(mod_longident, LIDENT) { $1 }
;
type_longident:
    mk_longident(mod_ext_longident, LIDENT)  { $1 }
;
mod_longident:
    mk_longident(mod_longident, UIDENT)  { $1 }
;
mod_ext_longident:
    mk_longident(mod_ext_longident, UIDENT) { $1 }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN
      { lapply ~loc:$sloc $1 $3 }
  | mod_ext_longident LPAREN error
      { expecting $loc($3) "module path" }
;
mty_longident:
    mk_longident(mod_ext_longident,ident) { $1 }
;
clty_longident:
    mk_longident(mod_ext_longident,LIDENT) { $1 }
;
class_longident:
   mk_longident(mod_longident,LIDENT) { $1 }
;

/* For compiler-libs: parse all valid longidents and a little more:
   final identifiers which are value specific are accepted even when
   the path prefix is only valid for types: (e.g. F(X).(::)) */
any_longident:
  | mk_longident (mod_ext_longident,
     ident | constr_extra_ident | val_extra_ident { $1 }
    ) { $1 }
  | constr_extra_nonprefix_ident { Lident $1 }
;

/* Toplevel directives */

toplevel_directive:
  HASH dir = mkrhs(ident)
  arg = ioption(mk_directive_arg(toplevel_directive_argument))
    { mk_directive ~loc:$sloc dir arg }
;

%inline toplevel_directive_argument:
  | STRING        { let (s, _, _) = $1 in Pdir_string s }
  | INT           { let (n, m) = $1 in Pdir_int (n ,m) }
  | val_longident { Pdir_ident $1 }
  | mod_longident { Pdir_ident $1 }
  | FALSE         { Pdir_bool false }
  | TRUE          { Pdir_bool true }
;

/* Miscellaneous */

(* The symbol epsilon can be used instead of an /* empty */ comment. *)
%inline epsilon:
  /* empty */
    { () }
;

%inline raw_string:
  s = STRING
    { let body, _, _ = s in body }
;

name_tag:
    BACKQUOTE ident                             { $2 }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
%inline nonrec_flag:
    /* empty */                                 { Recursive }
  | NONREC                                      { Nonrecursive }
;
%inline no_nonrec_flag:
    /* empty */ { Recursive }
  | NONREC      { not_expecting $loc "nonrec flag" }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
  inline_private_flag
    { $1 }
;
%inline inline_private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
mutable_virtual_flags:
    /* empty */
      { Immutable, Concrete }
  | MUTABLE
      { Mutable, Concrete }
  | VIRTUAL
      { Immutable, Virtual }
  | MUTABLE VIRTUAL
  | VIRTUAL MUTABLE
      { Mutable, Virtual }
;
private_virtual_flags:
    /* empty */  { Public, Concrete }
  | PRIVATE { Private, Concrete }
  | VIRTUAL { Public, Virtual }
  | PRIVATE VIRTUAL { Private, Virtual }
  | VIRTUAL PRIVATE { Private, Virtual }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a MUTABLE keyword. *)
virtual_with_mutable_flag:
  | VIRTUAL { Immutable }
  | MUTABLE VIRTUAL { Mutable }
  | VIRTUAL MUTABLE { Mutable }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a PRIVATE keyword. *)
virtual_with_private_flag:
  | VIRTUAL { Public }
  | PRIVATE VIRTUAL { Private }
  | VIRTUAL PRIVATE { Private }
;
%inline no_override_flag:
    /* empty */                                 { Fresh }
;
%inline override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;
optlabel:
   | OPTLABEL                                   { $1 }
   | QUESTION LIDENT COLON                      { $2 }
;

/* Attributes and extensions */

single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | MATCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | NONREC { "nonrec" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
  mkloc(
      single_attr_id { $1 }
    | single_attr_id DOT attr_id { $1 ^ "." ^ $3.txt }
  ) { $1 }
;
attribute:
  LBRACKETAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
post_item_attribute:
  LBRACKETATAT attr_id payload RBRACKET
    { Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
floating_attribute:
  LBRACKETATATAT attr_id payload RBRACKET
    { mark_symbol_docs $sloc;
      Attr.mk ~loc:(make_loc $sloc) $2 $3 }
;
%inline post_item_attributes:
  post_item_attribute*
    { $1 }
;
%inline attributes:
  attribute*
    { $1 }
;
ext:
  | /* empty */     { None }
  | PERCENT attr_id { Some $2 }
;
%inline no_ext:
  | /* empty */     { None }
  | PERCENT attr_id { not_expecting $loc "extension" }
;
%inline ext_attributes:
  ext attributes    { $1, $2 }
;
extension:
  | LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
  | QUOTED_STRING_EXPR
    { mk_quotedext ~loc:$sloc $1 }
;
item_extension:
  | LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
  | QUOTED_STRING_ITEM
    { mk_quotedext ~loc:$sloc $1 }
;
payload:
    structure { PStr $1 }
  | COLON signature { PSig $2 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, None) }
  | QUESTION pattern WHEN seq_expr { PPat ($2, Some $4) }
;
%%
