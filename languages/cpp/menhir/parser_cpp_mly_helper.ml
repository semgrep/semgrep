open Common
open Ast_cpp
open Either_
module Ast = Ast_cpp
module Flag = Flag_parsing
module Log = Log_parser_cpp.Log

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(* TODO: switch to use logger *)
let warning s v =
  if !Flag.verbose_parsing then Common2.warning ("PARSING: " ^ s) v else v

let error s tok = raise (Parsing_error.Other_error (s, tok))
let fake s = Tok.fake_tok s

(*****************************************************************************)
(* Parse helpers functions *)
(*****************************************************************************)

(*-------------------------------------------------------------------------- *)
(* Type related *)
(*-------------------------------------------------------------------------- *)

type storage_opt = NoSto | StoTypedef of tok | Sto of storage wrap
type shortLong = Short of tok | Long of tok | LongLong of tok * tok
type sign = Signed of tok | UnSigned of tok

type decl = {
  storageD : storage_opt;
  typeD : sign option * shortLong option * typeC option;
  qualifD : type_qualifiers;
  modifierD : modifier list;
}

let ii_of_sign = function
  | Signed ii
  | UnSigned ii ->
      ii

let ii_of_short_long = function
  | Short ii
  | Long ii
  | LongLong (ii, _) ->
      ii

let nullDecl =
  {
    storageD = NoSto;
    typeD = (None, None, None);
    qualifD = Ast.nQ;
    modifierD = [];
  }

let addStorageD x decl =
  match decl with
  | { storageD = NoSto; _ } -> { decl with storageD = x }
  | { storageD = (StoTypedef ii | Sto (_, ii)) as y; _ } ->
      if x =*= y then decl |> warning "duplicate storage classes"
      else error "multiple storage classes" ii

let addModifierD x decl =
  (* old: check: warning "duplicate inline" *)
  { decl with modifierD = x :: decl.modifierD }

let addTypeD ty decl =
  match (ty, decl) with
  | Left3 (Signed _), { typeD = Some (Signed _), _b, _c; _ } ->
      decl |> warning "duplicate 'signed'"
  | Left3 (UnSigned _), { typeD = Some (UnSigned _), _b, _c; _ } ->
      decl |> warning "duplicate 'unsigned'"
  | Left3 sign, { typeD = Some _, _b, _c; _ } ->
      error "both signed and unsigned specified" (ii_of_sign sign)
  | Left3 x, { typeD = None, b, c; _ } -> { decl with typeD = (Some x, b, c) }
  | Middle3 (Short _), { typeD = _a, Some (Short _), _c; _ } ->
      decl |> warning "duplicate 'short'"
  (* gccext: long long allowed *)
  | Middle3 (Long t1), { typeD = a, Some (Long t2), c; _ } ->
      { decl with typeD = (a, Some (LongLong (t1, t2)), c) }
  | Middle3 (Long _), { typeD = _a, Some (LongLong _), _c; _ } ->
      decl |> warning "triplicate 'long'"
  | Middle3 sl, { typeD = _a, Some _, _c; _ } ->
      error "both long and short specified" (ii_of_short_long sl)
  | Middle3 x, { typeD = a, None, c; _ } -> { decl with typeD = (a, Some x, c) }
  | Right3 _t, { typeD = _a, _b, Some _; _ } ->
      (* old: was error before, but tedious to get an ii for error *)
      decl |> warning "two or more data types"
  | Right3 t, { typeD = a, b, None; _ } -> { decl with typeD = (a, b, Some t) }

let addQualif tq1 tq2 =
  (* old: check: warning "duplicate 'const'", warning "duplicate 'volatile'"*)
  tq1 :: tq2

let addQualifD qu qu2 = { qu2 with qualifD = addQualif qu qu2.qualifD }

(*-------------------------------------------------------------------------- *)
(* Declaration/Function related *)
(*-------------------------------------------------------------------------- *)

(* stdC: type section, basic integer types (and ritchie)
 * To understand the code, just look at the result (right part of the PM)
 * and go back.
 * old: before TSized and TPrimitive, when there was a complex TBase
 * there was more checks:
 *  error "signed, unsigned valid only for char and int" (Common.hd_exn "unexpected empty list" iit)
 *  error "long or short specified with floatint type" (Common.hd_exn "unexpected empty list" iit)
 *  error "the only valid combination is long double" (Common.hd_exn "unexpected empty list" iit)
 *  error "long, short valid only for int or float" (Common.hd_exn "unexpected empty list" iit)
 *
 * if do short uint i, then gcc say parse error, strange ? it is
 * not a parse error, it is just that we dont allow with typedef
 * either short/long or signed/unsigned. In fact, with
 * parse_typedef_fix2 (with et() and dt()) now I say too parse
 * error so this code is executed only when do short struct
 * {....} and never with a typedef cos now we parse short uint i
 * as short ident ident => parse error (cos after first short i
 * pass in dt() mode)
 *)
let type_and_storage_from_decl
    { storageD = st; qualifD = qu; typeD = ty; modifierD = mods } =
  let ty =
    match ty with
    | None, None, None -> (
        (* c++ext: *)
        match st with
        | Sto (Auto, ii) -> TAuto ii
        | _ ->
            (* old: error "no type (could default to 'int')" (Common.hd_exn "unexpected empty list" iit) *)
            TPrimitive (TInt, Tok.unsafe_fake_tok "int"))
    | None, None, Some t -> t
    | sign_opt, short_long_opt, topt ->
        let sign =
          match sign_opt with
          | None -> []
          | Some (Signed t) -> [ (TSigned, t) ]
          | Some (UnSigned t) -> [ (TUnsigned, t) ]
        in
        let short_long =
          match short_long_opt with
          | None -> []
          | Some (Short t) -> [ (TShort, t) ]
          | Some (Long t) -> [ (TLong, t) ]
          | Some (LongLong (t1, t2)) -> [ (TLong, t1); (TLong, t2) ]
        in
        let typ =
          match topt with
          | None -> None
          | Some typc -> Some (nQ, typc)
        in
        TSized (sign @ short_long, typ)
  in
  ((qu, ty), st, mods)

let id_of_dname_for_typedef dname =
  match dname with
  | DN (None, [], IdIdent id) -> id
  | _ -> error "expecting an ident for typedef" (ii_of_dname dname)

let make_onedecl ~v_namei ~mods ~sto v_type : onedecl =
  let specs = mods |> List_.map (fun m -> M m) in
  match v_namei with
  (* less: could check sto, because typedef can't be anonymous since c++17
   * lesS: use mods?
   *)
  | None -> EmptyDecl v_type
  | Some (dn, iniopt) -> (
      match sto with
      | StoTypedef t ->
          (* less: use mods? *)
          let id = id_of_dname_for_typedef dn in
          TypedefDecl (t, v_type, id)
      | NoSto
      | Sto _ -> (
          let more_specs =
            match sto with
            | NoSto -> []
            | Sto sto -> [ ST sto ]
            | _ -> raise Impossible
          in
          match (dn, iniopt) with
          | DN n, _ ->
              V
                ( { name = n; specs = specs @ more_specs },
                  { v_init = iniopt; v_type } )
          | DNStructuredBinding (l, (id, ids), r), Some ini ->
              StructuredBinding (v_type, (l, id :: ids, r), ini)
          | DNStructuredBinding _ids, None ->
              error "expecting an init for structured_binding" (ii_of_dname dn))
      )

let type_and_specs_from_decl decl =
  let { storageD = st; _ } = decl in
  let t, _storage, _inline = type_and_storage_from_decl decl in
  match st with
  | NoSto -> (t, [])
  | Sto (Register, ii) -> (t, [ ST (Register, ii) ])
  | StoTypedef ii
  | Sto (_, ii) ->
      error "storage class specified for parameter of function" ii

let fixNameForParam ii (name, ftyp) =
  match name with
  | None, [], IdIdent id -> (id, ftyp)
  | _ -> error "parameter have qualifier" ii

let type_and_storage_for_funcdef_from_decl decl =
  let returnType, storage, _inline = type_and_storage_from_decl decl in
  match storage with
  | StoTypedef tok -> error "function definition declared 'typedef'" tok
  | _x -> (returnType, storage)

(*
 * this function is used for func definitions (not declarations).
 * In that case we must have a name for the parameter.
 * This function ensures that we give only parameterTypeDecl with well
 * formed Classic constructor.
 *
 * todo?: do we accept other declaration in ?
 * so I must add them to the compound of the deffunc. I dont
 * have to handle typedef pb here cos C forbid to do VF f { ... }
 * with VF a typedef of func cos here we dont see the name of the
 * argument (in the typedef)
 *)
let fixOldCDecl ii (ty : type_) : type_ =
  match snd ty with
  | TFunction { ft_params = params; _ } -> (
      (* stdC: If the prototype declaration declares a parameter for a
       * function that you are defining (it is part of a function
       * definition), then you must write a name within the declarator.
       * Otherwise, you can omit the name. *)
      match Ast.unparen params with
      | [ P { p_name = None; p_type = ty2; _ } ] -> (
          match Ast.unwrap_typeC ty2 with
          | TPrimitive (TVoid, _) -> ty
          | _ ->
              (* less: there is some valid case actually, when use interfaces
               * and generic callbacks where specific instances do not
               * need the extra parameter (happens a lot in plan9).
               * Maybe this check is better done in a scheck for C.
                   let info = Lib_parsing_cpp.ii_of_any (Type ty2) +> Common.hd_exn "unexpected empty list" in
                   pr2 (spf "SEMANTIC: parameter name omitted (but I continue) at %s"
                         (Parse_info.string_of_info info)
                   );
               *)
              ty)
      | params ->
          params
          |> List.iter (fun param ->
                 match param with
                 | P { p_name = None; p_type = _ty2; _ } ->
                     (* see above
                        let info = Lib_parsing_cpp.ii_of_any (Type ty2) +> Common.hd_exn "unexpected empty list" in
                        (* if majuscule, then certainly macro-parameter *)
                        pr2 (spf "SEMANTIC: parameter name omitted (but I continue) at %s"
                            (Parse_info.string_of_info info)
                        );
                     *)
                     ()
                 | _ -> ());
          ty)
  (* todo? can we declare prototype in the decl or structdef,
   *  ... => length <> but good kan meme
   *)
  | _ ->
      (* gcc says parse error but I dont see why *)
      error "seems this is not a function" ii

(* TODO: this is ugly ... use record! *)
let fixFunc ((name, ty, _stoTODO), cp) : func_definition =
  let ent = { name; specs = [] } in
  let ftyp =
    match ty with
    | aQ, TFunction ({ ft_params = params; _ } as ftyp) ->
        (* it must be nullQualif, cos parser construct only this *)
        assert (aQ =*= nQ);

        (match Ast.unparen params with
        | [ P { p_name = None; p_type = ty2; _ } ] -> (
            match Ast.unwrap_typeC ty2 with
            | TPrimitive (TVoid, _) -> ()
            (* failwith "internal errror: fixOldCDecl not good" *)
            | _ -> ())
        | params ->
            params
            |> List.iter (function
                 | P { p_name = Some _s; _ } -> ()
                 (* failwith "internal errror: fixOldCDecl not good" *)
                 | _ -> ()));
        ftyp
    | _ ->
        Log.warn (fun m ->
            m "weird, not a functionType. Got %s" (Ast_cpp.show_type_ ty));
        (* this is possible if someone used a typedef to a function type, or
         * when tree-sitter-cpp did some error recovery and wrongly parsed
         * something as a function when it's really not
         *)
        {
          ft_params = Tok.unsafe_fake_bracket [];
          ft_ret = ty;
          ft_specs = [];
          ft_const = None;
          ft_throw = [];
          ft_requires = None;
        }
  in
  ( ent,
    {
      f_type = ftyp;
      (* TODO move in f_specs f_storage = sto; *) f_body = cp;
      f_specs = [];
    } )

let fixFieldOrMethodDecl (xs, semicolon) : class_member =
  match xs with
  | [ V (ent, { v_init; v_type = _q, TFunction ft }) ] ->
      (* todo? define another type instead of onedecl? *)
      let fbody =
        match v_init with
        | None -> FBDecl semicolon
        | Some (EqInit (tokeq, InitExpr (C (Int ((_, tk) as pi)))))
          when Parsed_int.eq_const pi 0 ->
            FBZero (tokeq, tk, semicolon)
        | _ -> error "can't assign expression to method decl" semicolon
      in
      let def = { f_type = ft; f_body = fbody; f_specs = [] } in
      F (Func (ent, def))
  | _ -> F (DeclList (xs, semicolon))

(*-------------------------------------------------------------------------- *)
(* shortcuts *)
(*-------------------------------------------------------------------------- *)
(* used only in the .dyp now *)
let mk_e e = e
let mk_funcall e1 args = Call (e1, args)

let mk_constructor specs id (lp, params, rp) cp =
  let params = List_.optlist_to_list params in
  let ftyp =
    {
      ft_ret = (nQ, TPrimitive (TVoid, snd id));
      ft_params = (lp, params, rp);
      ft_specs = [];
      (* TODO *)
      ft_const = None;
      ft_throw = [];
      ft_requires = None;
    }
  in
  let name = name_of_id id in
  let ent = { name; specs } in
  (ent, { f_type = ftyp; f_body = cp; f_specs = [] })

let mk_destructor specs tilde id (lp, _voidopt, rp) exnopt cp =
  let ftyp =
    {
      ft_ret = (nQ, TPrimitive (TVoid, snd id));
      ft_params = (lp, [], rp);
      ft_specs = [];
      ft_const = None;
      ft_throw = Option.to_list exnopt;
      ft_requires = None;
    }
  in
  let name = (None, noQscope, IdDestructor (tilde, id)) in
  let ent = { name; specs } in
  (ent, { f_type = ftyp; f_body = cp; f_specs = [] })
