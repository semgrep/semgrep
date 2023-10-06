(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open AST_generic
module G = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to build or convert AST_generic elements.
 *
 * The very often used helper functions are actually in AST_generic.ml at
 * the end (e.g., AST_generic.basic_entity).
 * This module is for the more rarely used helpers.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_ident = fst

let name_of_ids_with_opt_typeargs xs =
  match List.rev xs with
  | [] -> failwith "name_of_ids_with_opt_typeargs: empty ids"
  | [ (x, None) ] -> Id (x, empty_id_info ())
  | (x, topt) :: xs ->
      let qualif = if xs =*= [] then None else Some (QDots (xs |> List.rev)) in
      IdQualified
        {
          name_last = (x, topt);
          name_middle = qualif;
          name_top = None;
          name_info = empty_id_info ();
        }

(* internal *)
let name_to_qualifier = function
  | Id (id, _idinfo) -> [ (id, None) ]
  | IdQualified { name_last = id, topt; name_middle = qu; _ } ->
      let rest =
        match qu with
        | None -> []
        | Some (QDots xs) -> xs
        (* TODO, raise exn? *)
        | Some (QExpr _) -> []
      in
      rest @ [ (id, topt) ]

(* used for Parse_csharp_tree_sitter.ml
 * less: could move there? *)
let add_id_opt_type_args_to_name name (id, topt) =
  let qdots = name_to_qualifier name in
  IdQualified
    {
      name_last = (id, topt);
      name_middle = Some (QDots qdots);
      name_top = None;
      name_info = empty_id_info () (* TODO reuse from name?*);
    }

(* used for Parse_hack_tree_sitter.ml
 * less: could move there? *)
let add_type_args_to_name name type_args =
  match name with
  | Id (ident, id_info) ->
      (* Only IdQualified supports typeargs *)
      IdQualified
        {
          name_last = (ident, Some type_args);
          name_middle = None;
          name_top = None;
          name_info = id_info;
        }
  | IdQualified qualified_info -> (
      match qualified_info.name_last with
      | _id, Some _x ->
          IdQualified qualified_info
          (* TODO: Enable raise Impossible *)
          (* raise Impossible *)
          (* Never should have to overwrite type args, but also doesn't make sense to merge *)
      | id, None ->
          IdQualified { qualified_info with name_last = (id, Some type_args) })

let add_type_args_opt_to_name name topt =
  match topt with
  | None -> name
  | Some t -> add_type_args_to_name name t

let name_of_ids ?(case_insensitive = false) xs =
  match List.rev xs with
  | [] -> failwith "name_of_ids: empty ids"
  | [ x ] -> Id (x, empty_id_info ~case_insensitive ())
  | x :: xs ->
      let qualif =
        if xs =*= [] then None
        else Some (QDots (xs |> List.rev |> Common.map (fun id -> (id, None))))
      in
      IdQualified
        {
          name_last = (x, None);
          name_middle = qualif;
          name_top = None;
          name_info = empty_id_info ();
        }

let add_suffix_to_name suffix name =
  match name with
  | Id (first_id, _) -> name_of_ids [ first_id; suffix ]
  | IdQualified ({ name_last; name_middle; _ } as q_info) ->
      let new_name_middle =
        match name_middle with
        | Some (QDots qualifiers) -> Some (G.QDots (qualifiers @ [ name_last ]))
        | Some (QExpr (expr, tok)) ->
            Some (G.QExpr ({ expr with e = N name }, tok))
        | None -> Some (G.QDots [ name_last ])
      in
      IdQualified
        {
          q_info with
          name_last = (suffix, None);
          name_middle = new_name_middle;
        }

let name_of_id ?(case_insensitive = false) id =
  Id (id, empty_id_info ~case_insensitive ())

let name_of_dot_access e =
  let rec fetch_ids = function
    | G.N (G.Id (x, _)) -> Some [ x ]
    | G.N (G.IdQualified { name_last; name_middle; _ }) ->
        let* name_middle =
          match name_middle with
          | None -> Some []
          | Some (QDots xs) -> Some (List.map (fun (id, _) -> id) xs)
          | Some (QExpr _) -> None
        in
        let name_last = fst name_last in
        Some (name_middle @ [ name_last ])
    | G.DotAccess (e1, _, G.FN (G.Id (x, _))) ->
        let* xs = fetch_ids e1.e in
        Some (xs @ [ x ])
    | ___else___ -> None
  in
  let* xs = fetch_ids e.e in
  Some (name_of_ids xs)

(* TODO: you should not need to use that. This is mostly because
 * Constructor and PatConstructor currently takes a dotted_ident instead
 * of a name, and because module_name accepts only DottedName
 * but C# and C++ allow names.
 *)
let dotted_ident_of_name (n : name) : dotted_ident =
  match n with
  | Id (id, _) -> [ id ]
  | IdQualified { name_last = ident, _toptTODO; name_middle; _ } ->
      let before =
        match name_middle with
        (* we skip the type parts in ds ... *)
        | Some (QDots ds) -> ds |> Common.map fst
        | Some (QExpr _) ->
            logger#error "unexpected qualifier type";
            []
        | None -> []
      in
      before @ [ ident ]

let expr_to_stmt e =
  match e.e with
  | StmtExpr stmt -> stmt
  | _ -> ExprStmt (e, sc) |> G.s

(* In Go/Swift a pattern can be a complex expressions. It is just
 * matched for equality with the thing it's matched against, so in that
 * case it should be a pattern like | _ when expr = x.
 * For Python you can actually have a PatDisj of exception classes.
 * coupling: see pattern_to_expr below
 *)
let rec expr_to_pattern e =
  match e.e with
  | N (Id (id, info)) -> PatId (id, info)
  | Container (Tuple, (t1, xs, t2)) ->
      PatTuple (t1, xs |> Common.map expr_to_pattern, t2)
  | L l -> PatLiteral l
  | Container (List, (t1, xs, t2)) ->
      PatList (t1, xs |> Common.map expr_to_pattern, t2)
  | Ellipsis t -> PatEllipsis t
  (* TODO:  PatKeyVal and more *)
  | _ -> OtherPat (("ExprToPattern", fake ""), [ E e ])

exception NotAnExpr

(* sgrep: this is to treat pattern metavars as expr metavars *)
let rec pattern_to_expr p =
  (match p with
  | PatId (id, info) -> N (Id (id, info))
  | PatTuple (t1, xs, t2) ->
      Container (Tuple, (t1, xs |> Common.map pattern_to_expr, t2))
  | PatLiteral l -> L l
  | PatList (t1, xs, t2) ->
      Container (List, (t1, xs |> Common.map pattern_to_expr, t2))
  | OtherPat (("ExprToPattern", _), [ E e ]) -> e.e
  | _ -> raise NotAnExpr)
  |> G.e

(* Primarily for usage in converting an Assign into a DefStmt. We fail to
 * produce an entity name if we don't definitely have something which is
 * a name.
 *)
let expr_to_entity_name_opt (e : G.expr) : G.entity_name option =
  match e.G.e with
  | N name -> Some (EN name)
  | _ -> None

(* We would like to do more things here, like transform certain
 * N in TyN, but we can't do that from the Xxx_to_generic.ml
 * (e.g., Python_to_generic.ml). Indeed, certain transformations
 * require Naming_AST to have correctly resolved certain Ids.
 * See Graph_code_AST_xxx.expr_to_type_after_naming() below for that situation.
 *)
let expr_to_type e = TyExpr e |> G.t

(* TODO: recognize foo(args)? like in Kotlin/Java *)
let expr_to_class_parent e : class_parent = (expr_to_type e, None)

(* See also exprstmt, and stmt_to_expr in AST_generic.ml *)

let cond_to_expr = function
  | Cond e -> e
  | OtherCond (categ, xs) -> OtherExpr (categ, xs) |> G.e

(* old: there was a stmt_to_item before *)
(* old: there was a stmt_to_field before *)

(* see also Java_to_generic.entity_to_param *)
(* see also Python_to_generic.expr_to_attribute *)
(* see also Php_generic.list_expr_to_opt *)
(* see also Php_generic.name_of_qualified_ident (also in Java) *)

let opt_to_label_ident = function
  | None -> LNone
  | Some id -> LId id

(* used in abstract interpreter and type for PHP where we now reuse
 * 'AST_generic.arithmetic_operator' above *)
(*
let is_boolean_operator = function
  | Plus (* unary too *) | Minus (* unary too *)
  | Mult | Div | Mod
  | Pow | FloorDiv | MatMult (* Python *)
  | LSL | LSR | ASR (* L = logic, A = Arithmetic, SL = shift left *)
  | BitOr | BitXor | BitAnd | BitNot | BitClear (* unary *)
  | Range | Nullish | NotNullPostfix | Elvis | Length
  | RangeInclusive
    -> false
  | And | Or | Xor | Not
  | Eq     | NotEq
  | PhysEq | NotPhysEq
  | Lt | LtE | Gt | GtE
  | Cmp | Concat | Append
  | RegexpMatch | NotMatch
  | In | NotIn | Is | NotIs
    -> true
*)

(* this will be used for the lhs of an Assign. Maybe one day Assign
 * will have a more precise type and we can be more precise too here.
 *)
let entity_name_to_expr name idinfo_opt =
  match (name, idinfo_opt) with
  (* assert idinfo = _idinfo below? *)
  | EN (Id (id, idinfo)), None -> N (Id (id, idinfo)) |> G.e
  | EN (Id (id, _idinfo)), Some idinfo -> N (Id (id, idinfo)) |> G.e
  | EN (IdQualified n), None -> N (IdQualified n) |> G.e
  | EN (IdQualified n), Some idinfo ->
      N (IdQualified { n with name_info = idinfo }) |> G.e
  | EDynamic e, _ -> e
  | EPattern pat, _ -> G.OtherExpr (("EPattern", fake ""), [ P pat ]) |> G.e
  | OtherEntity (categ, xs), _ -> G.OtherExpr (categ, xs) |> G.e

let argument_to_expr arg =
  match arg with
  | Arg e -> e
  | ArgKwd (id, e)
  | ArgKwdOptional (id, e) ->
      let n = name_of_id id in
      let k = N n |> G.e in
      G.keyval k (fake "") e
  | ArgType _
  | OtherArg _ ->
      raise NotAnExpr

(* used in controlflow_build and semgrep *)
let vardef_to_assign (ent, def) =
  let name_or_expr = entity_name_to_expr ent.name None in
  let v =
    match def.vinit with
    | Some v -> v
    | None -> L (Null (Tok.unsafe_fake_tok "null")) |> G.e
  in
  Assign (name_or_expr, Tok.unsafe_fake_tok "=", v) |> G.e

let assign_to_vardef_opt ((e1, _tk, e2) : G.expr * G.tok * G.expr) =
  match e1.G.e with
  | Cast (ty, _, e) ->
      let* name = expr_to_entity_name_opt e in
      let ent = { name; attrs = []; tparams = [] } in
      Some (DefStmt (ent, VarDef { vinit = Some e2; vtype = Some ty }) |> G.s)
  | _ ->
      let* name = expr_to_entity_name_opt e1 in
      let ent = { name; attrs = []; tparams = [] } in
      Some (DefStmt (ent, VarDef { vinit = Some e2; vtype = None }) |> G.s)

(* used in controlflow_build *)
let funcdef_to_lambda (ent, def) resolved =
  let idinfo = { (empty_id_info ()) with id_resolved = ref resolved } in
  let name_or_expr = entity_name_to_expr ent.name (Some idinfo) in
  let v = Lambda def |> G.e in
  Assign (name_or_expr, Tok.unsafe_fake_tok "=", v) |> G.e

let funcbody_to_stmt = function
  | FBStmt st -> st
  | FBExpr e -> G.exprstmt e
  | FBDecl sc -> Block (sc, [], sc) |> G.s
  | FBNothing -> Block (Tok.unsafe_fake_bracket []) |> G.s

let has_keyword_attr kwd attrs =
  attrs
  |> List.exists (function
       | KeywordAttr (kwd2, _) -> kwd =*= kwd2
       | _ -> false)

let name_is_global = function
  | Global (* OSS *)
  | GlobalName _ (* Pro *)
  | EnclosedVar (* OSS / a class variable *) ->
      true
  | LocalVar
  | Parameter
  | ImportedEntity _
  | ImportedModule _
  | TypeName
  | Macro
  | EnumConstant ->
      false

(* just used in cpp_to_generic.ml for now, could be moved there *)
let parameter_to_catch_exn_opt p =
  match p with
  | Param p -> Some (CatchParam p)
  | ParamEllipsis t -> Some (CatchPattern (PatEllipsis t))
  | ParamPattern p -> Some (G.CatchPattern p)
  (* TODO: valid in exn spec? *)
  | ParamRest (_, _p)
  | ParamHashSplat (_, _p) ->
      None
  | ParamReceiver _
  | OtherParam _ ->
      None

(*****************************************************************************)
(* Abstract position and svalue for comparison *)
(*****************************************************************************)

(* update: you should now use AST_generic.equal_any which internally
 * does not care about position information.
 * TODO: can we remove this function now then?
 *)
let abstract_for_comparison_visitor =
  object
    inherit [_] AST_generic.map_legacy as super
    method! visit_tok _env _i = Tok.Ab

    method! visit_id_info env ii =
      super#visit_id_info env { ii with AST_generic.id_svalue = ref None }
  end

let abstract_for_comparison_any x =
  abstract_for_comparison_visitor#visit_any () x

(*****************************************************************************)
(* Associative-Commutative (AC) matching *)
(*****************************************************************************)

let is_associative_operator op =
  match op with
  | Or
  | And
  | BitOr
  | BitAnd
  | BitXor
  | Concat ->
      true
  (* TODO: Plus, Mult, ... *)
  | __else__ -> false

let ac_matching_nf op args =
  (* yes... here we use exceptions like a "goto" to avoid the option monad *)
  let rec nf args1 =
    args1
    |> Common.map (function
         | Arg e -> e
         | ArgKwd _
         | ArgKwdOptional _
         | ArgType _
         | OtherArg _ ->
             raise_notrace Exit)
    |> Common.map nf_one |> List.flatten
  and nf_one e =
    match e.e with
    | Call ({ e = IdSpecial (Op op1, _tok1); _ }, (_, args1, _)) when op =*= op1
      ->
        nf args1
    | _ -> [ e ]
  in
  if is_associative_operator op then (
    try Some (nf args) with
    | Exit ->
        logger#error
          "ac_matching_nf: %s(%s): unexpected ArgKwd | ArgType | ArgOther"
          (show_operator op)
          (show_arguments (Tok.unsafe_fake_bracket args));
        None)
  else None

let undo_ac_matching_nf tok op : expr list -> expr option = function
  | [] -> None
  | [ arg ] -> Some arg
  | a1 :: a2 :: args ->
      let mk_op x y =
        Call
          ( IdSpecial (Op op, tok) |> G.e,
            Tok.unsafe_fake_bracket [ Arg x; Arg y ] )
        |> G.e
      in
      Some (List.fold_left mk_op (mk_op a1 a2) args)

let set_e_range l r e =
  match (Tok.loc_of_tok l, Tok.loc_of_tok r) with
  | Ok l, Ok r -> e.e_range <- Some (l, r)
  | Error _, _
  | _, Error _ ->
      (* Probably not super useful to dump the whole expression, or to log the
       * fake tokens themselves. Perhaps this will be useful for debugging
       * isolated examples, though. *)
      logger#debug "set_e_range failed: missing token location";
      ()

class ['self] extract_info_visitor =
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    method! visit_tok globals tok = Common.push tok globals

    method! visit_expr globals x =
      match x.e with
      (* Ignore the tokens from the expression str is aliased to *)
      | Alias ((_str, t), _e) -> Common.push t globals
      | _ -> super#visit_expr globals x
  end

let ii_of_any any =
  let v = new extract_info_visitor in
  let globals = ref [] in
  v#visit_any globals any;
  List.rev !globals
[@@profiling]

let info_of_any any =
  match ii_of_any any with
  | x :: _ -> x
  | [] -> assert false

(*e: function [[Lib_AST.ii_of_any]] *)

let first_info_of_any any =
  let xs = ii_of_any any in
  let xs = List.filter Tok.is_origintok xs in
  let min, _max = Tok_range.min_max_toks_by_pos xs in
  min

(*****************************************************************************)
(* Extract ranges *)
(*****************************************************************************)

(* Also used below by `nearest_any_of_pos` *)
let smaller t1 t2 =
  if compare t1.Tok.pos.bytepos t2.Tok.pos.bytepos < 0 then t1 else t2

let larger t1 t2 =
  if compare t1.Tok.pos.bytepos t2.Tok.pos.bytepos > 0 then t1 else t2

let incorporate_tokens ranges (left, right) =
  match !ranges with
  | None -> ranges := Some (left, right)
  | Some (orig_left, orig_right) ->
      ranges := Some (smaller orig_left left, larger orig_right right)

let incorporate_token ranges tok =
  if Tok.is_origintok tok then
    let tok_loc = Tok.unsafe_loc_of_tok tok in
    incorporate_tokens ranges (tok_loc, tok_loc)

class ['self] range_visitor =
  object (self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    method! visit_tok ranges tok = incorporate_token ranges tok

    method! visit_expr ranges expr =
      match expr.e_range with
      | None -> (
          let saved_ranges = !ranges in
          ranges := None;
          super#visit_expr ranges expr;
          expr.e_range <- !ranges;
          match saved_ranges with
          | None -> ()
          | Some r -> incorporate_tokens ranges r)
      | Some range -> incorporate_tokens ranges range

    method! visit_stmt ranges stmt =
      match stmt.s_range with
      | None -> (
          let saved_ranges = !ranges in
          ranges := None;
          super#visit_stmt ranges stmt;
          stmt.s_range <- !ranges;
          match saved_ranges with
          | None -> ()
          | Some r -> incorporate_tokens ranges r)
      | Some range -> incorporate_tokens ranges range

    (* Ignore the tokens from the aliased expression *)
    method! visit_Alias ranges id _e = self#visit_ident ranges id
  end

let extract_ranges_with_anys :
    AST_generic.any list -> (Tok.location * Tok.location) option =
  let v = new range_visitor in
  let ranges = ref None in
  fun anys ->
    List.iter (v#visit_any ranges) anys;
    let res = !ranges in
    ranges := None;
    res

let extract_ranges any = extract_ranges_with_anys [ any ]

let set_e_range_with_anys anys e =
  match extract_ranges_with_anys anys with
  | Some (l, r) -> e.e_range <- Some (l, r)
  | None ->
      logger#debug "set_e_range_with_anys failed: no locations found";
      ()

let range_of_tokens tokens =
  List.filter Tok.is_origintok tokens |> Tok_range.min_max_toks_by_pos
[@@profiling]

let range_of_any_opt any =
  (* Even if the ranges are cached, calling `extract_ranges` to get them
   * is extremely expensive (due to `mk_visitor`). Testing taint-mode
   * open-redirect rule on Django, we spent ~16 seconds computing range
   * info (despite caching). If we bypass `extract_ranges` as we do here,
   * that time drops to just ~1.5 seconds! *)
  match any with
  | G.E e when Option.is_some e.e_range -> e.e_range
  | G.S s when Option.is_some s.s_range -> s.s_range
  | G.Tk tok -> (
      match Tok.loc_of_tok tok with
      | Ok tok_loc -> Some (tok_loc, tok_loc)
      | Error _ -> None)
  | G.Anys [] -> None
  | _ -> extract_ranges any
[@@profiling]

(*****************************************************************************)
(* Nearest Any node of a position *)
(*****************************************************************************)

type any_range = {
  range : (Tok.location * Tok.location) option ref;
  any : (AST_generic.any * (Tok.location * Tok.location)) option ref;
  position : int; (* charpos *)
}

class ['self] any_range_visitor =
  let pos_within pos (t1', t2') =
    let _, _, t2'_charpos = Tok.end_pos_of_loc t2' in
    pos >= t1'.Tok.pos.bytepos && pos <= t2'_charpos
  in
  let range_within (t1, t2) (t1', t2') =
    let _, _, t2_charpos = Tok.end_pos_of_loc t2 in
    let _, _, t2'_charpos = Tok.end_pos_of_loc t2' in
    t1.Tok.pos.bytepos >= t1'.Tok.pos.bytepos && t2_charpos <= t2'_charpos
  in
  let set_any_range info (any, range) =
    let charpos = info.position in
    match (!(info.any), range) with
    | _, None -> ()
    | None, Some range ->
        if pos_within charpos range then info.any := Some (any, range)
    | Some (_, cur_range), Some range ->
        if range_within range cur_range && pos_within charpos range then
          info.any := Some (any, range)
  in
  let handle_any info any visit_fn =
    let saved_ranges = !(info.range) in
    info.range := None;
    visit_fn ();
    set_any_range info (any, !(info.range));
    match saved_ranges with
    | None -> ()
    | Some range -> incorporate_tokens info.range range
  in
  object (self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    method! visit_tok info tok = incorporate_token info.range tok

    method! visit_type_ info type_ =
      handle_any info (T type_) (fun () -> super#visit_type_ info type_)

    method! visit_pattern info pat =
      handle_any info (P pat) (fun () -> super#visit_pattern info pat)

    method! visit_field info field =
      handle_any info (Fld field) (fun () -> super#visit_field info field)

    method! visit_argument info arg =
      handle_any info (Ar arg) (fun () -> super#visit_argument info arg)

    method! visit_directive info directive =
      handle_any info (Dir directive) (fun () ->
          super#visit_directive info directive)

    method! visit_attribute info attr =
      handle_any info (At attr) (fun () -> super#visit_attribute info attr)

    method! visit_definition info def =
      handle_any info (Def def) (fun () -> super#visit_definition info def)

    method! visit_parameter info param =
      handle_any info (Pa param) (fun () -> super#visit_parameter info param)

    method! visit_expr info expr =
      handle_any info (E expr) (fun () -> super#visit_expr info expr)

    method! visit_stmt info stmt =
      handle_any info (S stmt) (fun () -> super#visit_stmt info stmt)

    (* Ignore the tokens from the aliased expression *)
    method! visit_Alias ranges id _e = self#visit_ident ranges id
  end

let nearest_any_of_pos prog position =
  let v = new any_range_visitor in
  let info = { range = ref None; any = ref None; position } in
  v#visit_program info prog;
  !(info.any)

(*****************************************************************************)
(* Fix token locations *)
(*****************************************************************************)

let fix_token_locations_visitor =
  object (_self : 'self)
    inherit [_] AST_generic.map_legacy as super

    method! visit_id_info _fix ii =
      (* The id_info contains locations that should not be modified, and they
       * are likely outside the sub-AST of interest anyways. *)
      ii

    method! visit_expr fix e =
      super#visit_expr fix
        { e with e_range = Option.map (fun (x, y) -> (fix x, fix y)) e.e_range }

    method! visit_stmt fix s =
      super#visit_stmt fix
        { s with s_range = Option.map (fun (x, y) -> (fix x, fix y)) s.s_range }

    method! visit_tok fix t = Tok.fix_location fix t
  end

(* Exposing just these methods because we want the .mli file for this module,
 * and it's impractical to write out the type of an autogenerated visitor. If we
 * need more, we could consider putting the visitor in a separate file without
 * an mli and allowing direct access to it. *)
let fix_token_locations_any = fix_token_locations_visitor#visit_any
let fix_token_locations_program = fix_token_locations_visitor#visit_program
