(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
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
module Log = Log_tainting.Log
module G = AST_generic
module R = Rule
module T = Taint
module Taints = T.Taint_set

(*****************************************************************************)
(* Taint shapes *)
(*****************************************************************************)

module Fields = struct
  module F = Map.Make (struct
    type t = T.offset

    (* In taint shapes we consider 'Ofld' and 'Ostr' to be the same, given that
       in some languages like JS/TS you can treat records as if they were dicts
       with string keys. *)
    let compare (o1 : t) (o2 : t) =
      match (o1, o2) with
      | Ofld fld1, Ofld fld2 -> String.compare (fst fld1.ident) (fst fld2.ident)
      | Ostr str1, Ostr str2 -> String.compare str1 str2
      | Ofld fld1, Ostr str2 -> String.compare (fst fld1.ident) str2
      | Ostr str1, Ofld fld2 -> String.compare str1 (fst fld2.ident)
      | Oint i1, Oint i2 -> Int.compare i1 i2
      | Oany, Oany -> 0
      | (Ofld _ | Ostr _), (Oint _ | Oany) -> -1
      | Oint _, Oany -> -1
      | Oany, (Ofld _ | Ostr _ | Oint _) -> 1
      | Oint _, (Ofld _ | Ostr _) -> 1
  end)

  include F

  (* At present Map.filter_map does not guarantee physical equality when
     the map ends up being the same because nothing really changed. *)
  let filter_map f fields =
    let changed = ref false in
    let fields' =
      fields
      |> filter_map (fun o cell ->
          let opt_cell' = f o cell in
          (match opt_cell' with
          | Some cell' when phys_equal cell' cell -> ()
          | None
          | Some _ ->
              changed := true);
          opt_cell')
    in
    if !changed then fields' else fields
end

(** A shape approximates an object or data structure, and tracks the taint
   associated with its fields and indexes.

   Taint shapes are a bit like types. Right now this is mainly to support
   field- and index-sensitivity, but shapes also provide a good foundation to
   later add alias analysis.  This is somewhat inspired by

       "Polymorphic type, region and effect inference"
       by Jean-Pierre Talpin and Pierre Jouvelot

   History
   -------
   Previously, we had a flat environment from l-values to their taint, and we had
   to "reconstruct" the shape of objects when needed. For example, to check if a
   variable was a struct, we looked for l-values in the environment that were an
   "extension" of that variable. By recording shapes explicitly, implementing
   field-sensitivity becomes more natural.

   Example
   -------
   For example, a record expression `{ a: "taint", b: "safe" }` would have
   the shape `Obj { .a -> Cell({"taint"}, _|_) }`, recording that the field `a`
   is tainted by the string literal `"taint"`. A field like '.a' (the dot '.'
   indicates that it's a field) or an index like '[0]' will always have a 'cell'
   shape, because they denote l-values. The first argument of a 'Cell' is its
   xtaint or "taint status" (see 'Xtaint.t'). For each field and index, we track
   its xtaint individually (field- and index-sensitivity). Field '.a' in
   `Obj { .a -> Cell({"taint"}, _|_) }` has the the taint set {"taint"} attached.
   The second argument of 'Cell' is the shape of the objects stored in that cell.
   The shape of field '.a' is '_|_' ("bottom") which is given to primitive types,
   or whenever we "don't care" (or to act as "to-do" as well).

   NOTE: Shapes are more advanced in the Pro version.
 *)
type shape =
  | Bot  (** _|_, don't know or don't care *)
  | Obj of obj
      (** An "object" or struct-like thing.

         Tuples or lists are also represented by 'Obj' shapes! We just treat
         constant indexes as if they were fields, and use 'Oany' to capture the
         non-constant indexes.
        *)

and cell =
  | Cell of Xtaint.t * shape
      (** A cell or "reference" represents the "storage" of a value, like
          a variable in C.

          A cell may be explicitly tainted ('`Tainted'), not explicitly tainted
          ('`None' / "0"),  or explicitly clean ('`Clean' / "C").

          A cell that is not explicitly tainted inherits any taints from "parent"
          refs. A cell that is explicitly clean it is clean regardless.

          For example, given a variable `x` and the following statements:

              x.a := "taint";
              x.a.u := "clean";

          We could assign the following shape to `x`:

              Cell(`None, Obj {
                      .a -> Cell({"taint"}, Obj {
                              .u -> Cell(`Clean, _|_)
                              })
                      })

          We have that `x` itself has no taint directly assigned to it, but `x.a` is
          tainted (by the string `"taint"`). Other fields like `x.b` are not tainted.
          When it comes to `x.a`, we have that `x.a.u` has been explicitly marked clean,
          so `x.a.u` will be considered clean despite `x.a` being tainted. Any other field
          of `x.a` such as `x.a.v` will inherit the same taint as `x.a`.

          INVARIANT(cell): To keep shapes minimal:
            1. If the xtaint is '`None', then the shape is not 'Bot' and we can reach
               another 'cell' whose xtaint is either '`Tainted' or '`Clean'.
            2. If the xtaint is '`Clean', then the shape is 'Bot'.
               (If we add aliasing we may need to revisit this, and instead just mark
                every reachable 'cell' as clean too.)

          TODO: We can attach "region ids" to refs and assign taints to regions rather than
            to refs directly, then we can have alias analysis.
        *)

and obj = cell Fields.t
(**
      This a mapping from a 'Taint.offset' to a shape 'cell'. Kept abstract in
      the .mli -- nothing outside this module needs to know it's a
      'Fields.t', they only ever get an 'obj' from pattern-matching on 'Obj'
      and pass it back into this module's own operations.

      If an 'Obj' shape tracks an 'Oany' offset (an arbitrary index,
      see 'Taint.offset'), then the taint and shape given to 'Oany' would
      also be the taint and shape given to any field that is not being
      explicitly tracked. If there is no 'Oany' in the 'Obj' shape, then a
      field that is not explicitly tracked would just have an arbitrary or
      "don't care" shape, and the taint that it inherits from its "parent"
      'cell's.

      THINK: Instead of 'Oany' maybe have an explicit field ?

      For example, given the assignment `x = { a: "taint", b: "safe" }`,
      the shape of `x` would be `Cell(`None, Obj { .a -> Cell({"taint"}, _|_) })`.
      The field `b` is omitted in the shape, and if we ask for it's taint and
      shape we would get the empty taint set (because `x`'s outermost 'Cell'
      has no taint), and the shape '_|_' because, given that we are not
      tracking `b`, it means we don't care about it's shape. In a shape like
      `{ [*] -> Cell({"taint"}, _|_) }}` where `[*]` denotes 'Oany', the taint
      and shape  of any concrete index would be given by the taint and shape
      of '[*]'.
    *)

(*************************************)
(* Equality *)
(*************************************)
(* TODO: Should we just define these in terms of `compare_*` ? *)

let rec equal_cell cell1 cell2 =
  let (Cell (taints1, shape1)) = cell1 in
  let (Cell (taints2, shape2)) = cell2 in
  Xtaint.equal taints1 taints2 && equal_shape shape1 shape2

and equal_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, Bot -> true
  | Obj obj1, Obj obj2 -> equal_obj obj1 obj2
  | Bot, Obj _
  | Obj _, Bot ->
      false

and equal_obj obj1 obj2 = Fields.equal equal_cell obj1 obj2

(*************************************)
(* Pretty-printing *)
(*************************************)

let rec show_cell cell =
  let (Cell (xtaint, shape)) = cell in
  spf "cell<%s>(%s)" (Xtaint.show xtaint) (show_shape shape)

and show_shape = function
  | Bot -> "_|_"
  | Obj obj -> spf "obj {|%s|}" (show_obj obj)

and show_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, o_cell) ->
      spf "%s: %s" (T.show_offset o) (show_cell o_cell))
  |> List.of_seq |> String.concat "; "

(*********************************************************)
(* Helpers *)
(*********************************************************)

(* UNSAFE: Violates INVARIANT(cell), see 'internal_UNSAFE_find_offset_in_obj' *)
let cell_none_bot = Cell (`None, Bot)

(* Temporarily breaks INVARIANT(cell) by initializing a field with the shape
 * 'cell<0>(_|_)', but right away the field should be either tainted or cleaned.
 * The caller must restore the invariant. *)
let internal_UNSAFE_find_offset_in_obj o obj =
  match Fields.find_opt o obj with
  | Some _ -> (o, obj)
  | None ->
      let num_fields = Fields.cardinal obj in
      if num_fields <= Limits_semgrep.taint_MAX_OBJ_FIELDS then
        let obj = Fields.add o cell_none_bot obj in
        (o, obj)
      else (
        Log.warn (fun m ->
            m "Already tracking too many fields, will not track %s"
              (T.show_offset o));
        (Oany, obj))

(*********************************************************)
(* Misc *)
(*********************************************************)

let taints_and_shape_are_relevant taints shape =
  match (Taints.is_empty taints, shape) with
  | true, Bot -> false
  | __else__ ->
      (* Either 'taints' is non-empty, or 'shape' is non-'Bot' and hence
       * by INVARIANT(cell) it contains some taint or has field marked clean. *)
      true

(* TODO: This should fix shapes too. *)
let fix_poly_taint_with_offset offset taints =
  let type_of_offset o =
    match o with
    | T.Ofld n -> !(n.id_info.id_type)
    | _ -> None
  in
  let add_offset_to_lval o ({ offset; _ } as orig_lval : T.lval) =
    let extended_lval = { orig_lval with offset = orig_lval.offset @ [ o ] } in
    if
      (* If the offset we are trying to take is already in the
           list of offsets, don't append it! This is so we don't
           never-endingly loop the dataflow and make it think the
           Arg taint is never-endingly changing.

           For instance, this code example would previously loop,
           if `x` started with an `Arg` taint:
           while (true) { x = x.getX(); }
      *)
      (not (List.mem o offset))
      &&
      (* For perf reasons we don't allow offsets to get too long.
       * Otherwise in a long chain of function calls where each
       * function adds some offset, we could end up a very large
       * amount of polymorphic taint.
       * This actually happened with rule
       * semgrep.perf.rules.express-fs-filename from the Pro
       * benchmarks, and file
       * WebGoat/src/main/resources/webgoat/static/js/libs/ace.js.
       *
       * TODO: This is way less likely to happen if we had better
       *   type info and we used it to remove taint, e.g. if Boolean
       *   and integer expressions didn't propagate taint. *)
      List.length offset < Limits_semgrep.taint_MAX_POLY_OFFSET
    then extended_lval
    else (
      Log.warn (fun m ->
          m "Taint_lval_env.fix_poly_taint_with_offset: %s is too long"
            (T.show_lval extended_lval));
      orig_lval)
  in
  let fix_var o (var : T.var) : T.var =
    match var with
    | Taint_var lval ->
        let lval' = add_offset_to_lval o lval in
        Taint_var lval'
    | Taint_in_shape_var lval ->
        let lval' = add_offset_to_lval o lval in
        Taint_in_shape_var lval'
    | Propagator_var _ ->
        (* This means that we may be losing field-sensitivity here.
           If we e.g. had `$FROM.sink($TO)` and `obj.sink(x.a)`, the `.a` will
           not be attached to the taint coming from `obj`. Don't know yet if
           that is going to be a big problem, if it is we'll find a solution.
           Right now I am just expecting `obj` to have a taint label encoding
           its type, in which case this limitation should not be a problem. *)
        var
    | Control_var -> var
  in
  offset
  |> List.fold_left
       (fun taints o ->
         match (type_of_offset o, o) with
         | Some { t = TyFun _; _ }, _ ->
             (* We have an l-value like `o.f` where `f` has a function type,
              * so it's a method call, we return nothing here. We cannot just
              * return `xtaint`, which is the taint of `o` in the environment;
              * whether that taint propagates or not is determined in
              * 'check_tainted_instr'/'Call'. Otherwise, if `o` had taint var
              * 'o@i', the call `o.getX()` would have taints '{o@i, o@i.x}'
              * when it should only have taints '{o@i.x}'. *)
             Taints.empty
         | _, Oany ->
             (* Cannot handle this offset. *)
             taints
         | __any__, ((Ofld _ | Ostr _ | Oint _) as o) ->
             (* Not a method call (to the best of our knowledge) or
              * an unresolved Java `getX` method. *)
             let taints' =
               taints
               |> Taints.map (fun taint ->
                   match taint.orig with
                   | Var var -> { taint with orig = Var (fix_var o var) }
                   | Src _ -> taint)
             in
             taints')
       taints

(*********************************************************)
(* Unification (merging shapes) *)
(*********************************************************)

let rec unify_cell cell1 cell2 =
  if phys_equal cell1 cell2 then cell1
  else
    let (Cell (xtaint1, shape1)) = cell1 in
    let (Cell (xtaint2, shape2)) = cell2 in
    (* TODO: Apply 'Flag_semgrep.max_taint_set_size' here too ? *)
    let xtaint = Xtaint.union xtaint1 xtaint2 in
    let shape = unify_shape shape1 shape2 in
    Cell (xtaint, shape)

and unify_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, shape
  | shape, Bot ->
      (* 'Bot' acts like a do-not-care. *)
      shape
  | Obj obj1, Obj obj2 -> Obj (unify_obj obj1 obj2)

and unify_obj obj1 obj2 =
  (* THINK: Apply taint_MAX_OBJ_FIELDS limit ? *)
  Fields.union (fun _ x y -> Some (unify_cell x y)) obj1 obj2

(*********************************************************)
(* Object shapes *)
(*********************************************************)

let add_field_to_obj_check_invariant obj offset taints shape =
  match (Xtaint.of_taints taints, shape) with
  | `None, Bot ->
      (* We skip this offset to maintain INVARIANT(cell). *)
      obj
  | xtaint, shape -> Fields.add offset (Cell (xtaint, shape)) obj

let tuple_like_obj taints_and_shapes : shape =
  let _index, obj =
    taints_and_shapes
    |> List.fold_left
         (fun (i, obj) (taints, shape) ->
           let obj =
             add_field_to_obj_check_invariant obj (T.Oint i) taints shape
           in
           (i + 1, obj))
         (0, Fields.empty)
  in
  (* See INVARIANT(cell) *)
  if Fields.is_empty obj then Bot else Obj obj

let record_or_dict_like_obj taints_and_shapes : shape =
  let obj =
    taints_and_shapes
    |> List.fold_left
         (fun obj field ->
           match field with
           | `Field (name, taints, shape) ->
               add_field_to_obj_check_invariant obj (T.Ofld name) taints shape
           | `Entry (e, taints, shape) ->
               let offset =
                 match e.IL.e with
                 | Literal (Int pi) -> (
                     match Parsed_int.to_int_opt pi with
                     | None -> T.Oany
                     | Some i -> T.Oint i)
                 | Literal (String (_, (s, _), _)) -> Ostr s
                 | __else__ -> T.Oany
               in
               add_field_to_obj_check_invariant obj offset taints shape
           | `Spread shape -> (
               match shape with
               | Obj obj' -> unify_obj obj obj'
               | Bot ->
                   Log.err (fun m ->
                       m
                         "record_or_dict_like_obj: expected Obj shape but \
                          found %s"
                         (show_shape shape));
                   obj))
         Fields.empty
  in
  (* See INVARIANT(cell) *)
  if Fields.is_empty obj then Bot else Obj obj

(*********************************************************)
(* Collect/union all taints *)
(*********************************************************)

(* THINK: Generalize to "fold" ? *)
let rec gather_all_taints_in_cell_acc acc cell =
  let (Cell (xtaint, shape)) = cell in
  match xtaint with
  | `Clean ->
      (* Due to INVARIANT(cell) we can just stop here. *)
      acc
  | `None -> gather_all_taints_in_shape_acc acc shape
  | `Tainted taints ->
      gather_all_taints_in_shape_acc (Taints.union taints acc) shape

and gather_all_taints_in_shape_acc acc = function
  | Bot -> acc
  | Obj obj -> gather_all_taints_in_obj_acc acc obj

and gather_all_taints_in_obj_acc acc obj =
  Fields.fold
    (fun _ o_cell acc -> gather_all_taints_in_cell_acc acc o_cell)
    obj acc

let gather_all_taints_in_cell = gather_all_taints_in_cell_acc Taints.empty
let gather_all_taints_in_shape = gather_all_taints_in_shape_acc Taints.empty

(*********************************************************)
(* Find an offset *)
(*********************************************************)

let rec find_in_cell_w_carry ~taints offset cell =
  let (Cell (xtaint, shape)) = cell in
  match offset with
  | [] -> `Found cell
  | _ :: _ -> (
      match xtaint with
      | `Clean ->
          if shape <> Bot then
            Log.err (fun m ->
                m "BUG: Taint_shape.find_in_cell: INVARIANT(cell).2 is broken");
          `Clean
      | `None -> find_in_shape_w_carry ~taints offset shape
      | `Tainted taints -> find_in_shape_w_carry ~taints offset shape)

and find_in_shape_w_carry ~taints offset shape =
  let not_found = `Not_found (taints, shape, offset) in
  match shape with
  (* offset <> [] *)
  | Bot -> not_found
  | Obj obj -> find_in_obj_w_carry ~taints offset obj

and find_in_obj_w_carry ~taints (offset : T.offset list) obj =
  let not_found = `Not_found (taints, Obj obj, offset) in
  (* offset <> [] *)
  match offset with
  | [] ->
      Log.err (fun m -> m "BUG: Taint_shape.fix_xtaint_obj: empty offset");
      not_found
  | o :: offset -> (
      match o with
      | Oany (* arbitrary index [*] *) -> (
          (* consider all fields/indexes *)
          match
            Fields.fold
              (fun _ cell acc ->
                match (acc, find_in_cell_w_carry ~taints offset cell) with
                | None, (`Not_found _ | `Clean) -> None
                | Some cell, (`Not_found _ | `Clean)
                | None, `Found cell ->
                    Some cell
                | Some cell1, `Found cell2 -> Some (unify_cell cell1 cell2))
              obj None
          with
          | None -> not_found
          | Some cell -> `Found cell)
      | Ofld _
      | Oint _
      | Ostr _ -> (
          match Fields.find_opt o obj with
          | None -> not_found
          | Some o_cell -> find_in_cell_w_carry ~taints offset o_cell))

let find_in_cell offset cell =
  find_in_cell_w_carry ~taints:Taints.empty offset cell

let option_of_find_result res =
  match res with
  | `Clean -> None
  | `Not_found (taints, _shape, offset) ->
      (* TODO: Fix _shape too. *)
      let taints = fix_poly_taint_with_offset offset taints in
      Some (taints, Bot)
  | `Found (Cell (xtaint, shape)) -> Some (Xtaint.to_taints xtaint, shape)

let find_in_cell_poly offset cell =
  find_in_cell offset cell |> option_of_find_result

let find_in_shape_poly ~taints offset shape =
  match offset with
  | [] -> Some (taints, shape)
  | _ :: _ ->
      find_in_shape_w_carry ~taints offset shape |> option_of_find_result

(*********************************************************)
(* Update the xtaint and shape of an offset *)
(*********************************************************)

(* Finds an 'offset' within a 'cell' and updates it via 'f'. *)
let rec update_offset_in_cell ~f offset cell =
  let (Cell (xtaint, shape)) = cell in
  let xtaint', shape' =
    match offset with
    | [] -> f xtaint shape
    | _ :: _ ->
        let shape = update_offset_in_shape ~f offset shape in
        (xtaint, shape)
  in
  match (xtaint', shape') with
  (* Restore INVARIANT(cell).1 *)
  | `None, Bot -> None
  | `Tainted taints, Bot when Taints.is_empty taints -> None
  (* Restore INVARIANT(cell).2 *)
  | `Clean, Obj _ ->
      (* If we are tainting an offset of this cell, the cell cannot be
         considered clean anymore. *)
      Some (Cell (`None, shape'))
  | `Clean, Bot
  | `None, Obj _
  | `Tainted _, (Bot | Obj _) ->
      if phys_equal xtaint' xtaint && phys_equal shape' shape then Some cell
      else Some (Cell (xtaint', shape'))

and update_offset_in_shape ~f offset shape =
  match shape with
  | Bot ->
      let shape = Obj Fields.empty in
      update_offset_in_shape ~f offset shape
  | Obj obj -> (
      match update_offset_in_obj ~f offset obj with
      | None -> Bot
      | Some obj' -> if phys_equal obj' obj then shape else Obj obj')

and update_offset_in_obj ~f offset obj =
  let obj' =
    match offset with
    | [] ->
        Log.err (fun m ->
            m "internal_UNSAFE_update_obj: Impossible happened: empty offset");
        obj
    | o :: offset -> (
        let o, obj = internal_UNSAFE_find_offset_in_obj o obj in
        match o with
        | Oany (* arbitrary index [*] *) ->
            (* consider all fields/indexes *)
            Fields.filter_map (fun _o' -> update_offset_in_cell ~f offset) obj
        | Ofld _
        | Oint _
        | Ostr _ ->
            obj
            |> Fields.update o (fun opt_cell ->
                let* cell = opt_cell in
                update_offset_in_cell ~f offset cell))
  in
  if Fields.is_empty obj' then None else Some obj'

(*********************************************************)
(* Updating an offset *)
(*********************************************************)

let update_offset_and_unify new_taints new_shape offset opt_cell =
  if taints_and_shape_are_relevant new_taints new_shape then
    let new_xtaint =
      (* THINK: Maybe Dataflow_tainting 'check_xyz' should be returning 'Xtaint.t'? *)
      Xtaint.of_taints new_taints
    in
    let cell = opt_cell ||| cell_none_bot in
    let add_new_taints xtaint shape =
      let shape = unify_shape new_shape shape in
      match xtaint with
      | `None
      | `Clean ->
          (* Since we're adding taint we cannot have `Clean here. *)
          (new_xtaint, shape)
      | `Tainted taints as xtaint ->
          if
            !Flag_semgrep.max_taint_set_size =|= 0
            || Taints.cardinal taints < !Flag_semgrep.max_taint_set_size
          then (Xtaint.union new_xtaint xtaint, shape)
          else (
            Log.warn (fun m ->
                m
                  "Already tracking too many taint sources for %s, will not \
                   track more"
                  (offset |> List.map T.show_offset |> String.concat ""));
            (xtaint, shape))
    in
    update_offset_in_cell ~f:add_new_taints offset cell
  else
    (* To maintain INVARIANT(cell) we cannot return 'cell_none_bot'! *)
    opt_cell

(*********************************************************)
(* Clean taint *)
(*********************************************************)

(* TODO: Reformulate in terms of 'update_offset_in_cell' *)
let rec clean_cell (offset : T.offset list) cell =
  let (Cell (xtaint, shape)) = cell in
  match offset with
  | [] ->
      (* See INVARIANT(cell)
       *
       * THINK: If we had aliasing, we would have to keep the previous shape
       *  and just clean it all ? And we would also need to remove the 'Clean'
       *  mark from other cells that may be pointing to this cell in order to
       *  maintain the invariant ? *)
      Cell (`Clean, Bot)
  | [ Oany ] ->
      (* If an object is tainted, and we clean all its fields/indexes, then we
       * just clean the object itself. For example, if we assume that an array `a`
       * is tainted, and then we see `a[*]` being sanitized, then we assume that
       * `a` itself is being sanitized; otherwise `sink(a)` could be reported. *)
      Cell (`Clean, Bot)
  | _ :: _ ->
      let shape = clean_shape offset shape in
      Cell (xtaint, shape)

and clean_shape offset shape =
  match shape with
  | Bot ->
      let shape = Obj Fields.empty in
      clean_shape offset shape
  | Obj obj -> Obj (clean_obj offset obj)

and clean_obj offset obj =
  match offset with
  | [] ->
      Log.err (fun m -> m "clean_obj: Impossible happened: empty offset");
      obj
  | o :: offset -> (
      let o, obj = internal_UNSAFE_find_offset_in_obj o obj in
      match o with
      | Oany -> Fields.map (clean_cell offset) obj
      | o ->
          Fields.update o (Option.map (fun cell -> clean_cell offset cell)) obj)

(*********************************************************)
(* Update token trace *)
(*********************************************************)

let rec internal_UNSAFE_map_xtaint_cell f cell =
  let (Cell (xtaint, shape)) = cell in
  let xtaint' = f xtaint in
  let shape' = internal_UNSAFE_map_xtaint_shape f shape in
  if phys_equal xtaint' xtaint && phys_equal shape' shape then cell
  else Cell (xtaint', shape')

and internal_UNSAFE_map_xtaint_shape f shape =
  match shape with
  | Bot -> shape
  | Obj obj ->
      let obj' = internal_UNSAFE_map_xtaint_obj f obj in
      if phys_equal obj' obj then shape else Obj obj'

and internal_UNSAFE_map_xtaint_obj f obj =
  Fields.map (internal_UNSAFE_map_xtaint_cell f) obj

let add_tainted_token_to_shape tok shape =
  shape
  |> internal_UNSAFE_map_xtaint_shape (fun xtaint ->
      match xtaint with
      | `None
      | `Clean ->
          xtaint
      | `Tainted taints ->
          let taints =
            taints
            |> Taints.map (fun t -> { t with rev_tokens = tok :: t.rev_tokens })
          in
          `Tainted taints)

(*********************************************************)
(* Enumerate tainted offsets *)
(*********************************************************)

let rec enum_in_cell cell : (T.offset list * Taints.t) Seq.t =
  let (Cell (taints, shape)) = cell in
  let x =
    match taints with
    | `Tainted taints -> Seq.cons ([], taints) Seq.empty
    | `Clean
    | `None ->
        Seq.empty
  in
  Seq.append x (enum_in_shape shape)

and enum_in_shape = function
  | Bot -> Seq.empty
  | Obj obj -> enum_in_obj obj

and enum_in_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, cell) ->
      enum_in_cell cell
      |> Seq.map (fun (offset, taints) -> (o :: offset, taints)))
  |> Seq.concat
