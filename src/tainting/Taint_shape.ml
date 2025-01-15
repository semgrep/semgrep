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
open Shape_and_sig.Shape
module Fields = Shape_and_sig.Fields
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature

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

let debug_offset offset =
  match offset with
  | [] -> "<NO OFFSET>"
  | _ :: _ -> offset |> List_.map T.show_offset |> String.concat ""

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
      && (* For perf reasons we don't allow offsets to get too long.
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
                      | Var lval ->
                          let lval' = add_offset_to_lval o lval in
                          { taint with orig = Var lval' }
                      | Shape_var lval ->
                          let lval' = add_offset_to_lval o lval in
                          { taint with orig = Shape_var lval' }
                      | Src _
                      | Control ->
                          taint)
             in
             taints')
       taints

(*********************************************************)
(* Unification (merging shapes) *)
(*********************************************************)

let rec unify_cell cell1 cell2 =
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
  | ( Fun { params = params1; effects = effects1 },
      Fun { params = params2; effects = effects2 } ) ->
      if Signature.equal_params params1 params2 then
        Fun { params = params1; effects = Effects.union effects1 effects2 }
      else (
        (* TODO: We could actually handle this. *)
        Log.warn (fun m ->
            m
              "Trying to unify two fun shapes with different parameters: %s ~ \
               %s"
              (Signature.show_params params1)
              (Signature.show_params params2));
        shape1)
  | Arg arg1, Arg arg2 ->
      if T.equal_arg arg1 arg2 then shape1
      else (
        (* TODO: We do not handle this right now, we would need to record and
         *   solve constraints. It can happen with code like e.g.
         *
         *     def foo(a, b):
         *       tup = (a,)
         *       tup[0] = b
         *       return tup
         *
         * Then the consequence would be that the signature of `foo` would ignore
         * the shape of `b`.
         *)
        Log.warn (fun m ->
            m "Trying to unify two different arg shapes: %s ~ %s"
              (T.show_arg arg1) (T.show_arg arg2));
        shape1)
  (* 'Arg' acts like a shape variable. *)
  | Arg _, (Obj _ as obj)
  | (Obj _ as obj), Arg _ ->
      obj
  | Arg _, (Fun _ as func)
  | (Fun _ as func), Arg _ ->
      func
  | Obj _, Fun _
  | Fun _, Obj _ ->
      (* This could be caused by bugs in Semgrep, or by an if-then-else in a
       * dynamic language like Python where the same variable has different types
       * in each branch, or by unsafe casts in C/C++ perhaps. *)
      Log.err (fun m ->
          m "Trying to unify incompatible shapes: %s ~ %s" (show_shape shape1)
            (show_shape shape2));
      (* Not sure what to do here, so we just pick one arbitrary shape. *)
      shape1

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
               | Bot
               | Arg _
               | Fun _ ->
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
  | Arg arg ->
      let taint =
        { T.orig = T.Shape_var (T.lval_of_arg arg); rev_tokens = [] }
      in
      Taints.add taint acc
  | Fun _ ->
      (* Consider a third-party/opaque function to which we pass a record that
       * contains a function object. Should be gather the taints in the function
       * shape? In principle, no, since taints within a function shape aren't
       * reachable until the function gets called...
       *
       * TODO: We could perhaps consider gathering the concrete taint sources
       * that may be reachable if the function ever gets called? *)
      acc

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
  | Arg _ ->
      (* TODO: Here we should "refine" the arg shape, it should be an Obj shape. *)
      Log.debug (fun m ->
          m "Could not find offset %s in polymorphic shape %s"
            (debug_offset offset) (show_shape shape));
      not_found
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not find offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      not_found

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
  let xtaint, shape =
    match (cell, offset) with
    | Cell (xtaint, shape), [] -> f xtaint shape
    | Cell (xtaint, shape), _ :: _ ->
        let shape = update_offset_in_shape ~f offset shape in
        (xtaint, shape)
  in
  match (xtaint, shape) with
  (* Restore INVARIANT(cell).1 *)
  | `None, Bot -> None
  | `Tainted taints, Bot when Taints.is_empty taints -> None
  (* Restore INVARIANT(cell).2 *)
  | `Clean, (Obj _ | Arg _ | Fun _) ->
      (* If we are tainting an offset of this cell, the cell cannot be
         considered clean anymore. *)
      Some (Cell (`None, shape))
  | `Clean, Bot
  | `None, (Obj _ | Arg _ | Fun _)
  | `Tainted _, (Bot | Obj _ | Arg _ | Fun _) ->
      Some (Cell (xtaint, shape))

and update_offset_in_shape ~f offset shape =
  match shape with
  | Bot
  | Arg _ ->
      let shape = Obj Fields.empty in
      update_offset_in_shape ~f offset shape
  | Obj obj -> (
      match update_offset_in_obj ~f offset obj with
      | None -> Bot
      | Some obj -> Obj obj)
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not update offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      shape

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
                  (offset |> List_.map T.show_offset |> String.concat ""));
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
  | Bot
  | Arg _ ->
      let shape = Obj Fields.empty in
      clean_shape offset shape
  | Obj obj -> Obj (clean_obj offset obj)
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not update offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      shape

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
  | Arg _ ->
      (* TODO: First need to record taint shapes in 'ToLval'.  *)
      Seq.empty
  | Fun _ -> Seq.empty

and enum_in_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, cell) ->
         enum_in_cell cell
         |> Seq.map (fun (offset, taints) -> (o :: offset, taints)))
  |> Seq.concat
