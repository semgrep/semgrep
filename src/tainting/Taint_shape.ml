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

(*********************************************************)
(* Object shapes *)
(*********************************************************)

let tuple_like_obj taints_and_shapes : obj =
  let _index, obj =
    taints_and_shapes
    |> List.fold_left
         (fun (i, obj) (taints, shape) ->
           match (Xtaint.of_taints taints, shape) with
           | `None, Bot ->
               (* We skip this index to maintain INVARIANT(cell). *)
               (i + 1, obj)
           | xtaint, shape ->
               let cell = Cell (xtaint, shape) in
               (i + 1, Fields.add (T.Oint i) cell obj))
         (0, Fields.empty)
  in
  obj

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
      let taint = { T.orig = T.Shape_var (T.lval_of_arg arg); tokens = [] } in
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

let rec find_in_cell offset cell =
  let (Cell (_xtaint, shape)) = cell in
  match offset with
  | [] -> Some cell
  | _ :: _ -> find_in_shape offset shape

and find_in_shape offset shape =
  match shape with
  (* offset <> [] *)
  | Bot -> None
  | Obj obj -> find_in_obj offset obj
  | Arg _ ->
      (* TODO: Here we should "refine" the arg shape, it should be an Obj shape. *)
      Log.debug (fun m ->
          m "Could not find offset %s in polymorphic shape %s"
            (debug_offset offset) (show_shape shape));
      None
  | Fun _ ->
      (* This is an error, we just don't want to crash here. *)
      Log.err (fun m ->
          m "Could not find offset %s in function shape %s"
            (debug_offset offset) (show_shape shape));
      None

and find_in_obj (offset : T.offset list) obj =
  (* offset <> [] *)
  match offset with
  | [] ->
      Log.err (fun m -> m "fix_xtaint_obj: Impossible happened: empty offset");
      None
  | o :: offset -> (
      match o with
      | Oany (* arbitrary index [*] *) ->
          (* consider all fields/indexes *)
          Fields.fold
            (fun _ cell acc ->
              match (acc, find_in_cell offset cell) with
              | None, None -> None
              | Some cell, None
              | None, Some cell ->
                  Some cell
              | Some cell1, Some cell2 -> Some (unify_cell cell1 cell2))
            obj None
      | Ofld _
      | Oint _
      | Ostr _ ->
          let* o_cell = Fields.find_opt o obj in
          find_in_cell offset o_cell)

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
