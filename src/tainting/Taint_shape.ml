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
module T = Taint
module Taints = T.Taint_set

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Record the "shape" of the things we track, taint shapes are a bit like types.
 * Right now this is mainly to support field- and index-sensitivity. Shapes also
 * provide a good foundation to later add alias analysis.
 *
 * This is somewhat inspired by
 * "Polymorphic type, region and effect inference"
 * by Jean-Pierre Talpin and Pierre Jouvelot.
 *
 * Previously, we had a flat environment from l-values to their taint, and we had
 * to "reconstruct" the shape of objects when needed. For example, to check if a
 * variable was a struct, we looked for l-values in the environment that were an
 * "extension" of that variable. By recording shapes explicitly, implementing
 * field-sensitivity becomes more natural.
 *
 * TODO: We want `Dataflow_tainting.check_tainted_xyz` to return a taint shape.
 * TODO: Add 'Ptr' shapes and track aliasing.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

module Fields = Map.Make (struct
  type t = T.offset

  let compare = T.compare_offset
end)

(* THINK: To assign a proper taint signature to a function like:
 *
 *      def foo(t):
 *          return t
 *
 *  we need shape variables... but then we need to handle unification
 *  between shape variables too.
 *)
type shape = Bot | Obj of obj | Arg of T.arg

(* THINK: rename 'ref' as 'cell' or 'var' to make it less confusing wrt OCaml 'ref' type ? *)
and ref = Ref of Xtaint.t * shape
and obj = ref Fields.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* UNSAFE: Violates INVARIANT(ref), see 'internal_UNSAFE_find_offset_in_obj' *)
let ref_none_bot = Ref (`None, Bot)

(* Temporarily breaks INVARIANT(ref) by initializing a field with the shape
 * 'ref<0>(_|_)', but right away the field should be either tainted or cleaned.
 * The caller must restore the invariant. *)
let internal_UNSAFE_find_offset_in_obj o obj =
  match Fields.find_opt o obj with
  | Some _ -> (o, obj)
  | None ->
      let num_fields = Fields.cardinal obj in
      if num_fields <= Limits_semgrep.taint_MAX_OBJ_FIELDS then
        let obj = Fields.add o ref_none_bot obj in
        (o, obj)
      else (
        Log.warn (fun m ->
            m "Already tracking too many fields, will not track %s"
              (T.show_offset o));
        (Oany, obj))

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let taints_and_shape_are_relevant taints shape =
  match (Taints.is_empty taints, shape) with
  | true, Bot -> false
  | __else__ ->
      (* Either 'taints' is non-empty, or 'shape' is non-'Bot' and hence
       * by INVARIANT(ref) it contains some taint or has field marked clean. *)
      true

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)

let rec equal_ref ref1 ref2 =
  let (Ref (taints1, shape1)) = ref1 in
  let (Ref (taints2, shape2)) = ref2 in
  Xtaint.equal taints1 taints2 && equal_shape shape1 shape2

and equal_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, Bot -> true
  | Obj obj1, Obj obj2 -> equal_obj obj1 obj2
  | Arg arg1, Arg arg2 -> T.equal_arg arg1 arg2
  | Bot, _
  | Obj _, _
  | Arg _, _ ->
      false

and equal_obj obj1 obj2 = Fields.equal equal_ref obj1 obj2

(*****************************************************************************)
(* Comparison *)
(*****************************************************************************)

let rec compare_ref ref1 ref2 =
  let (Ref (taints1, shape1)) = ref1 in
  let (Ref (taints2, shape2)) = ref2 in
  match Xtaint.compare taints1 taints2 with
  | 0 -> compare_shape shape1 shape2
  | other -> other

and compare_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, Bot -> 0
  | Obj obj1, Obj obj2 -> compare_obj obj1 obj2
  | Arg arg1, Arg arg2 -> T.compare_arg arg1 arg2
  | Bot, Obj _
  | Bot, Arg _
  | Obj _, Arg _ ->
      -1
  | Obj _, Bot
  | Arg _, Bot
  | Arg _, Obj _ ->
      1

and compare_obj obj1 obj2 = Fields.compare compare_ref obj1 obj2

(*****************************************************************************)
(* Pretty-printing *)
(*****************************************************************************)

let rec show_ref ref =
  let (Ref (xtaint, shape)) = ref in
  spf "ref<%s>(%s)" (Xtaint.show xtaint) (show_shape shape)

and show_shape = function
  | Bot -> "_|_"
  | Obj obj -> spf "obj {|%s|}" (show_obj obj)
  | Arg arg -> "'{" ^ T.show_arg arg ^ "}"

and show_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, o_ref) -> spf "%s: %s" (T.show_offset o) (show_ref o_ref))
  |> List.of_seq |> String.concat "; "

(*****************************************************************************)
(* Object shapes *)
(*****************************************************************************)

let tuple_like_obj taints_and_shapes : obj =
  let _index, obj =
    taints_and_shapes
    |> List.fold_left
         (fun (i, obj) (taints, shape) ->
           match (Xtaint.of_taints taints, shape) with
           | `None, Bot ->
               (* We skip this index to maintain INVARIANT(ref). *)
               (i + 1, obj)
           | xtaint, shape ->
               let ref = Ref (xtaint, shape) in
               (i + 1, Fields.add (T.Oint i) ref obj))
         (0, Fields.empty)
  in
  obj

(*****************************************************************************)
(* Unification (merging shapes) *)
(*****************************************************************************)

let rec unify_ref ref1 ref2 =
  let (Ref (xtaint1, shape1)) = ref1 in
  let (Ref (xtaint2, shape2)) = ref2 in
  (* TODO: Apply 'Flag_semgrep.max_taint_set_size' here too ? *)
  let xtaint = Xtaint.union xtaint1 xtaint2 in
  let shape = unify_shape shape1 shape2 in
  Ref (xtaint, shape)

and unify_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, shape
  | shape, Bot ->
      shape
  | Obj obj1, Obj obj2 -> Obj (unify_obj obj1 obj2)
  | Arg _, (Obj _ as obj)
  | (Obj _ as obj), Arg _ ->
      obj
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

and unify_obj obj1 obj2 =
  (* THINK: Apply taint_MAX_OBJ_FIELDS limit ? *)
  Fields.union (fun _ x y -> Some (unify_ref x y)) obj1 obj2

(*****************************************************************************)
(* Collect/union all taints *)
(*****************************************************************************)

(* THINK: Generalize to "fold" ? *)
let rec gather_all_taints_in_ref_acc acc ref =
  let (Ref (xtaint, shape)) = ref in
  match xtaint with
  | `Clean ->
      (* Due to INVARIANT(ref) we can just stop here. *)
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

and gather_all_taints_in_obj_acc acc obj =
  Fields.fold
    (fun _ o_ref acc -> gather_all_taints_in_ref_acc acc o_ref)
    obj acc

let gather_all_taints_in_ref = gather_all_taints_in_ref_acc Taints.empty
let gather_all_taints_in_shape = gather_all_taints_in_shape_acc Taints.empty

(*****************************************************************************)
(* Find an offset *)
(*****************************************************************************)

let rec find_in_ref offset ref =
  let (Ref (_xtaint, shape)) = ref in
  match offset with
  | [] -> Some ref
  | _ :: _ -> find_in_shape offset shape

and find_in_shape offset = function
  (* offset <> [] *)
  | Bot -> None
  | Obj obj -> find_in_obj offset obj
  | Arg _ ->
      (* TODO: Here we should "refine" the arg shape, it should be an Obj shape. *)
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
            (fun _ ref acc ->
              match (acc, find_in_ref offset ref) with
              | None, None -> None
              | Some ref, None
              | None, Some ref ->
                  Some ref
              | Some ref1, Some ref2 -> Some (unify_ref ref1 ref2))
            obj None
      | Ofld _
      | Oint _
      | Ostr _ ->
          let* o_ref = Fields.find_opt o obj in
          find_in_ref offset o_ref)

(*****************************************************************************)
(* Update the xtaint and shape of an offset *)
(*****************************************************************************)

(* Finds an 'offset' within a 'ref' and updates it via 'f'. *)
let rec update_offset_in_ref ~f offset ref =
  let xtaint, shape =
    match (ref, offset) with
    | Ref (xtaint, shape), [] -> f xtaint shape
    | Ref (xtaint, shape), _ :: _ ->
        let shape = update_offset_in_shape ~f offset shape in
        (xtaint, shape)
  in
  match (xtaint, shape) with
  (* Restore INVARIANT(ref).1 *)
  | `None, Bot -> None
  | `Tainted taints, Bot when Taints.is_empty taints -> None
  (* Restore INVARIANT(ref).2 *)
  | `Clean, (Obj _ | Arg _) ->
      (* If we are tainting an offset of this ref, the ref cannot be
         considered clean anymore. *)
      Some (Ref (`None, shape))
  | `Clean, Bot
  | `None, (Obj _ | Arg _)
  | `Tainted _, (Bot | Obj _ | Arg _) ->
      Some (Ref (xtaint, shape))

and update_offset_in_shape ~f offset = function
  | Bot
  | Arg _ ->
      let shape = Obj Fields.empty in
      update_offset_in_shape ~f offset shape
  | Obj obj -> (
      match update_offset_in_obj ~f offset obj with
      | None -> Bot
      | Some obj -> Obj obj)

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
            Fields.filter_map (fun _o' -> update_offset_in_ref ~f offset) obj
        | Ofld _
        | Oint _
        | Ostr _ ->
            obj
            |> Fields.update o (fun opt_ref ->
                   let* ref = opt_ref in
                   update_offset_in_ref ~f offset ref))
  in
  if Fields.is_empty obj' then None else Some obj'

(*****************************************************************************)
(* Updating an offset *)
(*****************************************************************************)

let update_offset_and_unify new_taints new_shape offset opt_ref =
  if taints_and_shape_are_relevant new_taints new_shape then
    let new_xtaint =
      (* THINK: Maybe Dataflow_tainting 'check_xyz' should be returning 'Xtaint.t'? *)
      Xtaint.of_taints new_taints
    in
    let ref = opt_ref ||| ref_none_bot in
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
    update_offset_in_ref ~f:add_new_taints offset ref
  else (* To maintain INVARIANT(ref) we cannot return 'ref_none_bot'! *)
    opt_ref

(*****************************************************************************)
(* Clean taint *)
(*****************************************************************************)

(* TODO: Reformulate in terms of 'update_offset_in_ref' *)
let rec clean_ref (offset : T.offset list) ref =
  let (Ref (xtaint, shape)) = ref in
  match offset with
  | [] ->
      (* See INVARIANT(ref)
       *
       * THINK: If we had aliasing, we would have to keep the previous shape
       *  and just clean it all ? And we would also need to remove the 'Clean'
       *  mark from other refs that may be pointing to this ref in order to
       *  maintain the invariant ? *)
      Ref (`Clean, Bot)
  | [ Oany ] ->
      (* If an object is tainted, and we clean all its fields/indexes, then we
       * just clean the object itself. For example, if we assume that an array `a`
       * is tainted, and then we see `a[*]` being sanitized, then we assume that
       * `a` itself is being sanitized; otherwise `sink(a)` could be reported. *)
      Ref (`Clean, Bot)
  | _ :: _ ->
      let shape = clean_shape offset shape in
      Ref (xtaint, shape)

and clean_shape offset = function
  | Bot
  | Arg _ ->
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
      | Oany -> Fields.map (clean_ref offset) obj
      | o -> Fields.update o (Option.map (fun ref -> clean_ref offset ref)) obj)

(*****************************************************************************)
(* Instantiation *)
(*****************************************************************************)

let rec instantiate_shape ~inst_lval ~inst_taints = function
  | Bot -> Bot
  | Obj obj ->
      let obj =
        obj
        |> Fields.filter_map (fun _o ref ->
               (* This is essentially a recursive call to 'instantiate_shape'!
                * We rely on 'update_offset_in_ref' to maintain INVARIANT(ref). *)
               update_offset_in_ref
                 ~f:(internal_UNSAFE_inst_xtaint_shape ~inst_lval ~inst_taints)
                 [] ref)
      in
      if Fields.is_empty obj then Bot else Obj obj
  | Arg arg -> (
      match inst_lval (T.lval_of_arg arg) with
      | Some (_taints, shape) -> shape
      | None ->
          Log.warn (fun m ->
              m "Could not instantiate arg shape: %s" (T.show_arg arg));
          Arg arg)

and internal_UNSAFE_inst_xtaint_shape ~inst_lval ~inst_taints xtaint shape =
  (* This may break INVARIANT(ref) but 'update_offset_in_ref' will restore it. *)
  let xtaint =
    match xtaint with
    | `None
    | `Clean ->
        xtaint
    | `Tainted taints -> `Tainted (inst_taints taints)
  in
  let shape = instantiate_shape ~inst_lval ~inst_taints shape in
  (xtaint, shape)

(*****************************************************************************)
(* Enumerate tainted offsets *)
(*****************************************************************************)

let rec enum_in_ref ref : (T.offset list * Taints.t) Seq.t =
  let (Ref (taints, shape)) = ref in
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

and enum_in_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, ref) ->
         enum_in_ref ref
         |> Seq.map (fun (offset, taints) -> (o :: offset, taints)))
  |> Seq.concat
