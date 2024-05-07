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

let base_tag_strings = [ __MODULE__; "taint" ]
let _tags = Logs_.create_tags base_tag_strings
let warning = Logs_.create_tags (base_tag_strings @ [ "warning" ])
let error = Logs_.create_tags (base_tag_strings @ [ "error" ])

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

module Fields = Map.Make (struct
  type t = T.offset

  let compare = T.compare_offset
end)

type shape = Bot | Obj of obj

(* THINK: rename 'ref' as 'cell' or 'var' to make it less confusing wrt OCaml 'ref' type ? *)
and ref = Ref of Xtaint.t * shape
and obj = ref Fields.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* UNSAFE: Violates INVARIANT(ref), see 'unsafe_find_offset_in_obj *)
let ref_none_bot = Ref (`None, Bot)

(* Temporarily breaks 'unsafe_find_offset_in_obj' by initializing a field with a
 * 'ref<0>(_|_)' shape, the field will immediately be after be tainted or cleaned. *)
let unsafe_find_offset_in_obj o obj =
  match Fields.find_opt o obj with
  | Some _ -> (o, obj)
  | None ->
      let num_fields = Fields.cardinal obj in
      if num_fields <= Limits_semgrep.taint_MAX_OBJ_FIELDS then
        let obj = Fields.add o ref_none_bot obj in
        (o, obj)
      else (
        Logs.debug (fun m ->
            m ~tags:warning
              "Already tracking too many fields, will not track %s"
              (T.show_offset o));
        (Oany, obj))

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
  | Bot, _
  | Obj _, _ ->
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
  | Bot, Obj _ -> -1
  | Obj _, Bot -> 1

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
           let xtaint =
             if Taints.is_empty taints then `None else `Tainted taints
           in
           match (xtaint, shape) with
           | `None, Bot -> (* See INVARIANT(ref) *) (i + 1, obj)
           | __else__ ->
               let ref = Ref (xtaint, shape) in
               (i + 1, Fields.add (T.Oint i) ref obj))
         (0, Fields.empty)
  in
  obj

(*****************************************************************************)
(* Union (merging shapes) *)
(*****************************************************************************)

let rec union_ref ref1 ref2 =
  let (Ref (xtaint1, shape1)) = ref1 in
  let (Ref (xtaint2, shape2)) = ref2 in
  (* TODO: Apply 'Flag_semgrep.max_taint_set_size' here too ? *)
  let xtaint = Xtaint.union xtaint1 xtaint2 in
  let shape = union_shape shape1 shape2 in
  Ref (xtaint, shape)

and union_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, shape
  | shape, Bot ->
      shape
  | Obj obj1, Obj obj2 -> Obj (union_obj obj1 obj2)

and union_obj obj1 obj2 =
  (* THINK: Apply taint_MAX_OBJ_FIELDS limit ? *)
  Fields.union (fun _ x y -> Some (union_ref x y)) obj1 obj2

(*****************************************************************************)
(* Collect/union all taints *)
(*****************************************************************************)

(* THINK: Generalize to "fold" ? *)
let rec union_taints_in_ref_acc acc ref =
  let (Ref (xtaint, shape)) = ref in
  match xtaint with
  | `Clean ->
      (* Due to INVARIANT(ref) we can just stop here. *)
      acc
  | `None -> union_taints_in_shape_acc acc shape
  | `Tainted taints -> union_taints_in_shape_acc (Taints.union taints acc) shape

and union_taints_in_shape_acc acc = function
  | Bot -> acc
  | Obj obj -> union_taints_in_obj_acc acc obj

and union_taints_in_obj_acc acc obj =
  Fields.fold (fun _ o_ref acc -> union_taints_in_ref_acc acc o_ref) obj acc

let union_taints_in_ref = union_taints_in_ref_acc Taints.empty
let union_taints_in_shape = union_taints_in_shape_acc Taints.empty

(*****************************************************************************)
(* Find an offset *)
(*****************************************************************************)

let rec find_in_ref offset ref =
  let (Ref (xtaint, shape)) = ref in
  match (offset, shape) with
  | [], Obj obj when Fields.cardinal obj =|= 1 && Fields.mem Oany obj ->
      (* Backwards compatibility "hack"
       * If it's an object where all fields are tainted, we also add
       * the taint here. *)
      let (Ref (index_xtaint, _)) = Fields.find Oany obj in
      let xtaint = Xtaint.union xtaint index_xtaint in
      Some (Ref (xtaint, shape))
  | [], _ -> Some ref
  | _ :: _, _ -> find_in_shape offset shape

and find_in_shape offset = function
  (* offset <> [] *)
  | Bot -> None
  | Obj obj -> find_in_obj offset obj

and find_in_obj (offset : T.offset list) obj =
  (* offset <> [] *)
  match offset with
  | [] ->
      Logs.debug (fun m ->
          m ~tags:error "fix_xtaint_obj: Impossible happened: empty offset");
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
              | Some ref1, Some ref2 -> Some (union_ref ref1 ref2))
            obj None
      | o -> (
          match Fields.find_opt o obj with
          | None -> None
          | Some o_ref -> find_in_ref offset o_ref))

(*****************************************************************************)
(* [UNSAFE] Update the xtaint of an offset *)
(*****************************************************************************)

(* Unsafe because it does not guarantee preserving INVARIANT(ref). *)

let rec unsafe_update_ref f offset ref =
  match (ref, offset) with
  | Ref (xtaint, shape), [] ->
      let xtaint, shape = f xtaint shape in
      Ref (xtaint, shape)
  | Ref (xtaint, shape), _ :: _ ->
      let xtaint =
        (* If we are tainting an offset of this ref, the ref cannot be
           considered clean anymore. *)
        match xtaint with
        | `Clean -> `None
        | `None
        | `Tainted _ ->
            xtaint
      in
      let shape = unsafe_update_shape f offset shape in
      Ref (xtaint, shape)

and unsafe_update_shape f offset = function
  | Bot ->
      let shape = Obj Fields.empty in
      unsafe_update_shape f offset shape
  | Obj obj ->
      let obj = unsafe_update_obj f offset obj in
      Obj obj

and unsafe_update_obj f offset obj =
  match offset with
  | [] ->
      Logs.debug (fun m ->
          m ~tags:error "update_obj: Impossible happened: empty offset");
      obj
  | o :: offset -> (
      let o, obj = unsafe_find_offset_in_obj o obj in
      match o with
      | Oany -> Fields.map (unsafe_update_ref f offset) obj
      | o ->
          Fields.update o
            (Option.map (fun ref -> unsafe_update_ref f offset ref))
            obj)

(*****************************************************************************)
(* Tainting an offset *)
(*****************************************************************************)

let unify_ref_shape new_taints new_shape offset opt_ref =
  let new_taints =
    (* THINK: Maybe Dataflow_tainting 'check_xyz' should be returning 'Xtaint.t'? *)
    Xtaint.of_taints new_taints
  in
  let ref = opt_ref |> Option.value ~default:(Ref (`None, Bot)) in
  let add_new_taints xtaint shape =
    let shape = union_shape new_shape shape in
    match xtaint with
    | `None
    | `Clean ->
        (* Assumption: either new_taints has taint or shape has taints somewhere *)
        (new_taints, shape)
    | `Tainted taints as xtaint ->
        if
          !Flag_semgrep.max_taint_set_size =|= 0
          || Taints.cardinal taints < !Flag_semgrep.max_taint_set_size
        then (Xtaint.union new_taints xtaint, shape)
        else (
          Logs.debug (fun m ->
              m ~tags:warning
                "Already tracking too many taint sources for %s, will not \
                 track more"
                (offset |> List_.map T.show_offset |> String.concat ""));
          (`Tainted taints, shape))
  in
  (* if Taints.is_empty new_taints then
     Logs.debug (fun m ->
         m ~tags:error "taint_ref: Impossible happened: empty taint set"); *)
  unsafe_update_ref add_new_taints offset ref

(*****************************************************************************)
(* Clean taint *)
(*****************************************************************************)

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
      (* If an object is tainted, and we clean all its indexes, then we instead
       * clean the object itself. *)
      Ref (`Clean, Bot)
  | _ :: _ ->
      let shape = clean_shape offset shape in
      Ref (xtaint, shape)

and clean_shape offset = function
  | Bot ->
      let shape = Obj Fields.empty in
      clean_shape offset shape
  | Obj obj -> Obj (clean_obj offset obj)

and clean_obj offset obj =
  match offset with
  | [] ->
      Logs.debug (fun m ->
          m ~tags:error "clean_obj: Impossible happened: empty offset");
      obj
  | o :: offset -> (
      let o, obj = unsafe_find_offset_in_obj o obj in
      match o with
      | Oany -> Fields.map (clean_ref offset) obj
      | o -> Fields.update o (Option.map (fun ref -> clean_ref offset ref)) obj)

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

and enum_in_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, ref) ->
         enum_in_ref ref
         |> Seq.map (fun (offset, taints) -> (o :: offset, taints)))
  |> Seq.concat
