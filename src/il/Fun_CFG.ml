(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)

(*****************************************************************************)
(* Positions *)
(*****************************************************************************)

type pos = Pos.t option
(* Source pos for lambda declarations, given by the name given to the lambda in
  the IL. This should be 'None' IFF the lambda was given a fresh "tmp" name. *)

module PosMap = Map.Make (struct
  type t = pos

  let compare pos1 pos2 = Option.compare Pos.compare pos1 pos2
end)

let pos_of_name (name : IL.name) =
  let _, tok = name.ident in
  match Tok.loc_of_tok tok with
  | Error _ -> None
  | Ok loc -> Some loc.pos

let check_if_real_pos__warn_once =
  let errored = Atomic.make false in
  fun pos ->
    if Option.is_none pos && Atomic.compare_and_set errored false true then
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m
            "BUG: IL.Fun_CFG: Non-unique lambda declarations must have source \
             positions")

(*****************************************************************************)
(* CFG *)
(*****************************************************************************)

type t = { params : IL.param list; cfg : IL.cfg; lambdas : lambdas_cfgs }
and lambdas_cfgs = lambda_cfg IL.NameMap.t

and lambda_cfg =
  | Uniq of pos * t
      (** No name-clashing, lambda name has just one declaration. *)
  | Multi of t PosMap.t
      (** We have the same lambda name used for multiple lambda declarations,
        thus to find the right CFG we need to disambiguate using the source
        position of the declaration. This can happen due to a failure of naming,
        e.g. could have two lambdas named `foo` in the sources, both unresolved.

        We handle this mostly for **backwards compatibility**; previously
        we assigned a fresh var to each lambda so we did not have such problem.
        For clashing lambda definitions we will not only do the most basic
        processing, simply to report obvious taint findings within them. *)

(*****************************************************************************)
(* Lambdas *)
(*****************************************************************************)

let empty_lambdas = IL.NameMap.empty

let find_lambda (lambdas : lambdas_cfgs) name =
  match IL.NameMap.find_opt name lambdas with
  | None -> Error `NotLambda
  | Some lambda_cfg -> (
      match lambda_cfg with
      | Uniq (_, cfg) -> Ok cfg
      | Multi pos2cfg -> (
          (* If we have multiple declarations, we can only disambiguate
      if the 'name' we are given points to one specific declaration.
      Note that when we compare names in 'IL.NameMap', we ignore their
      source position and just look at their sid. *)
          let pos = pos_of_name name in
          check_if_real_pos__warn_once pos;
          match PosMap.find_opt pos pos2cfg with
          | None -> Error `Multi
          | Some cfg -> Ok cfg))

let record_lambda (lambdas : lambdas_cfgs) name fcfg =
  let name_pos = pos_of_name name in
  let record = function
    | None -> Some (Uniq (name_pos, fcfg))
    | Some (Uniq (pos0, fcfg0)) ->
        (* Found yet another declaration for the same lambda name. *)
        check_if_real_pos__warn_once pos0;
        check_if_real_pos__warn_once name_pos;
        Some (Multi (PosMap.of_list [ (pos0, fcfg0); (name_pos, fcfg) ]))
    | Some (Multi pos2cfg) -> Some (Multi (PosMap.add name_pos fcfg pos2cfg))
  in
  IL.NameMap.update name record lambdas

let seq_of_lambdas (lambdas : lambdas_cfgs) =
  lambdas |> IL.NameMap.to_seq
  |> Seq.concat_map (fun (name, lcfg) ->
         match lcfg with
         | Uniq (pos, cfg) -> Seq.return (name, pos, cfg)
         | Multi pos2cfg ->
             pos2cfg |> PosMap.to_seq
             |> Seq.map (fun (pos, cfg) -> (name, pos, cfg)))

let rec reachable_nodes fcfg =
  let main_nodes = CFG.reachable_nodes fcfg.cfg in
  let lambdas_nodes =
    fcfg.lambdas |> seq_of_lambdas
    |> Seq.map (fun (_lname, _pos, lcfg) -> reachable_nodes lcfg)
  in
  Seq.concat (Seq.cons main_nodes lambdas_nodes)

let is_lambda (lambdas : lambdas_cfgs) (lval : IL.lval) =
  match lval with
  | { base = Var name; rev_offset = [] } -> (
      match find_lambda lambdas name with
      | Error err -> Error err
      | Ok cfg -> Ok (name, cfg))
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      Error `NotVar

let lambdas_names (lambdas : lambdas_cfgs) =
  lambdas |> IL.NameMap.to_seq |> Seq.map fst |> IL.NameSet.of_seq

let union_lambdas ~base:(lambdas1 : lambdas_cfgs) lambdas2 =
  let merge _name cfgs1 cfgs2 =
    match (cfgs1, cfgs2) with
    | Uniq (pos1, cfg1), Uniq (pos2, cfg2) ->
        check_if_real_pos__warn_once pos1;
        check_if_real_pos__warn_once pos2;
        Some (Multi (PosMap.of_list [ (pos1, cfg1); (pos2, cfg2) ]))
    | Uniq (pos, cfg), Multi pos2map
    | Multi pos2map, Uniq (pos, cfg) ->
        check_if_real_pos__warn_once pos;
        Some (Multi (PosMap.add pos cfg pos2map))
    | Multi multi1, Multi multi2 ->
        Some
          (Multi
             (PosMap.union
                (fun pos _cfg1 cfg2 ->
                  (* We don't expect shadowing, unless there is a bug. *)
                  check_if_real_pos__warn_once pos;
                  Some cfg2)
                multi1 multi2))
  in
  IL.NameMap.union merge lambdas1 lambdas2
