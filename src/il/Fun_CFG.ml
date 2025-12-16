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
open Common

type t = { params : IL.param list; cfg : IL.cfg; lambdas : lambdas_cfgs }
and lambdas_cfgs = t IL.NameMap.t

let empty_lambdas = IL.NameMap.empty
let find_lambda (lambdas : lambdas_cfgs) name = IL.NameMap.find_opt name lambdas

let record_lambda (lambdas : lambdas_cfgs) name fcfg =
  IL.NameMap.add name fcfg lambdas

let seq_of_lambdas (lambdas : lambdas_cfgs) = lambdas |> IL.NameMap.to_seq

let is_lambda (lambdas : lambdas_cfgs) (lval : IL.lval) =
  match lval with
  | { base = Var name; rev_offset = [] } ->
      let* cfg = find_lambda lambdas name in
      Some (name, cfg)
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      None

let rec reachable_nodes fcfg =
  let main_nodes = CFG.reachable_nodes fcfg.cfg in
  let lambdas_nodes =
    fcfg.lambdas |> seq_of_lambdas
    |> Seq.map (fun (_lname, lcfg) -> reachable_nodes lcfg)
  in
  Seq.concat (Seq.cons main_nodes lambdas_nodes)

let union_lambdas ~base:(lambdas1 : lambdas_cfgs) lambdas2 =
  let shadow _name _cfg1 cfg2 = Some cfg2 in
  IL.NameMap.union shadow lambdas1 lambdas2
