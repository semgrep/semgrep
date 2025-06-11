(* Iago Abal
 *
 * Copyright (C) 2025 Semgrep Inc.
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

type t = { parsing_stats : Summary_stats.t } [@@deriving show]

let zero = { parsing_stats = Summary_stats.zero }

let combine qprof1 qprof2 =
  {
    parsing_stats =
      Summary_stats.combine qprof1.parsing_stats qprof2.parsing_stats;
  }

let combine_opt opt_qprof1 opt_qprof2 =
  match (opt_qprof1, opt_qprof2) with
  | None, None -> None
  | Some _, _
  | _, Some _ ->
      let qprof1 = opt_qprof1 ||| zero in
      let qprof2 = opt_qprof2 ||| zero in
      Some (combine qprof1 qprof2)

let map_opt f opt_qprof = Some (f (opt_qprof ||| zero))

let add_parse_time file parse_time qprof =
  { parsing_stats = Summary_stats.update qprof.parsing_stats file parse_time }
