(* Brandon Wu
 *
 * Copyright (C) 2019-2024 Semgrep, Inc.
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

module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The internal state associated with the /semgrep/search command, which
   starts a streaming search process with the LSP.
   Will be reset on every new search request!
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  rules : Rule.search_rule list;
  files : Fpath.t list;
  xconf : Match_env.xconfig; [@opaque]
}
[@@deriving show]
