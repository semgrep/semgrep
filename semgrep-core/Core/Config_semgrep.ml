(*s: semgrep/core/Config_semgrep.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
(*e: pad/r2c copyright *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Semgrep engine configuration.
 *
 * The goal of this module is to gather in one place all the possible
 * ways to configure the semgrep matching engine. At some point, we
 * may let the user enable/disable certain features on a per-rule or even
 * per-pattern basis. For example, constant propagation may be too powerful
 * sometimes and prevent people to find certain code.
 *
 * Note that each feature in this file will change the matching results;
 * for non-functinal settings such as optimizations (e.g., using a
 * cache) use instead Flag_semgrep.ml
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type t = {
  constant_propagation: bool;
  (* TODO: move deep_expr_stmt features in Flag_semgrpe.ml here *)
}

(*****************************************************************************)
(* Default config *)
(*****************************************************************************)

let default_config = {
  constant_propagation = true;
}

(*e: semgrep/core/Config_semgrep.ml *)
