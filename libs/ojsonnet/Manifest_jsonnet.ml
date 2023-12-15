(* Yoann Padioleau
 *
 * Copyright (C) 2022-2023 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See https://jsonnet.org/ref/spec.html#manifestation
 *
 * This file used to contain lots of code but after switching to
 * store explicit environment in Value_jsonnet.ml, the manifest_value()
 * function had to be mutually recursive with eval_expr() and so was
 * moved in Eval_jsonnet.ml
 * Still it's nice to hide this implememtation detail and still provide
 * a separate Manifest_jsonnet.ml file, even if it's just wrapping
 * Eval_jsonnet.manifest_value()
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let manifest_value = Eval_jsonnet.manifest_value
