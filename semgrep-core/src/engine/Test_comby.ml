(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
*)
open Common
open Comby

let _logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing the Comby matching and fixing engine.
 *
 * This is the first step in integrating Comby in semgrep. We first
 * need to debug how to use the Comby API in comby.mli
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let test_comby _pat file =
  let (_d,_b,e) = Common2.dbe_of_filename file in
  let _m =
    match Matchers.Alpha.select_with_extension ("." ^ e) with
    | None -> failwith (spf "no Alpha Comby module for extension %s" e)
    | Some x -> x
  in
  ()
