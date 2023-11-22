(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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
open Testutil

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let real_fetch_tests () =
  let fetch_ocaml_rules () =
    match
      Rule_fetching.rules_from_dashdash_config ~rewrite_rule_ids:false
        ~token_opt:None ~registry_caching:false (Rules_config.R (Pack "ocaml"))
    with
    | [ { rules; _ } ] ->
        Alcotest.(check bool) "fetch ocaml rules" true (not @@ Common.null rules)
    | _ -> Alcotest.fail "fetch ocaml rules; got no rules"
  in
  pack_tests "fetch tests"
    [
      ("fetch ocaml rules 1", fetch_ocaml_rules);
      ("fetch ocaml rules 2", fetch_ocaml_rules);
      ("fetch ocaml rules 3", fetch_ocaml_rules);
      ("fetch ocaml rules 4", fetch_ocaml_rules);
      ("fetch ocaml rules 5", fetch_ocaml_rules);
    ]

let tests = pack_suites "OSemgrep Fetch" [ real_fetch_tests () ]
