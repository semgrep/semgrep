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

let t = Testo.create

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let real_fetch_tests =
  let fetch_ocaml_rules () =
    match
      Rule_fetching.rules_from_dashdash_config ~rewrite_rule_ids:false
        ~token_opt:None (Rules_config.R (Pack "ocaml"))
    with
    | [ { rules; _ } ], [] ->
        Alcotest.(check bool) "fetch ocaml rules" true (not @@ List_.null rules)
    | _ -> Alcotest.fail "fetch ocaml rules; got no rules or got rule errors"
  in
  (* It seems like the intention of these tests were to test network flakeyness
   * by repetition.
   *)
  let broken = "networking isn't mocked + testing flakeyness" in
  Testo.categorize "fetch tests"
    [
      t ~broken "fetch ocaml rules 1" fetch_ocaml_rules;
      t ~broken "fetch ocaml rules 2" fetch_ocaml_rules;
      t ~broken "fetch ocaml rules 3" fetch_ocaml_rules;
      t ~broken "fetch ocaml rules 4" fetch_ocaml_rules;
      t ~broken "fetch ocaml rules 5" fetch_ocaml_rules;
    ]

let tests = Testo.categorize_suites "OSemgrep Fetch" [ real_fetch_tests ]
