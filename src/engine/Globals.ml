(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
(* The goal of this module is mostly to document the "dangerous" globals
 * used inside Semgrep.
 *
 * Ultimately, we want to elimitate all globals. Until now, those globals did
 * not create too many issues because of the use of Parmap and fork
 * in Core_scan.ml (the modifications of those globals in the child process
 * do not affect the state in the parent process), but as soon as we migrate to
 * using domains instead with OCaml 5.0, those globals will haunt us back.
 * Maybe Domain-local-storage globals could help, but even better if we
 * can eliminate them.
 *
 * To find candidates for those "dangerous" globals, you can start with:
 *  $ ./bin/osemgrep --experimental -e 'val $V: $T ref' -l ocaml src/ libs/
 *  $ ./bin/osemgrep --experimental -e 'let $V: $T = ref $X' -l ocaml src/ libs/
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Useful for defensive programming, especially in tests which may leave
 * bad state behind.
 * Note that it's currently unused, because we should prefer to fix our tests
 * to restore the globals they modified, but as a last resort, you can
 * use this function.
 *)
let reset () =
  Core_error.g_errors := [];
  Core_profiling.profiling := false;
  AST_generic_equals.busy_with_equal := AST_generic_equals.Not_busy;
  Rule.last_matched_rule := None;
  Pro_hooks.reset_pro_hooks ();
  (* TODO?
   * - Http_helpers.client_ref ?
   * - many more
   *)
  ()
