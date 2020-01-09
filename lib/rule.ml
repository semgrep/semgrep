(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019 r2c
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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type rule = {
  id: string;
  pattern: Sgrep_generic.pattern;
  message: string;
  severity: severity;
  languages: Lang.t list; (* at least one element *)
}


 (* The rules will be tried in sequence in check() below. Note that the
  * order of the rules matter as we will issue a message only for the
  * first rule that matches.
  *)
 and rules = rule list

 and severity =
  | Error
  | Warning

  (* This is subtle. Sometimes one wants to warn against the use of
   * certain functions except when they are called in a certain way.
   * For instance foo(bar()) would be ok but all other use of foo(...)
   * would be bad. Sgrep right now can not express "difference patterns"
   * but one can abuse the fact that we run the sgrep rules in sequence
   * and stop when one of the rule is matching.
   * Indeed one can do:
   *
   *   - foo(bar())
   *   OK:
   *   - foo(...)
   *   ERROR: please don't use foo(), only foo(bar()) is ok.
   *
   * Another use could be to report lint stars or you-saved-puppies :)
   *)
  | Ok
