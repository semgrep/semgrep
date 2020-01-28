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

 and rules = rule list

 and severity =
  | Error
  | Warning
