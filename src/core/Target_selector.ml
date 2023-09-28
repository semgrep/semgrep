(* Martin Jambon
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
(* How to select target files e.g. "files that look like C files".

    Note that target selection is more abstract than just specifying file
    extensions. Semgrep should have ways to detect image files, image content
    (without a file name), scripts written in a specific language without a
    file extension, and many other content types.
    These known detectable content types should be offered to the rule author
    so they can select target files of interest accurately and without
    thinking hard. Here is what some of our current rules contain:

        paths:
          include:
          - *.jpg
          - *.png
          ...
          exclude:
          - *.pdf

   This is quite fragile. Enter target selectors!

   related work we could leverage:
     - MIME types
     - the 'file' command in UNIX
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The list below is currently interpreted as an Or.
   Example:

     target_selector = [Javascript; Typescript];

   selects all the files that can be parsed and analyzed
   as TypeScript ( *.js, *.ts, *.tsx) since TypeScript is an extension of
   JavaScript.

   TODO: support a variety of predefined target selectors (in addition
   to the language selector we already support):
     - big categories like: text, binary, program, minified, executable,
       video, compressed, etc.
     - support disjonction and negation (image and not pdf, xml and not svg,
       minified and javascript, javascript and executable script)
     - makefile (does not have extension, but does follow naming convention)
     - maybe other categories like tests, config, ?

   This would reduce the maintenance burden for custom target selectors
   (the 'paths:' above) and allow mixing them with different target analyzers.
   For example, we could select all the Bash scripts but analyze them with
   spacegrep.
*)
type t = Lang.t list [@@deriving show]
