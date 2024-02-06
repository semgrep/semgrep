(* Cooper Pierce
 *
 * Copyright (c) Semgrep Inc.
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

(** Sources of content which may be consumed by Semgrep.

   A source is the {e user-relevant location} where content (e.g., source code)
   originated. This should be used when storing a location which will later be
   reported to a user.

   While a source may imply that content can be obtained from a given location,
   it should generally be stored alongside the content to be consumed. This is
   so that it can be used principally for reporting where a finding originated
   independently of getting the content, which allows de-coupling reporting
   where a finding originated from obtaining the contents.

   This is since when these uses are fused, it is more difficult to
   {ul
      {- create a target where the contents are not the verbatim file contents}
      {- associate additional data to findings about location which we might
      want to report to users (e.g., "nested within a metavariable pattern";
      "via extract mode")}
      {- scan anything which is not resident on the filesystem (e.g.,
      metavariable pattern, git object)---currently this requires writing a
      tempfile.}
    }
    A major contributor to this is that [Match_search_mode] and similar
    currently require the contents of the scan target to be in a file.
 *)

(** The type for sources.

    {ul
      {- [File] sources are for files, and should have as their path the
        relative path from the scanned project root.}
    }
 *)
type t = File of Fpath.t [@@deriving show, eq, ord, sexp]

val to_string : t -> string
(** [to_string s] is the path [s] as a user-facing string. This is the version
   which should be displayed to the user. Cf. the derived show implementation,
   which is for internal debugging purposes.
 *)

val to_string_opt : ?unspecified:string -> t option -> string
(** [to_string_opt ~unspecified s] is the path [s] as a user-facing string, or
   "<unspecified>", if [s] is [None].
 *)
