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

(** Origins of content which may be consumed by Semgrep.

   An origin is the {e user-relevant origin} of content (e.g., source code).
   This should be used when reporting findings to a user.

   While an origin may imply that content can be obtained from a given
   location, it should generally be stored alongside the content to be
   consumed, rather than used in lieu of it.

   This is so that origins can be used principally for {e reporting} where the
   content from which a finding was generated originated independently of
   getting the content of the target. This means we can de-couple reporting (1)
   where a finding originated from (2) obtaining the contents.

   This is since when (1) and (2) are fused, it is more difficult to
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

   This differs from {!Pos.t} and similar types ({!Loc.t}, {!Tok.location})
   since it does not report a location/byte position within a target, but
   rather where {e the target} came from.

 *)

(** The type for origins.

    {ul
      {- [File path] origins are for files, and should have as [path] the
        relative path from the scanned project root.}
    }

    Possible future variants:

    {ul
      {- [Stdin] origins are for content read from standard input.}
      {- [Network uri] origins are for content obtained from [uri] over the
      network.}
      {- [GitBlob info] origins are for git blob objects with metadata given in
      [info] (e.g., the blob's sha, commits the blob is present at, etc..).}
    }
 *)
type t = File of Fpath.t [@@deriving show, eq, ord]
(* TODO: Evaluate futher using Ppath.t instead of Fpath.t, since it documents
   we want this to be a relative path from the project root. *)

val to_string : t -> string
(** [to_string origin] is [origin] as a user-facing string. This is the version
    which should be displayed to the user. Cf. the derived show implementation,
    which is for internal debugging purposes.
 *)

val to_string_opt : ?unspecified:string -> t option -> string
(** [to_string_opt ~unspecified origin] is [origin] as a user-facing
    string, or "<unspecified>", if [origin] is [None].
 *)
