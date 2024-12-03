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
      {- [GitBlob { sha; paths }] sources are for {{:
        https://git-scm.com/book/en/v2/Git-Internals-Git-Objects } git blob
        objects}, and have the blob's [sha] alongside relevant commit hashes
        and the path from the git root at each commit.}
    }

    Possible future variants:

    {ul
      {- [Stdin] origins are for content read from standard input.}
      {- [Network uri] origins are for content obtained from [uri] over the
      network.}
    }
 *)
type t =
  | File of Fpath.t
  (* TODO: Evaluate futher using Ppath.t instead of Fpath.t, since it documents
     we want this to be a relative path from the project root. *)
  | GitBlob of {
      sha : Git_wrapper.hash;
          (** The sha of the {e blob}. Used for git operations and to
              identify the object. *)
      paths : (Git_wrapper.commit * Fpath.t) list;
          (** The paths corresponding to this blob for whichever commits are of
              interest. This is stored since it is not efficient to calculate
              given only the blob's sha, and requires additional information
              like what commits we care about, if not all. These are relative
              to the git root.

              This is used for e.g., a rule's path-based include & excludes. *)
    }
[@@deriving show, eq, ord]

val to_string : t -> string
(** [to_string origin] is [origin] as a user-facing string. This is the version
    which should be displayed to the user. Cf. the derived show implementation,
    which is for internal debugging purposes.
 *)

val to_string_opt : ?unspecified:string -> t option -> string
(** [to_string_opt ~unspecified origin] is [origin] as a user-facing
    string, or "<unspecified>", if [origin] is [None].
 *)
