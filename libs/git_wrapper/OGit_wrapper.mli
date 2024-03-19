module Store = Git_unix.Store
module Hash = Store.Hash

type hash = Hash.t [@@deriving show, eq, ord]
type commit = Git.Commit.Make(Hash).t [@@deriving show, eq, ord]
type author = Git.User.t [@@deriving show, eq, ord]
type blob = Git.Blob.Make(Hash).t [@@deriving show, eq, ord]

type blob_with_extra = { blob : blob; path : Fpath.t; size : int }
[@@deriving show]

val commit_digest : commit -> hash
(** [commit_digest commit] is the SHA of the commit*)

val commit_author : commit -> author
(** [commit_author commit] is the author of the commit*)

val blob_digest : blob -> hash
(** [blob_digest blob] is the SHA of the blob*)

val string_of_blob : blob -> string
(** [string_of_blob blob] is the content of the blob*)

val hex_of_hash : hash -> string
(** [hex_of_hash hash] is the hexadecimal representation of the hash*)

val load_store : ?path:Fpath.t -> unit -> (Store.t, Store.error) result Lwt.t
(** [load_store ?path ()] is the representation of the git repository at [path] or the current directory if
    [path] is not provided*)

val load_store_exn : ?path:Fpath.t -> unit -> Store.t Lwt.t
(** [load_store_exn ?path ()] is the representation of the git repository at [path] or the current directory if
    [path] is not provided. It raises an error if the repository is not found*)

val commit_blobs_by_date : Store.t -> (commit * blob_with_extra list) list Lwt.t
(** [commit_blobs_by_date store] is the list of commits and the blobs they reference, ordered by date*)
