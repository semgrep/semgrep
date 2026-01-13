(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Create a temporary git repo for testing purposes, cd into it,
   call a function, tear down the repo, and restore the original cwd.
   This is an extension of commons/Testutil_files and its verbose
   option is reused.

   At least one regular file must be specified for the operation to succeed
   e.g. [File ("empty", "")].

   User name and email are set locally for the repo using default values
   which can be overridden.

   'really_create_git_repo:false' allows for tests to not create a git repo but
   create temporary files and remove them when done. Default is true.

   'force_add_gitignored_files:true' will cause gitignored files to be
   added anyway.
   Default is false.
*)
val with_git_repo :
  ?verbose:bool ->
  ?force_add_gitignored_files:bool ->
  ?really_create_git_repo:bool ->
  ?user_email:string ->
  ?user_name:string ->
  Testutil_files.t list ->
  (Fpath.t -> 'a) ->
  'a

(* A few masks to use in Testo.create ~normalize *)

(* Mask lines like this one:
   [main (root-commit) 45e8b46] Add all the files
*)
val mask_temp_git_hash : string -> string
