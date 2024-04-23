(* Martin Jambon
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extension of Testutil_files to help create git repo for testing purpose *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let create_git_repo ?(honor_gitignore = true)
    ?(user_email = "tester@example.com") ?(user_name = "Tester") () =
  (* to not mess with git commands output *)
  flush stdout;
  flush stderr;
  Git_wrapper.init ();
  (* We set user name and email to avoid warnings in some git
     versions. *)
  Git_wrapper.config_set "user.name" user_name;
  Git_wrapper.config_set "user.email" user_email;
  Git_wrapper.add ~force:(not honor_gitignore) [ Fpath.v "." ];
  let msg =
    if honor_gitignore then "Add files"
    else "Add all the files (including gitignored files)"
  in
  Git_wrapper.commit msg

(*****************************************************************************)
(* Masks *)
(*****************************************************************************)
(* TODO? do all of that in with_git_repo so we don't expose
 * those git command output to users of with_git_repo?
 *)

(* Mask lines like this one:
   [main (root-commit) 45e8b46] Add all the files
*)
let mask_temp_git_hash =
  Testo.mask_line ~after:"[main (root-commit) " ~before:"]" ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let with_git_repo ?verbose ?honor_gitignore ?(really_create_git_repo = true)
    ?user_email ?user_name (files : Testutil_files.t list) func =
  Testutil_files.with_tempfiles ?verbose ~chdir:true files (fun cwd ->
      if really_create_git_repo then
        create_git_repo ?honor_gitignore ?user_email ?user_name ();
      func cwd)
