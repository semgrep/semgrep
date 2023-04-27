(* TODO: this should be moved to libs/Git_wrapper.ml but ported to use
 * Bos because Lwt is causing issues with jsoo
 *)

(*****************************************************************************)
(* Use lwt_process *)
(*****************************************************************************)

(*diff unified format regex https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html#Detailed-Unified
 * The above documentation isn't great, so unified diff format is
 * @@ -start,count +end,count @@
 * where count is optional
 * Start and end here are misnomers. Start here refers to where this line starts in the A file being compared
 * End refers to where this line starts in the B file being compared
 * So if we have a line that starts at line 10 in the A file, and starts at line 20 in the B file, then we have
 * @@ -10 +20 @@
 * If we have a multiline diff, then we have
 * @@ -10,3 +20,3 @@
 * where the 3 is the number of lines that were changed
 * We use a named capture group for the lines, and then split on the comma if it's a multiline diff *)
let _git_diff_lines_re = {|@@ -\d*,?\d* \+(?P<lines>\d*,?\d*) @@|}
let git_diff_lines_re = SPcre.regexp _git_diff_lines_re

(* TODO: use Git_project.ml instead? *)
let is_git_repo () =
  let cmd = ("git", [| "git"; "rev-parse"; "--is-inside-work-tree" |]) in
  (* We use Lwt_process.exec here instead of the standard library because we can do timeouts *)
  let%lwt status =
    Lwt_process.exec ?timeout:(Some 1.0) ?stdin:(Some `Dev_null)
      ?stdout:(Some `Dev_null) ?stderr:(Some `Dev_null) cmd
  in
  match status with
  | Unix.WEXITED 0 -> Lwt.return true
  | _ -> Lwt.return false

let dirty_lines_of_file file =
  (* In the future we can make the HEAD part a parameter, and allow users to scan against other branches *)
  let cmd = ("git", [| "git"; "diff"; "-U0"; "HEAD"; file |]) in
  let lines =
    Lwt_process.pread_lines ?timeout:(Some 1.0) ?stdin:(Some `Dev_null)
      ?stderr:(Some `Dev_null) cmd
  in
  let%lwt lines = Lwt_stream.to_list lines in
  (* Unlines is such a good function name *)
  let lines = Common2.unlines lines in
  let matched_ranges = SPcre.exec_all ~rex:git_diff_lines_re lines in
  (* get the first capture group, then optionally split the comma if multiline diff *)
  let range_of_substrings substrings =
    let line = Pcre.get_substring substrings 1 in
    let lines = Str.split (Str.regexp ",") line in
    let first_line =
      match lines with
      | [] -> assert false
      | h :: _ -> h
    in
    let start = int_of_string first_line in
    let change_count =
      if List.length lines > 1 then int_of_string (List.nth lines 1) else 1
    in
    let end_ = change_count + start in
    (start, end_)
  in
  let matches =
    match matched_ranges with
    | Ok ranges ->
        Array.map
          (fun s ->
            try range_of_substrings s with
            | Not_found -> (-1, -1))
          ranges
    | Error _ -> [||]
  in
  Lwt.return matches

(* Once osemgrep is ported, this function will probably be subsumed by something else *)

let dirty_files () =
  let cmd =
    ("git", [| "git"; "status"; "--porcelain"; "--ignore-submodules" |])
  in
  let lines =
    Lwt_process.pread_lines ?timeout:(Some 1.0) ?stdin:(Some `Dev_null)
      ?stderr:(Some `Dev_null) cmd
  in
  let%lwt lines = Lwt_stream.to_list lines in
  (* First 3 chars are the status *)
  let files = Common.map (fun l -> Str.string_after l 3) lines in
  Lwt.return files
