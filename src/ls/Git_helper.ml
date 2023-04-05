(*diff unified format regex https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html#Detailed-Unified*)
let _git_diff_lines_re = {|@@ -\d*,?\d* \+(?P<lines>\d*,?\d*) @@|}
let git_diff_lines_re = SPcre.regexp _git_diff_lines_re

let is_git_repo () =
  let cmd = ("git", [| "git"; "rev-parse"; "--is-inside-work-tree" |]) in
  let%lwt status =
    Lwt_process.exec ?timeout:(Some 1.0) ?stdin:(Some `Dev_null)
      ?stdout:(Some `Dev_null) ?stderr:(Some `Dev_null) cmd
  in
  match status with
  | Unix.WEXITED 0 -> Lwt.return true
  | _ -> Lwt.return false

let dirty_lines_of_file file =
  (* Going to assume that  *)
  let cmd = ("git", [| "git"; "diff"; "-U0"; "HEAD"; file |]) in
  let lines =
    Lwt_process.pread_lines ?timeout:(Some 1.0) ?stdin:(Some `Dev_null)
      ?stderr:(Some `Dev_null) cmd
  in
  let%lwt lines = Lwt_stream.to_list lines in
  let lines = Common2.unlines lines in
  let matched_ranges = SPcre.exec_all ~rex:git_diff_lines_re lines in
  let range_of_substrings substrings =
    let line = Pcre.get_substring substrings 1 in
    let lines = Str.split (Str.regexp ",") line in
    let start = int_of_string (List.hd lines) in
    let end_ =
      if List.length lines > 1 then int_of_string (List.nth lines 1) else 1
    in
    let end_ = end_ - 1 + start in
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

let dirty_files () =
  let cmd =
    ("git", [| "git"; "status"; "--porcelain"; "--ignore-submodules" |])
  in
  let lines =
    Lwt_process.pread_lines ?timeout:(Some 1.0) ?stdin:(Some `Dev_null)
      ?stderr:(Some `Dev_null) cmd
  in
  let%lwt lines = Lwt_stream.to_list lines in
  let files = Common.map (fun l -> Str.string_after l 3) lines in
  Lwt.return files
