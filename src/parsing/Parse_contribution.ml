(** Collect information about the project contributions from git log. *)

open Common

let git_json_format =
  "--pretty=format:{\"commit_hash\": \"%H\", \"commit_timestamp\": \"%ai\", \
   \"contributor\": {\"commit_author_name\": \"%an\", \"commit_author_email\": \
   \"%ae\"}}"

let get_git_logs () : string list =
  let cmd = Bos.Cmd.(v "git" % "log" % git_json_format) in
  let lines_r = Bos.OS.Cmd.run_out cmd in
  let lines = Bos.OS.Cmd.out_lines ~trim:true lines_r in
  let lines =
    match lines with
    | Ok (lines, (_, `Exited 0)) -> lines
    | _ -> []
  in
  (* out_lines splits on newlines, so we always have an extra space at the end *)
  List.filter (fun f -> not (String.trim f = "")) lines

let get_contributions () : Semgrep_output_v1_j.contributions =
  let logs = get_git_logs () in
  let contributions =
    Common.map (fun f -> Semgrep_output_v1_j.contribution_of_string f) logs
  in
  contributions
