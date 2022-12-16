open Common


let regexp_comment_line = "#.*"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let cat_and_filter_comments file =
  let xs = Common.cat file in
  let xs = xs |> List.map
             (Str.global_replace (Str.regexp regexp_comment_line) "" ) in
  let xs = xs |> Common.exclude Common2.is_blank_string in
  xs

(*****************************************************************************)
(* csv *)


(*****************************************************************************)
(* hierarchy ? header of section like in kernel_files.meta ? *)

(*
  (* split by header of section *)
  ..
    let xs = xs +> Common.split_list_regexp "^[^ ]" in

    let group = xs +> List.map (fun s ->
      assert (s =~ "^[ ]+\\([^ ]+\\) *: *\\(.*\\)");
      let (dir, email) = matched2 s in
      let emails = Common.split "[ ,]+" email in
      (dir, emails)
    ) in
    Subsystem ((dir, emails), group)
*)

(*****************************************************************************)
let title_colon_elems_space_separated file =

  let xs = cat_and_filter_comments file in

  xs |> List.map (fun s ->
    assert (s =~ "^\\([^ ]+\\):\\(.*\\)");
    let (title, elems_str) = matched2 s in
    let elems = Common.split "[ \t]+" elems_str in
    title, elems
  )
