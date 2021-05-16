(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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

open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Analyzing semgrep logs (SEMGREP_CORE_DEBUG=1; semgrep --debug ...)
 * to find slow rules/files/languages.
 *
 * Note that we should instead report those statistics more directly
 * in semgrep/semgrep-core instead of extracting it from our logs.
 *
 * coupling: the regexps in this module are strongly coupled with the
 * logging code in semgrep-core and semgrep.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let debug = ref false

(* timeout value used when running semgrep-core.
 * todo: we could extract it from the log.
 *)
let timeout = ref 10.

type run = {
  lang : string;
  rule : string;
  files : (filename * float) list;
  timeout : filename list;
}
[@@deriving show]

type stat_per_lang = {
  xlang : string;
  total_rules : int;
  total_time : float;
  total_files : int;
}

let report_stat_per_lang x =
  pr2 (spf "LANG = %s" x.xlang);
  pr2 (spf " # rules = %d" x.total_rules);
  pr2 (spf " total time = %.1f" x.total_time);
  pr2 (spf " total #files = %d" x.total_files);
  pr2 (spf " avg #files = %d" (x.total_files / x.total_rules));
  ()

let map_with_previous f base xs =
  let rec aux prev xs =
    match xs with
    | [] -> []
    | x :: xs ->
        let res = f prev x in
        res :: aux x xs
  in
  aux base xs

let parse_run (rule, xs) =
  let lang =
    match
      xs
      |> Common.find_some_opt (fun s ->
             if s =~ ".*Executed as.*-lang \\([^ ]+\\) .*" then
               Some (Common.matched1 s)
             else None)
    with
    | Some s -> s
    | None -> "no_language_found"
  in
  let last_time = ref 0. in
  let files =
    xs
    |> Common.map_filter (fun s ->
           match () with
           | _ when s =~ "\\[\\([0-9]+\\.[0-9]+\\) .* done with \\(.*\\)" ->
               let time, f = Common.matched2 s in
               let time = float_of_string time in
               last_time := time;
               Some (f, time)
           | _ when s =~ ".*raised Timeout in .* for \\(.*\\)" ->
               let f = Common.matched1 s in
               (* best guess *)
               last_time := !last_time +. !timeout;
               Some (f, !last_time)
           | _ -> None)
    |> map_with_previous
         (fun (_, prevtime) (f, time) -> (f, time -. prevtime))
         ("<nofile>", 0.)
  in
  let timeout =
    xs
    |> Common.map_filter (fun s ->
           if s =~ ".*raised Timeout in .* for \\(.*\\)" then
             Some (Common.matched1 s)
           else None)
  in
  { lang; rule; files; timeout }

let stat file =
  (* parsing *)
  let xs = Common.cat file in
  let ys = xs |> Common2.split_list_regexp "^Running rule" in
  let runs = ys |> List.map parse_run in

  if !debug then runs |> List.iter (fun r -> pr2 (show_run r));

  (* reporting *)
  pr2 (spf "TIMEOUT FILES (timeout = %.1f" !timeout);
  let timeout_files =
    runs |> List.map (fun x -> x.timeout) |> List.flatten |> Common2.uniq
  in
  timeout_files |> List.iter pr2_gen;

  pr2 "SLOW FILES";
  let problematic_files =
    runs
    |> List.map (fun x -> x.files)
    |> List.flatten
    |> Common.exclude (fun (file, _) -> List.mem file timeout_files)
    |> Common.sort_by_val_highfirst |> Common.take_safe 30
  in
  problematic_files |> List.iter pr2_gen;

  let problematic_rules =
    runs
    |> List.map (fun x ->
           ( (x.rule, List.length x.files, x.lang),
             x.files |> List.map snd |> Common2.sum_float ))
    |> Common.sort_by_val_highfirst |> Common.take_safe 30
  in
  pr2 "PROBLEMATIC RULES";
  problematic_rules |> List.iter pr2_gen;

  pr2 "STATS PER LANGUAGES";
  let groups = runs |> Common.group_by (fun x -> x.lang) in
  let stats =
    groups
    |> List.map (fun (xlang, xs) ->
           let total_rules = List.length xs in
           let total_time =
             xs
             |> List.map (fun x -> x.files)
             |> List.flatten |> List.map snd |> Common2.sum_float
           in
           let total_files =
             xs |> List.map (fun x -> List.length x.files) |> Common2.sum
           in
           { xlang; total_rules; total_time; total_files })
  in

  stats
  |> List.map (fun stat -> (stat, stat.total_time))
  |> Common.sort_by_val_highfirst
  |> List.iter (fun (stat, _) -> report_stat_per_lang stat);

  ()
