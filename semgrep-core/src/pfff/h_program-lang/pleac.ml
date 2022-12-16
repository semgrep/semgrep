(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

(*
 * PLEAC, the Programming Language Examples Alike Cookbook,
 * is a great resource when learning a new language. This module
 * provides some functions to parse pleac data and to generate
 * regular code that can then be analyzed and indexed and
 * then visualized like any other code.
 *
 * See http://pleac.sourceforge.net/
 *
 * Important files:
 *  - skeleton.sgml
 *  - *.data, language implementations
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type code_excerpt = string list
type section = string

type comment_style =
  string (* comment_start *) * string (* comment_end *)

type sections = (section, code_excerpt) Common.assoc

type skeleton =
  (string (* section1 *) *
   ((string (* section2 title *) * section) list))
    list


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let skip_no_heading xs =
  xs |> Common.exclude (fun (s, _) -> s =$= Common2.split_list_regexp_noheading)


let mangle_to_generate_filename s =
  Str.global_replace (Str.regexp "[- /.,':()]") "_" s

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* ex: (* @@PLEAC@@_1.0 *), or # @@PLEAC@@_1.0 *)
let regexp_section_pleac_data = "\\(.*\\) @@PLEAC@@_\\([0-9\\.]+\\)\\(.*\\)"
let parse_data_file file =
  file
  |> Common.cat
  |> Common2.split_list_regexp regexp_section_pleac_data |> skip_no_heading
  |> List.map (fun (s, group) ->
    if s =~ regexp_section_pleac_data
    then
      let (_, section, _) = Common.matched3 s in
      section, group
    else
      failwith ("Pleac.parse_data_file: impossible: " ^ s)
  )

let detect_comment_style file =
  file
  |> Common.cat
  |> Common2.return_when (fun s ->
    if s =~ regexp_section_pleac_data
    then
      let (s1, _s2, s3) = Common.matched3 s in
      Some (s1, s3)
    else None
  )

(* ex: <sect1 id="strings" label="1"><title>Strings</title> *)
let regexp_skeleton_section1 = "<sect1 .*<title>\\(.*\\)</title>"

(* ex: <sect2><title>Short Sleeps</title> *)
let regexp_skeleton_section2 = "<sect2><title>\\(.*\\)</title>"

(* ex: PLEAC:3.9:CAELP *)
let regexp_skeleton_section_number = "PLEAC:\\(.*\\):"


(* It's a sgml file so we could parse it using pxp and then visiting it
 * but using regexps is probably ok.
*)
let parse_skeleton_file file =
  file
  |> Common.cat
  |> Common2.split_list_regexp regexp_skeleton_section1 |> skip_no_heading
  |> List.map (fun (s, group) ->
    if s =~ regexp_skeleton_section1
    then
      let section1 = Common.matched1 s in
      section1,
      group
      |> Common2.split_list_regexp regexp_skeleton_section2 |> skip_no_heading
      |> List.map (fun (s2, group) ->
        if s2 =~ regexp_skeleton_section2
        then
          let section2 = Common.matched1 s2 in
          section2,
          group |> Common2.return_when (fun s3 ->
            if s3 =~ regexp_skeleton_section_number
            then Some (Common.matched1 s3)
            else None
          )
        else
          failwith ("Pleac.parse_data_file: impossible: " ^ s)
      )
    else
      failwith ("Pleac.parse_data_file: impossible: " ^ s)
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* todo? could also split using class with sections and static methods with
 * subsections. So could use M-x Pleac_Strings::TAB :)
*)

type gen_mode =
  | OneFilePerSection
  | OneDirPerSection

let gen_source_files
    skeleton sections (comment_start, comment_end)
    ~gen_mode
    ~output_dir
    ~ext_file
    ~hook_start_section2
    ~hook_line_body
    ~hook_end_section2
  =

  if not (Common2.command2_y_or_no("rm -rf " ^ output_dir))
  then failwith "ok we stop";

  Common.command2("mkdir -p " ^ output_dir);

  let hsections = Common.hash_of_list sections in

  let estet_sect1 = (Common2.repeat "*" 70) |> Common.join "" in
  let estet_sect2 = (Common2.repeat "-" 70) |> Common.join "" in

  (match gen_mode with
   | OneFilePerSection ->
       skeleton |> List.iter (fun (section1, xs) ->
         let file =
           Filename.concat output_dir
             (mangle_to_generate_filename section1) ^ "." ^ ext_file
         in
         Common.with_open_outfile file (fun (pr_no_nl, _chan) ->
           let pr s = pr_no_nl (s ^ "\n") in

           pr (spf "%s %s %s" comment_start estet_sect1  comment_end);
           pr (spf "%s %s %s" comment_start section1 comment_end);
           pr (spf "%s %s %s" comment_start estet_sect1  comment_end);
           xs |> List.iter (fun (section2, secnumber) ->
             let code_opt =
               try
                 Some (Hashtbl.find hsections secnumber)
               with Not_found ->
                 pr2 (spf "Section %s was not found in data file" secnumber);
                 None
             in
             pr (spf "%s %s %s" comment_start estet_sect2  comment_end);
             pr (spf "%s %s %s" comment_start section2 comment_end);
             pr (spf "%s %s %s" comment_start estet_sect2  comment_end);
             code_opt |> Option.iter (fun code -> code |> List.iter pr)
           )
         )
       )
   | OneDirPerSection ->
       skeleton |> List.iter (fun (section1, xs) ->
         let dir =
           Filename.concat output_dir
             (mangle_to_generate_filename section1) in

         Common.command2("mkdir -p " ^ dir);

         xs |> List.iter (fun (section2, secnumber) ->
           let file =
             Filename.concat dir
               (mangle_to_generate_filename section2) ^ "." ^ ext_file in

           let code_opt =
             try
               Some (Hashtbl.find hsections secnumber)
             with Not_found ->
               pr2 (spf "Section %s was not found in data file" secnumber);
               None
           in
           code_opt |> Option.iter (fun code ->
             Common.with_open_outfile file (fun (pr_no_nl, _chan) ->
               let pr s = pr_no_nl (s ^ "\n") in
               pr (spf "%s %s %s" comment_start estet_sect1  comment_end);
               pr (spf "%s %s %s" comment_start section2 comment_end);
               pr (spf "%s %s %s" comment_start estet_sect1  comment_end);
               pr (hook_start_section2 (mangle_to_generate_filename section2));
               code |> List.iter (fun s ->
                 pr (hook_line_body s)
               );
               pr (hook_end_section2 (mangle_to_generate_filename section2));
             )
           )
         )
       )
  )
