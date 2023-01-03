(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
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
module PI = Parse_info
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Generating TAGS file (for emacs or vim)
 *
 * Supposed syntax for emacs TAGS (.tags) files, as analysed from output
 * of etags, read in etags.c and discussed with Francesco Potorti.
 * src: otags readme:
 *
 * <file> ::= <page>+
 * <page> ::= <header><body>
 * <header> ::= <NP><CR><file-name>,<body-length><CR>
 * <body> ::= <tag-line>*
 * <tag-line> ::= <prefix><DEL><tag><SOH><line-number>,<begin-char-index><CR>
 * pad: when tag is already at the beginning of the line:
 * <tag-line> ::=<tag><DEL><line-number>,<begin-char-index><CR>
 *
 * <NP> ::= ascii NP, (emacs ^L)
 * <DEL> ::= ascii DEL, (emacs ^?)
 * <SOH> ::= ascii SOH, (emacs ^A)
 * <CR> :: ascii CR
 *
 * See also http://en.wikipedia.org/wiki/Ctags#Tags_file_formats
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* see http://en.wikipedia.org/wiki/Ctags#Tags_file_formats *)
let header = "\x0c\n"
let footer = ""

type tag = {
  tag_definition_text : string;
  tagname : string;
  line_number : int;
  (* offset of beginning of tag_definition_text, when have 0-indexed filepos *)
  byte_offset : int;
  (* only used by vim *)
  kind : Entity_code.entity_kind;
}

let mk_tag s1 s2 i1 i2 k =
  {
    tag_definition_text = s1;
    tagname = s2;
    line_number = i1;
    byte_offset = i2;
    kind = k;
  }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_tag t =
  spf "%s\x7f%s\x01%d,%d\n" t.tag_definition_text t.tagname t.line_number
    t.byte_offset

(* of tests/misc/functions.php *)
(*
let fake_defs = [
  mk_tag "function a() {" "a" 3 7;
  mk_tag "function b() {" "b" 7 32;
  mk_tag "function c() {" "c" 14 65;
  mk_tag "function d() {" "d" 20 107;
]
*)

(* helpers used externally by language taggers *)
let tag_of_info filelines info kind =
  let line = PI.line_of_info info in
  let pos = PI.pos_of_info info in
  let col = PI.col_of_info info in
  let s = PI.str_of_info info in
  mk_tag filelines.(line) s line (pos - col) kind

(* C-s for "kind" in http://ctags.sourceforge.net/FORMAT *)
let vim_tag_kind_str tag_kind =
  match tag_kind with
  | E.Class -> "c"
  | E.Constant -> "d"
  | E.Function -> "f"
  | E.Method -> "f"
  | E.Type -> "t"
  | E.Field -> "m"
  | E.Module
  | E.Package
  | E.Global
  | E.Macro
  | E.TopStmts
  | E.Other _
  | E.ClassConstant
  | E.Constructor
  | E.File
  | E.Dir
  | E.MultiDirs
  | E.Exception
  | E.Prototype
  | E.GlobalExtern ->
      ""

(* vim uses '/' as a marker for the tag definition text, so if this
 * test contains '/' they must be escaped.
 *)
let vim_escape_slash str = Str.global_replace (Str.regexp "/") "\\/" str

(* For methods, in addition to the tag for the precise 'class::method'
 * name, it can be convenient to generate another tag with just the
 * 'method' name so people can quickly jump to some code with just the
 * method name. Of course if there is also a function somewhere using the
 * same name then this function could be hard to reach so we generate
 * an (imprecise) method tag only when there is no ambiguity.
 *)
let add_method_tags_when_unambiguous files_and_defs =
  (* step1: global analysis on all defs, remember all names and methods *)
  let h_toplevel_names =
    files_and_defs
    |> List.map (fun (_file, tags) ->
           tags
           |> Common.map_filter (fun t ->
                  match t.kind with
                  | E.Class
                  | E.Function
                  | E.Constant ->
                      Some t.tagname
                  | _ -> None))
    |> List.flatten |> Common.hashset_of_list
  in
  let h_grouped_methods =
    files_and_defs
    |> List.map (fun (_file, tags) ->
           tags
           |> Common.map_filter (fun t ->
                  match t.kind with
                  | E.Method ->
                      if t.tagname =~ ".*::\\(.*\\)" then
                        Some (Common.matched1 t.tagname, t)
                      else
                        failwith
                          ("method tag should contain '::[, got: " ^ t.tagname)
                  | _ -> None)
           (* could skip the group_assoc_bykey and do Hashtbl.find_all below instead *))
    |> List.flatten |> Common.group_assoc_bykey_eff |> Common.hash_of_list
  in
  (* step2: add method tag when no ambiguity *)
  files_and_defs
  |> List.map (fun (file, tags) ->
         ( file,
           tags
           |> List.map (fun t ->
                  match t.kind with
                  | E.Method ->
                      if t.tagname =~ ".*::\\(.*\\)" then
                        let methodname = Common.matched1 t.tagname in
                        if
                          (not (Hashtbl.mem h_toplevel_names methodname))
                          && List.length
                               (Hashtbl.find h_grouped_methods methodname)
                             = 1
                        then [ t; { t with tagname = methodname } ]
                        else [ t ]
                      else
                        failwith
                          ("method tag should contain '::[, got: " ^ t.tagname)
                  | _ -> [ t ])
           |> List.flatten ))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let threshold_long_line = 1000

let generate_TAGS_file tags_file files_and_defs =
  Common.with_open_outfile tags_file (fun (pr_no_nl, _chan) ->
      pr_no_nl header;
      files_and_defs
      |> List.iter (fun (file, defs) ->
             let all_defs =
               defs
               |> Common.map_filter (fun tag ->
                      if
                        String.length tag.tag_definition_text
                        > threshold_long_line
                      then (
                        pr2_once
                          (spf "WEIRD long string in %s, passing the tag" file);
                        None)
                      else Some (string_of_tag tag))
               |> Common.join ""
             in
             let size_defs = String.length all_defs in
             pr_no_nl (spf "%s,%d\n" file size_defs);
             pr_no_nl all_defs;
             pr_no_nl "\x0c\n"));
  ()

(* http://vimdoc.sourceforge.net/htmldoc/tagsrch.html#tags-file-format *)
let generate_vi_tags_file tags_file files_and_defs =
  Common.with_open_outfile tags_file (fun (pr_no_nl, _chan) ->
      let all_tags =
        files_and_defs
        |> List.map (fun (file, defs) ->
               defs
               |> Common.map_filter (fun tag ->
                      if String.length tag.tag_definition_text > 300 then (
                        pr2
                          (spf "WEIRD long string in %s, passing the tag" file);
                        None)
                      else Some (tag.tagname, (tag, file))))
        |> List.flatten |> Common.sort_by_key_lowfirst
      in
      all_tags
      |> List.iter (fun (_tagname, (tag, file)) ->
             (* {tagname}<Tab>{tagfile}<Tab>{tagaddress}
              * "The two characters semicolon and double quote [...] are
              * interpreted by Vi as the start of a comment, which makes the
              * following be ignored."
              *)
             pr_no_nl
               (spf "%s\t%s\t/%s/;\"\t%s\n" tag.tagname file
                  (vim_escape_slash tag.tag_definition_text)
                  (vim_tag_kind_str tag.kind))))
