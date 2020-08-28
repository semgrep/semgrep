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

module Xml = Xml_parse

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Functions to parse the XML source of the php manual.
 * 
 * The source can be found via 
 *  $ svn co http://svn.php.net/repository/phpdoc/en/trunk/
 * 
 * Here is an example of XML for array_change_key_case function:
 * 
 * <?xml version="1.0" encoding="utf-8"?>
 * <!-- $Revision: 297028 $ -->
 * <refentry xml:id="function.array-change-key-case" 
 *    xmlns="http://docbook.org/ns/docbook">
 * <refnamediv>
 * <refname>array_change_key_case</refname>
 * <refpurpose>Changes all keys in an array</refpurpose>
 * </refnamediv>
 * ...
 *)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* e.g. array-intersect.xml -> array_intersect *)
let function_name_of_xml_filename filename =
  let (_d,b,e) = Common2.dbe_of_filename filename in
  if e <> "xml" then failwith "not a xml file";

  Str.global_replace (Str.regexp "-") "_" b

(*
let _ = Common.example
  (function_name_of_xml_filename "array-intersect.xml" =$=
      "array_intersect")
*)


let parse_xml file =
  try 
    Xml.parse_file file
  with Xml_types.Error err ->
    pr2 (Xml.error err);
    failwith "parsing error"

    
let extract_text_xml_list xs =
  xs |> List.map (Xml.to_string_fmt ~escape_pcdata:false) |> Common.join " "

(*
 * The format of each function is quite big. Maybe it would be
 * simpler to just curl the pre-formatted output on the manual, e.g. with
 * links -dump http://www.php.net/manual/en/function.call-user-func-array.php
 * 
 *)
let extract_useful_doc xml =
  (* todo *)
  let refpurpose = ref ""in

  let programlisting = ref "" in
  let screen = ref "" in
  
  xml |> Xml_utils.iter_rec (fun (s, _attrs, body) ->
    match s with
    | "refpurpose" -> 
        refpurpose := extract_text_xml_list body
    | "programlisting" ->
        (* want the first one *)
        if !programlisting = "" then begin
          programlisting := extract_text_xml_list body;
        end
    | "screen" ->
        if !screen = "" then begin
          screen := extract_text_xml_list body;
        end
          
    | _ -> ()
  );
  spf "Purpose: %s\nExample: %s\nOutput: %s" 
    !refpurpose
    (Common2.indent_string 2 !programlisting)
    (Common2.indent_string 2 !screen)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let find_functions_reference_of_dir phpdoc_reference_dir =
  Common2.files_of_dir_or_files_no_vcs_post_filter
    ".*/functions/"
    [phpdoc_reference_dir]


let build_doc_function_finder phpdoc_reference_dir =
  let files = find_functions_reference_of_dir phpdoc_reference_dir in
  let h =
    files |> List.map (fun file -> 
      let func = function_name_of_xml_filename file in
      func, file) 
    |> Common.hash_of_list
  in
  (fun funcname ->
    let file = Hashtbl.find h funcname in
    let xml = parse_xml file in
    extract_useful_doc xml
  )
