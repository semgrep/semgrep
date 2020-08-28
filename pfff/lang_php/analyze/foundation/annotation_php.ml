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
 * Helpers around code annotations in comments, e.g. @author.
 * 
 * Should I put this file in parsing_php/ ? On one hand it is some information
 * that are in comments and so that could be considered as parsing related.
 * But adding this info on the AST then requires to load the AST to 
 * access the information. Like for function types, it is useful
 * to have faster acess to high level information about functions.
 * Hence annotations/tags are stored in the database in the extra_id 
 * field. 
 * 
 * Moreover we may want to have as "annotations" things
 * which are not really in comment, like the THIS_FUNCTION_EXPIRES
 * macro call in the body of the function, or has_call_to_func_num_args
 * etc.
 * 
 * Most of this file is quite facebook specific, but other projects
 * may find it useful. 
 * less: should I extract facebook specific stuff and put it in facebook/ ?
 * 
 * less: could also extra the message associated with the annotation, such
 * as the reason for the @not-dead-code
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type email = string

let is_email s = 
  s =~ ".*@.*"

type unixname = string
 (* with tarzan *)
(* todo? should make sure it's a valid unixname, but it requires access
 * to the list of employees
 *)
let is_unixname s =
  (* update, we now accept just usename as email *)
  s =~ "[a-z][a-z0-9_]+"


type annotation = 
  | Owner of unixname
  | Emails of (email * notification_kind option) list
  | Status of string (* inactive usually *)

  (* deprecated *)
  | Author of string

  (* see http://www.intern.facebook.com/intern/wiki/index.php/Dead_code_reaper 
   * coupling: Deadcode_php.default_hooks contains a list that you should
   * update if you add new entries here. C-s for CalledFromPhpsh in 
   * deadcode_php.ml
  *)
  | CalledFromPhpsh
  | CalledOutsideTfb
  | CalledDynamically
  | NotDeadCode
  | Have_THIS_FUNCTION_EXPIRES_ON

  (* phpunit *)
  | DataProvider of method_callback

  (* dependency injection *)
  | Inject
  | Generics of string

  | Other of string

 (* see http://www.intern.facebook.com/intern/wiki/index.php/UnitTests/PHP#Controlling_Failure_Notification *)
 and notification_kind = 
   | Immediate
   | Consistent
   | Daily

 (* see http://www.phpunit.de/manual/current/en/appendixes.annotations.html#appendixes.annotations.dataProvider 
  * although the documentation does not give any format, for instance 
  * the @dataProvider class::method is not mentioned
  *)
 and method_callback =
   | Method of string
   | MethodExternal of string (* class *) * string
 (* with tarzan *)

exception AnnotationPb of string * Cst_php.info

(*****************************************************************************)
(* string -> annotation *)
(*****************************************************************************)

(* str is usually a comment, hence the strip_comment_marks below *)
let extract_annotations str tok =
  let lines = Common2.lines str in 

  lines |> Common2.map_flatten (fun str ->

    let str = Comment_php.strip_comment_marks str in

    match () with

    (*  
     * We allow both space and ',' as separator. That is both 
     * 
     *    @emails xxx@foo.com yyy@lists.bar.com
     *    @emails xxx@foo.com, yyy@lists.bar.com
     * 
     * are valid. Moreover each email can have its own notification.
     *)

    | _ when str =~ "@emails[ ]+\\(.*\\)$" ->

        let s = Common.matched1 str in
        let xs = Common.split "[ ,]+" s in

        let emails = xs |> List.map (fun s ->
          
          if s =~ "^\\([^:]+\\):?\\([a-z]*\\)$" 
          then 
            let (email, annot) = Common.matched2 s in

            let notification = 
              match annot with
              | "" -> None
              | "immediate" -> Some Immediate
              | "consistent" -> Some Consistent
              | "daily" -> Some Daily

              (* 
               * sgrimm uses 'trash' in his own tests on the 
               * unittest infrastructure, so remove this false positive
               * see flib/intern/unittest/__tests__/util.php:
               *   @emails bad-addr@nowhere.xx:trash
               * 
               *)
              | "trash" -> None
                  
              | s ->
                  raise (AnnotationPb ("wrong notification: " ^ s, tok))
            in

            if (not (is_email email || is_unixname email)) 
            then raise (AnnotationPb ("not a valid email address: " ^ s, tok));
            
            (email, notification)
          else raise (AnnotationPb ("not a valid email address: " ^ s, tok))
        ) in

        [Emails emails]

    | _ when str =~ "@owner[ ]+\\([a-z]+\\)" ->
        let name = Common.matched1 str in
        [Owner name]
    | _ when str =~ "@status[ ]+\\([a-z]+\\)" ->
        let name = Common.matched1 str in
        [Status name]

    | _ when str =~ "@author[ ]+\\([^ ]+\\)" ->
        let name = Common.matched1 str in
        [Author name]

    | _ when str =~ "^<\\([a-zA-Z0-9_]+\\)>$" ->
        let name = Common.matched1 str in
        [Generics name]

    | _ when 
          str =~ "@dataProvider[ ]+\\([a-zA-Z_0-9]+\\)::\\([a-zA-Z_0-9]+\\)" ->
        let (aclass, amethod) = Common.matched2 str in
        [DataProvider (MethodExternal (aclass, amethod))]

    | _ when str =~ "@dataProvider[ ]+\\([a-zA-Z_0-9]+\\)" ->
        let amethod = Common.matched1 str in
        [DataProvider (Method amethod)]
       
    | _ -> 
        let xs = Common2.all_match "\\(@[A-Za-z-]+\\)" str in
        xs |> List.map (function
        | "@called-from-phpsh" -> CalledFromPhpsh
        | "@called-outside-tfb" -> CalledOutsideTfb
        | "@called-dynamically" -> CalledDynamically
        | "@not-dead-code" -> NotDeadCode

        | "@Inject" -> Inject
        | s -> Other s
        )
  )

(*
let _ = example 
  (extract_annotations "@emails foo@bar:immediate, bar-list@foo\n@owner pad" = 
      [Emails [("foo@bar",      Some Immediate);
               ("bar-list@foo", None)];
       Owner "pad";
      ]
  )

let _ = example 
  (extract_annotations "// @emails foo@bar" = 
      [Emails [("foo@bar",      None);]]
  )
let _ = example 
  (extract_annotations "// @emails    foo@bar" = 
      [Emails [("foo@bar",      None);]]
  )
*)

(*****************************************************************************)
(* annotation -> string *)
(*****************************************************************************)


(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

(* auto generated by './ocamltarzan -choice vof ../analyze_php/annotation_php.ml' *)
let vof_email v = OCaml.vof_string v
  
let vof_unixname v = OCaml.vof_string v
  
let rec vof_annotation =
  function
  | Owner v1 -> let v1 = vof_unixname v1 in OCaml.VSum (("Owner", [ v1 ]))
  | Emails v1 ->
      let v1 =
        OCaml.vof_list
          (fun (v1, v2) ->
             let v1 = vof_email v1
             and v2 = OCaml.vof_option vof_notification_kind v2
             in OCaml.VTuple [ v1; v2 ])
          v1
      in OCaml.VSum (("Emails", [ v1 ]))
  | Author v1 ->
      let v1 = OCaml.vof_string v1 in OCaml.VSum (("Author", [ v1 ]))
  | Status v1 ->
      let v1 = OCaml.vof_string v1 in OCaml.VSum (("Status", [ v1 ]))
  | CalledFromPhpsh -> OCaml.VSum (("CalledFromPhpsh", []))
  | CalledOutsideTfb -> OCaml.VSum (("CalledOutsideTfb", []))
  | CalledDynamically -> OCaml.VSum (("CalledDynamically", []))
  | NotDeadCode -> OCaml.VSum (("NotDeadCode", []))
  | Inject -> OCaml.VSum (("Inject", []))
  | Generics v1 ->
      let v1 = OCaml.vof_string v1 in OCaml.VSum (("Generics", [ v1 ]))

  | Have_THIS_FUNCTION_EXPIRES_ON ->
      OCaml.VSum (("Have_THIS_FUNCTION_EXPIRES_ON", []))
  | DataProvider v1 ->
      let v1 = vof_method_callback v1
      in OCaml.VSum (("DataProvider", [ v1 ]))
  | Other v1 ->
      let v1 = OCaml.vof_string v1 in OCaml.VSum (("Other", [ v1 ]))
and vof_notification_kind =
  function
  | Immediate -> OCaml.VSum (("Immediate", []))
  | Consistent -> OCaml.VSum (("Consistent", []))
  | Daily -> OCaml.VSum (("Daily", []))
and vof_method_callback =
  function
  | Method v1 ->
      let v1 = OCaml.vof_string v1 in OCaml.VSum (("Method", [ v1 ]))
  | MethodExternal ((v1, v2)) ->
      let v1 = OCaml.vof_string v1
      and v2 = OCaml.vof_string v2
      in OCaml.VSum (("MethodExternal", [ v1; v2 ]))


(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let str_debug_of_annotation a = 
  let v = vof_annotation a in
  OCaml.string_of_v v

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let annotations_of_program_with_comments2 (_ast, tokens) =
 tokens |> List.map (function
  | Parser_php.T_COMMENT info
  | Parser_php.T_DOC_COMMENT info 
    ->
      let s = Parse_info.str_of_info info in
      let annots = extract_annotations s info in
      (* add location information to the annotation by reusing the
       * location information of the comment (that means
       * that multiple annotations in one comment will share
       * the same location, which is not very precise, but should
       * be good enough to locate the annotation when we
       * do checks related to annotations.
       *)
      annots |> List.map (fun annot -> annot, info)
  | _ -> []
 ) |> List.flatten

 
let annotations_of_program_with_comments a = 
  Common.profile_code "Annotation_php.annotations" (fun () ->
    annotations_of_program_with_comments2 a)

let annotations_before tok all_toks =
  let comment_opt = Comment_php.comment_before tok all_toks in
  match comment_opt with
  | Some ii ->
      let s = Parse_info.str_of_info ii in
      extract_annotations s ii
  | None -> []

let annotations_after tok all_toks =
  let comment_opt = Comment_php.comment_after tok all_toks in
  match comment_opt with
  | Some ii ->
      let s = Parse_info.str_of_info ii in
      extract_annotations s ii
  | None -> []
