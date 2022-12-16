(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
(* Annotation extraction from comments.
 *
 * todo: might be very facebook specific and made obsolete by the
 * new module features in ES6 (import/export).
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type annotation =
  | ProvidesModule of Module_pre_es6.module_
  | ProvidesLegacy of Module_pre_es6.module_
  | RunWhenReady
  | Other of string

(*****************************************************************************)
(* String -> annotation *)
(*****************************************************************************)

(* We currently use a line-based and regexp approach to extract annotations.
 * Maybe at some point it could be better to have a comment lexer instead.
 * But the format of a comment has some line layout. For instance
 * annotations usually must be at the beginning of the line (after a possible
 * comment esthetic prefix mark), so the line-based approach may be better.
*)
let extract_annotations str =
  let lines = Common2.lines str |> List.map Comment_js.strip_comment_marks in
  lines |> Common2.map_flatten (fun str ->
    match () with
    | _ when str =~ "@providesModule[ \t]+\\([A-Za-z-_0-9]+\\)" ->
        [ProvidesModule (Common.matched1 str)]
    | _ when str =~ "@providesLegacy[ \t]+\\([A-Za-z-_0-9]+\\)" ->
        [ProvidesLegacy (Common.matched1 str)]
    | _ ->
        let xs = Common2.all_match "\\(@[A-Za-z-]+\\)" str in
        xs |> List.map (function
          | "@runWhenReady" -> RunWhenReady
          | s -> Other s
        )
  )


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* The returned parse_info is the one associated with the whole comment.
 * We use it in the tag generation. It's not very precise because
 * it's the info of the whole comment, and not just of the annotation,
 * but this should be good enough for the tags. Being more precise
 * would require to lex the comment.
*)
let annotations_of_program_with_comments { Parse_info. tokens = toks; _} =
  toks |> List.map (function
    | Parser_js.TComment tok ->
        let s = Parse_info.str_of_info tok in
        let annots = extract_annotations s in

        (* add location information to the annotation by reusing the
         * location information of the comment (that means
         * that multiple annotations in one comment will share
         * the same location, which is not very precise, but should
         * be good enough to locate the annotation when we
         * do checks related to annotations or for tags.
        *)
        annots |> List.map (fun annot -> annot, tok)
    | _ -> []
  ) |> List.flatten
