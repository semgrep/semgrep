(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
*)

open Common

module HC = Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type org_line =
  | Header of int * string (* full string, including starting stars *)
  (* todo? could also highlight the http refs and other markup ? *)
  | Comment of string
  | Other of string

type org = org_line list

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let parse file =
  let xs = Common.cat file in
  xs |> List.map (fun s ->
    let s = s ^ "\n" in
    match () with
    | _ when s =~ "^\\([*]+\\)" ->
        let header = Common.matched1 s in
        Header (String.length header, s)
    | _ when s =~ "^#.*" ->
        Comment s
    | _ ->
        Other s
  )


(*****************************************************************************)
(* Highlighting *)
(*****************************************************************************)

let highlight org =
  org |> Common.index_list_1 |> List.map (fun (org, line) ->
    let filepos = { Common2.l = line; c = 0; } in
    match org with
    | Comment s ->
        s, Some (HC.Comment), filepos
    | Other s ->
        let categ =
          (match s with
           | _ when s =~ "http://"  ->
               HC.EmbededUrl
           | _ when s =~ "https://"  ->
               HC.EmbededStyle
           | _ -> HC.Normal
          )
        in
        s, Some categ, filepos
    | Header (int, s) ->
        let categ =
          (match int with
           | 0 -> raise Impossible
           | 1 -> Some HC.CommentSection0
           | 2 -> Some HC.CommentSection1
           | 3 -> Some HC.CommentSection2
           | 4 -> Some HC.CommentSection3
           | 5 -> Some HC.CommentSection4
           | _ -> None
          )
        in
        s, categ, filepos
  )
