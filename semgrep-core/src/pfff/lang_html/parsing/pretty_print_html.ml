(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

open Ast_html

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let string_of_html_tree tree =
  match tree with
  | Element ((Tag ("__root__", _)), [], xs) ->
      let rec aux x =
        match x with
        | Element ((Tag (stag, _), attrs, xs)) ->
            let subs = xs |> List.map aux |> Common.join "" in
            let start_tag =
              match attrs with
              | [] -> spf "<%s>" stag
              | _ ->
                  spf "<%s %s>" stag
                    (attrs |> List.map (fun ((Attr (sattr,_)), (Val (sval,_)))->
                       spf "%s=%s" sattr sval
                     ) |> Common.join " "
                    )
            in
            let end_tag = spf "</%s>" stag in
            start_tag ^ subs ^ end_tag
        | Data (s, _) -> s
      in
      xs |> List.map aux |> Common.join ""
  | _ -> failwith "no root node"
