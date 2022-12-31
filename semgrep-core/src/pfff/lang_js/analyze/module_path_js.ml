(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Resolving paths mentioned in imports (andreturning a canonical form).
 *
 * You can do:
 *  - import './xxx.js'
 *  - import '../xxx/yyy.js'
 *  - import 'xxx/yyy.js'
 *  - import 'xxx'
 *
 * and this leads to different files on the disk:
 *  - src/xxx.js
 *  - xxx/yyy.js
 *  - node_modules/xxx/yyy.js
 *  - node_modules/xxx/index.js
 *  - node_modules/xxx/api/dist.js
 *
 * By resolving paths we can have canonical names for imports
 * and so reference the same module entity in codegraph even
 * if the module is represented by different strings.
 *
 * reference:
 *  - TODO https://nodejs.org/api/modules.html
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let main_entry_of_package_json file json =
  match json with
  | J.Object xs -> (
      try
        match List.assoc "main" xs with
        | J.String s -> s
        | _ -> raise Not_found
      with
      | Not_found -> failwith (spf "no main entry in %s" file))
  | _ -> failwith (spf "wrong package.json format for %s" file)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve_path ~root ~pwd str =
  let candidates =
    [
      Filename.concat root (Filename.concat pwd str);
      Filename.concat root (Filename.concat pwd (spf "%s.js" str));
      (* less: should always look at package.json? or useful opti? *)
      Filename.concat root (spf "node_modules/%s" str);
      Filename.concat root (spf "node_modules/%s.js" str);
      Filename.concat root (spf "node_modules/%s/index.js" str);
    ]
  in
  try
    let found =
      candidates
      |> List.find (fun path ->
             Sys.file_exists path && not (Sys.is_directory path))
    in
    Some (Common.fullpath found)
  with
  | Not_found -> (
      (* look in package.json (of root/package or root/node_modules/package) *)
      let package_json_candidates =
        [
          Filename.concat root (spf "%s/package.json" str);
          Filename.concat root (spf "node_modules/%s/package.json" str);
        ]
      in
      try
        let package_json =
          package_json_candidates |> List.find Sys.file_exists
        in
        let json = J.load_json package_json in
        let main_path = main_entry_of_package_json package_json json in
        let dir = Filename.dirname package_json in
        let file = Filename.concat dir main_path in

        let candidates = [ file; spf "%s.js" file ] in
        candidates
        |> Common.find_opt (fun path ->
               Sys.file_exists path && not (Sys.is_directory path))
      with
      | Not_found ->
          pr2 (spf "could not find a package.json for %s" str);
          None)
