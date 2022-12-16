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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Some code organizations are really bad. But because it's harder
 * to convince people to change it, sometimes it's simpler to create
 * a parallel organization, an "overlay" using simple symlinks
 * that represent a better organization. One can then show
 * statistics on those overlayed code organization, adapt layers,
 * etc
 *
 * related: LFS on code.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type overlay = {
  (* the filenames are in a readable path format *)
  orig_to_overlay: (Common.filename, Common.filename) Hashtbl.t;
  overlay_to_orig: (Common.filename, Common.filename) Hashtbl.t;
  data: (Common.filename (* overlay *) * Common.filename) list;

  (* in realpath format. This information is then specific
   * to one user ... but infering back the root_orig/root_overlay
   * from an arbitrary directory can be tedious.
  *)
  root_orig: Common.dirname;
  root_overlay: Common.dirname;
}

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

let load_overlay file =
  Common2.get_value file

let save_overlay overlay file =
  Common2.write_value overlay file

(*****************************************************************************)
(* Check consistency *)
(*****************************************************************************)

let check_overlay ~dir_orig ~dir_overlay =
  let dir_orig = Common.fullpath dir_orig in
  let files =
    Common.files_of_dir_or_files_no_vcs_nofilter [dir_orig]
    |> Common.exclude (fun file -> file =~ ".*/OVERLAY/.*")
  in

  let dir_overlay = Common.fullpath dir_overlay in
  let links =
    Common.cmd_to_list (spf "find %s -type l" dir_overlay) in

  let links = links |> Common.map_filter (fun file ->
    try Some (Common.fullpath file)
    with Failure s ->
      pr2 s;
      None
  )
  in
  let files2 =
    links |> List.map (fun file_or_dir ->
      Common.files_of_dir_or_files_no_vcs_nofilter [file_or_dir]
    ) |> List.flatten
  in
  pr2 (spf "#files orig = %d, #links overlay = %d, #files overlay = %d"
         (List.length files) (List.length links) (List.length files2)
      );
  let h = Hashtbl.create 101 in
  files2 |> List.iter (fun file ->
    if Hashtbl.mem h file
    then pr2 (spf "this one is a dupe: %s" file);
    Hashtbl.add h file true;
  );

  let (_common, only_in_orig, only_in_overlay) =
    Common2.diff_set_eff files files2 in


  only_in_orig |> List.iter (fun l ->
    pr2 (spf "this one is missing: %s" l);
  );
  only_in_overlay |> List.iter (fun l ->
    pr2 (spf "this one is gone now: %s" l);
  );
  if not (null only_in_orig && null only_in_overlay)
  then failwith "Overlay is not OK"
  else pr2 "Overlay is OK"

(*****************************************************************************)
(* Generate equivalences *)
(*****************************************************************************)

let overlay_equivalences ~dir_orig ~dir_overlay  =
  let dir_overlay = Common.fullpath dir_overlay in
  let dir_orig = Common.fullpath dir_orig in

  let links =
    Common.cmd_to_list (spf "find %s -type l" dir_overlay) in

  let equiv =
    links |> List.map (fun link ->
      let stat = Common2.unix_stat_eff link in
      match stat.Unix.st_kind with
      | Unix.S_DIR ->
          let (children, _) =
            Common2.cmd_to_list_and_status (spf
                                              "cd %s; find * -type f" (link)) in
          let dir = Common.fullpath link in

          children |> List.map (fun child ->
            let overlay = Filename.concat link child in
            let orig = Filename.concat dir child in
            overlay, orig
          )
      | Unix.S_REG ->
          [(link, Common.fullpath link)]
      | _ ->
          []
    ) |> List.flatten
  in
  let data =
    equiv |> Common.map_filter (fun (overlay, orig) ->
      try
        Some (
          Common.readable ~root:dir_overlay overlay,
          Common.readable ~root:dir_orig orig
        )
      with exn ->
        pr2 (spf "PB with %s, exn = %s" orig (Common.exn_to_s exn));
        None
    )
  in
  {
    data = data;
    overlay_to_orig = Common.hash_of_list data;
    orig_to_overlay = Common.hash_of_list (data |> List.map Common2.swap);
    root_overlay = dir_overlay;
    root_orig = dir_orig;
  }

let gen_overlay ~dir_orig ~dir_overlay ~output =
  let equiv = overlay_equivalences ~dir_orig ~dir_overlay in
  equiv.data |> List.iter pr2_gen;
  save_overlay equiv output

(*****************************************************************************)
(* Adapt layer *)
(*****************************************************************************)

let adapt_layer layer overlay =
  { layer with Layer_code.
            files = layer.Layer_code.files |> Common.map_filter (fun (file, info) ->
              try
                Some (Hashtbl.find overlay.orig_to_overlay file, info)
              with Not_found ->
                pr2 (spf "PB could not find %s in overlay" file);
                None
            );
  }

(* copy paste of the one in main_codemap.ml *)
let layers_in_dir dir =
  Common2.readdir_to_file_list dir |> Common.map_filter (fun file ->
    if file =~ "layer.*marshall"
    then Some (Filename.concat dir file)
    else None
  )

let adapt_layers ~overlay ~dir_layers_orig ~dir_layers_overlay =
  let layers = layers_in_dir dir_layers_orig in

  layers |> List.iter (fun layer_filename ->
    pr2 (spf "processing %s" layer_filename);
    let layer = Layer_code.load_layer layer_filename in
    let layer' = adapt_layer layer overlay in
    Layer_code.save_layer layer'
      (Filename.concat dir_layers_overlay (Common2.basename layer_filename))
  )


(*****************************************************************************)
(* Adapt database code *)
(*****************************************************************************)

let adapt_database db overlay =
  { db with Database_code.
         files = db.Database_code.files |> Common.map_filter (fun (file, info) ->
           try
             Some (Hashtbl.find overlay.orig_to_overlay file, info)
           with Not_found ->
             pr2 (spf "PB could not find %s in overlay" file);
             None
         );
         entities = db.Database_code.entities |> Array.map (fun e ->
           { e with Database_code.
                 e_file =
                   try
                     (Hashtbl.find overlay.orig_to_overlay e.Database_code.e_file)
                   with Not_found ->
                     "not_found_file_overlay";
           }
         );
  }
