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
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * The goal of this module is to provide a data-structure to represent
 * code "layers" (a.k.a. code "aspects"). The idea is to imitate google
 * earth layers (e.g. the wikipedia layer, panoramio layer, etc), but
 * for code. One can have a deadcode layer, a test coverage layer,
 * and then can display those layers or not on an existing codebase in
 * codemap. The layer is basically some mapping from files to a
 * set of lines with a specific color code.
 *
 *
 * A few design choices:
 *
 *  - one could store such information directly into database_xxx.ml
 *    and have pfff_db compute such information (for instance each function
 *    could have a set of properties like unit_test, or dead) but this
 *    would force people to build their own db to visualize the results.
 *    One could compute this information in database_light_xxx.ml, but this
 *    will augment the size of the light db slowing down the codemap launch
 *    even when the people don't use the layers. So it's more flexible to just
 *    separate layer_code.ml from database_code.ml and have multiple persistent
 *    files for each information. Also it's quite convenient to have
 *    utilities like sgrep to be easily extendable to transform a query result
 *    into a layer.
 *
 *  - How to represent a layer at the macro and micro level in codemap ?
 *
 *    At the micro-level one has just to display the line with the
 *    requested color. At the macro-level have to either do a majority
 *    scheme or mixing scheme where for instance draw half of the
 *    treemap rectangle in red and the other in green.
 *
 *    Because different layers could have different composition needs
 *    it is simpler to just have the layer say how it should be displayed
 *    at the macro_level. See the 'macro_level' field below.
 *
 *  - how to have a layer data-structure that can cope with many
 *    needs ?
 *
 *   Here are some examples of layers and how they are "encoded" by the
 *   'layer' type below:
 *
 *    * deadcode (dead function, dead class, dead statement, dead assignnements)
 *
 *      How? dead lines in red color. At the macro_level one can give
 *      a grey_xxx color  with a percentage (e.g. grey53).
 *
 *    * test coverage (static or dynamic)
 *
 *      How? covered lines in green, not covered in red ? Also
 *      convey a GreyLevel visualization by setting the 'macro_level' field.
 *
 *    * age of file
 *
 *      How? 2010 in green, 2009 in yelow, 2008 in red and so on.
 *      At the macro_level can do a mix of colors.
 *
 *    * bad smells
 *
 *      How? each bad smell could have a different color and macro_level
 *      showing a percentage of the rectangle with the right color
 *      for each smells in the file.
 *
 *    * security patterns (bad smells)
 *
 *    * activity ?
 *
 *      How whow add and delete information ?
 *      At the micro_level can't show the delete, but at macro_level
 *      could divide the treemap_rectangle in 2 where percentage of
 *      add and delete, and also maybe white to show the amount of add
 *      and delete. Could also use my big circle scheme.
 *      How link to commit message ? TODO
 *
 *
 * later:
 *  - could  associate more than just a color, e.g. a commit message when want
 *    to display a version-control layer, or some filling-patterns in
 *    addition to the color.
 *  - Could have  better precision than the line.
 *
 * history:
 *  - I was writing some treemap generator specific for the deadcode
 *    analysis, the static coverage, the dynamic coverage, and the activity
 *    in a file (see treemap_php.ml). I was also offering different
 *    way to visualize the result (DegradeArchiColor | GreyLevel | YesNo).
 *    It was working fine but there was no easy way to combine 2
 *    visualisations, like the age "layer" and the "deadcode" layer
 *    to see correlations. Also adding simple layers like
 *    visualizing all calls to HTML() or XHP was requiring to
 *    write another treemap generator. To be more generic and flexible require
 *    a real 'layer' type.
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

type color = string (* Simple_color.emacs_color *)

(* note: the filenames must be in readable format so layer files can be reused
 * by multiple users.
 *
 * alternatives:
 *  - could have line range ? useful for layer matching lots of
 *    consecutive lines in a file ?
 *  - todo? have more precision than just the line ? precise pos range ?
 *
 *  - could for the lines instead of a 'kind' to have a 'count',
 *    and then some mappings from range of values to a color.
 *    For instance on a coverage layer one could say that from X to Y
 *    then choose this color, from Y to Z another color.
 *    But can emulate that by having a "coverage1", "coverage2"
 *    kind with the current scheme.
 *
 *  - have a macro_level_composing_scheme: Majority | Mixed
 *    that is then interpreted in codemap instead of forcing
 *    the layer creator to specific how to show the micro_level
 *    data at the macro_level.
*)

type layer = {
  title: string;
  description: string;
  files: (filename * file_info) list;
  kinds: (kind * color) list;
}
and file_info = {

  micro_level: (int (* line *) * kind) list;

  (* The list can be empty in which case codemap can use
   * the micro_level information and show a mix of colors.
   *
   * The list can have just one element too and have a kind
   * different than the one used in the micro_level. For instance
   * for the coverage one can have red/green at micro_level
   * and grey_xxx at macro_level.
  *)
  macro_level: (kind * float (* percentage of rectangle *)) list;
}
(* ugly: because of the ugly way OCaml.json_of_v currently works
 * the kind can not start with a uppercase
*)
and kind = string

(* with tarzan *)


(* The filenames in the index are in absolute path format. That way they
 * can be used from codemap in hashtbl and compared to the
 * current file.
*)
type layers_with_index = {
  root: Common.dirname;
  layers: (layer * bool (* is active *)) list;

  micro_index:
    (filename, (int, color) Hashtbl.t) Hashtbl.t;
  macro_index:
    (filename, (float * color) list) Hashtbl.t;
}

(*****************************************************************************)
(* Reusable properties *)
(*****************************************************************************)

let red_green_properties = [
  "ok", "green";
  "bad", "red";
  "no_info", "white";
]

let heat_map_properties = [
  "cover 100%", "red3";
  "cover 90%", "red1";
  "cover 80%", "orange";
  "cover 70%", "yellow";
  "cover 60%", "YellowGreen";
  "cover 50%", "green";
  "cover 40%", "cyan";
  "cover 30%", "cyan3";
  "cover 20%", "DeepSkyBlue1";
  "cover 10%", "blue";
  (* Should we use a dark blue for 0, as it is the case usually with
   * heatmaps? The picture can become very blue then.
   * Do not use white though because draw_macrolevel use white when nothing
   * was found so we want to differentiate such cases
  *)
  "cover 0%", "blue4"; (* alternative: snow4 *)

  (* when we zoom on a file we just show red/green coverage, no heat color *)
  "ok", "green";
  "bad", "red";

  "base", "azure4";
  "no_info", "white";
]

(*****************************************************************************)
(* Multi layers indexing *)
(*****************************************************************************)

(* Am I reinventing database indexing ? Should use a real database
 * to store layer information so one can then just use SQL to
 * fastly get all the information relevant to a file and a line ?
 * I doubt MySQL can be as fast and light as my JSON + hashtbl indexing.
*)
let build_index_of_layers ~root layers =
  let hmicro = Common2.hash_with_default (fun () -> Hashtbl.create 101) in
  let hmacro = Common2.hash_with_default (fun () -> []) in

  layers
  |> List.filter (fun (_layer, active) -> active)
  |> List.iter (fun (layer, _active) ->
    let hkind = Common.hash_of_list layer.kinds in

    layer.files |> List.iter (fun (file, finfo) ->

      let file = Filename.concat root file in

      (* todo? v is supposed to be a float representing a percentage of
       * the rectangle but below we will add the macro info of multiple
       * layers together which mean the float may not represent percentage
       * anynore. They still represent a part of the file though.
       * The caller would have to first recompute the sum of all those
       * floats to recompute the actual multi-layer percentage.
      *)
      let color_macro_level =
        finfo.macro_level |> Common.map_filter (fun (kind, v) ->
          (* some sanity checking *)
          try Some (v, Hashtbl.find hkind kind)
          with Not_found ->
            (* I was originally doing a failwith, but it can be convenient
             * to be able to filter kinds in codemap by just editing the
             * JSON file and removing certain kind definitions
            *)
            pr2_once (spf "PB: kind %s was not defined" kind);
            None
        )
      in
      hmacro#update file (fun old -> color_macro_level @ old);

      finfo.micro_level |> List.iter (fun (line, kind) ->
        try
          let color = Hashtbl.find hkind kind in

          hmicro#update file (fun oldh ->
            (* We add so the same line could be assigned multiple colors.
             * The order of the layer could determine which color should
             * have priority.
            *)
            Hashtbl.add oldh line color;
            oldh
          )
        with Not_found ->
          pr2_once (spf "PB: kind %s was not defined" kind);
      )
    );
  );
  {
    layers = layers;
    root = root;
    macro_index = hmacro#to_h;
    micro_index = hmicro#to_h;
  }


(*****************************************************************************)
(* Layers helpers *)
(*****************************************************************************)
let has_active_layers layers =
  layers.layers |> List.map snd |> Common2.or_list

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

(* generated by ocamltarzan *)

let vof_emacs_color s = OCaml.vof_string s
let vof_filename s = OCaml.vof_string s


let rec
  vof_layer {
    title = v_title;
    description = v_description;
    files = v_files;
    kinds = v_kinds
  } =
  let bnds = [] in
  let arg =
    OCaml.vof_list
      (fun (v1, v2) ->
         let v1 = vof_kind v1
         and v2 = vof_emacs_color v2
         in OCaml.VTuple [ v1; v2 ])
      v_kinds in
  let bnd = ("kinds", arg) in
  let bnds = bnd :: bnds in
  let arg =
    OCaml.vof_list
      (fun (v1, v2) ->
         let v1 = vof_filename v1
         and v2 = vof_file_info v2
         in OCaml.VTuple [ v1; v2 ])
      v_files in
  let bnd = ("files", arg) in
  let bnds = bnd :: bnds in
  let arg = OCaml.vof_string v_description in
  let bnd = ("description", arg) in
  let bnds = bnd :: bnds in
  let arg = OCaml.vof_string v_title in
  let bnd = ("title", arg) in let bnds = bnd :: bnds in OCaml.VDict bnds
and
  vof_file_info { micro_level = v_micro_level; macro_level = v_macro_level }
  =
  let bnds = [] in
  let arg =
    OCaml.vof_list
      (fun (v1, v2) ->
         let v1 = vof_kind v1
         and v2 = OCaml.vof_float v2
         in OCaml.VTuple [ v1; v2 ])
      v_macro_level in
  let bnd = ("macro_level", arg) in
  let bnds = bnd :: bnds in
  let arg =
    OCaml.vof_list
      (fun (v1, v2) ->
         let v1 = OCaml.vof_int v1
         and v2 = vof_kind v2
         in OCaml.VTuple [ v1; v2 ])
      v_micro_level in
  let bnd = ("micro_level", arg) in
  let bnds = bnd :: bnds in OCaml.VDict bnds
and vof_kind v = OCaml.vof_string v

(*****************************************************************************)
(* OCaml.v -> layer *)
(*****************************************************************************)

let emacs_color_ofv v = OCaml.string_ofv v
let filename_ofv v = OCaml.string_ofv v

let record_check_extra_fields = ref true

module OCamlx = struct
  open OCaml
  module J = JSON

(*
let stag_incorrect_n_args _loc tag _v =
  failwith ("stag_incorrect_n_args on: " ^ tag)
*)

(*
let unexpected_stag loc v =
  failwith ("unexpected_stag:")
*)

(*
let record_only_pairs_expected loc v =
  failwith ("record_only_pairs_expected:")
*)

  let record_duplicate_fields _loc _dup_flds _v =
    failwith ("record_duplicate_fields:")

  let record_extra_fields _loc _flds _v =
    failwith ("record_extra_fields:")

  let record_undefined_elements _loc _v _xs =
    failwith ("record_undefined_elements:")

  let record_list_instead_atom _loc _v =
    failwith ("record_list_instead_atom:")

  let tuple_of_size_n_expected  _loc n v =
    failwith (spf "tuple_of_size_n_expected: %d, got %s" n (Common2.dump v))

  let rec json_of_v v =
    match v with
    | VString s -> J.String s
    | VSum (s, vs) ->J.Array ((J.String s)::(List.map json_of_v vs ))
    | VTuple xs -> J.Array (xs |> List.map json_of_v)
    | VDict xs -> J.Object (xs |> List.map (fun (s, v) ->
      s, json_of_v v
    ))
    | VList xs -> J.Array (xs |> List.map json_of_v)
    | VNone -> J.Null
    | VSome v -> J.Array [ J.String "Some"; json_of_v v]
    | VRef v -> J.Array [ J.String "Ref"; json_of_v v]
    | VUnit -> J.Null (* ? *)
    | VBool b -> J.Bool b

    (* Note that 'Inf' can be used as a constructor but is also recognized
     * by float_of_string as a float (infinity), so when I was implementing
     * this code by reverse engineering the generated sexp, it was important
     * to guard certain code.
    *)
    | VFloat f -> J.Float f
    | VChar c -> J.String (Common2.string_of_char c)
    | VInt i -> J.Int i
    | VTODO _v1 -> J.String "VTODO"
    | VVar _v1 ->
        failwith "json_of_v: VVar not handled"
    | VArrow _v1 ->
        failwith "json_of_v: VArrow not handled"

(*
 * Assumes the json was generated via 'ocamltarzan -choice json_of', which
 * have certain conventions on how to encode variants for instance.
 *)
  let rec (v_of_json: J.t -> v) = fun j ->
    match j with
    | J.String s -> VString s
    | J.Int i -> VInt i
    | J.Float f -> VFloat f
    | J.Bool b -> VBool b
    | J.Null -> raise Todo

    (* Arrays are used for represent constructors or regular list. Have to
     * go sligtly deeper to disambiguate.
    *)
    | J.Array xs ->
        (match xs with
         (* VERY VERY UGLY. It is legitimate to have for instance tuples
          * of strings where the first element is a string that happen to
          * look like a constructor. With this ugly code we currently
          * not handle that :(
          *
          * update: in the layer json file, one can have a filename
          * like Makefile and we don't want it to be a constructor ...
          * so for now I just generate constructors strings like
          * __Pass so we know it comes from an ocaml constructor.
         *)
         | (J.String s)::xs when s =~ "^__\\([A-Z][A-Za-z_]*\\)$" ->
             let constructor = Common.matched1 s in
             VSum (constructor, List.map v_of_json  xs)
         | ys ->
             VList (ys |> List.map v_of_json)
        )
    | J.Object flds ->
        VDict (flds |> List.map (fun (s, fld) ->
          s, v_of_json fld
        ))

  let save_json file json =
    let s = J.string_of_json json in
    Common.write_file ~file s

end

(* I have not yet an ocamltarzan script for the of_json ... but I have one
 * for of_v, so have to pass through OCaml.v ... ugly
*)

let rec layer_ofv__ =
  let _loc = "Xxx.layer"
  in
  function
  | (OCaml.VDict field_sexps as sexp) ->
      let title_field = ref None and description_field = ref None
      and files_field = ref None and kinds_field = ref None
      and duplicates = ref [] and extra = ref [] in
      let rec iter =
        (function
          | (field_name, field_sexp) :: tail ->
              ((match field_name with
                 | "title" ->
                     (match !title_field with
                      | None ->
                          let fvalue = OCaml.string_ofv field_sexp
                          in title_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "description" ->
                     (match !description_field with
                      | None ->
                          let fvalue = OCaml.string_ofv field_sexp
                          in description_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "files" ->
                     (match !files_field with
                      | None ->
                          let fvalue =
                            OCaml.list_ofv
                              (function
                                | OCaml.VList ([ v1; v2 ]) ->
                                    let v1 = filename_ofv v1
                                    and v2 = file_info_ofv v2
                                    in (v1, v2)
                                | sexp ->
                                    OCamlx.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in files_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "kinds" ->
                     (match !kinds_field with
                      | None ->
                          let fvalue =
                            OCaml.list_ofv
                              (function
                                | OCaml.VList ([ v1; v2 ]) ->
                                    let v1 = kind_ofv v1
                                    and v2 = emacs_color_ofv v2
                                    in (v1, v2)
                                | sexp ->
                                    OCamlx.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in kinds_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
               iter tail)
          | [] -> ())
      in
      (iter field_sexps;
       if !duplicates <> []
       then OCamlx.record_duplicate_fields _loc !duplicates sexp
       else
       if !extra <> []
       then OCamlx.record_extra_fields _loc !extra sexp
       else
         (match ((!title_field), (!description_field), (!files_field),
                 (!kinds_field))
          with
          | (Some title_value, Some description_value,
             Some files_value, Some kinds_value) ->
              {
                title = title_value;
                description = description_value;
                files = files_value;
                kinds = kinds_value;
              }
          | _ ->
              OCamlx.record_undefined_elements _loc sexp
                [ ((!title_field = None), "title");
                  ((!description_field = None), "description");
                  ((!files_field = None), "files");
                  ((!kinds_field = None), "kinds") ]))
  | sexp -> OCamlx.record_list_instead_atom _loc sexp

and layer_ofv sexp = layer_ofv__ sexp
and file_info_ofv__ =
  let _loc = "Xxx.file_info"
  in
  function
  | (OCaml.VDict field_sexps as sexp) ->
      let micro_level_field = ref None and macro_level_field = ref None
      and duplicates = ref [] and extra = ref [] in
      let rec iter =
        (function
          | (field_name, field_sexp) :: tail ->
              ((match field_name with
                 | "micro_level" ->
                     (match !micro_level_field with
                      | None ->
                          let fvalue =
                            OCaml.list_ofv
                              (function
                                | OCaml.VList ([ v1; v2 ]) ->
                                    let v1 = OCaml.int_ofv v1
                                    and v2 = kind_ofv v2
                                    in (v1, v2)
                                | sexp ->
                                    OCamlx.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in micro_level_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "macro_level" ->
                     (match !macro_level_field with
                      | None ->
                          let fvalue =
                            OCaml.list_ofv
                              (function
                                | OCaml.VList ([ v1; v2 ]) ->
                                    let v1 = kind_ofv v1
                                    and v2 = OCaml.float_ofv v2
                                    in (v1, v2)
                                | sexp ->
                                    OCamlx.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in macro_level_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
               iter tail)
          | [] -> ())
      in
      (iter field_sexps;
       if !duplicates <> []
       then OCamlx.record_duplicate_fields _loc !duplicates sexp
       else
       if !extra <> []
       then OCamlx.record_extra_fields _loc !extra sexp
       else
         (match ((!micro_level_field), (!macro_level_field)) with
          | (Some micro_level_value, Some macro_level_value) ->
              {
                micro_level = micro_level_value;
                macro_level = macro_level_value;
              }
          | _ ->
              OCamlx.record_undefined_elements _loc sexp
                [ ((!micro_level_field = None), "micro_level");
                  ((!macro_level_field = None), "macro_level") ]))
  | sexp -> OCamlx.record_list_instead_atom _loc sexp
and file_info_ofv sexp = file_info_ofv__ sexp
and kind_ofv__ = let _loc = "Xxx.kind" in fun sexp -> OCaml.string_ofv sexp
and kind_ofv sexp = kind_ofv__ sexp

(*****************************************************************************)
(* Json *)
(*****************************************************************************)

let json_of_layer layer =
  layer |> vof_layer |> OCamlx.json_of_v

let layer_of_json json =
  json |> OCamlx.v_of_json |> layer_ofv

(*****************************************************************************)
(* Load/Save *)
(*****************************************************************************)

(* we allow to save in JSON format because it may be useful to let
 * the user edit the layer file, for instance to adjust the colors.
*)
let load_layer file =
  (* pr2 (spf "loading layer: %s" file); *)
  if File_type.is_json_filename file
  then J.load_json file |> layer_of_json
  else Common2.get_value file

let save_layer layer file =
  if File_type.is_json_filename file
  (* layer +> vof_layer +> OCaml.string_of_v +> Common.write_file ~file *)
  then layer |> json_of_layer |> OCamlx.save_json file
  else  Common2.write_value layer file

(*****************************************************************************)
(* Layer builder helper *)
(*****************************************************************************)

(* Simple layer builder - group by file, by line, by property.
 * The layer can also be used to summarize statistics per dirs and
 * subdirs and so on.
*)
let simple_layer_of_parse_infos ~root ~title ?(description="") xs kinds =
  let ranks_kinds =
    kinds |> List.map (fun (k, _color) -> k)
    |> Common.index_list_1 |> Common.hash_of_list
  in

  (* group by file, group by line, uniq categ *)
  let files_and_lines = xs |> List.map (fun (tok, kind) ->
    let file = Parse_info.file_of_info tok in
    let line = Parse_info.line_of_info tok in
    let file' = Common2.relative_to_absolute file in
    Common.readable ~root file', (line, kind)
  )
  in

  let (group_by_file: (Common.filename * (int * kind) list) list) =
    Common.group_assoc_bykey_eff files_and_lines
  in

  {
    title = title;
    description = description;
    kinds = kinds;
    files = group_by_file |> List.map (fun (file, lines_and_kinds) ->

      let (group_by_line: (int * kind list) list) =
        Common.group_assoc_bykey_eff lines_and_kinds
      in
      let all_kinds_in_file =
        group_by_line |> List.map snd |> List.flatten |> Common2.uniq in

      (file, {
         micro_level =
           group_by_line |> List.map (fun (line, kinds) ->
             let kinds = Common2.uniq kinds in
             (* many kinds om same line, keep highest prio *)
             match kinds with
             | [] -> raise Impossible
             | [x] -> line, x
             | _ ->
                 let sorted = kinds |> List.map (fun x ->
                   x, Hashtbl.find ranks_kinds x) |> Common.sort_by_val_lowfirst
                 in
                 line, List.hd sorted |> fst
           );

         macro_level =
           (* we could give a percentage per kind but right now
            * we instead give a priority based on the rank of the kinds
            * in the kind list
           *)
           all_kinds_in_file |> List.map (fun kind ->
             (kind, 1. /. (float_of_int (Hashtbl.find ranks_kinds kind)))
           )
       })
    );
  }


(* old: superseded by Layer_code.layer.files and file_info
 * type stat_per_file =
 *  (string (* a property *), int list (* lines *)) Common.assoc
 *
 * type stats =
 *  (Common.filename, stat_per_file) Hashtbl.t
 *
 *
 * old:
 * let (print_statistics: stats -> unit) = fun h ->
 * let xxs = Common.hash_to_list h in
 * pr2_gen (xxs);
 * ()
 *
 * let gen_security_layer xs =
 * let _root = Common.common_prefix_of_files_or_dirs xs in
 * let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in
 *
 * let h = Hashtbl.create 101 in
 *
 * files +> Common.index_list_and_total +> List.iter (fun (file, i, total) ->
 * pr2 (spf "processing: %s (%d/%d)" file i total);
 * let ast = Parse_php.parse_program file in
 * let stat_file = stat_of_program ast in
 * Hashtbl.add h file stat_file
 * );
 * Common.write_value h "/tmp/bigh";
 * print_statistics h
*)


(* Generates a layer_red_green<output> and layer_heatmap<output> file.
 * Take a list of files with a percentage and possibly micro_level
 * information.
*)
(*
let layer_red_green_and_heatmap ~root ~output xs =
  raise Todo
*)

(*****************************************************************************)
(* Layer stat *)
(*****************************************************************************)

(* todo? could be useful also to show # of files involved instead of
 * just the line count.
*)
let stat_of_layer layer =
  let h = Common2.hash_with_default (fun () -> 0) in

  layer.kinds |> List.iter (fun (kind, _color) ->
    h#add kind 0
  );
  layer.files |> List.iter (fun (_file, finfo) ->
    finfo.micro_level |> List.iter (fun (_line, kind) ->
      h#update kind (fun old -> old + 1)
    )
  );
  h#to_list


let filter_layer f layer =
  { layer with
    files = layer.files |> List.filter (fun (file, _) -> f file);
  }
