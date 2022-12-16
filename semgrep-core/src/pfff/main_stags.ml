(*
 * Please imagine a long and boring gnu-style copyright notice
 * appearing just here.
 *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * A more precise etags/ctags.
 *
 * Both etags and ctags are working at a lexical level
 * and so have lots of false positives for the definitions. For instance
 * on PHP, a line containing $idx will be considered as a candidate when
 * searching for idx. By using a real parser tagging a file becomes
 * trivial and correct (but slower I have to admit).
 *
 * Note that for OCaml the situation was kinda reversed. 'otags'
 * can work only at the AST level, which requires to
 * correctly parse the file. Nevertheless many files using camlp4 are
 * causing otags to fatal. One option is to help otags by passing it
 * the correct -pp flags. Another option is to at least default to
 * a lexical-level tag generator which is what I do for OCaml.
 * update: with the new .cmt format, one can finally have a precise
 * tag generator for OCaml code too.
 *
 * usage:
 *  $ stags -lang web -o TAGS *
 *
 * related work:
 *  - for ocaml: ocp-index, merlin, ocamlspot
 *  - https://www.gnu.org/software/global/
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below).
*)
let verbose = ref false

(* action mode *)
let action = ref ""

(* obsolete ? *)
let heavy_tagging = ref false

let lang = ref "php"

let output_file = ref "TAGS"

type format = Vim | Emacs
let format = ref Emacs

(*****************************************************************************)
(* Some debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Language specific *)
(*****************************************************************************)

let rec defs_of_files_or_dirs lang xs =
  let verbose = !verbose in
  let _heavy_tagging = !heavy_tagging in

  match lang with
  | "hack" ->
      Tags_php.php_defs_of_files_or_dirs ~verbose ~include_hack:true xs
  | "php" ->
      Tags_php.php_defs_of_files_or_dirs ~verbose (*~heavy_tagging*) xs
  | "js" ->
      Tags_js.tags_of_files_or_dirs ~verbose xs
  | "ml" ->
      Tags_ml.defs_of_files_or_dirs ~verbose xs

  | "web" ->
      let tag1 = defs_of_files_or_dirs "php" xs in
      let tag2 = defs_of_files_or_dirs "js" xs in
      tag1 @ tag2
  | ("cmt" | "java" | "php2") ->
      (match xs with
       | [root] ->
           let files = Find_source.files_of_root ~lang root in
           let only_defs = true in
           let g =
             match lang with
             #if FEATURE_CMT
             | "cmt" ->
                 let cmt_files = files in
                 let ml_files = Find_source.files_of_root ~lang:"ml" root in
                 Graph_code_cmt.build ~verbose ~root ~cmt_files ~ml_files
                                                                 #endif
             | "java" -> Graph_code_java.build ~verbose ~only_defs root files
             | "php2" -> Graph_code_php.build ~verbose ~only_defs root files|>fst
             | _ -> raise Impossible
           in
           Graph_code_tags.defs_of_graph_code ~verbose g

       | _ -> failwith "the cmt|java options accept only a single dir or file"
      )

  | _ -> failwith ("language not supported: " ^ lang)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  Logger.log Config_pfff.logger "stags" None;

  (* This can augment the size of the tags file
     let xs = List.map relative_to_absolute xs in
  *)
  let tags_file =
    (*
     * Common.relative_to_absolute "TAGS" in
     * let res = Common.y_or_no (spf "writing data in %s" tag_file) in
     * if not res
     * then failwith "ok I stop";
     *)
    !output_file
  in
  let files_and_defs = defs_of_files_or_dirs !lang xs in
  pr2 (spf "Writing data in %s" tags_file);


  (match !format with
   | Emacs -> Tags_file.generate_TAGS_file tags_file files_and_defs;
   | Vim -> Tags_file.generate_vi_tags_file tags_file files_and_defs;
  );
  ()

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  []

let options () =
  [
    "-lang", Arg.Set_string lang,
    (spf " <str> choose language (default = %s)" !lang);
    "-o", Arg.Set_string output_file,
    " <file> default = TAGS";
    "-vim", Arg.Unit (fun () -> format := Vim),
    " ";
    "-emacs", Arg.Unit (fun () -> format := Emacs),
    " ";
    "-symlinks", Arg.Unit (fun () -> Common.follow_symlinks := true;),
    " follow symlinks";
    "-heavy_tagging", Arg.Set heavy_tagging,
    " generates some extra tags with semantic prefix: F_, C_, M_\n";

  ] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  [
    "-verbose", Arg.Set verbose,
    " ";
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "stags version: %s" Config_pfff.version);
      exit 0;
    ), "  guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  (* Common_extra.set_link();
     let argv = Features.Distribution.mpi_adjust_argv Sys.argv in
  *)

  let usage_msg =
    "Usage: " ^ Common2.basename Sys.argv.(0) ^
    " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

     (* --------------------------------------------------------- *)
     (* actions, useful to debug subpart *)
     (* --------------------------------------------------------- *)
     | xs when List.mem !action (Common.action_list (all_actions())) ->
         Common.do_action !action xs (all_actions())

     | _ when not (Common.null_string !action) ->
         failwith ("unrecognized action or wrong params: " ^ !action)

     (* --------------------------------------------------------- *)
     (* main entry *)
     (* --------------------------------------------------------- *)
     | x::xs ->
         main_action (x::xs)

     (* --------------------------------------------------------- *)
     (* empty entry *)
     (* --------------------------------------------------------- *)
     | [] ->
         Common.usage usage_msg (options())
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )
