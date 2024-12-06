open Fpath_.Operators
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Type and helpers for rule tests (e.g., semgrep-rules/.../foo.yml.test)
 *
 * Partially translated from utils.py and test.py
 *)

(*****************************************************************************)
(* Test helpers *)
(*****************************************************************************)

let fixtest_suffix = ".fixed"
let yml_extensions = [ ".yml"; ".yaml" ]
let yml_test_suffixes = List_.map (fun ext -> ".test" ^ ext) yml_extensions

(* old: was a thing where we split up the file exts into a suffix list, then
   compared other suffix lists
   seems easier to just keep the ext as a string and check if it ends with
   the proper suffix
*)
let is_config_fixtest_suffix path =
  Fpath.basename path |> String.ends_with ~suffix:fixtest_suffix

(* alt: hannes:
   let is_config_fixtest_suffix path =
     let any_ext =
       let base = Fpath.basename path in
       match String.split_on_char '.' base with
       | _ :: tl -> tl
       | [] -> []
     in
     List.mem "fixed" any_ext
*)

let is_config_test_suffix path =
  let name = Fpath.basename path in
  List.exists (fun suffix -> String.ends_with ~suffix name) yml_test_suffixes
  && not (is_config_fixtest_suffix path)

(* alt: hannes:
   let is_config_test_suffix path =
     let ext =
       let fst = Fpath.get_ext path in
       let snd = Fpath.(get_ext (rem_ext path)) in
       snd ^ fst
     in
     List.mem ext (List_.map (fun e -> ".test" ^ e) yml_extensions)
     && not (is_config_fixtest_suffix path)
*)

let is_config_suffix path =
  let name = Fpath.basename path in
  List.exists (fun suffix -> String.ends_with ~suffix name) yml_extensions
  && (not (is_config_fixtest_suffix path))
  && not (is_config_test_suffix path)

(* alt: hannes:
   let is_config_suffix path =
     List.mem (Fpath.get_ext path) yml_extensions
     && not (is_config_test_suffix path)
*)

(* Brandon: I don't really understand what this code is really for. *)
let relatively_eq parent_target target parent_config config =
  match
    ( Fpath.relativize ~root:parent_target target,
      Fpath.relativize ~root:parent_config config )
  with
  | Some rel1, Some rel2 ->
      let l1 = Fpath.segs rel1 in
      let l2 = Fpath.segs rel2 in
      let s = List.length l2 in
      if List.length l1 < s then false
      else
        let s' = s - 1 in
        (* now l1 must be equal or greater in length to l2
           l1: x1 x2 ... xs xs+1 ... xn
           l2: y1 y2 ... ys

           we would like to check that all elements up to the sth index (exclusive)
           are equal

           and that xs and ys are the same, modulo suffix
        *)
        List.equal ( = ) (List_.take s' l1) (List_.take s' l2)
        && Fpath.equal
             (List.nth l1 s' |> Fpath.v |> Fpath.rem_ext ~multi:true)
             (List.nth l2 s' |> Fpath.v |> Fpath.rem_ext ~multi:true)
  | _ -> false

let get_config_filenames original_config =
  if UFile.is_reg ~follow_symlinks:true original_config then [ original_config ]
  else
    let configs = Common2.glob (Common.spf "%s/**" !!original_config) in
    configs
    |> List_.filter_map (fun file ->
           let fpath = Fpath.v file in
           if
             is_config_suffix fpath
             && (not (String.starts_with ~prefix:"." (Fpath.basename fpath)))
             && not
                  (String.starts_with ~prefix:"."
                     (Fpath.basename (Fpath.parent fpath)))
           then Some fpath
           else None)

(* alt: hannes:
   let get_all_files path =
     let str = Fpath.to_string path in
     Sys.readdir str |> Array.to_list
     |> List.filter (fun f -> Sys.file_exists f && not (Sys.is_directory f))
     |> List_.map (Fpath.add_seg path)


   let get_config_filenames target =
     let does_not_start_with_dot p =
       not (String.starts_with ~prefix:"." (Fpath.basename p))
     in
     match target with
     | Dir (path, None) ->
         let str = Fpath.to_string path in
         if Sys.file_exists str then
           if Sys.is_directory str then
             get_all_files path
             |> List.filter is_config_suffix
             |> List.filter does_not_start_with_dot
             |> List.filter (fun path ->
                    does_not_start_with_dot (Fpath.parent path))
           else [ path ]
         else []
     | Dir (_, Some str)
     | File (_, str) ->
         if Sys.file_exists str then [ Fpath.v str ] else []
*)

let get_config_test_filenames ~original_config ~configs ~original_target =
  if
    UFile.is_reg ~follow_symlinks:true original_config
    && UFile.is_reg ~follow_symlinks:true original_target
  then [ (original_config, [ original_target ]) ]
  else
    let targets =
      (if UFile.is_reg ~follow_symlinks:true original_target then
         Common2.glob (Common.spf "%s/**" !!(Fpath.parent original_target))
       else Common2.glob (Common.spf "%s/**" !!original_target))
      |> List_.map Fpath.v
    in

    let target_matches_config target config =
      let correct_suffix =
        (is_config_test_suffix target || not (is_config_suffix target))
        && not (is_config_fixtest_suffix target)
      in
      (Fpath.is_file_path original_target
      || relatively_eq original_target target original_config config)
      && Fpath.is_file_path target && correct_suffix
    in

    List_.map
      (fun config ->
        ( config,
          List.filter
            (fun target -> target_matches_config target config)
            targets ))
      configs

(* alt: hannes:
   let relative_eq parent_target target parent_config config =
     let rel_to a par =
       match Fpath.find_prefix a par with
       | None -> a
       | Some pre -> Option.get (Fpath.rem_prefix pre a)
     in
     let rel1 = rel_to target parent_target
     and rel2 = rel_to config parent_config in
     Fpath.(equal (rem_ext ~multi:true rel1) (rem_ext ~multi:true rel2))

   let get_config_test_filenames target configs =
     match target with
     | File (path, str) -> Map_.add (Fpath.v str) [ path ] Map_.empty
     | Dir (path, cfg_opt) ->
         let original_config =
           Option.value ~default:path (Option.map Fpath.v cfg_opt)
         in
         let targets = get_all_files path in
         let is_file p =
           let s = Fpath.to_string p in
           Sys.file_exists s && not (Sys.is_directory s)
         in
         let target_matches_config config target =
           let correct_suffix =
             (is_config_test_suffix target || not (is_config_suffix target))
             && not (is_config_fixtest_suffix target)
           in

           relative_eq path target original_config config
           && is_file target && correct_suffix
         in
         List.fold_left
           (fun m config ->
             let tgts = List.filter (target_matches_config config) targets in
             Map_.add config tgts m)
           Map_.empty configs
*)
