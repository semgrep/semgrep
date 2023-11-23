(*****************************************************************************)
(* Test helpers *)
(*****************************************************************************)

(* These aren't technically from publish.py, but from test.py, which is not
   yet ported.
   I have ported these individually, which could be used in the future when
   we port test.py
*)
let fixtest_suffix = ".fixed"
let yml_extensions = [ ".yml"; ".yaml" ]
let yml_test_suffixes = Common.map (fun ext -> ".test" ^ ext) yml_extensions

(* old: was a thing where we split up the file exts into a suffix list, then
   compared other suffix lists
   seems easier to just keep the ext as a string and check if it ends with
   the proper suffix
*)
let is_config_fixtest_suffix path =
  Fpath.basename path |> String.ends_with ~suffix:fixtest_suffix

let is_config_test_suffix path =
  let name = Fpath.basename path in
  List.exists (fun suffix -> String.ends_with ~suffix name) yml_test_suffixes
  && not (is_config_fixtest_suffix path)

let is_config_suffix path =
  let name = Fpath.basename path in
  List.exists (fun suffix -> String.ends_with ~suffix name) yml_extensions
  && (not (is_config_fixtest_suffix path))
  && not (is_config_test_suffix path)

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
        List.equal ( = ) (Common.take s' l1) (Common.take s' l2)
        && Fpath.equal
             (List.nth l1 s' |> Fpath.v |> Fpath.rem_ext ~multi:true)
             (List.nth l2 s' |> Fpath.v |> Fpath.rem_ext ~multi:true)
  | _ -> false

let get_config_filenames original_config =
  if Common2.is_file (Fpath.to_string original_config) then [ original_config ]
  else
    let configs =
      Common2.(glob (spf "%s/**" (Fpath.to_string original_config)))
    in
    configs
    |> Common.map_filter (fun file ->
           let fpath = Fpath.v file in
           if
             is_config_suffix fpath
             && (not (String.starts_with ~prefix:"." (Fpath.basename fpath)))
             && not
                  (String.starts_with ~prefix:"."
                     (Fpath.basename (Fpath.parent fpath)))
           then Some fpath
           else None)

let get_config_test_filenames ~original_config ~configs ~original_target =
  if
    Common2.is_file (Fpath.to_string original_config)
    && Common2.is_file (Fpath.to_string original_target)
  then [ (original_config, [ original_target ]) ]
  else
    let targets =
      (if Common2.is_file (Fpath.to_string original_target) then
         Common2.(
           glob (spf "%s/**" (Fpath.to_string (Fpath.parent original_target))))
       else Common2.(glob (spf "%s/**" (Fpath.to_string original_target))))
      |> Common.map Fpath.v
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

    Common.map
      (fun config ->
        ( config,
          List.filter
            (fun target -> target_matches_config target config)
            targets ))
      configs
