open Fpath_.Operators

let related_file_of_target ?polyglot_pattern_path ~ext file =
  let dirname, basename, _e = Filename_.dbe_of_filename !!file in
  let fail () =
    let msg =
      Common.spf "could not find %s file for test '%s' in either %s or %s" ext
        basename dirname
        (Common2.string_of_option Fpath.to_string polyglot_pattern_path)
    in
    Error msg
  in
  let candidate1 = Filename_.filename_of_dbe (dirname, basename, ext) in
  if Sys.file_exists candidate1 then Ok (Fpath.v candidate1)
  else
    match polyglot_pattern_path with
    | Some polyglot_pattern_path ->
        let candidate2 =
          Filename_.filename_of_dbe (!!polyglot_pattern_path, basename, ext)
        in
        if Sys.file_exists candidate2 then Ok (Fpath.v candidate2) else fail ()
    | _ -> fail ()

(* Allows the semgrep-core test runner that we use to test matches to also test
 * autofix. The format is pretty simple: add a `.fix` file with the fix pattern
 * and a `.fixed` file with the expected contents of the target after fixes are
 * applied.
 *
 * There are also end-to-end tests which test autofix on the CLI side.
 * Unfortunately, modifying them involves updating a large snapshot file, and
 * maintaining any large quantity of them would be onerous. Additionally, it's
 * not easy to adapt tests from our large existing test suite in semgrep-core to
 * also exercise autofix.
 *
 * Semgrep's `--test` flag can also test autofix
 * (https://github.com/semgrep/semgrep/pull/5190), but it has the same
 * problems as the existing autofix e2e tests for these purposes. *)
let compare_fixes ?polyglot_pattern_path ~file matches =
  let expected_fixed_text =
    let expected_fixed_file =
      related_file_of_target ?polyglot_pattern_path ~ext:"fixed" file
    in
    Result.map UFile.read_file expected_fixed_file
  in
  let processed_matches =
    Autofix.produce_autofixes (List_.map Core_result.mk_processed_match matches)
  in
  match expected_fixed_text with
  (* if we have no fixes and no .fixed file, no problem *)
  | Error _ -> ()
  (* but if we do have a .fixed file, ensure that the fixes are the same *)
  | Ok expected_fixed_text ->
      let fixed_text =
        processed_matches
        |> List_.filter_map (fun (m : Core_result.processed_match) ->
               m.autofix_edit)
        |> Autofix.apply_fixes_to_file_exn file
      in
      Alcotest.(check string) "applied autofixes" expected_fixed_text fixed_text
