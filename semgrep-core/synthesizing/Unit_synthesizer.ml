(*s: semgrep/matching/Unit_matcher.ml *)
open Common
open OUnit

(*****************************************************************************)
(* Semgrep Unit tests *)
(*****************************************************************************)

(*s: function [[Unit_matcher.unittest]] *)
let unittest ~any_gen_of_string =
  "sgrep(generic) features" >:: (fun () ->

    (* spec: pattern string, code string, should_match boolean *)
    let cases = [
          "ex1.py", "4:9-4:29"
    ]
    in
    let check_pattern pat (file, range) =
      (* TODO: Semgrep_generic.match_any_file_range is a hypothetical function that
      checks whether pattern matches the file at the specified range *)
      let matches_with_env = Semgrep_generic.match_any_file_range pattern (file, range) in
        assert_bool (spf "pattern:|%s| should match |%s" spattern scode)
          (matches_with_env <> [])
    in
    cases |> List.iter (fun (file, range) ->
      let patterns = Synthesizer.synthesize_pattern range file in
      List.map (fun (label, pat) -> check_pattern pat (file, range)) patterns
  )
(*e: function [[Unit_matcher.unittest]] *)
(*e: semgrep/matching/Unit_matcher.ml *)
