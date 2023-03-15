module Out = Output_from_core_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* One-off experiment for Raja.
 *
 * This experiment consists in returning the enclosing function name
 * and function body in the extra_extra JSON field for each match
 * results. This code path is triggered when passing --core-opts='-raja'
 * to the Semgrep CLI.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let adjust_core_match_results (x : Out.core_match_results) :
    Out.core_match_results =
  let matches =
    x.matches
    |> Common.map (fun (m : Out.core_match) ->
           let extra =
             {
               m.extra with
               extra_extra =
                 Some
                   (`Assoc
                     [
                       ("function_name", `String "TODO: funcname");
                       ("function_body", `String "TODO: body text or range?");
                     ]);
             }
           in
           { m with extra })
  in
  { x with matches }
