module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from target_manager.py (yield_verbose_lines())
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_skipped ppf
    ( respect_git_ignore,
      maturity,
      max_target_bytes,
      semgrep_ignored,
      include_ignored,
      exclude_ignored,
      file_size_ignored,
      other_ignored,
      errors ) =
  (* not sure why but pysemgrep does not use the classic heading for skipped*)
  Fmt.pf ppf "%s@.Files skipped:@.%s@." (String.make 40 '=')
    (String.make 40 '=');
  (* nope: Fmt_helpers.pp_heading ppf "Files skipped"; *)
  (* TODO: always skipped *)
  Fmt.pf ppf " %a@." Fmt.(styled `Bold string) "Skipped by .gitignore:";
  if respect_git_ignore then (
    Fmt.pf ppf " %a@.@."
      Fmt.(styled `Bold string)
      "(Disable by passing --no-git-ignore)";
    Fmt.pf ppf "  • <all files not listed by `git ls-files` were skipped>@.")
  else (
    Fmt.pf ppf " %a@.@."
      Fmt.(styled `Bold string)
      "(Disabled with --no-git-ignore)";
    Fmt.pf ppf "  • <none>@.");
  Fmt.pf ppf "@.";

  let pp_list (xs : Out.skipped_target list) =
    match xs with
    | [] -> Fmt.pf ppf "  • <none>@."
    | xs ->
        List.iter
          (fun (Out.{ path; _ } : Out.skipped_target) ->
            Fmt.pf ppf "  • %s@." path)
          (List.sort
             (fun (a : Out.skipped_target) (b : Out.skipped_target) ->
               String.compare a.path b.path)
             xs)
  in

  Fmt.pf ppf " %a@. %a@.@."
    Fmt.(styled `Bold string)
    "Skipped by .semgrepignore:"
    Fmt.(styled `Bold string)
    "(See: \
     https://semgrep.dev/docs/ignoring-files-folders-code/#understanding-semgrep-defaults)";
  pp_list semgrep_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@.@."
    Fmt.(styled `Bold string)
    "Skipped by --include patterns:";
  pp_list include_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@.@."
    Fmt.(styled `Bold string)
    "Skipped by --exclude patterns:";
  pp_list exclude_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " %a@. %a@.@."
    Fmt.(styled `Bold string)
    ("Skipped by limiting to files smaller than "
    ^ string_of_int max_target_bytes
    ^ " bytes:")
    Fmt.(styled `Bold string)
    "(Adjust with the --max-target-bytes flag)";
  pp_list file_size_ignored;
  Fmt.pf ppf "@.";

  (match maturity with
  | Maturity.Develop ->
      Fmt.pf ppf " %a@.@."
        Fmt.(styled `Bold string)
        "Skipped for other reasons:";
      pp_list other_ignored;
      Fmt.pf ppf "@."
  | _else_ -> ());

  Fmt.pf ppf " %a@.@."
    Fmt.(styled `Bold string)
    "Partially analyzed due to parsing or internal Semgrep errors";
  pp_list errors;
  Fmt.pf ppf "@."
