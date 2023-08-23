open File.Operators

let pp_targets_debug ppf (target_roots, semgrepignored_targets, targets) =
  Fmt.pf ppf "target roots: [@.";
  target_roots |> List.iter (fun root -> Fmt.pf ppf "  %s@." !!root);
  Fmt.pf ppf "]@.";
  Fmt.pf ppf "skipped targets: [@.";
  semgrepignored_targets
  |> List.iter (fun x ->
         Fmt.pf ppf "  %s" (Semgrep_output_v1_t.show_skipped_target x));
  Fmt.pf ppf "]@.";
  Fmt.pf ppf "selected targets: [@.";
  targets |> List.iter (fun file -> Fmt.pf ppf "target = %s@." !!file);
  Fmt.pf ppf "]@.";
  ()
