(*
   Select spacegrep or spacecat behavior depending on the command name.
   This allows us to build a single executable, which is more compact
   than two.

   Cmdliner would let us create a single 'space' executable that would be
   called as 'space grep', 'space cat', 'space-grep', 'spacegrep', etc.
   but it may be confusing for users. Instead, we default to spacegrep
   behavior if the command name is unrecognized and we don't show spacecat
   on the spacegrep/default help page.
*)

let dispatch () =
  Printexc.record_backtrace true;
  match Filename.basename Sys.argv.(0) with
  | "spacecat" | "space-cat" -> Spacecat_main.main ()
  | _ -> Spacegrep_main.main ()

let () = dispatch ()
