open Cmdliner

(* Turn "a" into "-a" and "abc" into "--abc" *)
let add_option_dashes option_names =
  Common.map
    (fun s ->
      assert (s <> "");
      if String.length s = 1 then "-" ^ s else "--" ^ s)
    option_names

(* Define a flag that can be negated e.g. --foo and --no-foo.
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
   See https://github.com/dbuenzli/cmdliner/issues/164
*)
let negatable_flag ?(default = false) ?env ~neg_options ~doc options =
  let neg_doc =
    let options_str = add_option_dashes options |> String.concat "/" in
    Printf.sprintf "negates %s" options_str
  in
  let enable = (true, Arg.info options ~doc ?env) in
  let disable = (false, Arg.info neg_options ~doc:neg_doc) in
  Arg.value (Arg.vflag default [ enable; disable ])
