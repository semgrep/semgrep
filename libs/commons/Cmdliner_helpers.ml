open Common
open Cmdliner

let uri =
  let parser str = Ok (Uri.of_string str) in
  let pp = Fmt.(using Uri.to_string string) in
  Arg.conv ~docv:"<URL>" (parser, pp)

let sha1 =
  let parser str =
    match Digestif.SHA1.of_hex_opt str with
    | Some sha1 -> Ok sha1
    | None -> Error (`Msg (Fmt.str "Invalid SHA1 value: %S" str))
  in
  let pp = Digestif.SHA1.pp in
  Arg.conv ~docv:"<SHA1>" (parser, pp)

(* Turn "a" into "-a" and "abc" into "--abc" *)
let add_option_dashes option_names =
  Common.map
    (fun s ->
      assert (s <> "");
      if String.length s =|= 1 then "-" ^ s else "--" ^ s)
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

(* Parse command-line arguments representing a number of bytes, such as
 * '5 mb' or '3.2GiB'
 *
 * ported from bytesize.py
 *)

let units_conversion =
  [
    ("B", 1.);
    ("KIB", 2. ** 10.);
    ("MIB", 2. ** 20.);
    ("GIB", 2. ** 30.);
    ("TIB", 2. ** 40.);
    ("KB", 10. ** 3.);
    ("MB", 10. ** 6.);
    ("GB", 10. ** 9.);
    ("TB", 10. ** 12.);
  ]

let number_of_bytes_converter : int Cmdliner.Arg.conv =
  let parser s =
    let fail =
      `Error (spf "Invalid representation for a number of bytes: %s" s)
    in
    let s = String.uppercase_ascii s in
    if s =~ "^\\([^ BKMGT]*\\)[ ]*\\([BKMGT][A-Z]*\\)$" then
      let number, unit = Common.matched2 s in
      match
        (float_of_string_opt number, List.assoc_opt unit units_conversion)
      with
      | Some n, Some unit -> `Ok (int_of_float (n *. unit))
      | _else_ -> fail
    else
      match int_of_string_opt s with
      | Some i -> `Ok i
      | None -> fail
  in
  let printer ppf x = Format.pp_print_int ppf x in
  (parser, printer)
