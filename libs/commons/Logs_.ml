(* Yoann Padioleau
 *
 * Copyright (C) 2022 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(** This global is used by the reporter to print the difference between
    the time the log call was done and the time the program was started.
    TODO? Actually, the implementation is a bit dumb and probably show weird
     metrics when we use [lwt]. For such case, it's better to add a _counter_
     and use the tag mechanism to really show right metrics.

     This variable is set when we configure loggers.
*)

(* in seconds *)
let time_program_start = ref 0.

let default_skip_libs =
  [
    "ca-certs";
    "bos";
    "cohttp.lwt.client";
    "cohttp.lwt.io";
    "conduit_lwt_server";
    "mirage-crypto-rng.lwt";
    "mirage-crypto-rng-lwt";
    "mirage-crypto-rng.unix";
    "handshake";
    "tls.config";
    "tls.tracing";
    "eio_linux";
    "x509";
  ]

(* used for testing *)
let in_mock_context = ref false

(*****************************************************************************)
(* String tags *)
(*****************************************************************************)
(*
   The interface of the Logs.Tag module is complicated. Here we assume
   tags are strings, that's it.
*)

(*
   The tag syntax is a dot-separated identifier similar to pytest markers.
   coupling: update the error message below when changing this syntax
*)
let tag_syntax = {|\A[A-Za-z_][A-Za-z_0-9]*(?:[.][A-Za-z_][A-Za-z_0-9]*)*\z|}

let has_valid_tag_syntax =
  let re = Re.Pcre.regexp tag_syntax in
  fun tag -> Re.execp re tag

let check_tag_syntax tag =
  if not (has_valid_tag_syntax tag) then
    invalid_arg
      (spf
         "Logs.create_tag: invalid syntax for test tag %S.\n\
          It must be a dot-separated sequence of one or more alphanumeric\n\
          identifiers e.g. \"foo_bar.v2.todo\" . It must match the following \
          regexp:\n\
         \  %s" tag tag_syntax)

let create_tag (tag : string) : string Logs.Tag.def =
  check_tag_syntax tag;
  Logs.Tag.def tag Format.pp_print_string

let create_tag_set (tag_list : string Logs.Tag.def list) : Logs.Tag.set =
  List.fold_left
    (fun set tag -> Logs.Tag.add tag (Logs.Tag.name tag) set)
    Logs.Tag.empty tag_list

let create_tags (tags : string list) : Logs.Tag.set =
  tags |> List_.map create_tag |> create_tag_set

let string_of_tag (Logs.Tag.V (def, _)) = Logs.Tag.name def

let string_of_tags tags =
  if Logs.Tag.is_empty tags then ""
  else
    let str =
      Logs.Tag.fold (fun tag list -> string_of_tag tag :: list) tags []
      |> String.concat ", "
    in
    spf "(%s)" str

(* This whole logging is going to be so sloooow <sigh>.
   In my opinion, the Format module is not suitable for logging,
   being potentially extremely slow. -- Martin
*)
let pp_tags fmt tags = Format.pp_print_string fmt (string_of_tags tags)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* ANSI escape sequences for colored output, depending on log level *)
let color level =
  match level with
  | Logs.Warning -> Some "33" (*yellow*)
  | Logs.Error -> Some "31" (*red*)
  | _else -> None

(* print an ANSI escape sequence - not worth to use an extra library
   (such as ANSIterminal) for this *)
let pp_sgr ppf style =
  Format.pp_print_as ppf 0 "\027[";
  Format.pp_print_as ppf 0 style;
  Format.pp_print_as ppf 0 "m"

(* alt: use Mtime_clock.now () *)
let now () : float = UUnix.gettimeofday ()

(* a complicated way of saying (not (is_empty (inter a b))) *)
let has_nonempty_intersection tag_str_list tag_set =
  Logs.Tag.fold
    (fun (V (def, _) : Logs.Tag.t) ok ->
      ok || List.mem (Logs.Tag.name def) tag_str_list)
    tag_set false

let has_tag opt_str_list tag_set =
  match opt_str_list with
  | None (* = no filter *) -> true
  | Some tag_str_list -> has_nonempty_intersection tag_str_list tag_set

let read_tags_from_env_var opt_var =
  match opt_var with
  | None -> None
  | Some var -> (
      match USys.getenv_opt var with
      | None -> None
      | Some str -> Some (String.split_on_char ',' str))

(* log reporter

   This code was copy-pasted and derived from the example in the Logs library.
   The Logs library interface makes us write this code that is frankly
   incomprehensible and excessively complicated given how little it provides.
*)
let reporter ~dst ~require_one_of_these_tags
    ~read_tags_from_env_var:(opt_env_var : string option) () =
  let require_one_of_these_tags =
    match read_tags_from_env_var opt_env_var with
    | Some _ as some_tags -> some_tags
    | None -> require_one_of_these_tags
  in
  let report _src level ~over k msgf =
    let pp_style, _style, style_off =
      match color level with
      | None -> ((fun _ppf _style -> ()), "", "")
      | Some x -> (pp_sgr, x, "0")
    in
    let k _ =
      over ();
      k ()
    in
    let r =
      msgf (fun ?header ?(tags = Logs.Tag.empty) fmt ->
          match level with
          | App ->
              (* App level: no timestamp, tags, or other decorations *)
              Format.kfprintf k dst (fmt ^^ "@.")
          | _ ->
              (* Tag-based filtering *)
              if has_tag require_one_of_these_tags tags then
                let current = now () in
                (* Add a header *)
                Format.kfprintf k dst
                  ("@[[%05.2f]%a%a: " ^^ fmt ^^ "@]@.")
                  (current -. !time_program_start)
                  Logs_fmt.pp_header (level, header) pp_tags tags
              else (* print nothing *)
                Format.ikfprintf k dst fmt)
    in
    Format.fprintf dst "%a" pp_style style_off;
    r
  in
  { Logs.report }

(* Note that writing to a freshly-opened file path can still write to
   a terminal. Such an example is '/dev/stderr'. *)
let isatty chan =
  let fd = UUnix.descr_of_out_channel chan in
  !ANSITerminal.isatty fd

let create_formatter opt_file =
  let chan, fmt =
    match opt_file with
    | None -> (UStdlib.stderr, UFormat.err_formatter)
    | Some out_file ->
        let oc =
          (* This truncates the log file, which is usually what we want for
             Semgrep. *)
          UStdlib.open_out (Fpath.to_string out_file)
        in
        (oc, UFormat.formatter_of_out_channel oc)
  in
  (isatty chan, fmt)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Enable basic logging (level = Logs.Warning) so that you can use Logging
 * calls even before a precise call to setup_logging.
 *)
let enable_logging () =
  Logs.set_level ~all:true (Some Logs.Warning);
  Logs.set_reporter
    (reporter ~dst:UFormat.err_formatter ~require_one_of_these_tags:None
       ~read_tags_from_env_var:None ());
  ()

let setup_logging ?(highlight_setting = Std_msg.get_highlight_setting ())
    ?log_to_file:opt_file ?(skip_libs = default_skip_libs)
    ?require_one_of_these_tags ?(read_tags_from_env_var = Some "LOG_TAGS")
    ~level () =
  let isatty, dst = create_formatter opt_file in
  let highlight =
    match highlight_setting with
    | On -> true
    | Off -> false
    | Auto -> isatty
  in
  let style_renderer =
    match highlight with
    | true -> Some `Ansi_tty
    | false -> None
  in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  time_program_start := now ();
  if not !in_mock_context then
    Logs.set_reporter
      (reporter ~dst ~require_one_of_these_tags ~read_tags_from_env_var ());
  Logs.debug (fun m ->
      m "setup_logging: highlight_setting=%s, highlight=%B"
        (Std_msg.show_highlight_setting highlight_setting)
        highlight);
  (* from https://github.com/mirage/ocaml-cohttp#debugging *)
  (* Disable all third-party libs logs *)
  Logs.Src.list ()
  |> List.iter (fun src ->
         match Logs.Src.name src with
         | x when List.mem x skip_libs -> Logs.Src.set_level src None
         (* those are the one we are really interested in *)
         | "application" -> ()
         | s -> failwith ("Logs library not handled: " ^ s))

(*****************************************************************************)
(* Missing basic functions *)
(*****************************************************************************)

let sdebug ?src ?tags str = Logs.debug ?src (fun m -> m ?tags "%s" str)
let sinfo ?src ?tags str = Logs.info ?src (fun m -> m ?tags "%s" str)
let swarn ?src ?tags str = Logs.warn ?src (fun m -> m ?tags "%s" str)
let serr ?src ?tags str = Logs.err ?src (fun m -> m ?tags "%s" str)
