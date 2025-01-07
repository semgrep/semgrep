(* Yoann Padioleau
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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
(* Prelude *)
(*****************************************************************************)
(* A few helpers for the Logs library.
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(* unix time in seconds *)
let now () : float = UUnix.gettimeofday ()

(* This global is used by the reporter to print the difference between
   the time the log call was done and the time the program was started.

   TODO? Actually, the implementation is a bit dumb and probably show weird
   metrics when we use [lwt]. For such case, it's better to add a _counter_
   and use the tag mechanism to really show right metrics.
   alt: use Mtime_clock.now ()
*)
let time_program_start = now ()

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
let default_tag_str = "default"
let default_tags = [ default_tag_str ]
let default_tag = create_tag default_tag_str
let default_tag_set = create_tag_set [ default_tag ]

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

(* a complicated way of saying (not (is_empty (inter a b))) *)
let has_nonempty_intersection tag_str_list tag_set =
  Logs.Tag.fold
    (fun (V (def, _) : Logs.Tag.t) ok ->
      ok || List.mem (Logs.Tag.name def) tag_str_list)
    tag_set false

(* Consult environment variables from left-to-right in order of precedence. *)
let read_str_from_env_vars (vars : string list) : string option =
  List.find_map (fun var -> USys.getenv_opt var) vars

let read_comma_sep_strs_from_env_vars (vars : string list) : string list option
    =
  vars |> read_str_from_env_vars |> Option.map (String.split_on_char ',')

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
(* The "reporter" *)
(*****************************************************************************)

(* This code was copy-pasted and derived from the example in the Logs library.
   The Logs library interface makes us write this code that is frankly
   incomprehensible and excessively complicated given how little it provides.
*)
let mk_reporter ?(additional_reporters : Logs.reporter list = []) ~dst
    ~require_one_of_these_tags ~read_tags_from_env_vars:(env_vars : string list)
    ~highlight () =
  let require_one_of_these_tags =
    match read_comma_sep_strs_from_env_vars env_vars with
    | Some tags -> tags
    | None -> require_one_of_these_tags
  in
  (* Each debug message is implicitly tagged with "all". *)
  let select_all_debug_messages = List.mem "all" require_one_of_these_tags in

  let report src level ~over k msgf =
    let src_name = Logs.Src.name src in
    let is_default_src = src_name = "application" in
    let pp_style, _style, style_off =
      match color level with
      | Some x when highlight -> (pp_sgr, x, "0")
      | Some _
      | None ->
          ((fun _ppf _style -> ()), "", "")
    in
    let k _ =
      over ();
      k ()
    in
    let r =
      msgf (fun ?header ?(tags = default_tag_set) fmt ->
          let pp_w_time ~tags =
            let current = now () in
            (* Add a header that will look like [00.02][ERROR](lib):
             * coupling: if you modify the format, please update
             * the Testutil_logs.mask* regexps.
             *)
            Format.kfprintf k dst
              ("@[[%05.2f]%a%a%s: " ^^ fmt ^^ "@]@.")
              (current -. time_program_start)
              Logs_fmt.pp_header (level, header) pp_tags tags
              (if is_default_src then "" else "(" ^ src_name ^ ")")
          in
          match level with
          | App ->
              (* App level: no timestamp, tags, or other decorations *)
              Format.kfprintf k dst (fmt ^^ "@.")
          | Error
          | Warning
          | Info ->
              (* Print no tags for levels other than Debug since we can't
                 filter these messages by tag. *)
              pp_w_time ~tags:Logs.Tag.empty
          | Debug ->
              (* Tag-based filtering *)
              if
                select_all_debug_messages
                || has_nonempty_intersection require_one_of_these_tags tags
              then pp_w_time ~tags
              else (* print nothing *)
                Format.ikfprintf k dst fmt)
    in
    Format.fprintf dst "%a" pp_style style_off;
    r
  in
  (* Copied directly from the Logs.mli docs. Just calls a bunch of reporters in
     a row *)
  let combine r1 r2 =
    let report src level ~over k msgf =
      let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
      r2.Logs.report src level ~over (fun () -> v) msgf
    in
    { Logs.report }
  in
  List.fold_left combine { Logs.report } additional_reporters

(*****************************************************************************)
(* Specifying the log level with an environment variable *)
(*****************************************************************************)

let log_level_of_string_opt (str : string) : Logs.level option option =
  match str with
  | "app" -> Some (Some App)
  | "error" -> Some (Some Error)
  | "warning" -> Some (Some Warning)
  | "info" -> Some (Some Info)
  | "debug" -> Some (Some Debug)
  | "none" -> Some None
  | _ -> None

let read_level_from_env (vars : string list) : Logs.level option option =
  match read_str_from_env_vars vars with
  | None -> None
  | Some str -> log_level_of_string_opt str

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Enable basic logging so that you can use Logging calls even before a
 * precise call to setup_logging.
 *)
let setup_basic ?(level = Some Logs.Warning) () =
  Logs.set_level ~all:true level;
  Logs.set_reporter
    (mk_reporter ~dst:UFormat.err_formatter ~require_one_of_these_tags:[]
       ~read_tags_from_env_vars:[] ~highlight:false ());
  ()

let setup ?(highlight_setting = Console.get_highlight_setting ())
    ?log_to_file:opt_file ?(additional_reporters = [])
    ?(require_one_of_these_tags = default_tags)
    ?(read_level_from_env_vars = [ "LOG_LEVEL" ])
    ?(read_srcs_from_env_vars = [ "LOG_SRCS" ])
    ?(read_tags_from_env_vars = [ "LOG_TAGS" ]) ~level () =
  (* Override the log level if it's provided by an environment variable!
     This is for debugging a command that gets called by some wrapper. *)
  let level : Logs.level option =
    match read_level_from_env read_level_from_env_vars with
    | Some level_from_env -> level_from_env
    | None -> level
  in
  let show_srcs : Re.re list =
    read_comma_sep_strs_from_env_vars read_srcs_from_env_vars
    |> List_.optlist_to_list |> List_.map Re.Pcre.regexp
  in
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
  Logs.set_reporter
    (mk_reporter ~additional_reporters ~dst ~require_one_of_these_tags
       ~read_tags_from_env_vars ~highlight ());
  Logs.debug (fun m ->
      m "setup_logging: highlight_setting=%s, highlight=%B"
        (Console.show_highlight_setting highlight_setting)
        highlight);
  (* From https://github.com/mirage/ocaml-cohttp#debugging.
   * Disable all (third-party) libs logs unless specified in show_srcs
   * (which itself is derived from LOG_SRCS or similar environment variable).
   *)
  Logs.Src.list ()
  |> List.iter (fun src ->
         let src_name = Logs.Src.name src in
         let show_log =
           match src_name with
           (* those are the one we are really interested in *)
           | "application" -> true
           | x -> show_srcs |> List.exists (fun re -> Re.execp re x)
         in
         if not show_log then Logs.Src.set_level src None;
         Logs.debug (fun m ->
             m "%s logs for %s"
               (if show_log then "Showing" else "Skipping")
               src_name))

(*****************************************************************************)
(* Poor's man tracing *)
(*****************************************************************************)

let debug_trace_src = Logs.Src.create "debug_trace"

(* This ref and the setting and restoring of it allows
   with_debug_trace to keep track of whether the is another call to
   with_debug_trace wrapping the current call. Which enables only
   writing out backtraces on the outermost call to keep logs
   minimal. This could cause backtraces to not show up if someone
   catches and handles them between the first and the last
   with_debug_trace call. *)
let is_in_debug_trace_context = ref false

let with_debug_trace ?(src = debug_trace_src) ~__FUNCTION__
    ?(pp_input : (unit -> string) option) (f : unit -> 'a) : 'a =
  let name = __FUNCTION__ in
  let currently_in_debug_trace = !is_in_debug_trace_context in
  (match pp_input with
  | None -> Logs.debug ~src (fun m -> m "starting %s" name)
  | Some pp_input ->
      Logs.debug ~src (fun m ->
          m "starting %s with input:\n%s" name (pp_input ())));
  try
    is_in_debug_trace_context := true;
    let finally () = is_in_debug_trace_context := currently_in_debug_trace in
    let res = Common.protect ~finally f in
    Logs.debug ~src (fun m -> m "finished %s" name);
    res
  with
  | exn ->
      let exn' = Exception.catch exn in
      let msgf ppf =
        Format.fprintf ppf "exception during %s:\n" name;
        match pp_input with
        | None -> ()
        | Some pp_input ->
            Format.fprintf ppf "input:\n%s\n" (pp_input ());
            Format.fprintf ppf "exception: %s\n"
              (Printexc.to_string (Exception.get_exn exn'));
            (* Only print stack trace in the outermost handler to
                         prevent large duplications. *)
            if not currently_in_debug_trace then
              Format.fprintf ppf "backtrace:\n%s"
                (Printexc.raw_backtrace_to_string (Exception.get_trace exn'))
      in
      (* Purposefully not using ~src here so that it goes to the
         applications logs. *)
      (match exn with
      | Exception.Timeout _ ->
          (* %t the little known give me back my format stream
             specifier. *)
          Logs.debug (fun m -> m "%t" msgf)
      | _ -> Logs.err (fun m -> m "%t" msgf));
      Exception.reraise exn'

(*****************************************************************************)
(* Missing basic functions *)
(*****************************************************************************)

let app ?src ?tags str = Logs.app ?src (fun m -> m ?tags "%s" str)
let err ?src ?tags str = Logs.err ?src (fun m -> m ?tags "%s" str)
let warn ?src ?tags str = Logs.warn ?src (fun m -> m ?tags "%s" str)
let info ?src ?tags str = Logs.info ?src (fun m -> m ?tags "%s" str)
let debug ?src ?tags str = Logs.debug ?src (fun m -> m ?tags "%s" str)

let list to_string xs =
  Printf.sprintf "[%s]" (xs |> List_.map to_string |> String.concat ";")

let array to_string xs =
  Printf.sprintf "[|%s|]"
    (xs |> Array.to_list |> List_.map to_string |> String.concat ";")

let option to_string opt =
  match opt with
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" (to_string x)
