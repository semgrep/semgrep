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
let now () : float = Unix.gettimeofday ()

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
         \  %s"
         tag tag_syntax)

let create_tag (tag : string) : string Logs.Tag.def =
  check_tag_syntax tag;
  Logs.Tag.def tag Format.pp_print_string

let create_tag_set (tag_list : string Logs.Tag.def list) : Logs.Tag.set =
  List.fold_left
    (fun set tag -> Logs.Tag.add tag (Logs.Tag.name tag) set)
    Logs.Tag.empty tag_list

let create_tags (tags : string list) : Logs.Tag.set =
  tags |> List.map create_tag |> create_tag_set

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

(* a complicated way of saying (not (is_empty (inter a b))) *)
let has_nonempty_intersection tag_str_list tag_set =
  Logs.Tag.fold
    (fun (V (def, _) : Logs.Tag.t) ok ->
      ok || List.mem (Logs.Tag.name def) tag_str_list)
    tag_set false

(* Consult environment variables from left-to-right in order of precedence. *)
let read_str_from_env_vars (vars : string list) : string option =
  List.find_map (fun var -> Sys.getenv_opt var) vars

let read_comma_sep_strs_from_env_vars (vars : string list) : string list option
    =
  vars |> read_str_from_env_vars |> Option.map (String.split_on_char ',')

(* Note that writing to a freshly-opened file path can still write to
   a terminal. Such an example is '/dev/stderr'. *)
let isatty chan =
  let fd = Unix.descr_of_out_channel chan in
  !ANSITerminal.isatty fd

let create_dst opt_file =
  match opt_file with
  | None -> (isatty Stdlib.stderr, Format.get_err_formatter)
  | Some out_file ->
      let oc =
        (* This truncates the log file, which is usually what we want for
           Semgrep. *)
        Stdlib.open_out (Fpath.to_string out_file)
      in
      let key =
        Domain.DLS.new_key (fun () -> Format.formatter_of_out_channel oc)
      in
      (isatty oc, fun () -> Domain.DLS.get key)

(*****************************************************************************)
(* The "reporter" *)
(*****************************************************************************)

let mk_reporter
    ?(additional_reporters : (Logs.reporter -> Logs.reporter) list = []) ~dst
    ~require_one_of_these_tags ~read_tags_from_env_vars:(env_vars : string list)
    () =
  let require_one_of_these_tags =
    match read_comma_sep_strs_from_env_vars env_vars with
    | Some tags -> tags
    | None -> require_one_of_these_tags
  in
  (* Each debug message is implicitly tagged with "all".
     TODO: the default tag is "default", not "all"! In the present state,
     all debug logging is silent unless a nonempty list of tags is provided.
     Suggestion: remove support for tags entirely since selection by
     sources has been working well.
  *)
  let select_all_debug_messages = List.mem "all" require_one_of_these_tags in

  let report src level ~over k msgf =
    let dst = dst () in
    let src_name = Logs.Src.name src in
    let is_default_src = src_name = "application" in
    let k _ =
      over ();
      k ()
    in
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
  List.fold_left
    (fun reporter add_reporter -> add_reporter reporter)
    { Logs.report } additional_reporters

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

let default_is_active_source src =
  match Logs.Src.name src with
  | "application" -> true
  | _ -> false

(* This hook is set during setup. *)
let is_active_source_ref = Atomic.make default_is_active_source

(** Take a list of source names and return a function [is_active_src]
    suitable to activate log sources of that name and deactivate the rest. *)
let make_is_active_source source_names =
  let tbl = Hashtbl.create 10 in
  List.iter (fun name -> Hashtbl.replace tbl name ()) source_names;
  fun src -> Hashtbl.mem tbl (Logs.Src.name src)

(* This hook is set during setup. *)
let style_renderer_state = Atomic.make None

let set_style_renderer opt_style_renderer =
  Atomic.set style_renderer_state opt_style_renderer;
  Fmt_tty.setup_std_outputs ?style_renderer:opt_style_renderer ()

let with_style_renderer renderer func =
  let orig = Atomic.get style_renderer_state in
  Common.protect
    (fun () ->
      set_style_renderer renderer;
      func ())
    ~finally:(fun () -> set_style_renderer orig)

let set_level_for_all_sources ?(quiet_log_setup = false) ~is_active_src level =
  (* From https://github.com/mirage/ocaml-cohttp#debugging.
     Disable all (third-party) libs logs unless specified in show_srcs
     (which itself is derived from LOG_SRCS or similar environment variable).
  *)
  let active_sources, inactive_sources =
    Logs.Src.list () |> List.partition is_active_src
  in
  List.iter (fun src -> Logs.Src.set_level src level) active_sources;
  List.iter (fun src -> Logs.Src.set_level src None) inactive_sources;
  (* Using the application logger, show which sources are active and which
     ones are inactive. *)
  if not quiet_log_setup then (
    Logs.debug (fun m ->
        m "Skipping logs for: [%s]"
          (inactive_sources |> List.map Logs.Src.name |> String.concat ", "));
    Logs.debug (fun m ->
        m "Showing logs for: [%s]"
          (active_sources |> List.map Logs.Src.name |> String.concat ", ")))

(* Temporarily change the log level and optionally change which log sources
   are active. All active sources log at the same level. *)
let with_level ?quiet_log_setup ?sources level func =
  let is_active_src =
    match sources with
    | None -> Atomic.get is_active_source_ref
    | Some source_names -> make_is_active_source source_names
  in
  if not (Domain.is_main_domain ()) then
    invalid_arg
      "Logs_.with_level may not be called from another domain than the main \
       domain";
  let orig_is_active = Atomic.get is_active_source_ref in
  (* Assume the application is always active, so its log level is the
     log level used by all the active sources. *)
  let orig_level = Logs.Src.level Logs.default in
  Common.protect
    (fun () ->
      set_level_for_all_sources ?quiet_log_setup ~is_active_src level;
      Atomic.set is_active_source_ref is_active_src;
      func ())
    ~finally:(fun () ->
      (* Restore all globals to their original state *)
      set_level_for_all_sources ?quiet_log_setup ~is_active_src:orig_is_active
        orig_level;
      Atomic.set is_active_source_ref orig_is_active)

let with_reporter reporter func =
  let orig = Logs.reporter () in
  Common.protect
    (fun () ->
      Logs.set_reporter reporter;
      func ())
    ~finally:(fun () -> Logs.set_reporter orig)

(* Enable basic logging so that you can use Logging calls even before a
 * precise call to setup_logging.
 *)
let with_basic_setup ?(level = Some Logs.Warning) func =
  with_reporter
    (mk_reporter ~dst:Format.get_err_formatter ~require_one_of_these_tags:[]
       ~read_tags_from_env_vars:[] ()) (fun () -> with_level level func)

(*
   Logs should be used as follows:
   - Each library identifies as one or more log sources.
   - Only the application may identify as the default source.
   Each source has its own log level. If we want, we can set these log
   levels for each source independently.

   Here, we use the following simplification:
   - activation of a list of sources other the default
   - all activated sources use the same log level

   TODO: takes sources and levels as a key/value list in addition to
   the environment variables. This will allow easily changing log levels
   for specific tests.
   TODO: remove tags since we don't use them or used them wrong and it's
   complicated
*)
let with_setup ?(highlight_setting : Console.highlight_setting option)
    ?log_to_file:opt_file ?(additional_reporters = [])
    ?(require_one_of_these_tags = default_tags)
    ?(read_level_from_env_vars = [ "LOG_LEVEL" ])
    ?(read_srcs_from_env_vars = [ "LOG_SRCS" ])
    ?(read_tags_from_env_vars = [ "LOG_TAGS" ]) ?quiet_log_setup ~level func =
  if not (Domain.is_main_domain ()) then
    invalid_arg
      "Logs_.setup may not be called from another domain than the main domain";
  (* Override the log level if it's provided by an environment variable!
     This is for debugging a command that gets called by some wrapper. *)
  let level : Logs.level option =
    match read_level_from_env read_level_from_env_vars with
    | Some level_from_env -> level_from_env
    | None -> level
  in
  let show_srcs : Re.re list =
    read_comma_sep_strs_from_env_vars read_srcs_from_env_vars
    |> List_.optlist_to_list |> List.map Re.Pcre.regexp
  in
  let is_active_src src =
    match Logs.Src.name src with
    | "application" -> true
    | x -> show_srcs |> List.exists (fun re -> Re.execp re x)
  in
  let active_source_names =
    Logs.Src.list () |> List.filter is_active_src |> List.map Logs.Src.name
  in
  let isatty, dst = create_dst opt_file in
  let style_renderer =
    match highlight_setting with
    | None -> Atomic.get style_renderer_state
    | Some highlight_setting -> (
        let highlight =
          match highlight_setting with
          | On -> true
          | Off -> false
          | Auto -> isatty
        in
        match highlight with
        | true -> Some `Ansi_tty
        | false -> None)
  in
  with_style_renderer style_renderer @@ fun () ->
  let reporter =
    mk_reporter ~additional_reporters ~dst ~require_one_of_these_tags
      ~read_tags_from_env_vars ()
  in
  with_reporter reporter (fun () ->
      with_level ?quiet_log_setup ~sources:active_source_names level func)

(*****************************************************************************)
(* Poor's man tracing *)
(*****************************************************************************)

let debug_trace_src = Logs.Src.create "debug_trace"

(* This state keeps track of whether there is another call to
   [with_debug_trace] wrapping the current call; this enables only
   writing out the outermost call's backtrace to keep logs
   minimal. This could cause backtraces to not show up if someone
   catches and handles them between the first and the last
   with_debug_trace call.

   This value should really be a [Hook.t]; however, to avoid a circular
   dependency between [common] and [parallelism] it is left as a domain-local
   value.
   *)
let in_debug_trace = Domain.DLS.new_key (const false)

let with_debug_trace ?(src = debug_trace_src) ~__FUNCTION__
    ?(pp_input : (unit -> string) option) (f : unit -> 'a) : 'a =
  let name = __FUNCTION__ in
  let currently_tracing = Domain.DLS.get in_debug_trace in
  (match pp_input with
  | None -> Logs.debug ~src (fun m -> m "starting %s" name)
  | Some pp_input ->
      Logs.debug ~src (fun m ->
          m "starting %s with input:\n%s" name (pp_input ())));
  try
    let res =
      Common.protect
        ~finally:(fun () -> Domain.DLS.set in_debug_trace currently_tracing)
        (fun () ->
          Domain.DLS.set in_debug_trace true;
          f ())
    in
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
            if not currently_tracing then
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
  Printf.sprintf "[%s]" (xs |> List.map to_string |> String.concat ";")

let array to_string xs =
  Printf.sprintf "[|%s|]"
    (xs |> Array.to_list |> List.map to_string |> String.concat ";")

let option to_string opt =
  match opt with
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" (to_string x)

let if_in_debug src f =
  match Logs.Src.level src with
  | Some Logs.Debug -> f ()
  | Some Logs.(App | Error | Warning | Info)
  | None ->
      ()

let msg_with_detail ~src level desc detail =
  let effective_level =
    match Logs.Src.level src with
    | Some _ as l -> l
    | None -> Logs.level ()
  in
  let msg =
    match effective_level with
    | Some Logs.Debug -> desc ^ ": " ^ detail ()
    | _ -> desc
  in
  Logs.msg ~src level (fun m -> m "%s" msg)
