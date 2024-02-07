(*
   Utilities for printing user-facing messages with optional color on stderr.
*)

type highlight_setting = Auto | On | Off
type highlight = On | Off

let highlight_setting = ref Auto
let highlight = ref (Off : highlight)

let isatty () =
  !ANSITerminal.isatty UUnix.stdout && !ANSITerminal.isatty UUnix.stderr

let get_highlight_setting () = !highlight_setting
let get_highlight () = !highlight

let set_highlight ?(highlight_setting = (Auto : highlight_setting)) () =
  let hl : highlight =
    match highlight_setting with
    | Auto -> if isatty () then On else Off
    | On -> On
    | Off -> Off
  in
  highlight := hl

let setup = set_highlight

let with_highlight temp func =
  let orig = get_highlight_setting () in
  set_highlight ~highlight_setting:temp ();
  Common.finalize func (fun () -> set_highlight ~highlight_setting:orig ())

let highlight_error str =
  match get_highlight () with
  | On ->
      ANSITerminal.sprintf
        [ ANSITerminal.white; ANSITerminal.Bold; ANSITerminal.on_red ]
        "%s" str
  | Off -> str

let highlight_warning str =
  match get_highlight () with
  | On ->
      ANSITerminal.sprintf
        [ ANSITerminal.white; ANSITerminal.Bold; ANSITerminal.on_yellow ]
        "%s" str
  | Off -> str

let highlight_success str =
  match get_highlight () with
  | On ->
      ANSITerminal.sprintf
        [ ANSITerminal.white; ANSITerminal.Bold; ANSITerminal.on_green ]
        "%s" str
  | Off -> str

let error_tag () = highlight_error " ERROR "
let warning_tag () = highlight_warning " WARNING "
let success_tag () = highlight_success " SUCCESS "
let print str = UPrintf.printf "%s\n%!" str
let eprint str = UPrintf.eprintf "%s\n%!" str
