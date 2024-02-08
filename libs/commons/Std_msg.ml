(*
   Utilities for printing user-facing messages with optional color on stderr.
*)

type highlight_setting = Auto | On | Off [@@deriving show]
type highlight = On | Off [@@deriving show]

let highlight_setting = ref Auto
let highlight = ref (Off : highlight)

let isatty () =
  !ANSITerminal.isatty UUnix.stdout && !ANSITerminal.isatty UUnix.stderr

let get_highlight_setting () = !highlight_setting
let get_highlight () = !highlight

let setup ?highlight_setting:(hs = (Auto : highlight_setting)) () =
  let hl : highlight =
    match hs with
    | Auto -> if isatty () then On else Off
    | On -> On
    | Off -> Off
  in
  highlight_setting := hs;
  highlight := hl

let with_highlight temp func =
  let orig = get_highlight_setting () in
  setup ~highlight_setting:temp ();
  Common.finalize func (fun () -> setup ~highlight_setting:orig ())

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
