(*
   Utilities for printing user-facing messages with optional color on stderr.
*)

type highlight_setting = Auto | On | Off [@@deriving show]
type highlight = On | Off [@@deriving show]

let highlight_setting : highlight_setting ref = ref Auto
let highlight : highlight ref = ref (Off : highlight)

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

type style = Error | Warning | Success

let color_of_style = function
  | Error -> ANSITerminal.red
  | Warning -> ANSITerminal.yellow
  | Success -> ANSITerminal.green

let style_string style str =
  match get_highlight () with
  | On -> ANSITerminal.sprintf [ color_of_style style ] "%s" str
  | Off -> str

let strong_style_string style str =
  match get_highlight () with
  | On ->
      ANSITerminal.sprintf
        [ ANSITerminal.white; ANSITerminal.Bold; color_of_style style ]
        "%s" str
  | Off -> str

let error str = style_string Error str
let warning str = style_string Warning str
let success str = style_string Success str
let strong_error str = strong_style_string Error str
let strong_warning str = strong_style_string Warning str
let strong_success str = strong_style_string Success str
let error_tag () = strong_error " ERROR "
let warning_tag () = strong_warning " WARNING "
let success_tag () = strong_success " SUCCESS "
let print str = UPrintf.printf "%s\n%!" str
let eprint str = UPrintf.eprintf "%s\n%!" str
