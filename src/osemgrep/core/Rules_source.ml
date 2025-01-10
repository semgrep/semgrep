type t =
  (* For --pattern/--lang/--replacement (also -e/-l/--replacement).
   * In theory we could even parse the string to get a XPattern.t
   * Analyzer.t is now an option to allow to use -e without -l in osemgrep
   *)
  | Pattern of string * Analyzer.t option * string option (* replacement *)
  (* --config. In theory we could even parse the string to get
   * some Rules_config.t list *)
  | Configs of Rules_config.config_string list
(* TODO? | ProjectUrl of Uri.t? or just use Configs for it? *)
[@@deriving show]
