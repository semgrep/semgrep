type t =
  (* For --pattern/--lang/--replacement (also -e/-l/--replacement).
   * In theory we could even parse the string to get a XPattern.t
   * Xlang.t is now an option to allow to use -e without -l in osemgrep
   *)
  | Pattern of string * Xlang.t option * string option (* replacement *)
  (* --config. In theory we could even parse the string to get
   * some Semgrep_dashdash_config.config_kind list *)
  | Configs of string (* Semgrep_dashdash_config.config_str *) list
(* TODO? | ProjectUrl of Uri.t? or just use Configs for it? *)
[@@deriving show]
