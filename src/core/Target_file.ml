type provided_target_language_info =
  (* The default *)
  | NoSpecialTargeting
  (* When `--scan-unknown-extensions` is provided, explicitly passed files
     are scanned with all languages in the rule *)
  | ExplicitFileScanUnknownExtension (* | TODO Lang for -lang? *)
[@@deriving show]

type target_file = Fpath.t * provided_target_language_info [@@deriving show]
type target_files = target_file list [@@deriving show]
type t = target_file [@@deriving show]

(* Helper functions for converting between Fpath lists and target_files *)

let no_info_target_files_of_fpaths : Fpath.t list -> target_files =
  Common.map (fun x -> (x, NoSpecialTargeting))

let v (file, _info) = file
let fpaths_of_target_files : target_files -> Fpath.t list = Common.map v
