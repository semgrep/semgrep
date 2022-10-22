type t = Config_semgrep_t.t

let default_config = Config_semgrep_j.t_of_string "{}"

let pp fmt t =
  let s = Config_semgrep_j.string_of_t t in
  Format.fprintf fmt "%s" s
