(* Masking functions useful to be used with Testo.
 * coupling: the regexp must match the format in Logs_.mk_reporter
 *)
let mask_time =
  Testo.mask_pcre_pattern
    ~replace:(fun _ -> "<MASKED TIMESTAMP>")
    {|\[([0-9]+\.[0-9]{2})\]|}

let mask_log_lines =
  Testo.mask_pcre_pattern
    ~replace:(fun _ -> "<MASKED LOG LINE>")
    {|\[[0-9]+\.[0-9]{2}\][^\n]*|}
