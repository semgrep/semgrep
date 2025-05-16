let pp open_bracket close_bracket pp_elt fmt seq =
  let pp_comma fmt () = Format.fprintf fmt ",@ " in
  Format.fprintf fmt "%s%a%s" open_bracket
    (Format.pp_print_seq ~pp_sep:pp_comma pp_elt)
    seq close_bracket
