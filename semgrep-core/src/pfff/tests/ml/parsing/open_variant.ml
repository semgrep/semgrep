type common_flag = [
  | `AccPublic
  | `AccSynthetic
  | `AccRFU of int (** The int is a mask. *)
  | `AccFinal
]
