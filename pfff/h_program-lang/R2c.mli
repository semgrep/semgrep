(*s: pfff/h_program-lang/R2c.mli *)

(*s: signature [[R2c.error_to_json]] *)
val error_to_json: Error_code.error -> Json_type.t
(*e: signature [[R2c.error_to_json]] *)

(*s: signature [[R2c.string_of_errors]] *)
val string_of_errors: Error_code.error list -> string
(*e: signature [[R2c.string_of_errors]] *)
(*e: pfff/h_program-lang/R2c.mli *)
