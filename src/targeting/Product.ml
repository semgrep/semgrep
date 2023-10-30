module In = Input_to_core_t
module Out = Semgrep_output_v1_t

(* Note that both of these together imply that In.product equals Out.product *)
let _proof_that_input_subtype_output_product (x : In.product) : Out.product = x
let _proof_that_output_subtype_input_product (x : Out.product) : In.product = x
let all = [ `SAST; `SCA; `Secrets ]
