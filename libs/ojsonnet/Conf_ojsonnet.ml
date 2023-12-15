type eval_strategy = EvalSubst | EvalEnvir (* | EvalStrict *)
[@@deriving show { with_path = false }]

let eval_strategy = ref EvalEnvir

(* set to false to debug *)
let use_std = ref true

(* set also to false to help debug *)
let implement_self = ref true
let implement_dollar = ref true
