type eval_strategy = EvalSubst | EvalEnvir (* | EvalStrict *)
[@@deriving show { with_path = false }]

(* nosemgrep: no-ref-declarations-at-top-scope *)
let eval_strategy = ref EvalEnvir

(* set to false to debug *)
(* nosemgrep: no-ref-declarations-at-top-scope *)
let use_std = ref true

(* set also to false to help debug *)
(* nosemgrep: no-ref-declarations-at-top-scope *)
let implement_self = ref true

(* nosemgrep: no-ref-declarations-at-top-scope *)
let implement_dollar = ref true
