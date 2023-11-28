(* Note that we also have Engine_kind.t, which is an alias to semgrep_output_v1.engine_type with
   Just OSS | Pro. This might seem redundant with Engine_kind but here we are more concerned
   about configuring the engine, whereas Engine_kind.t is more useful as metrics for the users.
*)

type analysis_flavor = Intraprocedural | Interprocedural | Interfile
[@@deriving show]

type secrets_config = {
  (* Typically secrets will only run validators from semgrep.dev the
     allow_all_origins flag bypasses this security check. *)
  allow_all_origins : bool;
}
[@@deriving show]

type pro_config = {
  extra_languages : bool;
  analysis : analysis_flavor;
  secrets_config : secrets_config option; (* None = Disabled *)
}
[@@deriving show]

type t = OSS | PRO of pro_config [@@deriving show]
