(* We expose these language-specific functions here because they are needed in
 * Semgrep Pro.
 *
 * This lets us avoid having to duplicate the logic of these functions. *)
val is_js_angular_decorator : string -> bool
val go_package_alias : string -> string
