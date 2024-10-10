(*
   Guess whether a given file is indeed written in the specified
   programming language.
*)

open Common
open Fpath_.Operators
module Log = Log_targeting.Log

(****************************************************************************)
(* Types *)
(****************************************************************************)

(*
   Evaluation will be left-to-right and lazy, using the usual (&&) and (||)
   operators.
*)
type test =
  | And of test * test
  | Or of test * test
  | Not of test
  | Test_path of (Fpath.t -> bool)

let eval test path =
  let rec eval = function
    | And (f, g) -> eval f && eval g
    | Or (f, g) -> eval f || eval g
    | Not f -> not (eval f)
    | Test_path f -> f path
  in
  eval test

(****************************************************************************)
(* Helpers *)
(****************************************************************************)

let string_chop_prefix ~pref s =
  let len = String.length s in
  let preflen = String.length pref in
  if len >= preflen && Str.first_chars s preflen = pref then
    Some (String.sub s preflen (len - preflen))
  else None

let has_suffix suffixes =
  let f path =
    List.exists (fun suf -> Filename.check_suffix !!path suf) suffixes
  in
  Test_path f

let is_named valid_names =
  let f path =
    let name = Filename.basename !!path in
    List.mem name valid_names
  in
  Test_path f

let prepend_period_if_needed s =
  match s with
  | "" -> "."
  | s -> if s.[0] <> '.' then "." ^ s else s

(*
   Both '.d.ts' and '.ts' are considered extensions of 'hello.d.ts'.
*)
let has_extension extensions =
  has_suffix (List_.map prepend_period_if_needed extensions)

let has_lang_extension lang = has_extension (Lang.ext_of_lang lang)

let has_excluded_lang_extension lang =
  has_extension (Lang.excluded_exts_of_lang lang)

let has_an_extension =
  let f path = Filename.extension !!path <> "" in
  Test_path f

let is_executable =
  let f path =
    Sys.file_exists !!path
    &&
    let st = Unix.stat !!path in
    match st.st_kind with
    | S_REG ->
        (* at least some user has exec permission *)
        st.st_perm land 0o111 <> 0
    | _ -> false
  in
  (* ".exe" is intended for Windows, although this would be a binary file, not
     a script. *)
  Or (has_extension [ ".exe" ], Test_path f)

let get_first_line path =
  UFile.with_open_in path (fun ic ->
      try input_line ic with
      | End_of_file -> (* empty file *) "")

(*
   Get the first N bytes of the file, which is ideally obtained from
   a single filesystem block.
*)
let get_first_block ?(block_size = 4096) path =
  UFile.with_open_in path (fun ic ->
      let len = min block_size (in_channel_length ic) in
      really_input_string ic len)

let shebang_re = lazy (Pcre2_.regexp "^#![ \t]*([^ \t]*)[ \t]*([^ \t].*)?$")
let split_cmd_re = lazy (Pcre2_.regexp "[ \t]+")

(*
   A shebang supports at most the name of the script and one argument:

   #!/bin/bash -e -u
     ^^^^^^^^^ ^^^^^
       arg0    arg1

   To deal with that, the '/usr/bin/env' command offer the '-S' option prefix,
   which will split what follows '-S' into multiple arguments. So we
   may find things like these:

   #!/usr/bin/env -S bash -e -u
     ^^^^^^^^^^^^ ^^^^^^^^^^^^^
         arg0         arg1

   It's 'env' that will parse its argument and execute the command
   ['bash'; '-e'; '-u'] as expected.

   Examples:

     "#!/bin/bash -e -u"            -> ["/bin/bash"; "-e -u"]
     "#!/usr/bin/env -S bash -e -u" -> ["/usr/bin/env"; "bash"; "-e"; "-u"]
*)
let parse_shebang_line s =
  let matched = Pcre2_.exec_noerr ~rex:(Lazy.force shebang_re) s in
  match matched with
  | None -> None
  | Some matched -> (
      match Pcre2.get_substrings matched with
      | [| _; arg0; "" |] -> Some [ arg0 ]
      | [| _; "/usr/bin/env" as arg0; arg1 |] -> (
          (* approximate emulation of 'env -S'; should work if the command
             contains no quotes around the arguments. *)
          match string_chop_prefix ~pref:"-S" arg1 with
          | Some packed_args ->
              let args =
                Pcre2_.split_noerr ~rex:(Lazy.force split_cmd_re)
                  ~on_error:[ packed_args ] packed_args
                |> List.filter (fun fragment -> fragment <> "")
              in
              Some (arg0 :: args)
          | None -> Some [ arg0; arg1 ])
      | [| _; arg0; arg1 |] -> Some [ arg0; arg1 ]
      | [| _ |] -> None
      | _ -> assert false)

let get_shebang_command path = get_first_line path |> parse_shebang_line

let uses_shebang_command_name cmd_names =
  let f path =
    Log.info (fun m -> m "checking for a #! in %s" !!path);
    match get_shebang_command path with
    | Some ("/usr/bin/env" :: cmd_name :: _) -> List.mem cmd_name cmd_names
    | Some (cmd_path :: _) ->
        let cmd_name = Filename.basename cmd_path in
        List.mem cmd_name cmd_names
    | _ -> false
  in
  Test_path f

(*
   PCRE regexp using the default options.
   In case of an error, the result is false.
 *)
let regexp pat =
  let rex = Pcre2_.regexp pat in
  let f path =
    let s = get_first_block path in
    Pcre2_.pmatch_noerr ~rex s
  in
  Test_path f

let is_executable_script cmd_names =
  And
    ( Not has_an_extension,
      And (is_executable, uses_shebang_command_name cmd_names) )

(*
   Matches if either
   - language has extension in Lang.ext_of_lang
   - language is script with shebang in Lang.shebangs_of_lang

   General test for a script:
   - must have one of the approved extensions (e.g. "bash" or "sh")
   - or has no extension but has executable permission
     and one of the approved commands occurs on the shebang line.
     Example:

       #!/bin/bash -e
              ^^^^

       #!/usr/bin/env bash
                      ^^^^
*)
let matches_lang lang =
  let has_ext =
    And (has_lang_extension lang, Not (has_excluded_lang_extension lang))
  in
  match Lang.shebangs_of_lang lang with
  | [] -> has_ext
  (* Prefer extensions over shebangs *)
  | cmd_names -> Or (has_ext, is_executable_script cmd_names)

(****************************************************************************)
(* Language-specific definitions *)
(****************************************************************************)

(*
   Inspect Hack files, which may use the '.php' extension in addition to
   the Hack-specific extensions ('.hack' etc.).

   See https://docs.hhvm.com/hack/source-code-fundamentals/program-structure
*)
let is_hack =
  Or
    ( matches_lang Lang.Hack,
      And
        ( has_extension [ ".php" ],
          Or
            ( uses_shebang_command_name [ "hhvm" ],
              (* optional '#!' line followed by '<?hh': *)
              regexp "^(?:#![^\\n]*\\n)?<\\?hh\\s" ) ) )

let inspect_file_p (lang : Lang.t) path =
  let test =
    match lang with
    | Hack -> is_hack
    | Php -> And (matches_lang lang, Not is_hack)
    | Dockerfile ->
        (* TODO: add support for exact file name to lang.json *)
        Or (is_named [ "Dockerfile"; "dockerfile" ], has_lang_extension lang)
    | Apex
    | Bash
    | C
    | Cairo
    | Circom
    | Clojure
    | Cpp
    | Csharp
    | Dart
    | Elixir
    | Go
    | Html
    | Java
    | Js
    | Json
    | Jsonnet
    | Julia
    | Kotlin
    | Lisp
    | Lua
    | Move_on_sui
    | Move_on_aptos
    | Ocaml
    | Promql
    | Protobuf
    | Python2
    | Python3
    | Python
    | Ql
    | R
    | Ruby
    | Rust
    | Scala
    | Scheme
    | Solidity
    | Swift
    | Terraform
    | Ts
    | Vue
    | Xml
    | Yaml ->
        matches_lang lang
  in

  eval test path

let wrap_with_error_message lang path bool_res :
    (Fpath.t, Semgrep_output_v1_t.skipped_target) result =
  match bool_res with
  | true -> Ok path
  | false ->
      Error
        {
          path;
          reason = Wrong_language;
          details =
            Some
              (spf "target file doesn't look like language %s"
                 (Lang.to_string lang));
          rule_id = None;
        }

let inspect_file lang path =
  let bool_res = inspect_file_p lang path in
  wrap_with_error_message lang path bool_res

let inspect_files lang paths = Result_.partition (inspect_file lang) paths
