(*
   Strip comments and the like as a preprocessing phase.
*)

(* Suppresses warnings about PCRE; there appear to be some issues when moving
   this to use PCRE2 on some versions, e.g., 10.34 (Debian stable as of
   2024-04-18). *)
[@@@alert "-deprecated"]

open Printf

type comment_syntax = End_of_line of string | Multiline of string * string
type style = comment_syntax list

let shell_style = [ End_of_line "#" ]
let c_style = [ Multiline ("/*", "*/") ]
let cpp_style = End_of_line "//" :: c_style

let predefined_styles =
  [ ("c", c_style); ("cpp", cpp_style); ("shell", shell_style) ]

(* TODO: replace whole unicode characters by exactly space to avoid
         location errors, then rename the function to 'whiteout_utf8'. *)
let whiteout_ascii s =
  String.map
    (function
      | ('\r' | '\n') as line_delimiter -> line_delimiter
      | _byte -> ' ')
    s

let replace_end_of_line_comment ~start src =
  (* match from first occurrence of 'start' in the line until the
     end of line or end of input *)
  let rex = sprintf "%s[^\n]*\n?" (Pcre.quote start) |> Pcre_.regexp in
  Pcre_.substitute ~rex ~subst:whiteout_ascii src

let replace_multiline_comment ~start ~end_ src =
  let rex =
    sprintf "%s.*?%s" (Pcre.quote start) (Pcre.quote end_)
    |> Pcre_.regexp ~flags:[ `DOTALL ]
  in
  Pcre_.substitute ~rex ~subst:whiteout_ascii src

(* Apply comment filters from left to right *)
let remove_comments_from_string style src =
  List.fold_left
    (fun src filter ->
      match filter with
      | End_of_line start -> replace_end_of_line_comment ~start src
      | Multiline (start, end_) -> replace_multiline_comment ~start ~end_ src)
    src style

let remove_comments_from_src style src =
  Src_file.replace_contents src (fun s -> remove_comments_from_string style s)

(**************************************************************************)
(* Command-line handling for spacecat and spacegrep *)
(**************************************************************************)

module CLI = struct
  open Cmdliner

  let comment_style_conv =
    let parser name =
      match
        List.find_opt (fun (name2, _) -> name2 = name) predefined_styles
      with
      | Some style -> Ok style
      | None ->
          Error (`Msg ("Invalid argument for --comment-style option: " ^ name))
    in
    let printer fmt (name, _style) = Format.pp_print_string fmt name in
    Cmdliner.Arg.conv (parser, printer)

  let comment_style_term =
    let info =
      Arg.info [ "comment-style" ] ~docv:"NAME"
        ~doc:
          "Assume code comments that follow one of these predefined styles: c, \
           cpp, shell. 'c' implements classic C-style comments of the form /* \
           ... */. 'cpp' implements C++-style comments of the form /* ... */ \
           or // ... . 'shell' implements end-of-line comments\n\
          \            of the form # ... ."
    in
    Arg.value (Arg.opt (Arg.some comment_style_conv) None info)

  let eol_comment_start_term =
    let info =
      Arg.info [ "eol-comment-start" ] ~docv:"DELIM"
        ~doc:
          "Ignore end-of-line comments starting with $(docv). No default value."
    in
    Arg.value (Arg.opt Arg.(some string) None info)

  let multiline_comment_start_term =
    let info =
      Arg.info
        [ "multiline-comment-start" ]
        ~docv:"DELIM"
        ~doc:
          "Ignore multiline comments starting with $(docv). No default value."
    in
    Arg.value (Arg.opt Arg.(some string) None info)

  let multiline_comment_end_term =
    let info =
      Arg.info
        [ "multiline-comment-end" ]
        ~docv:"DELIM"
        ~doc:"Ignore multiline comments ending with $(docv). No default value."
    in
    Arg.value (Arg.opt Arg.(some string) None info)

  let merge_comment_options ~comment_style ~eol_comment_start
      ~multiline_comment_start ~multiline_comment_end : style =
    let style = [] in
    let style =
      match comment_style with
      | None -> style
      | Some (_name, predefined_style) -> predefined_style @ style
    in
    let style =
      match (multiline_comment_start, multiline_comment_end) with
      | None, None -> style
      | Some start, Some end_ -> Multiline (start, end_) :: style
      | Some _, None -> failwith "Missing --multiline-comment-end option"
      | None, Some _ -> failwith "Missing --multiline-comment-start option"
    in
    let style =
      match eol_comment_start with
      | None -> style
      | Some start -> End_of_line start :: style
    in
    style
end
