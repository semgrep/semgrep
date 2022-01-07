(*
   Strip comments and the like as a preprocessing phase.
*)

open Printf

type delimiters = { start : string; end_ : string }

type style = {
  name : string;
  start : string option;
  delimiters : delimiters option;
}

let style ?start ?delimiters name =
  let delimiters =
    match delimiters with
    | None -> None
    | Some (start, end_) -> Some { start; end_ }
  in
  { name; start; delimiters }

let shell_style = style ~start:"#" "shell"

let c_style = style ~delimiters:("/*", "*/") "c"

let cpp_style = style ~start:"//" ~delimiters:("/*", "*/") "cpp"

let predefined_styles = [ c_style; cpp_style; shell_style ]

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
  let rex = sprintf "%s[^\n]*\n?" (Pcre.quote start) |> SPcre.regexp in
  Pcre.substitute ~rex ~subst:whiteout_ascii src

let replace_multiline_comment ~start ~end_ src =
  let rex =
    sprintf "%s.*?%s" (Pcre.quote start) (Pcre.quote end_)
    |> SPcre.regexp ~flags:[ `DOTALL ]
  in
  Pcre.substitute ~rex ~subst:whiteout_ascii src

let remove_comments_from_string style src =
  let src =
    match style.start with
    | None -> src
    | Some start -> replace_end_of_line_comment ~start src
  in
  let src =
    match style.delimiters with
    | None -> src
    | Some { start; end_ } -> replace_multiline_comment ~start ~end_ src
  in
  src

let remove_comments_from_src style src =
  Src_file.replace_contents src (fun s -> remove_comments_from_string style s)

(**************************************************************************)
(* Command-line handling for spacecat and spacegrep *)
(**************************************************************************)

module CLI = struct
  open Cmdliner

  let comment_style_conv =
    let parser name =
      match List.find_opt (fun x -> x.name = name) predefined_styles with
      | Some style -> Ok style
      | None ->
          Error (`Msg ("Invalid argument for --comment-style option: " ^ name))
    in
    let printer fmt style = Format.pp_print_string fmt style.name in
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
    let style =
      match comment_style with
      | None -> style "custom"
      | Some x -> x
    in
    let style =
      match eol_comment_start with
      | None -> style
      | Some _ as start -> { style with start }
    in
    let style =
      let start, end_ =
        match style.delimiters with
        | None -> (None, None)
        | Some { start; end_ } -> (Some start, Some end_)
      in
      let start =
        match multiline_comment_start with
        | None -> start
        | Some x -> Some x
      in
      let end_ =
        match multiline_comment_end with
        | None -> end_
        | Some x -> Some x
      in
      let delimiters =
        match (start, end_) with
        | None, None -> None
        | Some start, Some end_ -> Some { start; end_ }
        | Some _, None -> failwith "Missing --multiline-comment-end option"
        | None, Some _ -> failwith "Missing --multiline-comment-start option"
      in
      { style with delimiters }
    in
    style
end
