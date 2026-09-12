(*
   A semi-generic type used to hold the results of parsing a file.
*)

open Printf

type stat = {
  total_line_count: int;
  error_line_count: int;
  error_count: int;
}

type ('a, 'b) t = {
  program: 'a option;
  extras: 'b list;
  errors: Tree_sitter_error.t list;
  stat: stat;
}

let count_error_lines errors =
  (* Make sure to count a line just once if it contains more
     than one error. *)
  match errors with
  | [] -> 0
  | _ ->
      let tbl = Hashtbl.create 100 in
      List.iter (fun (err : Tree_sitter_error.t) ->
        for row = err.start_pos.row to err.end_pos.row do
          Hashtbl.replace tbl row ()
        done
      ) errors;
      Hashtbl.length tbl

let create_stat src errors =
  let total_line_count = Src_file.get_num_lines src in
  let error_line_count = count_error_lines errors in
  let error_count = List.length errors in
  {
    total_line_count;
    error_line_count;
    error_count;
  }

let create src program extras errors =
  let stat = create_stat src errors in
  {
    program;
    extras;
    errors;
    stat;
  }

let export_stat ~out_file x =
  let contents =
    sprintf "%i %i %i\n"
      x.total_line_count x.error_line_count x.error_count
  in
  let oc = open_out out_file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)
