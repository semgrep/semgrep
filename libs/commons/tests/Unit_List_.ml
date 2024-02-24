(*
   Unit tests for our List_ module.
*)

open Printf

let string_of_int_list xs =
  sprintf "[%s]" (xs |> List_.map string_of_int |> String.concat "; ")

let string_of_int_option = function
  | None -> "None"
  | Some x -> sprintf "Some %i" x

let test_iter_with_view_into_neighbor_elements name list =
  Testo.create ~category:[ "List_.iter_with_view_into_neighbor_elements" ]
    ~checked_output:Stdout name (fun () ->
      printf "input list: %s\n" (string_of_int_list list);
      list
      |> List_.iter_with_view_into_neighbor_elements (fun ~prev ~cur ~next ->
             printf "prev=%s, cur=%i, next=%s\n"
               (string_of_int_option prev)
               cur
               (string_of_int_option next)))

let tests =
  Testo.categorize "List_"
    [
      test_iter_with_view_into_neighbor_elements "empty" [];
      test_iter_with_view_into_neighbor_elements "one element" [ 1 ];
      test_iter_with_view_into_neighbor_elements "two elements" [ 1; 2 ];
      test_iter_with_view_into_neighbor_elements "three elements" [ 1; 2; 3 ];
      test_iter_with_view_into_neighbor_elements "four elements" [ 1; 2; 3; 4 ];
    ]
