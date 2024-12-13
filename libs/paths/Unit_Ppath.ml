(*
   Unit tests for Ppath
*)

open Printf

let t = Testo.create

let test_conversions () =
  Testo.test "Ppath" (fun () ->
      let test_str f input expected_output =
        Alcotest.(check string) __LOC__ expected_output (f input)
      in
      let rewrite str =
        Ppath.to_string_for_tests (Ppath.of_string_for_tests str)
      in
      test_str rewrite "/" "/";
      test_str rewrite "//" "//";
      test_str rewrite "" "";
      test_str rewrite "/a/" "/a/";

      let norm input_str expected =
        let res =
          input_str |> Ppath.of_string_for_tests |> Ppath.to_string_for_tests
        in
        printf "test ppath normalization: %s -> %s\n%!" input_str res;
        Alcotest.(check string) __LOC__ expected res
      in
      let norm_err str =
        match Ppath.of_string_for_tests str with
        | exception Invalid_argument _ -> ()
        | res ->
            Alcotest.fail
              (sprintf "an error was expected but we got: %s -> %s\n" str
                 (Ppath.to_string_for_tests res))
      in
      norm "/a" "/a";
      norm "/a/b" "/a/b";
      norm "/ab/cd" "/ab/cd";
      norm "/" "/";
      norm "/a" "/a";
      norm "/a/b" "/a/b";
      norm "/." "/";
      norm "/a/./" "/a/";
      norm_err "/..";
      norm "/a/../b" "/b";
      norm "/a/.." "/";
      norm_err "/a/../..";
      norm "/a/b/../c/d/e/../.." "/a/c";
      norm "/a/" "/a/";
      norm "/a/b/" "/a/b/";

      let test_add_seg a b ab =
        Alcotest.(check string)
          __LOC__ ab
          (Ppath.add_seg (Ppath.of_string_for_tests a) b
          |> Ppath.to_string_for_tests)
      in
      test_add_seg "/" "a" "/a";
      test_add_seg "/a" "b" "/a/b";
      test_add_seg "/a/" "c" "/a/c";

      let mk_abs path_str = Fpath.(v "/fake/cwd" // v path_str) in
      let test_in_project_ok root path expected =
        match
          Ppath.in_project_unsafe ~phys_root:(mk_abs root) (mk_abs path)
        with
        | Ok res ->
            Alcotest.(check string)
              __LOC__ expected
              (Ppath.to_string_for_tests res)
        | Error msg -> Alcotest.fail msg
      in
      let test_in_project_fail root path =
        match
          Ppath.in_project_unsafe ~phys_root:(Fpath.v root) (Fpath.v path)
        with
        | Ok res -> Alcotest.fail (Ppath.to_string_for_tests res)
        | Error _ -> ()
      in
      test_in_project_ok "/a" "/a/b" "/b";
      test_in_project_ok "/a" "/a" "/";
      test_in_project_ok "/a" "/a/b/c" "/b/c";
      test_in_project_ok "/a" "/a/b/c/d" "/b/c/d";
      test_in_project_ok "/a/b" "/a/b/c/d" "/c/d";
      test_in_project_ok "/a/" "/a/b" "/b";
      test_in_project_ok "/a" "/a/b/" "/b/";
      test_in_project_ok "/a/b" "/a/b/c/.." "/";
      test_in_project_ok "/a/b" "/a/b/c/../" "/";
      test_in_project_ok "/a/b" "/a/b/./c/." "/c/";
      test_in_project_ok "a/b" "a/b/c" "/c";
      test_in_project_ok "." "a/b" "/a/b";
      test_in_project_ok "a" "./a/b" "/b";
      test_in_project_ok "." "." "/";
      test_in_project_ok "a/b" "a/b" "/";
      test_in_project_ok "/a/b" "/a/b" "/";
      test_in_project_fail "/a/b" "/a";
      test_in_project_fail "/a/b" "/b";
      test_in_project_fail "/a/b" "a")

let test_relativize () =
  let relativize root_str path_str =
    let root = Ppath.of_string_for_tests root_str in
    let path = Ppath.of_string_for_tests path_str in
    Ppath.relativize ~root path |> Fpath.to_string
  in
  let check root_str path_str expected_str =
    printf "relativize root:%S %S -> %S\n%!" root_str path_str expected_str;
    let res = relativize root_str path_str in
    Alcotest.(check string) __LOC__ expected_str res
  in
  check "/" "/a" "a";
  check "/" "/a/b" "a/b";
  check "/" "/a/" "a/";
  check "/a" "/a" ".";
  check "/a" "/a/" ".";
  check "/a/" "/a/" ".";
  check "/a/" "/a" ".";
  check "/" "/.gitignore" ".gitignore"

let test_of_relative_fpath () =
  let check path_str expected_ppath_str =
    printf "of_relative_fpath %S -> %S\n%!" path_str expected_ppath_str;
    let res =
      path_str |> Fpath.v |> Ppath.of_relative_fpath
      |> Ppath.to_string_for_tests
    in
    Alcotest.(check string) __LOC__ expected_ppath_str res
  in
  check "a" "/a";
  check "a/b" "/a/b";
  check "a/" "/a/";
  check "." "/"

let tests =
  Testo.categorize "Ppath"
    [
      t "conversions" test_conversions;
      t "relativize" test_relativize;
      t "of_relative_fpath" test_of_relative_fpath;
    ]
