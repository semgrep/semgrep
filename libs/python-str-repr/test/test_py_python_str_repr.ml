let py_unicode_version =
  let open Py in
  let unicodedata = PyModule.import "unicodedata" in
  let unicode_version = unicodedata $. String "unidata_version" in
  let unicode_version = Object.to_string unicode_version in
  match String.split_on_char '.' unicode_version with
  | major :: minor :: _ -> (int_of_string major, int_of_string minor)
  | _ -> assert false

let py_repr =
  let open Py in
  let py_repr_fn = Object.get_item_s (builtins ()) "str" $. String "__repr__" in
  PyWrap.(python_fn (string @-> returning string)) py_repr_fn

let repr = Python_str_repr.repr ~unicode_version:py_unicode_version

let test_repr =
  let inputs = [ ""; "a"; "'"; "\""; "tab\tthis" ] in
  List_.map
    (fun input ->
      ( Printf.sprintf "%S" input,
        `Quick,
        fun () -> Alcotest.(check string) input (py_repr input) (repr input) ))
    inputs

let test_repr_qcheck =
  let valid_utf8 =
    let folder acc _index = function
      | `Uchar _ -> acc
      | `Malformed _ -> false
    in
    Uutf.String.fold_utf_8 folder true
  in
  let test_case =
    QCheck2.Test.make
      ~print:(fun s -> Printf.sprintf "%S: %S vs %S" s (py_repr s) (repr s))
      ~name:"repr qcheck" QCheck2.Gen.string
      (fun s ->
        QCheck2.assume (valid_utf8 s);
        String.equal (py_repr s) (repr s))
  in
  [ QCheck_alcotest.to_alcotest test_case ]

let test_suites =
  [ ("py repr", test_repr); ("py repr qcheck", test_repr_qcheck) ]

let () =
  Printf.printf "Using Unicode version %d.%d\n" (fst py_unicode_version)
    (snd py_unicode_version);
  Alcotest.run "python-str-repr" test_suites
