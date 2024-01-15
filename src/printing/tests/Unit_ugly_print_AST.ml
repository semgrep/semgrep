open Common

let test_python_printer () =
  let printer = new Ugly_print_AST.python_printer in
  let check (source, expected) =
    (* Parse as a pattern to allow us to test printing snippets of code *)
    let ast = Parse_pattern.parse_pattern Lang.Python source in
    match printer#print_any ast with
    | Error e -> failwith (spf "Couldn't print `%s`:\n%s" source e)
    | Ok actual ->
        let actual = Immutable_buffer.to_string actual in
        Alcotest.(check string) source expected actual
  in
  List.iter check [ ("foo", "foo"); ("foo()", "foo()") ]

let tests =
  Testo.pack_tests "ugly printer"
    [ ("test python printer", test_python_printer) ]
