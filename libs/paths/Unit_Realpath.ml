(* TODO: copy paste of Unit_commons.with_file, but should be in Common.ml *)
let with_file contents f =
  let file, oc = Filename.open_temp_file "test_pfff_read_file_" ".dat" in
  Common.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      Sys.remove file)
    (fun () ->
      output_string oc contents;
      close_out oc;
      f file)

let realpath s = Unix.realpath s

let test_path_conversion () =
  let check_path path =
    Realpath.to_string (Realpath.of_string path) = realpath path
  in
  let data = String.make 150 'v' in
  assert (check_path ".");
  assert (check_path "..");
  assert (check_path "../..");
  assert (Realpath.to_string (Realpath.of_string "/") = realpath "/");
  with_file data (fun file ->
      let path = Realpath.of_string file in
      let max_len = 24 in
      assert (Realpath.to_string path = realpath file);
      assert (Realpath.to_fpath path |> File.read_file = data);
      assert (Realpath.to_fpath path |> File.cat = [ data ]);
      assert (
        File.read_file ~max_len (Realpath.to_fpath path)
        = Str.first_chars data max_len);
      assert (path |> Realpath.to_string |> Sys.file_exists);
      assert (path |> Realpath.to_string |> Sys.is_directory |> not))

let tests =
  Testutil.pack_tests "Realpath" [ ("path_conversion", test_path_conversion) ]
