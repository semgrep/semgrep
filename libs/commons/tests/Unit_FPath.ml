module FPath = FilePath

(* TODO: copy paste of Unit_commons.with_file, but should be in Common.ml *)
let with_file contents f =
  let file, oc = Filename.open_temp_file "test_pfff_read_file_" ".dat" in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      Sys.remove file)
    (fun () ->
      output_string oc contents;
      close_out oc;
      f file)

(* TODO: we should use Unix.realpath! but only available in 4.13 *)
let realpath s = Common.fullpath s

let test_path_conversion () =
  let check_path path =
    FPath.to_string (FPath.of_string path) = realpath path
  in
  let data = String.make 150 'v' in
  assert (check_path ".");
  assert (check_path "..");
  assert (check_path "../..");
  assert (FPath.to_string (FPath.of_string "/") = realpath "/");
  with_file data (fun file ->
      let path = FPath.of_string file in
      let max_len = 24 in
      assert (FPath.to_string path = realpath file);
      assert (FPath.read_file path = data);
      assert (FPath.cat path = [ data ]);
      assert (FPath.read_file ~max_len path = Str.first_chars data max_len);
      assert (FPath.file_exists path);
      assert (not (FPath.is_directory path)));
  assert (
    FPath.to_string FPath.(of_string "." / "." / ".." / "." / "..")
    = realpath "../..")

let tests =
  Testutil.pack_tests "FPath" [ ("path_conversion", test_path_conversion) ]
