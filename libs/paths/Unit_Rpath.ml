let t = Testo.create

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
  let check_path path_str =
    let rpath_str = Rpath.to_string (Rpath.of_string_exn path_str) in
    rpath_str = realpath path_str
  in
  let data = String.make 150 'v' in
  assert (check_path ".");
  assert (check_path "..");
  assert (check_path "../..");
  assert (Rpath.to_string (Rpath.of_string_exn "/") = realpath "/");
  with_file data (fun file ->
      let path = Rpath.of_string_exn file in
      let max_len = 24 in
      assert (Rpath.to_string path = realpath file);
      assert (Rpath.to_fpath path |> UFile.read_file = data);
      assert (Rpath.to_fpath path |> UFile.cat = [ data ]);
      assert (
        UFile.read_file ~max_len (Rpath.to_fpath path)
        = Str.first_chars data max_len);
      assert (path |> Rpath.to_string |> Sys.file_exists);
      assert (path |> Rpath.to_string |> Sys.is_directory |> not))

let tests =
  Testo.categorize "Rpath" [ t "path_conversion" test_path_conversion ]
