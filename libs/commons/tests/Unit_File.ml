let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Unit tests for our UFile, and UTmp modules. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let with_string str f =
  UTmp.with_temp_file ~contents:str (fun fpath ->
      (* note that this uses open_in_bin internally *)
      UFile.with_open_in fpath (fun chan -> f chan))

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let with_test_files f =
  let open Testutil_files in
  with_tempfiles ~chdir:true ~verbose:true
    [
      dir "dir" [];
      file "reg";
      symlink "broken-symlink" "missing";
      symlink "reg-link" "reg";
      symlink "reg-link2" "reg-link";
      symlink "dir-link" "dir";
    ]
    f

let test_is_dir () =
  with_test_files (fun _cwd ->
      Alcotest.(check bool)
        "" true
        (UFile.is_dir ~follow_symlinks:true (Fpath.v "dir"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir ~follow_symlinks:false (Fpath.v "dir"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir ~follow_symlinks:true (Fpath.v "dir-link"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:false (Fpath.v "dir-link"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:true (Fpath.v "reg"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:false (Fpath.v "reg"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:true (Fpath.v "broken-symlink"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:false (Fpath.v "broken-symlink"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:true (Fpath.v "missing"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:false (Fpath.v "missing")))

let test_is_reg () =
  with_test_files (fun _cwd ->
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:true (Fpath.v "dir"));
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:false (Fpath.v "dir"));
      Alcotest.(check bool)
        "" true
        (UFile.is_reg ~follow_symlinks:true (Fpath.v "reg"));
      Alcotest.(check bool)
        "" true
        (UFile.is_reg ~follow_symlinks:false (Fpath.v "reg"));
      Alcotest.(check bool)
        "" true
        (UFile.is_reg ~follow_symlinks:true (Fpath.v "reg-link"));
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:false (Fpath.v "reg-link"));
      Alcotest.(check bool)
        "" true
        (UFile.is_reg ~follow_symlinks:true (Fpath.v "reg-link2"));
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:false (Fpath.v "reg-link2"));
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:true (Fpath.v "broken-symlink"));
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:false (Fpath.v "broken-symlink"));
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:true (Fpath.v "missing"));
      Alcotest.(check bool)
        "" false
        (UFile.is_reg ~follow_symlinks:false (Fpath.v "missing")))

let test_is_dir_or_reg () =
  with_test_files (fun _cwd ->
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_reg ~follow_symlinks:true (Fpath.v "dir"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_reg ~follow_symlinks:false (Fpath.v "dir"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir ~follow_symlinks:true (Fpath.v "dir-link"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir ~follow_symlinks:false (Fpath.v "dir-link"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_reg ~follow_symlinks:true (Fpath.v "reg"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_reg ~follow_symlinks:false (Fpath.v "reg"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_reg ~follow_symlinks:true (Fpath.v "reg-link"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir_or_reg ~follow_symlinks:false (Fpath.v "reg-link"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_reg ~follow_symlinks:true (Fpath.v "reg-link2"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir_or_reg ~follow_symlinks:false (Fpath.v "reg-link2"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir_or_reg ~follow_symlinks:true (Fpath.v "broken-symlink"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir_or_reg ~follow_symlinks:false (Fpath.v "broken-symlink"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir_or_reg ~follow_symlinks:true (Fpath.v "missing"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir_or_reg ~follow_symlinks:false (Fpath.v "missing")))

let test_is_lnk () =
  with_test_files (fun _cwd ->
      Alcotest.(check bool) "" false (UFile.is_lnk (Fpath.v "dir"));
      Alcotest.(check bool) "" false (UFile.is_lnk (Fpath.v "reg"));
      Alcotest.(check bool) "" true (UFile.is_lnk (Fpath.v "dir-link"));
      Alcotest.(check bool) "" true (UFile.is_lnk (Fpath.v "reg-link"));
      Alcotest.(check bool) "" true (UFile.is_lnk (Fpath.v "reg-link2"));
      Alcotest.(check bool) "" true (UFile.is_lnk (Fpath.v "broken-symlink"));
      Alcotest.(check bool) "" false (UFile.is_lnk (Fpath.v "missing")))

let test_is_dir_or_lnk () =
  with_test_files (fun _cwd ->
      Alcotest.(check bool) "" true (UFile.is_dir_or_lnk (Fpath.v "dir"));
      Alcotest.(check bool) "" false (UFile.is_dir_or_lnk (Fpath.v "reg"));
      Alcotest.(check bool) "" true (UFile.is_dir_or_lnk (Fpath.v "dir-link"));
      Alcotest.(check bool) "" true (UFile.is_dir_or_lnk (Fpath.v "reg-link"));
      Alcotest.(check bool) "" true (UFile.is_dir_or_lnk (Fpath.v "reg-link2"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_lnk (Fpath.v "broken-symlink"));
      Alcotest.(check bool) "" false (UFile.is_dir_or_lnk (Fpath.v "missing")))

let test_is_lnk_or_reg () =
  with_test_files (fun _cwd ->
      Alcotest.(check bool) "" false (UFile.is_lnk_or_reg (Fpath.v "dir"));
      Alcotest.(check bool) "" true (UFile.is_lnk_or_reg (Fpath.v "reg"));
      Alcotest.(check bool) "" true (UFile.is_lnk_or_reg (Fpath.v "dir-link"));
      Alcotest.(check bool) "" true (UFile.is_lnk_or_reg (Fpath.v "reg-link"));
      Alcotest.(check bool) "" true (UFile.is_lnk_or_reg (Fpath.v "reg-link2"));
      Alcotest.(check bool)
        "" true
        (UFile.is_lnk_or_reg (Fpath.v "broken-symlink"));
      Alcotest.(check bool) "" false (UFile.is_lnk_or_reg (Fpath.v "missing")))

let test_is_dir_or_lnk_or_reg () =
  with_test_files (fun _cwd ->
      Alcotest.(check bool) "" true (UFile.is_dir_or_lnk_or_reg (Fpath.v "dir"));
      Alcotest.(check bool) "" true (UFile.is_dir_or_lnk_or_reg (Fpath.v "reg"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_lnk_or_reg (Fpath.v "dir-link"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_lnk_or_reg (Fpath.v "reg-link"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_lnk_or_reg (Fpath.v "reg-link2"));
      Alcotest.(check bool)
        "" true
        (UFile.is_dir_or_lnk_or_reg (Fpath.v "broken-symlink"));
      Alcotest.(check bool)
        "" false
        (UFile.is_dir_or_lnk_or_reg (Fpath.v "missing")))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  Testo.categorize "File"
    [
      t "input_line LF" (fun () ->
          with_string "foo\nbar\n" (fun chan ->
              let str1 = input_line chan in
              let str2 = input_line chan in
              Alcotest.(check (list string))
                __LOC__ [ "foo"; "bar" ] [ str1; str2 ]));
      t "input_line CRLF" (fun () ->
          with_string "foo\r\nbar\r\n" (fun chan ->
              let str1 = input_line chan in
              let str2 = input_line chan in
              (* when using open_in_bin, there is no translation so we
               * still get those \r
               *)
              Alcotest.(check (list string))
                __LOC__ [ "foo\r"; "bar\r" ] [ str1; str2 ]));
      (* this is used sometimes in Windows and it's ugly *)
      t "input_line CR" (fun () ->
          with_string "foo\rbar\r" (fun chan ->
              (* OCaml input_line does not recognize those a CR as a newline *)
              let str1 = input_line chan in
              Alcotest.(check (list string)) __LOC__ [ "foo\rbar\r" ] [ str1 ]));
      t "Common.input_text_line CRLF" (fun () ->
          with_string "foo\r\nbar\r\n" (fun chan ->
              (* Common.input_text_line will perform some translation
               * and remove \r regardless of how the file was opened
               * (via open_in_bin or open_in)
               *)
              let str1 = Common.input_text_line chan in
              let str2 = Common.input_text_line chan in
              Alcotest.(check (list string))
                __LOC__ [ "foo"; "bar" ] [ str1; str2 ]));
      t "is_dir" test_is_dir;
      t "is_reg" test_is_reg;
      t "is_dir_or_reg" test_is_dir_or_reg;
      t "is_lnk" test_is_lnk;
      t "is_dir_or_lnk" test_is_dir_or_lnk;
      t "is_lnk_or_reg" test_is_lnk_or_reg;
      t "is_dir_or_lnk_or_reg" test_is_dir_or_lnk_or_reg;
    ]
