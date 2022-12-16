open Common

open Pycaml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* My pycaml, a mix of
 * - Arty Yekes original pycaml 0.82,
 * - Henrik stuart port to python 2.5, done for coccinelle,
 * - Thomas Fischbacher heavy extension.
 *
 * Alternatives:
 *  - swig ?
 *
 *
 *
 * How talk to python ? How call nltk ?
 * - can do pipe to python
 * - can use pycaml pyrun_simplestring, and generate string that
 *   build variables to pass complex objects such as list, which is
 *   quite similar to how I generated R commands
 * - can build variables in ocaml via pycaml wrappers. It also
 *   easier that way to get results as no need to parse string. But
 *   it involves more effort as a kind of marshalling/unmarshalling has
 *   to be done.
 * - can do previous stuff but also using a wrapper over nltk as in
 *   nltk_ocaml.py. So then have this flow:
 *   ocaml code -> nltk.ml -> pycaml -> nltk_ocaml.py -> nltk -> python code
 *
 *
*)

exception PythonError of string

(* henrik have written similar things *)
let check_python_return v =
  if v = Pycaml.pynull ()
  then begin
    Pycaml.pyerr_print ();
    raise (PythonError "todo: not yet better diagnostic")
  end
  else v

(* alias *)
let cpr = check_python_return

(* need to initialize ? apparently no *)
(* src: henrik
   Unix.putenv "PYTHONPATH"
      (Printf.sprintf "%s/coccinelle" (Unix.getenv "HOME"));
   let _ = if not (py_isinitialized () != 0) then
   	(if !Flag.show_misc then Common.pr2 "Initializing python\n%!";
   	py_initialize()) in

   (* set argv *)
   let argv0 = Printf.sprintf "%s%sspatch" (Sys.getcwd ()) (match Sys.os_type with "Win32" -> "\\" | _ -> "/") in
   let _ = pycaml_setargs argv0 in
*)



(* Why no guarded_pystring_fromstring as have a guarded_pystring_asstring ?
 * Because ocaml is strongly typed so no pb in this direction;
 * converting from an ocaml string to a python string is always safe.
*)
let string_list_to_python xs =
  xs
  |> Array.of_list
  |> Array.map Pycaml.pystring_fromstring
  |> Pycaml.pylist_fromarray

let string_list_of_python pythonobj =
  pythonobj
  |> Pycaml.guarded_pylist_toarray
  |> Array.map Pycaml.guarded_pystring_asstring
  |> Array.to_list

(*  pythonobj +> Pycaml.py_string_list_as_array +> Array.to_list *)




let pycall (modul,dict) fstr args =
  raise Todo


(*****************************************************************************)
(* Test *)
(*****************************************************************************)

(* from pycaml.html *)
let test_pycaml () =

  let colorsys = Pycaml.pyimport_importmodule "colorsys" in
  let dict = Pycaml.pymodule_getdict colorsys in

  let triplet = pytuple3 (pyfloat_fromdouble 1.0,
                          pyfloat_fromdouble 0.5,
                          pyfloat_fromdouble 0.2) in

  let rgbtoyiq = pydict_getitemstring (dict,"rgb_to_yiq") in
  let triplet = pyeval_callobject (rgbtoyiq,triplet) in

  print_endline ((string_of_float
                    (pyfloat_asdouble (pytuple_getitem (triplet,0)))) ^
                 " " ^
                 (string_of_float
                    (pyfloat_asdouble (pytuple_getitem (triplet,1)))) ^
                 " " ^
                 (string_of_float
                    (pyfloat_asdouble (pytuple_getitem (triplet,2)))));
  ()

let test_pycaml2 () =
  raise Todo

(* test my_reverse() function, 1 argument *)
let test_pycaml3 () =
  let modul = cpr (Pycaml.pyimport_importmodule "python_ocaml") in
  let dict = cpr (Pycaml.pymodule_getdict modul) in

  let f = cpr (Pycaml.pydict_getitemstring(dict, "reverse_test")) in
  let xsin = [|1;2;3|] in
  let args = Pycaml.pytuple_fromsingle (int_array_to_python xsin ) in
  let res = cpr (Pycaml.pyeval_callobject (f,args)) in

  let xsout = Pycaml.py_int_list_as_array res in
  xsout |> Array.iter (fun i -> pr2 (i_to_s i));
  ()


let test_pycaml4 () =
  let modul = cpr (Pycaml.pyimport_importmodule "python_ocaml") in
  let dict = cpr (Pycaml.pymodule_getdict modul) in

  let f = cpr (Pycaml.pydict_getitemstring(dict, "append_test")) in
  let xsin = ["this";"is";"not"] in
  let args = Pycaml.pytuple_fromsingle (string_list_to_python xsin ) in
  let res = cpr (Pycaml.pyeval_callobject (f,args)) in

  let xsout = string_list_of_python res in
  xsout |> List.iter (fun s -> pr2 s);
  ()



let test_python_direct () =
  let _ = Pycaml.pyrun_simplestring
      (Common.unlines
         ["import nltk";
          "nltk.draw.tree.demo()";
         ]) in
  ()

(*
val test_pycaml : unit -> unit
val test_pycaml2 : unit -> unit
val test_pycaml3 : unit -> unit
*)



let actions () = [
  "-test_pycaml1", "",
  Common.mk_action_0_arg test_pycaml;
  "-test_pycaml2", "",
  Common.mk_action_0_arg test_pycaml2;
  "-test_pycaml3", "",
  Common.mk_action_0_arg test_pycaml3;
  "-test_pycaml4", "",
  Common.mk_action_0_arg test_pycaml4;
]
