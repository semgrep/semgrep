(*s: builtins_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011, 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: Facebook copyright *)
open Common 

module J = Json_type
module Json_in = Json_io

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * As opposed to OCaml or C++ or Java or most programming languages, 
 * there is no source code files where PHP builtin functions
 * and their types are declared. They are only defined in the PHP manual 
 * and somewhere in the source code of Zend.
 * In OCaml most library functions are written in OCaml itself or are
 * specified via an 'external' declaration as in:
 * 
 *   external (=) : 'a -> 'a -> bool = "%equal"
 * 
 * This is very convenient for certain tasks such as code browsing where
 * many functions would be seen as 'undefined' otherwise, or for 
 * the type inference  because builtin functions are not special anymore. 
 * 
 * Unfortunately, this is not the case for PHP. Fortunately the
 * good guys from HPHP have spent some time specifying in a IDL form
 * the interface of many of those builtin PHP functions (including the 
 * one in some popular PHP extensions). They've used those interfaces to 
 * generate C++ header files, but we can abuse them to instead generate
 * PHP "header" files that our tool can understand.
 * See hphp/src/idl/.
 * See also now /var/hphpi-releases/*/systemlib.php
 * 
 * Note that the PHP manual is stored in XML files and so can also
 * be automatically processed to extract the names and types
 * of the builtin functions, classes, and constants.
 *)

(*****************************************************************************)
(* HPHP IDL *)
(*****************************************************************************)

(* 
 * The goal of this module is to generate PHP files containing
 * the definitions of PHP builtins functions, classes and constants.
 * One can use the HipHop IDL files which contains in a nice format
 * most of those builtins.
 * See https://github.com/facebook/hiphop-php/tree/master/hphp/idl
 * 
 * 
 * Given an HipHop idl file like:
 * foo.idl.json:
 * ...
 * consts = [{
 * 'name'   => "INF",
 * 'type'   => Double,
 * },
 * ...
 * 
 * we want to generate this PHP file:
 * builtins_foo.php:
 * <?php
 * const INF = 0;
 * 
 * (The format has changed during the years, the latest change being
 *  it jsonification in https://phabricator.fb.com/D776085.
 *  Before it was defined in php files as in:
 * 
 *   DefineFunction(
 *    array(
 *     'name'   => "round",
 *     'desc'   => "Returns the rounded value ...",
 *     'flags'  =>  HasDocComment,
 *     'return' => array(
 *       'type'   => Double,
 *       'desc'   => "The rounded value",
 *     ),
 *     'args'   => array(
 *       array(
 *         'name'   => "val",
 *         'type'   => Variant,
 *         'desc'   => "The value to round",
 *       ),
 *       array(
 *         'name'   => "precision",
 *         'type'   => Int64,
 *         'value'  => "0",
 *         'desc'   => "The optional number of decimal digits to round to.",
 *       ),
 *     ),
 *   ));
 * 
 *  and before that it's used to be just:
 *      f('round',   Double,  array('val' => Variant,
 *                       'precision' => array(Int64, '0')));
 *  
 *  and in the manual at http://us3.php.net/round it is defined as:
 * 
 *     float round  ( float $val  [, int $precision = 0] )
 * )
 * 
 * 
 * Given those builtin files it's then easier for some external static
 * analysis tools like pfff to not report false positives such as
 * "error, undeclared constant:INF" by knowing the set of all
 * PHP builtins.
 * 
 * 
 * todo: extract type information too! to help hack typechecker
 * 
 * todo: sara said better maybe to use
 *  get_defined_functions() / get_declared_classes() / get_defined_constants()
 * (and maybe some ReflectionClass magic as well).
 *
 *
 * 
 * 
 * history: 
 *  - I was originally generating the files by analyzing the IDL
 *    files via pfff and by pattern matching the different cases.
 *  - As the IDL got more complicated, it was far simpler
 *    to just define in PHP a DefineFunction() function that would
 *    do the appropriate things, just like what they do for generating
 *    C++ headers. This is what was done by scripts/gen_builtins_php.php
 *    below.
 *  - with the jsonfication of the IDL, it became simpler to go back to
 *    use ocaml instead of an external php script.
 * 
 * alternatives:
 *  - could analyze Zend source code ? Do they have something similar ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Those types below are not used anymore, but it's good to have
 * some formal representations of the HPHP idl files anyway.
 *)

(* see hphp/src/idl/base.php. generated mostly via macro *)
type idl_type = 
  | Boolean

  (* maybe not used *)
  | Byte
  | Int16

  | Int32
  | Int64
  | Double
  | String

  | Int64Vec
  | StringVec
  | VariantVec

  | Int64Map
  | StringMap
  | VariantMap

  | Object
  | Resource

  | Variant

  | Numeric
  | Primitive
  | PlusOperand
  | Sequence

  | Any

  (* Added by me *)
  | NULL
(*  | Void *)


(*
type idl_param = {
  p_name: string;
  p_type: idl_type;
  p_isref: bool;
  p_default_val: string option;
}
*)

(*
type idl_entry = 
  | Global of string * idl_type
  | Function of 
      string * idl_type * idl_param list * bool (* has variable arguments *)
  (* todo: Class, Constant, etc *)
*)

let special_comment = 
  "// THIS IS @" ^ "generated BY builtins_php.ml -- DO NOT MODIFY\n" ^
  "// If you need to regenerate, see pfff/data/php_stdlib/Makefile\n"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let idl_type__str_conv = [
  Boolean, "Boolean";
  Byte, "Byte";
  Int16, "Int16";
  Int32, "Int32";
  Int64, "Int64";
  Double, "Double";
  String, "String";
  Int64Vec, "Int64Vec";
  StringVec, "StringVec";
  VariantVec, "VariantVec";
  Int64Map, "Int64Map";
  StringMap, "StringMap";
  VariantMap, "VariantMap";
  Object, "Object";
  Resource, "Resource";
  Variant, "Variant";
  Numeric, "Numeric";
  Primitive, "Primitive";
  PlusOperand, "PlusOperand";
  Sequence, "Sequence";
  Any, "Any";

  NULL,  "NULL";
  NULL,  "Null";
]

let (idl_type_of_string, _str_of_idl_type) = 
 Common2.mk_str_func_of_assoc_conv idl_type__str_conv

let _idl_type_of_string (s, _info) =
  try idl_type_of_string s
  with Not_found ->
    failwith ("not a idl type: " ^ s)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let do_not_decl = [
  (* Magic functions *)
  "die";
  "eval";
  "exit";
  "__halt_compiler";
  "echo";
  "print";
]

(* todo: need to have a flag for that. Doing that by default breaks
 * regression tests in cmf as many builtins become unknown.
 *)
let _do_not_decl_hack = [
  (* Manually typed in functions.hhi *)
  "array_fill";
  "array_key_exists";
  "chr";
  "count";
  "dechex";
  "fb_bsdiff";
  "fb_bspatch";
  "func_get_args";
  "implode";
  "is_array";
  "isset";
  "ord";
  "strip_tags";
  "gzcompress";
  "gzdecode";
  "gzdeflate";
  "gzencode";
  "gzinflate";
  "gzuncompress";
  "sort";
  "rsort";
  "asort";
  "arsort";
  "ksort";
  "krsort";
  "usort";
  "uasort";
  "uksort";
  "intval";
  "doubleval";
  "floatval";
  "strval";
  "boolval";
  "fb_get_enum_values";
  "fb_get_enum_names";
  "get_class_constants";
  (* Manually typed in printf.hhi *)
  "printf";
  "sprintf"
]

let ns_regexp = Str.regexp "\\"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo: use the 'type' field *)
let string_of_arg = function
  | J.Object xs ->
    (match List.assoc "name" xs with
    | J.String name ->
      let ref_str = 
        if List.mem_assoc "ref" xs
        then "&"
        else ""
      in
      let val_str =
        try 
          (match List.assoc "value" xs with
          | J.String v ->
            (* certain default values are not allowed in PHP land *)
            let v =
              match v with
              | "TimeStamp::Current()"
              | "null_array" | "null_string" | "null_resource"
                  -> "null"
              | "empty_array" -> "array()"
              | _ when v =~ ".*|.*" -> "null"
              | _ when v =~ ".*\\$\\$" -> "null"

              | "INT_MAX" -> "PHP_INT_MAX"
              | _ when v =~ "k_\\(.*\\)" -> Common.matched1 v

              | _ -> v
            in
            spf " = %s" v
          | _ -> failwith "wrong json format"
          )
        with Not_found -> ""
      in

      spf "%s$%s%s" ref_str name val_str
    | _ -> failwith "wrong json format"
    )
  | _ -> failwith "wrong json format"

(*****************************************************************************)
(* Main conversion *)
(*****************************************************************************)

(*
 * note: was useful to run jsonpat on the json files to extract its schema via
 * $> jsonpat -type hphp/idl/*.json
 * (e.g. to know whether some fields are always defined or not)
 *)
let generate_builtins idlfile pr_hook =
  pr_hook "<?hh     /* -*- php -*- */";
  pr_hook special_comment;

  let json = Json_in.load_json idlfile in
  match json with
  | J.Object [
    "preamble", _;
    "consts", J.Array consts;
    "funcs", J.Array funcs;
    "classes", J.Array classes;
  ]
  | J.Object [
    "consts", J.Array consts;
    "funcs", J.Array funcs;
    "classes", J.Array classes;
  ] -> 

    (* Constants:
     * 'name' => name of the constant
     * 'type' => type of the constant
     * 'desc' => additional note about this constant's schema
     *)
    consts |> List.iter (function 
    | J.Object (("name", J.String _name)::_) ->

      (* we don't really care about the value for static analysis purposes,
       * and certain constants have no values in the HipHop IDL files so just
       * put '0' here
       * 
       * todo: use the 'type' field to type those constants.
       * 
       * update: many constants are no more defined in .idl files so
       * let's skip them here. pfff/data/php_stdlib/builtin_constants.php
       * is now used instead.
       *)
      (* pr_hook (spf "define('%s', 0);" name) *)
      ()
    | _ -> failwith "wrong json format"
    );

   (*
    * array (
    *   'name'   => name of the function
    *   'desc'   => description of the function's purpose
    *   'flags'  => attributes of the function, see base.php for possible values
    *   'opt'    => optimization callback function's name for compiler
    *   'note'   => additional note about this function's schema
    *   'return' =>
    *      array (
    *        'type'  => return type, use Reference for ref return
    *        'desc'  => description of the return value
    *      )
    *   'args'   => arguments
    *      array (
    *        'name'  => name of the argument
    *        'type'  => type of the argument, use Reference for output parameter
    *        'value' => default value of the argument
    *        'desc'  => description of the argument
    *      )
    * )
    *)
    funcs |> List.iter (function
    | J.Object (flds) ->
      (match List.assoc "name" flds, 
             List.assoc "args" flds,
             (try List.assoc "flags" flds with Not_found -> J.Array [])
      with
      | J.String name, J.Array args, J.Array flags ->
        if List.mem name do_not_decl
        then ()
        else begin
          let body_str = "" in
          let xs = List.map string_of_arg args in
          let flags = flags |> List.map (function
            | J.String s -> s
            | _ -> failwith "wrong json format"
          )
          in
          let varargs = 
            List.mem "VariableArguments" flags ||
            List.mem "MixedVariableArguments" flags ||
            name = "sscanf"
          in
          let xs =
            if varargs
            then xs @ ["..."]
            else xs
          in
        
          let args_str = Common.join ", " xs in
          if name =~ "^__SystemLib"
          then ()
          else  pr_hook (spf "function %s(%s) { %s}"
                     name args_str body_str)
        end
      | _ -> failwith "wrong json format"
      )
    | _-> failwith "wrong json format"
    );
    
    (* array (
     * 'name'   => name of the class
     * 'desc'   => description of the class's purpose
     * 'flags'  => attributes of the class, see base.php for possible values
     * 'note'   => additional note about this class's schema
     * 'parent' => parent class name, if any
     * 'ifaces' => array of interfaces this class implements
     * 'bases'  => extra internal and special base classes this class requires
     * 'footer' => extra C++ inserted at end of class declaration
     * )
     * 
     * DefineConstant(..)
     * DefineConstant(..)
     * ...
     * DefineFunction(..)
     * DefineFunction(..)
     * ...
     * DefineProperty
     * DefineProperty
     * 
     * array (
     * 'name'  => name of the property
     * 'type'  => type of the property
     * 'flags' => attributes of the property
     * 'desc'  => description of the property
     * 'note'  => additional note about this property's schema
     * )
     *)
    classes |> List.iter (function
    | J.Object (flds) ->

      (* todo: use parent, ifaces *)
      (match List.assoc "name" flds,
             (try List.assoc "consts" flds with Not_found -> J.Array []),
             List.assoc "funcs" flds,
             List.assoc "flags" flds with
      | J.String name, J.Array consts, J.Array funcs, J.Array flags ->
        let name = match Str.split ns_regexp name with
          | [_] -> name
          | ["HH"; name] -> name
          | _ -> failwith ("can't use namespaces other than HH: " ^ name) in

        let flags = flags |> List.map (function
          | J.String s -> s
          | _ -> failwith "wrong json format"
        )
        in

        begin
        let has_parent = List.mem_assoc "parent" flds in
        let extends_str =
          if has_parent
          then 
            (match List.assoc "parent" flds with
            | J.String name -> spf " extends %s" name
            | _ -> failwith "wrong json format"
            )
          else ""
        in
        let abstract_str =
          if List.mem "IsAbstract" flags
          then "abstract "
          else ""
        in
        pr_hook (spf "%sclass %s%s {" abstract_str name extends_str);

        (* quite similar to consts above *)
        consts |> List.iter (function
        | J.Object (("name", J.String name)::_) ->
          pr_hook (spf "  const %s = 0;" name)
        | _ -> failwith "wrong json format"
        );

        (* quite similar to funcs above *)
        funcs |> List.iter (function
        | J.Object (flds) ->
          (match List.assoc "name" flds, 
                 List.assoc "args" flds,
                 try List.assoc "flags" flds with Not_found -> J.Array []
           with
          | J.String name, J.Array args, J.Array flags ->

            if List.mem name do_not_decl
            then ()
            else begin
            let body_str =
              if name = "__construct" && has_parent
              then "parent::__construct();"
              else ""
            in
            let xs = List.map string_of_arg args in
            let flags = flags |> List.map (function
              | J.String s -> s
              | _ -> failwith "wrong json format"
            )
            in
            let static_str =
              if List.mem "IsStatic" flags
              then "static "
              else ""
            in
            let varargs = 
              List.mem "VariableArguments" flags ||
                List.mem "MixedVariableArguments" flags ||
                name = "sscanf"
            in
            let xs =
              if varargs
              then xs @ ["..."]
              else xs
            in
        
            let args_str = Common.join ", " xs in
            pr_hook (spf "  %spublic function %s(%s) { %s}"
                       static_str name args_str body_str)
            end
          | _ -> failwith "wrong json format"
          )
        | _-> failwith "wrong json format"
        );

        pr_hook "}";
        end
      | _ -> 
        failwith "wrong json format"
      )
    | _ -> failwith "wrong json format"
    );
  | _ -> failwith "wrong json format"


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)



(* generating php_stdlib/ from idl files *)
let generate_php_stdlib ~src (* ~phpmanual_dir *) ~dest = 
  (* todo: put back this feature:
   * let phpdoc_finder = 
   *   Phpmanual_xml.build_doc_function_finder phpmanual_dir in
   *)

  let files = Common.files_of_dir_or_files_no_vcs_nofilter [src] in
  let files = files |> List.filter (fun f -> f =~ ".*\\.idl\\.json$") in

(*  if not (Common2.command2_y_or_no("rm -rf " ^ dest))
  then failwith "ok we stop";
*)
  Common.command2("mkdir -p " ^ dest);
  files |> List.iter (fun file -> 
    pr2 (spf "processing: %s" file);
      let (_d,b,_e) = Common2.dbe_of_filename file in
      if (* parse error on PHP_INT_MAX, which is why it's processed
          * specially in pfff/data/php_stdlib/Makefile
          *)
         b = "constants.idl" ||
         (* has an 'extension' field *)
         b = "phar.idl"
      then ()
      else begin
        let target = Common2.filename_of_dbe (dest, "builtins_" ^ b, "php") in
        Common.with_open_outfile target (fun (pr_no_nl, _chan) ->
          let pr_hook s = pr_no_nl (s ^ "\n") in
          generate_builtins file pr_hook;
        );
        let ast = Parse_php.parse_program target in
        (match ast with
        | [Cst_php.FinalDef _] ->
          Common.command2 (spf "rm -f %s" target);
        | _ -> ()
        );
      end
  );
  ()


(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  (* e.g. 
   * ./pfff_misc -generate_php_stdlib tests/idl/ 
   *    ~/local/software/phpdoc-svn/reference/   data/php_stdlib2
   * to test do:
   * ./pfff_misc -generate_php_stdlib tests/idl/array.idl.php
   *   ~/local/software-src/phpdoc-svn/reference/array/   data/php_stdlib2
   *)
  "-generate_php_stdlib", " <src_idl> <src_phpmanual> <dest>",
  Common.mk_action_2_arg (fun src (*phpmanual_dir*) dest ->
    generate_php_stdlib ~src (*~phpmanual_dir*) ~dest);
  "-builtins_of_idl", " <idl>",
  Common.mk_action_1_arg (fun idlfile ->
    generate_builtins idlfile Common.pr
  );    
]
(*e: builtins_php.ml *)
