(* will generate some java.lang.xxx.java files in dst by just extracting
 * the class definitions from src. Useful to build pfff/data/java_stdlib
 *)
val extract_from_sources :
  src:Common.dirname -> dst:Common.dirname -> Common.filename list -> unit
