open Common


type subsystem = SubSystem of string
type dir = Dir of string

let string_of_subsystem (SubSystem s) = s
let string_of_dir (Dir s) = s

type tree_reorganization = (subsystem * dir list) list

let dir_to_dirfinal (Dir s) =
  Str.global_replace (Str.regexp "/") "___" s

(*
let dirfinal_of_dir s =
  Dir (Str.global_replace (Str.regexp "___") "/" s)
*)


let all_subsystem reorg =
  reorg |> List.map fst |> List.map string_of_subsystem
let all_dirs reorg =
  reorg |> List.map snd |> List.concat |> List.map string_of_dir

let reverse_index reorg =
  let res = ref [] in
  reorg |> List.iter (fun (SubSystem s1, dirs) ->
    dirs |> List.iter (fun (Dir s2) ->
      push (Dir s2, SubSystem s1) res;
    );
  );
  List.rev !res




let (load_tree_reorganization : Common.filename -> tree_reorganization) =
  fun file ->
  let xs = Simple_format.title_colon_elems_space_separated file in
  xs |> List.map (fun (title, elems) ->
    SubSystem title, elems |> List.map (fun s -> Dir s)
  )

let debug_source_tree = false

let change_organization_dirs_to_subsystems reorg basedir =
  let cmd s =
    if debug_source_tree
    then pr2 s
    else Common.command2 s
  in
  reorg |> List.iter (fun (SubSystem sub, dirs) ->
    if not debug_source_tree
    then Common2.mkdir (spf "%s/%s" basedir sub);

    dirs |> List.iter (fun (Dir dir) ->
      let dir' = dir_to_dirfinal (Dir dir) in
      cmd (spf "mv %s/%s %s/%s/%s" basedir dir   basedir sub dir')
    );
  );
  ()

let change_organization_subsystems_to_dirs reorg basedir =
  let cmd s =
    if debug_source_tree
    then pr2 s
    else Common.command2 s
  in
  reorg |> List.iter (fun (SubSystem sub, dirs) ->
    dirs |> List.iter (fun (Dir dir) ->
      let dir' = dir_to_dirfinal (Dir dir) in
      cmd (spf "mv %s/%s/%s %s/%s" basedir sub dir' basedir dir)
    );
    if not debug_source_tree
    then Unix.rmdir (spf "%s/%s" basedir sub);
  );
  ()



let (change_organization:
       tree_reorganization -> Common.filename (* dir *) -> unit) =
  fun reorg dir ->
  pr2_gen reorg;
  pr2_gen dir;


  let subsystem_bools =
    all_subsystem reorg
    |> List.map (fun s -> (Sys.file_exists (Filename.concat dir s)))
  in
  let dirs_bools =
    all_dirs reorg
    |> List.map (fun s -> (Sys.file_exists (Filename.concat dir s)))
  in
  match () with
  | _ when Common2.and_list subsystem_bools ->
      assert (not (Common2.or_list dirs_bools));
      change_organization_subsystems_to_dirs reorg dir;
  | _ when Common2.and_list dirs_bools ->
      assert (not (Common2.or_list subsystem_bools));
      change_organization_dirs_to_subsystems reorg dir;
  | _ -> failwith "have a mix of subsystem and dirs, wierd"




let subsystem_of_dir2 (Dir dir) reorg  =
  let index = reverse_index reorg in
  let dirsplit = Common.split "/" dir in
  let index =
    index |> List.map (fun (Dir d, sub) -> Common.split "/" d, sub)
  in
  try
    index |> List.find (fun (dirsplit2, _sub) ->
      let len = List.length dirsplit2 in
      Common2.take_safe len dirsplit = dirsplit2
    ) |> snd
  with Not_found ->
    pr2 (spf "Cant find %s in reorganization information" dir);
    raise Not_found

let subsystem_of_dir a b =
  Common.profile_code "subsystem_of_dir" (fun () -> subsystem_of_dir2 a b)
