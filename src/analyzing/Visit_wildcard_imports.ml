(* Brandon Wu
 *
 * Copyright (C) 2023 r2c
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

module G = AST_generic

class ['self] import_visitor =
  object (_self : 'self)
    inherit ['self] AST_generic.iter

    method! visit_directive_kind store dk =
      match dk with
      | ImportAll (_t, DottedName name, _t') ->
          (* The purpose of these fake tokens is to be used to supplement the matching process,
             by augmenting unqualified names with potential implicit prefixes.
             We don't want the location data from these tokens we're taking, for the qualified
             wildcard import's path. This is because then, when matching using those prefixes,
             we might cause the match to create a nonsensical range, from the originating location
             of the wildcard import to the use.

             Since these are strictly meant to supplement matching, we will just wipe the location
             data from each token in a wildcard import.
          *)
          let name_with_fake_toks =
            Common.map
              (fun (s, t) ->
                (s, Tok.fix_location (fun _ -> Tok.fake_location) t))
              name
          in
          Common.push name_with_fake_toks store
      | ImportAll (_, FileName _, _)
      | _ ->
          ()
  end

let visit : AST_generic.any -> G.ident list list =
  let v = new import_visitor in
  fun any ->
    let ids = ref [] in
    v#visit_any ids any;
    !ids
  [@@profiling]
