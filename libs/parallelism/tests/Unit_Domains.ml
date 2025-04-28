(* Tests for our Domain module, and for various operations that
 * rely on domain-local state. *)

let t = Testo.create

(* Ensures that when new Domains are spawned, the assigned value
 * is read from the parent. *)
let test_hook_inherit_val () =
  let h = Hook.create 0 in

  (* Confirm that `with_hook_set` scopes the value of h. *)
  Hook.with_hook_set h 1 (fun () -> Alcotest.(check int) __LOC__ (Hook.get h) 1);
  Alcotest.(check int) __LOC__ (Hook.get h) 0;

  (* Confirm that when a domain is spawned, we inherit the value
   * that has been set by the hook versus re-running the initialization
   * function. *)
  let n =
    Hook.with_hook_set h 42 (fun () ->
        Domain.join (Domain.spawn (fun () -> Hook.get h)))
  in
  Alcotest.(check int) __LOC__ n 42

let tests =
  Testo.categorize "Domains" [ t "test_hook_inherit_val" test_hook_inherit_val ]
