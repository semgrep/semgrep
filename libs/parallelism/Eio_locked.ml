type 'a t = { mutable data : 'a; mutex : Eio.Mutex.t [@opaque] }
[@@deriving show]

let create data = { data; mutex = Eio.Mutex.create () }

(* All the below functions are *lock-taking* operations.
 *
 * I (brandon) thought a little bit about whether or not `read` should
 * be allowed to be unprotected, but was told this was unsafe, per this
 * discussion:
 * https://github.com/semgrep/semgrep-proprietary/pull/4090#discussion_r2164529565
 *)

let with_lock t f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let set v = t.data <- v in
      f ~set t.data)

let read t = Eio.Mutex.use_ro t.mutex (fun () -> t.data)
let set t v = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.data <- v)
