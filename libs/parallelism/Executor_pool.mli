(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(** An executor pool distributes jobs (functions to execute) among a pool of domain workers (threads).

    Domains are reused and can execute multiple jobs concurrently.
    Jobs are queued up if they cannot be started immediately due to all workers being busy.

    [Executor_pool] is the recommended way of leveraging OCaml 5's multicore capabilities.
    It is built on top of the low level [Eio.Domain_manager].

    Usually you will only want one pool for an entire application,
    so the pool is typically created when the application starts:

    {[
      let () =
        Eio_main.run @@ fun env ->
        Switch.run @@ fun sw ->
        let pool =
          Executor_pool.create
            ~sw (Eio.Stdenv.domain_mgr env)
            ~domain_count:4
        in
        main ~pool
    ]}

    The pool starts its domain workers (threads) immediately upon creation.

    NOTE: this is a modified version of the eio Executor pool. See the ml
    prelude for why
*)

type t
(** An executor pool. *)

val create : sw:Eio.Switch.t -> domain_count:int -> _ Eio.Domain_manager.t -> t
(** [create ~sw ~domain_count dm] creates a new executor pool.

    The executor pool will not block switch [sw] from completing;
    when the switch finishes, all domain workers and running jobs are cancelled.

    @param domain_count The number of domain workers to create.
                        The total number of domains should not exceed {!Domain.recommended_domain_count} or the number of cores on your system.
                        Additionally, consider reducing this number by 1 if your original domain will be performing CPU intensive work at the same time as the Executor_pool.
*)

val submit : t -> weight:float -> (unit -> 'a) -> ('a, exn) result
(** [submit t ~weight fn] runs [fn ()] using this executor pool.

    The job is added to the back of the queue.

    @param weight This value represents the anticipated proportion of a CPU core used by the job.
                  This value must be >= 0.0 and <= 1.0; Example: given an IO-bound job that averages 2% of a CPU core, pass [~weight:0.02].
                  Each domain worker starts new jobs until the total [~weight] of its running jobs reaches 1.0
 *)

val submit_exn : t -> weight:float -> (unit -> 'a) -> 'a
(** Same as {!submit} but raises if the job fails. *)

val submit_fork :
  sw:Eio.Switch.t -> t -> weight:float -> (unit -> 'a) -> 'a Eio.Promise.or_exn
(** Same as {!submit} but returns immediately, without blocking. *)
