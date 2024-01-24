(* Why does this exist?
 * Cohttp_lwt_unix is functorized by the implementation of the `Net` module, which
 * determines how the library is to handle remote interactions -- in particular,
 * SSL/TLS verification.
 * All of these settings are ordinarily set to sensible defaults, but one thing
 * it does not do in particular, which the Python `requests` library does, is read
 * from an external env variable to whitelist some CA certs.
 * Unfortunately, in the Cohttp_lwt_unix module, almost every single function takes
 * in an optional parameter of this `ctx` value, which has a default value. If we
 * want to change that `ctx` value, we would need to update every single call site,
 * and keep doing that in perpetuity.
 * It is easier to create a new functor, which allows us to produce an instance of
 * Cohttp_lwt_unix, but one where the `ctx` default is different. That is what this
 * module does.

 *)
module Make (Input : sig
  val ctx : Conduit_lwt_unix.ctx
end) =
struct
  module Net_with_ctx = struct
    include Cohttp_lwt_unix.Net

    let default_ctx =
      {
        Cohttp_lwt_unix.Net.resolver = Resolver_lwt_unix.system;
        ctx = Input.ctx;
      }
  end

  module Client : Cohttp_lwt.S.Client with type ctx = Cohttp_lwt_unix.Net.ctx =
    Cohttp_lwt.Make_client (Net_with_ctx.IO) (Net_with_ctx)
end
