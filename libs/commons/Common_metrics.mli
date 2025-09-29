type metered_error = ..
(** [metered_error] is an extensible type that can then be used to define errors
    that then can be metered with {!meter_error}. See that function for more
    info. *)

val meter_exception : exn -> unit
(** [meter_exception exn] will record an exception being raised. This is useful
    for tracking how often very common or noisy exceptions are raised in a
    service. For example an exception that occurs many times a second, logging
    an exception or recording it onto a span might create too much noise, or use
    up too much storage since traditionally those signals take up much more
    space. See {!meter_error} if you want to record an error that may not be an
    exception. *)

val meter_error : metered_error -> unit
(** [meter_error] will record a {!metered_error} that has occurred. This is
    useful if you want to record an error condition that's very common or noisy,
    but doesn't make sense as an exception. For example you may want to record
    every time you try checking if a number is prime and it's not. This would
    take up too much storage as a log or added to a trace span, but makes sense
    as an error metric, though it's not an exception. Ex:
{[
type Ometrics.metered_error += Prime_check_failed

(* ... *)
Ometrics.meter_error Prime_check_failed
]}
 *)
