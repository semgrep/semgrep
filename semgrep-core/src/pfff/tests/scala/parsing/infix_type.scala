package zio.test.mock

import zio.test.Assertion
import zio.{=!=, Has, IO, LightTypeTag, Tag, taggedIsSubtype, taggedTagType}

import java.util.UUID

/**
 * A `Capability[R, I, E, A]` represents a capability of environment `R` that takes an input `I`
 * and returns an effect that may fail with an error `E` or produce a single `A`.
 *
 * To represent polymorphic capabilities you must use one of lazy `Capability.Poly` types which
 * allow you to delay the declaration of some types to call site.
 *
 * To construct capability tags you should start by creating a `Mock[R]` and extend publicly
 * available `Effect`, `Method`, `Sink` or `Stream` type members.
 */
protected[mock] abstract class Capability[R <: Has[_]: Tag, I: Tag, E: Tag, A: Tag](val mock: Mock[R])
    extends Capability.Base[R] { self =>

  val inputTag: LightTypeTag  = taggedTagType(implicitly[Tag[I]])
  val errorTag: LightTypeTag  = taggedTagType(implicitly[Tag[E]])
  val outputTag: LightTypeTag = taggedTagType(implicitly[Tag[A]])

  def apply()(implicit ev1: I =:= Unit, ev2: A <:< Unit): Expectation[R] =
    Expectation.Call[R, I, E, A](
      self,
      Assertion.isUnit.asInstanceOf[Assertion[I]],
      ((_: I) => IO.unit).asInstanceOf[I => IO[E, A]]
    )
}
