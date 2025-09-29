package controllers

import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server._
import org.http4s.circe._
import org.http4s.multipart.Multipart
import io.circe.generic.auto._

object Http4sController {
  // ruleid: test
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "users" / userId =>
      Ok(s"User ID: $userId")
  }
}
