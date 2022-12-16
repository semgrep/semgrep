// symbols are actually not supported in Scala 3 and deprecated
// in Scala 2.13. You have to use Symbol("foo") instead of 'foo


package com.test.scala.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route

object TestRoute {
  val testRoute: Route =
    path("login") {
      get {
        parameters('test.as[String], 'redirPATH.?) {(test, redirPATH) =>
          complete(StatusCodes.OK)
        }
      }
    }
}
