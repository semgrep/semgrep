package app

import scalatags.Text.all._

object MinimalApplication extends cask.MainRoutes {

  val bootstrap = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.css"

  @cask.get("/")
  def hello() = doctype("html")(
    html(
      head(link(rel := "stylesheet", href := bootstrap)),
      body(
        div(cls := "container")(
          h1("Scala Chat!"),
          div(
            p(b("alice"), " ", "Hello World!"),
            p(b("bob"), " ", "I am cow, hear me moo") // there was a trailing comma here before
          ),
          div(
            input(`type` := "text", placeholder := "User name"),
            input(`type` := "text", placeholder := "Write a message!")
          )
        )
      )
    )
  )

  initialize()
}
