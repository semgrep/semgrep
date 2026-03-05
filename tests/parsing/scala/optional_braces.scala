// Scala 3 "optional braces"
// https://docs.scala-lang.org/scala3/reference/other-new-features/indentation.html#optional-braces-for-method-arguments
//
// ColonArgument ::= colon [LambdaStart] indent (CaseClauses | Block) outdent

// colon followed by indented block (no LambdaStart)
val evens = numbers.filter:
  _ % 2 == 0

// colon with LambdaStart on same line, body indented
val doubled = xs.map: x =>
  x * 2

// colon with parenthesized LambdaStart and multiple statements in indented block
val result = xs.foldLeft(0): (acc, x) =>
  val y = x * x
  acc + y

// colon after parenthesized arguments
def render(hasClas: Boolean)(using ctx: Context) =
  st.nav(id := "topnav")(
    Option.when(ctx.noBot):
      val url = langHref(routes.Home.url)
      st.section(
        linkTitle(url, trans.site.home())
      )
  )

// colon argument nested inside parens with multi-statement block
def buildMenu(isAdmin: Boolean)(using env: Env) =
  val sidebar = env.config.get("sidebar")
  div(cls := "menu", id := "main")(
    items.collect:
      case item if item.visible =>
        val icon = loadIcon(item.iconPath)
        li(cls := "entry")(
          a(href := item.url)(icon, span(item.label)),
          ul(role := "list")(
            item.children.map: child =>
              li(a(href := child.url)(child.label))
          )
        )
  )

// colon argument inside def body, followed by sibling def
def foo =
  xs.map: x =>
    x * 2

def bar =
  xs.filter:
    _ > 0

// colon argument inside def with multiple statements after
def baz =
  val y = xs.map: x =>
    x + 1
  y.size

// colon with HkTypeParamClause LambdaStart
val polyFn = withPoly: [T] =>
  val x = identity[T]
  x

// chained colon arguments
val res = xs
  .filter: x =>
    x > 0
  .map: x =>
    x * 2
