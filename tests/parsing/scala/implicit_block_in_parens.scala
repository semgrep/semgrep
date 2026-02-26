def test1a() =
  foo(x =>
    (3))

def test1b() = {
  foo(x =>
    (3))
}

def test2() =
  foo(value =>
    if (value.size > 0)
      Some(value)
    else None)

def test3() =
  new Foo(
    url =
      "hi",
    user = "bob"
  )
