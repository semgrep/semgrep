object Foo {

def unbox(res: Throwable) = res match{
    case e: java.util.concurrent.ExecutionException
      if e.getMessage == "Boxed Error" || e.getMessage == "Boxed Exception" =>
      e.getCause
    case r => r
  }
}
