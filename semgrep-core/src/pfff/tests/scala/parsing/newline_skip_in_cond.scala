object Foo {

  //implicit val DurationReader: Reader[Duration] = new MapStringReader( s =>
  def foo() {
    if (s.charAt(0) == 'i' &&
        s.charAt(1) == 'n' &&
         s.charAt(2) == 'f'
        && s.length() == 3){
      Duration.Inf
    } else if (s.charAt(0) == 'u' &&
               s.charAt(1) == 'n' &&
               s.length() == 5){
      Duration.Undefined
    }else Duration(upickle.core.Util.parseLong(s, 0, s.length()), TimeUnit.NANOSECONDS)
  }
}
