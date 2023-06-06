
object A:
  extension (a: File)
    inline def x: String = "hi" 
  end extension

  def foo(x: T) = x
  end def

end foo