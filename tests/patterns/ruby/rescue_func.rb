module SomeModule
  # ERROR:
  def someMethod(foo)
    someFunction(foo)
  rescue SomeClass::Error
    errorHandler()
  end
end
