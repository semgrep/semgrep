class A
  def foo(x,t) return 1 end
end

class B < A
  def foo(x,y)
    super
  end
end
