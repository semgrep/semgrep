class ['a] oarray n el =
  object(o: 'o)
    inherit ['a] osequence

    val data = Array.make n el

    method empty = raise Todo
    method add (i,v)  =
      Array.set data i v;
      o
  end
