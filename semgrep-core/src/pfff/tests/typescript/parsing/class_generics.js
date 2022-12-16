
class C<X> extends D<X> { }
class C<X> extends D<E<X>,E<X>> { }
class C<X> extends D<E<F<X>>,E<F<X>>> { }
class C<X> extends D<E<F<G<X>>>,E<F<G<X>>>> { }

class A<X> {
  foo<Y>(y:Y):X { }
}

var a:A<number>;
