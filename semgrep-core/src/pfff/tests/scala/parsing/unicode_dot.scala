trait AList[K[L[x]]] {
  def transform[M[_], N[_]](value: K[M], f: M ~> N): K[N]
  def traverse[M[_], N[_], P[_]](value: K[M], f: M ~> (N ∙ P)#l)(
      implicit np: Applicative[N]
  ): N[K[P]]
  def foldr[M[_], A](value: K[M], f: (M[_], A) => A, init: A): A
}
