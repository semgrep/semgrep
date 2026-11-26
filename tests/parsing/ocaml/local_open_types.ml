type pair = M.(t * u)
type applied = F(X).(t list)
type row = M.[ `A | `B of t ]
type package = M.(module S)
type dotted = M.N.(t)
type applied_dotted = F(X).N.(t)
