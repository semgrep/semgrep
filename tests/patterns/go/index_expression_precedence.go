package indices

func indices() {
  // ERROR: match
  use(table[key][next])
  // ERROR: match
  use(table[key + offset * stride][next])
  use(table[key])
  use(table[:end])
  use(pair[int, string])
  use(convert[int](arg))
}
