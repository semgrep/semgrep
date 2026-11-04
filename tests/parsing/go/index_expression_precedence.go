package indices

func indices() {
  use(table[key])
  use(table[key][next])
  use(table[key + offset * stride])
  use(table[key].field)
  use(table[key](arg))
  use(table[:end])
  use(pair[int, string])
  use(convert[int](arg))
}
